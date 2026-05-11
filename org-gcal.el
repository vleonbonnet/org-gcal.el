;;; org-gcal.el --- Org sync with Google Calendar -*- lexical-binding: t -*-

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/kidd/org-gcal.el
;; Version: 0.4.3
;; Maintainer: Raimon Grau <raimonster@gmail.com>
;; Package-Requires: ((aio "1.0") (alert "1.2") (emacs "26.1") (oauth2-auto "20240326.2225") (org "9.3") (persist "0.8") (request "20190901") (request-deferred "20181129"))
;; Copyright (C) :2014 myuhe all rights reserved.
;; Keywords: convenience,

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:
;;
;; Put the org-gcal.el to your
;; load-path.
;; Add to .emacs:
;; (require 'org-gcal)
;;
;;; Changelog:
;; 2014-01-03 Initial release.

(require 'alert)
(require 'json)
(require 'aio)
(require 'oauth2-auto)
(require 'org)
(require 'ol nil t)
(require 'org-archive)
(require 'org-clock)
(require 'org-element)
(require 'org-generic-id)
(require 'org-id)
(require 'parse-time)
(require 'persist)
(require 'request-deferred)
(require 'cl-lib)
(require 'rx)
(require 'subr-x)

;; Customization
;;; Code:

(defgroup org-gcal nil "Org sync with Google Calendar"
  :group 'org)

(defcustom org-gcal-up-days 30
  "Number of days to get events before today."
  :group 'org-gcal
  :type 'integer)

(defcustom org-gcal-down-days 60
  "Number of days to get events after today."
  :group 'org-gcal
  :type 'integer)

(defcustom org-gcal-auto-archive t
  "If non-nil, old events archive automatically."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-dir
  (concat user-emacs-directory "org-gcal/")
  "File in which to save token."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-token-file
  (expand-file-name ".org-gcal-token" org-gcal-dir)
  "File in which to save token."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-client-id nil
  "Client ID for OAuth."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-client-secret nil
  "Google calendar secret key for OAuth."
  :group 'org-gcal
  :type 'string)

(defvaralias 'org-gcal-file-alist 'org-gcal-fetch-file-alist)

(defcustom org-gcal-fetch-file-alist nil
  "\
Association list mapping calendar IDs to sync targets.  Each entry is
of the form (CALENDAR-ID . TARGET), where TARGET is either:

  FILE       — a path to an Org file (events are appended at top level), or
  (FILE . HEADING) — events are inserted as children of HEADING in FILE.

When HEADING is specified and does not yet exist in FILE, it is created
as a top-level heading automatically.

For each calendar-id, `org-gcal-fetch' and `org-gcal-sync' will retrieve
new events on the calendar and insert them into the file."
  :group 'org-gcal
  :type '(alist :key-type (string :tag "Calendar Id")
                :value-type (choice
                             (file :tag "Org file")
                             (cons :tag "Org file with heading"
                                   (file :tag "Org file")
                                   (string :tag "Heading")))))

(defun org-gcal--calendar-file (calendar-id-file)
  "Extract the file path from CALENDAR-ID-FILE.
CALENDAR-ID-FILE is a cons from `org-gcal-fetch-file-alist'."
  (let ((val (cdr calendar-id-file)))
    (if (consp val) (car val) val)))

(defun org-gcal--calendar-heading (calendar-id-file)
  "Extract the optional heading from CALENDAR-ID-FILE, or nil.
CALENDAR-ID-FILE is a cons from `org-gcal-fetch-file-alist'."
  (let ((val (cdr calendar-id-file)))
    (when (consp val) (cdr val))))

(defun org-gcal--entry-under-heading-p (heading)
  "Return non-nil if point is under a top-level heading titled HEADING.
If HEADING is nil, return t (matches any position, for backward
compatibility with configs that don't specify headings)."
  (if (null heading)
      t
    (save-excursion
      (condition-case nil
          (progn
            (while (org-up-heading-safe))
            (string= heading (org-get-heading t t t t)))
        (error nil)))))

(defcustom org-gcal-account nil
  "Default Google account email used for OAuth authentication.
When nil, falls back to the first calendar ID in
`org-gcal-fetch-file-alist'.  Per-calendar overrides can be set
via `org-gcal-account-alist'."
  :group 'org-gcal
  :type '(choice (const :tag "Use first calendar ID" nil)
                 (string :tag "Google account email")))

(defcustom org-gcal-account-alist nil
  "Alist mapping calendar IDs to Google account emails.
Each entry is (CALENDAR-ID . ACCOUNT-EMAIL).  When a calendar ID
is not found here, `org-gcal-account' (or the first calendar ID
in `org-gcal-fetch-file-alist') is used as the default."
  :group 'org-gcal
  :type '(alist :key-type (string :tag "Calendar ID")
                :value-type (string :tag "Google account email")))

(defun org-gcal--account (&optional calendar-id)
  "Return the Google account email for CALENDAR-ID.
Look up CALENDAR-ID in `org-gcal-account-alist'; if not found,
fall back to `org-gcal-account' or the first calendar ID in
`org-gcal-fetch-file-alist'."
  (or (cdr (assoc calendar-id org-gcal-account-alist))
      org-gcal-account
      (caar org-gcal-fetch-file-alist)))

(defcustom org-gcal-logo-file nil
  "Org-gcal logo image filename to display in notifications."
  :group 'org-gcal
  :type 'file)

(defcustom org-gcal-fetch-event-filters '()
  "Predicate functions to filter calendar events.
Predicate functions take an event, and if they return nil the
   event will not be fetched."
  :group 'org-gcal
  :type 'list)

(defcustom org-gcal-strip-html-descriptions nil
  "Whether to strip HTML tags and entities from event descriptions.
When non-nil, HTML in event descriptions fetched from Google Calendar
is converted to plain text before insertion into Org files.

This is the global default.  Use `org-gcal-strip-html-descriptions-overrides'
to override this setting for specific calendars."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-strip-html-descriptions-overrides nil
  "Per-calendar overrides for `org-gcal-strip-html-descriptions'.
An alist mapping calendar IDs to booleans.  Calendars listed here
use the specified value instead of the global default.

For example, to strip HTML globally but preserve it for a shared
calendar:

  (setq org-gcal-strip-html-descriptions t)
  (setq org-gcal-strip-html-descriptions-overrides
        \\='((\"shared-calendar@group.calendar.google.com\" . nil)))"
  :group 'org-gcal
  :type '(alist :key-type (string :tag "Calendar ID")
                :value-type (boolean :tag "Strip HTML")))

(defcustom org-gcal-notify-p t
  "If nil no more alert messages are shown for status updates."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-update-cancelled-events-with-todo t
  "If 't', mark cancelled events with the TODO keyword in
'org-gcal-cancelled-todo-keyword'."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-cancelled-todo-keyword "CANCELLED"
  "TODO keyword to use for cancelled events."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-local-timezone nil
  "Org-gcal local timezone. timezone value should use 'TZ
database name', which can be found in
'https://en.wikipedia.org/wiki/List_of_tz_database_time_zones'."
  :group 'org-gcal
  :type 'string)

(defvaralias 'org-gcal-remove-cancelled-events 'org-gcal-remove-api-cancelled-events)
(defcustom org-gcal-remove-api-cancelled-events 'ask
  "Whether to remove Org-mode headlines for events cancelled in Google Calendar.

The events will always be marked cancelled before they're removed if
'org-gcal-update-cancelled-events-with-todo' is true."
  :group 'org-gcal
  :type '(choice
          (const :tag "Never remove" nil)
          (const :tag "Prompt whether to remove" ask)
          (const :tag "Always remove without prompting" t)))

(defcustom org-gcal-remove-events-with-cancelled-todo nil
  "Whether to attempt to remove Org-mode headlines for cancelled events.
Specifically effects events marked with 'org-gcal-cancelled-todo-keyword'.

By default, this is set to nil so that if you decline removing an event when
'org-gcal-remove-api-cancelled-events' is set to 'ask', you won't be prompted
to remove the event again.  Set to t to override this.

Note that whether a headline is removed is still controlled by
'org-gcal-remove-api-cancelled-events'."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-managed-newly-fetched-mode "gcal"
  "Default value of 'org-gcal-managed-property' on newly-fetched events.

This is the value set on events fetched from a calendar by 'org-gcal-sync' and
'org-gcal-fetch'.

Values:

- “org”: Event is intended to be managed primarily by org-gcal. These events
  will be pushed to Google Calendar by 'org-gcal-sync', 'org-gcal-sync-buffer',
  and 'org-gcal-post-at-point' if they have been modified in the Org file. If
  the ETag is out of sync with Google Calendar, the Org headline will still be
  updated from Google Calendar.
- “gcal”: Event is intended to be managed primarily by org-gcal. These events
  will not be pushed to Google Calendar by bulk update functions like
  'org-gcal-sync', 'org-gcal-sync-buffer'. When running
  'org-gcal-post-at-point', the user will be prompted to approve pushing the
  event by default."
  :group 'org-gcal
  :type '(choice
          (const :tag "Event managed on Google Calendar" "gcal")
          (const :tag "Event managed in Org file" "org")))

(defcustom org-gcal-managed-update-existing-mode "gcal"
  "Default value of 'org-gcal-managed-property' for existing events without it.

This is the value set on existing entries containing calendar events when they
are updated by 'org-gcal-sync', 'org-gcal-fetch', or 'org-gcal-post-at-point'
and don't yet have a value for 'org-gcal-managed-property' set.

Values: see 'org-gcal-managed-newly-fetched-mode'."
  :group 'org-gcal
  :type '(choice
          (const :tag "Event managed on Google Calendar" "gcal")
          (const :tag "Event managed in Org file" "org")))

(defcustom org-gcal-managed-create-from-entry-mode "org"
  "Default value of 'org-gcal-managed-property' when creating event from entry.

This is the value set when 'org-gcal-post-at-point' creates a Google Calendar
event from an Org-mode entry. This is used when 'org-gcal-calendar-id-property'
or 'org-gcal-entry-id-property' is missing from an entry. If these are present,
'org-gcal-managed-update-existing-mode' is used instead.

Values: see 'org-gcal-managed-newly-fetched-mode'."
  :group 'org-gcal
  :type '(choice
          (const :tag "Event managed on Google Calendar" "gcal")
          (const :tag "Event managed in Org file" "org")))

(defcustom org-gcal-managed-post-at-point-update-existing 'prompt
  "Behavior when running 'org-gcal-post-at-point' on existing entries."

  :group 'org-gcal
  :type '(choice
          (const :tag "Never push to Google Calendar" never-push)
          (const :tag "Prompt whether to push to Google Calendar if run manually, never push during syncs" prompt)
          (const :tag "Prompt whether to push to Google Calendar, even during syncs" prompt-sync)
          (const :tag "Always push to Google Calendar" always-push)))

(defcustom org-gcal-recurring-events-mode 'top-level
  "How to treat instances of recurring events not already fetched.

Can be a single mode symbol applied to all calendars, or an alist
mapping calendar IDs to modes for per-calendar control.  When an
alist, use t as a key for the default mode.

Modes:

- `top-level': insert all instances at the top level of the appropriate file for
  the calendar ID in `org-gcal-fetch-file-alist'.
- `nested': insert instances of a recurring event under the Org-mode headline
  containing the parent event.  If a headline for the parent event doesn't exist,
  it will be created.
- `:instances' -- dual-pass sync.  Pass 1 fetches master events
  (singleEvents=false) and creates parent headings with inactive timestamps and
  repeaters.  Pass 2 fetches all instances (singleEvents=true) and inserts them
  as child headings with active timestamps under the parent.  Cancelled instances
  are removed.  Non-recurring events are handled normally with active timestamps.

Per-calendar example:

  \\='((\"cal-id-1\" . :instances)
    (\"cal-id-2\" . top-level)
    (t          . :instances))"
  :group 'org-gcal
  :type '(choice
          (const :tag "Insert at top level" top-level)
          (const :tag "Insert under headline for parent event" nested)
          (const :tag "Master + instances (dual-pass)" :instances)
          (alist :tag "Per-calendar modes"
                 :key-type (choice (string :tag "Calendar ID")
                                   (const :tag "Default" t))
                 :value-type (choice
                              (const :tag "Insert at top level" top-level)
                              (const :tag "Insert under headline for parent event" nested)
                              (const :tag "Master + instances (dual-pass)" :instances)))))

(defun org-gcal--recurring-mode-for-calendar (calendar-id)
  "Return the recurring events mode for CALENDAR-ID.
When `org-gcal-recurring-events-mode' is an alist, look up CALENDAR-ID
and fall back to the t entry or `top-level'."
  (if (listp org-gcal-recurring-events-mode)
      (or (cdr (assoc calendar-id org-gcal-recurring-events-mode))
          (cdr (assoc t org-gcal-recurring-events-mode))
          'top-level)
    org-gcal-recurring-events-mode))

(defcustom org-gcal-after-update-entry-functions nil
  "List of functions to run just before 'org-gcal--update-entry' returns.

This is the function called when an event is created, updated, or deleted. Each
function in the list is called with the following arguments:

- CALENDAR-ID: the calendar ID of the event, as a string.
- EVENT: the event data downloaded from the Google Calendar API and parsed using
  'org-gcal--json-read'.
- UPDATE-MODE: a symbol, one of
  - NEWLY-FETCHED: the event is newly fetched (see
    'org-gcal-managed-newly-fetched-mode').
  - UPDATE-EXISTING: a headline with existing calendar and event IDs is being
    updated (see 'org-gcal-managed-update-existing-mode').
  - CREATE-FROM-ENTRY: a headline without existing calendar and event IDs is
    being updated (see 'org-gcal-managed-create-from-entry-mode')."
  :group 'org-gcal
  :type 'list)

(defcustom org-gcal-entry-id-property "entry-id"
  "\
Org-mode property on org-gcal entries that records the calendar and event ID."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-calendar-id-property "calendar-id"
  "\
Org-mode property on org-gcal entries that records the Calendar ID."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-etag-property "ETag"
  "\
Org-mode property on org-gcal entries that records the ETag."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-managed-property "org-gcal-managed"
  " Org-mode property on org-gcal entries that records how an event is managed.

  For values the property can take, see 'org-gcal-managed-newly-fetched-mode'."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-drawer-name "org-gcal"
  "\
Name of drawer in which event time and description are stored on org-gcal
entries."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-description-mode 'drawer
  "Where to store event descriptions.

When set to `drawer' (the default), descriptions are stored inside
the org-gcal drawer alongside the timestamp.

When set to `body', descriptions are stored in the entry body after
all drawers, before the first sub-heading.  The org-gcal drawer
retains only the timestamp.  In this mode the body region between
drawers and the first sub-heading is owned by org-gcal and will be
replaced on each sync — put user notes in sub-headings instead."
  :group 'org-gcal
  :type '(choice
          (const :tag "Inside org-gcal drawer (default)" drawer)
          (const :tag "In entry body after drawers" body)))

(defcustom org-gcal-description-filter-function nil
  "Optional function to transform event descriptions before insertion.

Called with one argument — the raw description string from the
Google Calendar API — and should return the transformed string.
Useful for stripping boilerplate (e.g., Zoom details) or converting
HTML to Org markup.

Applied during `org-gcal--update-entry' before the description is
written.  NOT applied on the read path, so the stored text is what
gets pushed back to Google Calendar."
  :group 'org-gcal
  :type '(choice
          (const :tag "No filter" nil)
          (function :tag "Filter function")))

(defcustom org-gcal-event-default-duration 5
  "Default duration of events in minutes."
  :group 'org-gcal
  :type 'integer)

(defvar org-gcal--sync-lock nil
  "Set if a sync function is running.")

(defvar org-gcal-token-plist nil
  "Token plist.")

(defcustom org-gcal-default-transparency "opaque"
  "The default value to use for transparency when creating a new event.

See: https://developers.google.com/calendar/v3/reference/events/insert."
  :group 'org-gcal
  :type 'string)

(defun org-gcal-events-url (calendar-id)
  "URL used to request access to events on calendar CALENDAR-ID."
  (format "https://www.googleapis.com/calendar/v3/calendars/%s/events"
          (url-hexify-string calendar-id)))

(defun org-gcal-instances-url (calendar-id event-id)
  "URL used to request access to instances of recurring events.
Returns a URL for recurrent event EVENT-ID on calendar CALENDAR-ID."
  (format "https://www.googleapis.com/calendar/v3/calendars/%s/events/%s/instances"
          (url-hexify-string calendar-id)
          (url-hexify-string event-id)))

(cl-defstruct (org-gcal--event-entry
               (:constructor org-gcal--event-entry-create))
  ;; Entry ID. Created by 'org-gcal--format-entry-id'.
  entry-id
  ;; Optional marker pointing to entry-id.
  marker
  ;; Optional Event resource fetched from server.
  event
  ;; When non-nil, use inactive timestamps for this entry.
  inactive)

(defvar org-gcal--parent-event-cache nil
  "Hash table mapping recurring event IDs to their master event plists.
Populated during pass 1 (:masters) and read during pass 2 (:instances)
of the dual-pass sync.  Dynamically bound around the dual-pass flow.")

(defvar org-gcal--instance-collector nil
  "Hash table mapping recurringEventId to lists of instance event plists.
Populated during pass 2 (:instances) of the dual-pass sync, processed
by `org-gcal--compact-instances' after the event loop.  Dynamically
bound around the dual-pass flow.")

(persist-defvar
  org-gcal--sync-tokens nil
  "Storage for Calendar API sync tokens, used for performing incremental sync.

This is a a hash table mapping calendar IDs (as given in
'org-gcal-fetch-file-alist') to a list (EXPIRES SYNC-TOKEN).  EXPIRES is an
Emacs time value that stores the time after which we should perform a full sync
instead of an incremental sync using the SYNC-TOKEN stored from the Calendar
API.

Persisted between sessions of Emacs.  To clear sync tokens, call
'org-gcal-sync-tokens-clear'.")

(defmacro org-gcal--sync-tokens-get (key &optional remove?)
  "Get KEY from 'org-gcal--sync-tokens', or nil if not found.

This is a macro instead of a function so that it can be used as a place
expression in 'setf'.  In that case, if REMOVE? is non-nil, the key-value
pair will be removed instead of set."
  `(alist-get ,key org-gcal--sync-tokens nil ,remove? #'equal))

;;;###autoload
(defun org-gcal-sync (&optional skip-export silent)
  "Import events from calendars.
Export the ones to the calendar if unless
SKIP-EXPORT.  Set SILENT to non-nil to inhibit notifications."
  (interactive)
  (when org-gcal--sync-lock
    (user-error "org-gcal sync locked. If a previous sync has failed, call 'org-gcal--sync-unlock' to reset the lock and try again."))
  (org-gcal--sync-lock)
  ;; Don't scan upfront — org-generic-id-find auto-updates on cache miss,
  ;; and we do a full scan after fetching (before sync-buffer phase).
  (when org-gcal-auto-archive
    (dolist (i org-gcal-fetch-file-alist)
      (with-current-buffer
          (find-file-noselect (org-gcal--calendar-file i))
        (org-gcal--archive-old-event))))
  (let ((up-time (org-gcal--up-time))
        (down-time (org-gcal--down-time))
        (start-time (current-time))
        (cal-count 0)
        (cal-total (length org-gcal-fetch-file-alist)))
    (message "org-gcal: syncing %d calendars..." cal-total)
    (deferred:try
     (deferred:$
      (deferred:loop org-gcal-fetch-file-alist
                     (lambda (calendar-id-file)
                       (deferred:$
                        (org-gcal--sync-calendar calendar-id-file skip-export silent
                                                 up-time down-time)
                        (deferred:nextc it
                                        (lambda (_)
                                          ;; Save the calendar file after syncing.
                                          (let ((file (org-gcal--calendar-file calendar-id-file)))
                                            (when-let* ((buf (find-buffer-visiting file)))
                                              (with-current-buffer buf
                                                (when (buffer-modified-p) (save-buffer)))))
                                          (cl-incf cal-count)
                                          (message "org-gcal: syncing %d calendars... [%d/%d]"
                                                   cal-total cal-count cal-total)
                                          nil)))))
      ;; After syncing new events to Org, sync existing events in Org.
      (deferred:nextc it
                      (lambda (_)
                        ;; Save all calendar buffers before the ID scan so
                        ;; on-disk state matches in-memory state.
                        (dolist (cal org-gcal-fetch-file-alist)
                          (let ((file (expand-file-name (org-gcal--calendar-file cal))))
                            (when-let* ((buf (find-buffer-visiting file)))
                              (with-current-buffer buf
                                (when (buffer-modified-p) (save-buffer))))))
                        (org-generic-id-update-id-locations org-gcal-entry-id-property)
                        (when t
                          (mapc
                           (lambda (file)
                             (with-current-buffer (find-file-noselect file 'nowarn)
                               (org-with-wide-buffer
                                (org-gcal--sync-unlock)
                                (org-gcal-sync-buffer skip-export silent 'filter-time
                                                      'filter-managed))))
                           (org-generic-id-files)))
                        (message "org-gcal: syncing %d calendars... done (%.1fs)"
                                 cal-total
                                 (float-time (time-subtract (current-time) start-time))))))
     :finally
     (lambda ()
       (org-gcal--sync-unlock)))))


(defun org-gcal--sync-calendar (calendar-id-file skip-export silent
                                                 up-time down-time)
  "Sync events for CALENDAR-ID-FILE

CALENDAR-ID-FILE is a cons in 'org-gcal-fetch-file-alist', for which see."
  (let ((mode (org-gcal--recurring-mode-for-calendar (car calendar-id-file))))
    (if (eq mode :instances)
        ;; Dual-pass: masters first, then instances.
        ;; Use setq (not let) because deferred callbacks run outside
        ;; the dynamic scope of any let binding.
        (progn
          (setq org-gcal--parent-event-cache (make-hash-table :test 'equal)
                org-gcal--instance-collector (make-hash-table :test 'equal))
          (deferred:$
           (org-gcal--sync-calendar-events
            calendar-id-file skip-export silent nil up-time down-time nil
            :masters)
           (deferred:nextc it
                           (lambda (_)
                             (org-gcal--sync-calendar-events
                              calendar-id-file skip-export silent nil up-time down-time nil
                              :instances)))
           (deferred:nextc it
                           (lambda (_)
                             (setq org-gcal--parent-event-cache nil
                                   org-gcal--instance-collector nil)))))
      (org-gcal--sync-calendar-events
       calendar-id-file skip-export silent nil up-time down-time nil nil))))

(defun org-gcal--sync-calendar-events
    (calendar-id-file skip-export silent page-token up-time down-time
                      parent-events &optional instances-pass)
  "Sync events for CALENDAR-ID-FILE.

CALENDAR-ID-FILE is a cons in 'org-gcal-fetch-file-alist', for which see.
INSTANCES-PASS is nil for legacy, :masters for pass 1, :instances for pass 2."
  (let* ((calendar-id (car calendar-id-file))
         (calendar-file (org-gcal--calendar-file calendar-id-file))
         (page-token-cons (list nil))
         (token-key (if (eq instances-pass :instances)
                        (concat calendar-id "/instances")
                      calendar-id))
         (single-events (eq instances-pass :instances)))
    (deferred:$
     (org-gcal--sync-request-events calendar-id page-token up-time down-time
                                    token-key single-events)
     (deferred:nextc it
                     (lambda (response)
                       (let ((retry-fn
                              (lambda ()
                                (org-gcal--sync-calendar-events
                                 calendar-id-file skip-export silent page-token
                                 up-time down-time parent-events instances-pass))))
                         (org-gcal--sync-handle-response
                          response calendar-id-file page-token-cons down-time
                          retry-fn token-key))))
     (deferred:nextc it
                     (lambda (events)
                       (org-gcal--sync-handle-events calendar-id calendar-file
                                                     events nil up-time down-time
                                                     parent-events
                                                     (org-gcal--calendar-heading
                                                      calendar-id-file)
                                                     instances-pass)))
     (deferred:nextc it
                     (lambda (entries)
                       (org-gcal--sync-update-entries calendar-id entries skip-export)))
     ;; Retrieve the next page of results if needed.
     (deferred:nextc it
                     (lambda (_)
                       (let ((pt (car (last page-token-cons))))
                         (if pt
                             (org-gcal--sync-calendar-events
                              calendar-id-file skip-export silent pt
                              up-time down-time parent-events instances-pass)
                           (deferred:succeed nil))))))))

(defun org-gcal--sync-instances
    (calendar-id-file parent-event-id skip-export silent page-token
                      up-time down-time)
  "Sync instances of instances of recurring event PARENT-EVENT-ID.

CALENDAR-ID-FILE is a cons in 'org-gcal-fetch-file-alist', for which see."
  (let* ((calendar-id (car calendar-id-file))
         (calendar-file (org-gcal--calendar-file calendar-id-file))
         (page-token-cons (list nil)))
    (deferred:$
     (org-gcal--sync-request-instances calendar-id parent-event-id
                                       up-time down-time page-token)
     (deferred:nextc it
                     (lambda (response)
                       (let ((retry-fn
                              (lambda ()
                                (org-gcal--sync-instances
                                 calendar-id-file parent-event-id skip-export silent
                                 page-token up-time down-time))))
                         (org-gcal--sync-handle-response
                          response calendar-id-file page-token-cons down-time retry-fn))))
     (deferred:nextc it
                     (lambda (events)
                       (org-gcal--sync-handle-events calendar-id calendar-file
                                                     events t up-time down-time nil
                                                     (org-gcal--calendar-heading
                                                      calendar-id-file))))
     (deferred:nextc it
                     (lambda (entries)
                       (org-gcal--sync-update-entries calendar-id entries skip-export)))
     ;; Retrieve the next page of results if needed.
     (deferred:nextc it
                     (lambda (_)
                       (let ((pt (car (last page-token-cons))))
                         (if pt
                             (org-gcal--sync-instances
                              calendar-id-file parent-event-id skip-export silent
                              pt up-time down-time)
                           (deferred:succeed nil))))))))

(defun org-gcal--sync-event
    (calendar-id-file event-id skip-export)
  "Sync a single event given by EVENT-ID

CALENDAR-ID-FILE is a cons in 'org-gcal-fetch-file-alist', for which see."
  (let* ((calendar-id (car calendar-id-file))
         (calendar-file (org-gcal--calendar-file calendar-id-file)))

    (deferred:$
     (org-gcal--get-event calendar-id event-id)
     (deferred:nextc it
                     (lambda (event) (vector (request-response-data event))))
     (deferred:nextc it
                     (lambda (events)
                       (org-gcal--sync-handle-events calendar-id calendar-file
                                                     events nil nil nil nil
                                                     (org-gcal--calendar-heading
                                                      calendar-id-file))))
     (deferred:nextc it
                     (lambda (entries)
                       (org-gcal--sync-update-entries calendar-id entries skip-export))))))

(defun org-gcal--sync-request-events
    (calendar-id page-token up-time down-time &optional token-key single-events)
  "Request events on CALENDAR-ID, using PAGE-TOKEN if present.
TOKEN-KEY overrides the sync token lookup key (default: CALENDAR-ID).
When SINGLE-EVENTS is non-nil, pass singleEvents=true to the API."
  (let ((token (org-gcal--get-access-token calendar-id))
        (tk (or token-key calendar-id)))
   (request-deferred
    (org-gcal-events-url calendar-id)
    :type "GET"
    :headers
    `(("Accept" . "application/json")
      ("Authorization" . ,(format "Bearer %s" token)))
    :params
    (append
     `(("access_token" . ,token))
    (when single-events '(("singleEvents" . "true")))
    (when org-gcal-local-timezone `(("timeZone" . ,org-gcal-local-timezone)))
    (seq-let [expires sync-token]
        ;; Ensure 'org-gcal--sync-tokens-get' return value is actually a list
        ;; before passing to 'seq-let'.
        (when-let*
            ((x (org-gcal--sync-tokens-get tk))
             ((listp x)))
          x)
      (cond
       ;; Don't use the sync token if it's expired.
       ((and expires sync-token
             (time-less-p (current-time) expires))
        `(("syncToken" . ,sync-token)))
       (t
        (setf (org-gcal--sync-tokens-get tk 'remove) nil)
        `(("timeMin" . ,(org-gcal--format-time2iso up-time))
          ("timeMax" . ,(org-gcal--format-time2iso down-time))))))
     (when page-token `(("pageToken" . ,page-token))))
    :parser 'org-gcal--json-read)))

(defun org-gcal--sync-request-instances
    (calendar-id event-id up-time down-time page-token)
  "Request instances of recurring event EVENT-ID on CALENDAR-ID."
  (let ((token (org-gcal--get-access-token calendar-id)))
    (request-deferred
     (org-gcal-instances-url calendar-id event-id)
     :type "GET"
     :headers
     `(("Accept" . "application/json")
       ("Authorization" . ,(format "Bearer %s" token)))
     :params
     (append
      `(("access_token" . ,token)
        ("timeMin" . ,(org-gcal--format-time2iso up-time))
        ("timeMax" . ,(org-gcal--format-time2iso down-time)))
      (when page-token `(("pageToken" . ,page-token))))
     :parser 'org-gcal--json-read)))

(defun org-gcal--sync-handle-response
    (response calendar-id-file page-token-cons down-time retry-fn
              &optional token-key)
  "Handle RESPONSE in 'org-gcal--sync-calendar' for CALENDAR-ID-FILE.

Update PAGE-TOKEN from the response, and return a 'deferred' list of event
objects for further processing.
TOKEN-KEY overrides the sync token lookup key (default: calendar-id)."
  (let
      ((data (request-response-data response))
       (status-code (request-response-status-code response))
       (error-thrown (request-response-error-thrown response))
       (calendar-id (car calendar-id-file))
       (tk (or token-key (car calendar-id-file))))
    (cond
     ;; If there is no network connectivity, the response will
     ;; not include a status code.
     ((eq status-code nil)
      (org-gcal--notify
       "Got Error"
       "Could not contact remote service. Please check your network connectivity.")
      (error "Got error %S: %S" status-code error-thrown))
     ((eq 401 (or (plist-get (plist-get (request-response-data response) :error) :code)
                  status-code))
      (org-gcal--notify
       "Received HTTP 401"
       "OAuth token expired. Now trying to refresh-token")
      (deferred:$
       (org-gcal--refresh-token calendar-id)
       (deferred:nextc it
                       (lambda (_unused)
                         (funcall retry-fn)))))
     ((eq 403 status-code)
      (org-gcal--notify "Received HTTP 403"
                        "Ensure you enabled the Calendar API through the Developers Console, then try again.")
      (error "Got error %S: %S" status-code error-thrown))
     ((eq 410 status-code)
      (org-gcal--notify "Received HTTP 410"
                        "Calendar API sync token expired - performing full sync.")
      (setf (org-gcal--sync-tokens-get tk 'remove) nil)
      (funcall retry-fn))
     ;; We got some 2xx response, but for some reason no
     ;; message body.
     ((and (> 299 status-code) (eq data nil))
      (org-gcal--notify
       (concat "Received HTTP" (number-to-string status-code))
       "Error occured, but no message body.")
      (error "Got error %S: %S" status-code error-thrown))
     ((not (eq error-thrown nil))
      ;; Generic error-handler meant to provide useful
      ;; information about failure cases not otherwise
      ;; explicitly specified.
      (org-gcal--notify
       (concat "Status code: " (number-to-string status-code))
       (pp-to-string error-thrown))
      (error "Got error %S: %S" status-code error-thrown))
     ;; Fetch was successful. Return the list of events retrieved for
     ;; further processing.
     (t
      (nconc page-token-cons (list (plist-get data :nextPageToken)))
      (let ((next-sync-token (plist-get data :nextSyncToken)))
        (when next-sync-token
          (setf (org-gcal--sync-tokens-get tk)
                (list
                 ;; The first element is the expiration time of
                 ;; the sync token. Note that, if the expiration
                 ;; time already exists, we don't update it. We
                 ;; want to expire the token according to the
                 ;; time of the previous full sync.
                 (or
                  (car (org-gcal--sync-tokens-get tk))
                  down-time)
                 next-sync-token))))
      (org-gcal--filter (plist-get data :items))))))

(defun org-gcal--find-or-create-calendar-heading (calendar-id heading)
  "Find or create the org heading for CALENDAR-ID named HEADING.
Uses the `org-gcal-calendar' property to identify the heading uniquely,
avoiding collisions with user headings that have the same name.
Returns the position of the heading."
  (let ((pos nil))
    ;; Search for a heading with our marker property.
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (and (not pos) (re-search-forward
                            (format "^\\*+ %s" (regexp-quote heading)) nil t))
       (org-back-to-heading t)
       (when (equal (org-entry-get (point) "org-gcal-calendar") calendar-id)
         (setq pos (point)))
       (outline-next-heading)))
    (unless pos
      ;; Create the heading at end of buffer with the marker property.
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " heading "\n")
      (forward-line -1)
      (org-entry-put (point) "org-gcal-calendar" calendar-id)
      (setq pos (point)))
    pos))

(defun org-gcal--sync-handle-events
    (calendar-id calendar-file events recurring-instances? up-time down-time
                 parent-events &optional calendar-heading instances-pass)
  "Handle a list of EVENTS fetched from the Calendar API.

CALENDAR-ID and CALENDAR-FILE are defined in 'org-gcal--sync-inner'.
RECURRING-INSTANCES? is t if we're currently fetching instances of recurring
events and nil otherwise.
CALENDAR-HEADING, when non-nil, is a string naming a heading under which
new events should be inserted as children.
INSTANCES-PASS, when non-nil, is one of:
  :masters  — Pass 1 of `instances' mode (master events, inactive timestamps)
  :instances — Pass 2 of `instances' mode (individual instances as children)

Any parent recurring events are appended in-place to the list PARENT-EVENTS."
  (with-current-buffer (find-file-noselect calendar-file)
    ;; Expand multi-BYDAY weekly recurring events into per-day entries,
    ;; but skip in instances mode (instances naturally group under one parent).
    (unless (eq instances-pass :masters)
      (setq events (org-gcal--expand-multi-day-events events calendar-id)))
    (prog1
    (cl-loop
     for event across events
     if
     (let* ((entry-id (org-gcal--format-entry-id
                       calendar-id (plist-get event :id)))
            (recurring-event-id (plist-get event :recurringEventId))
            (recurrence (plist-get event :recurrence))
            (marker (org-gcal--id-find entry-id 'markerp))
            ;; In instances mode, master recurring events get inactive
            ;; timestamps; non-recurring events stay active.
            (inactive-p (and (eq instances-pass :masters) recurrence)))
       ;; Cache master events for pass 2 compaction.
       (when (and (eq instances-pass :masters) recurrence
                  org-gcal--parent-event-cache)
         (puthash (plist-get event :id) event
                  org-gcal--parent-event-cache))
       (cond
        ;; --- instances mode: pass 2 (instances) ---
        ;; Skip non-recurring events (handled in pass 1).
        ((and (eq instances-pass :instances)
              (not recurring-event-id))
         nil)
        ;; Instance of a recurring event: collect for compaction or
        ;; insert directly as child of parent heading.
        ((and (eq instances-pass :instances)
              recurring-event-id
              (not marker))
         (if (and org-gcal--instance-collector
                  (gethash recurring-event-id org-gcal--parent-event-cache))
             ;; Collect for batch compaction after the loop.
             (progn
               (push event
                     (gethash recurring-event-id
                              org-gcal--instance-collector))
               nil)
           ;; Fallback: no cached parent, insert directly.
           (let* ((parent-entry-id
                   (org-gcal--format-entry-id calendar-id recurring-event-id))
                  (parent-marker (org-gcal--id-find parent-entry-id 'markerp)))
             (when parent-marker
               (unless (org-gcal--event-cancelled-p event)
                 (atomic-change-group
                   (org-with-point-at parent-marker
                     (let ((level (org-current-level)))
                       (org-end-of-subtree t t)
                       (unless (bolp) (insert "\n"))
                       (insert (make-string (1+ level) ?*) " \n"
                               ":PROPERTIES:\n:END:\n")
                       (forward-line -2)
                       (org-back-to-heading t)
                       (org-gcal--update-entry calendar-id event 'newly-fetched)
                       (org-entry-put (point) org-gcal-managed-property
                                      org-gcal-managed-newly-fetched-mode)))))
               (when parent-marker (set-marker parent-marker nil)))
             nil)))
        ;; --- instances mode: pass 1 (masters) ---
        ;; Skip exception instances (have recurringEventId but no recurrence)
        ;; unless already tracked.
        ((and (not (eq instances-pass :instances))
              recurring-event-id
              (not marker))
         nil)
        ;; Cancelled events may lack start/end fields.  If we have an
        ;; existing heading, just mark it cancelled without a full update.
        ;; In instances pass 2, also remove the child heading's subtree.
        ((and marker (org-gcal--event-cancelled-p event))
         (org-with-point-at marker
           (org-gcal--handle-cancelled-entry))
         nil)
        ;; If event is present, collect it for later processing.
        (marker
         ;; Also feed existing instances into the collector for compaction.
         (when (and (eq instances-pass :instances)
                    recurring-event-id
                    org-gcal--instance-collector
                    (gethash recurring-event-id org-gcal--parent-event-cache))
           (push event (gethash recurring-event-id
                                org-gcal--instance-collector)))
         (org-gcal--event-entry-create
          :entry-id entry-id
          :marker marker
          :event event
          :inactive inactive-p))
        ;; If event doesn't already exist and is outside of the
        ;; range ['org-gcal-up-days', 'org-gcal-down-days'], ignore
        ;; it. This is necessary because when called with
        ;; "syncToken", the Calendar API will return all events
        ;; changed on the calendar, without respecting
        ;; 'org-gcal-up-days' or 'org-gcal-down-days', which means
        ;; repeated events far in the future will be downloaded.
        ;; Skip this check for master recurring events — their start
        ;; date is the first occurrence, not the current one.
        ;; Use condition-case because pre-epoch dates (e.g. birthdays)
        ;; can fail to parse on some platforms.
        ((and (not recurrence)
              (when-let*
                  ((up-time) (down-time)
                   (start (plist-get event :start))
                   (end (plist-get event :end))
                   ((condition-case nil
                        (or (time-less-p (org-gcal--parse-calendar-time start)
                                         up-time)
                            (time-less-p down-time
                                         (org-gcal--parse-calendar-time end)))
                      (error nil))))
                t))
         nil)
        ;; Don't insert instances of cancelled events that haven't already been
        ;; fetched.
        ((string= "cancelled" (plist-get event :status))
         nil)
        (t
         ;; Otherwise, insert a new entry into the fetch file.
         ;; When CALENDAR-HEADING is set, insert as a child of that
         ;; heading; otherwise append at top level.
         (atomic-change-group
           (if calendar-heading
               (let ((pos (org-gcal--find-or-create-calendar-heading
                           calendar-id calendar-heading)))
                 (goto-char pos)
                 (let ((level (org-current-level)))
                   (org-end-of-subtree t t)
                   (unless (bolp) (insert "\n"))
                   ;; Insert stub heading with empty properties drawer to
                   ;; prevent org--align-node-property from corrupting
                   ;; adjacent entries.
                   (insert (make-string (1+ level) ?*) " \n"
                           ":PROPERTIES:\n:END:\n")
                   (forward-line -2)
                   (org-back-to-heading t)
                   (org-gcal--update-entry calendar-id event 'newly-fetched
                                           inactive-p)
                   (org-entry-put (point) org-gcal-managed-property
                                  org-gcal-managed-newly-fetched-mode)))
             (org-with-point-at (point-max)
               (insert "\n* ")
               (org-gcal--update-entry calendar-id event 'newly-fetched
                                       inactive-p)
               (org-entry-put (point) org-gcal-managed-property
                              org-gcal-managed-newly-fetched-mode))))
         nil)))
     collect it)
    ;; After processing all events in pass 2, compact collected instances.
    (when (and (eq instances-pass :instances)
               org-gcal--instance-collector
               (> (hash-table-count org-gcal--instance-collector) 0))
      (org-gcal--compact-instances calendar-id calendar-file)))))

(defun org-gcal--sync-update-entries (calendar-id entries skip-export)
  "Update headlines given by 'org-gcal--event-entry' ENTRIES.

Find already retrieved entries and update them. This will update events that
have been moved from the default fetch file.  CALENDAR-ID is defined in
'org-gcal--sync-inner'."
  (deferred:$
   (deferred:loop entries
                  (lambda (entry)
                    (deferred:$
                     (let* ((entry-id (org-gcal--event-entry-entry-id entry))
                            (stored-marker (org-gcal--event-entry-marker entry))
                            ;; A marker whose buffer was killed (by revert,
                            ;; kill-buffer, or `set-marker ... nil') is still
                            ;; truthy but unusable, so re-find by entry-id.
                            (marker (if (and stored-marker
                                              (marker-buffer stored-marker))
                                         stored-marker
                                       (org-gcal--id-find entry-id)))
                            (event (org-gcal--event-entry-event entry))
                            (inactive-p (org-gcal--event-entry-inactive entry)))
                       (cond
                        ;; `org-gcal--id-find' returns nil when the entry was
                        ;; renamed/deleted between syncs.  Skip rather than
                        ;; falling through to `org-with-point-at nil', which
                        ;; would land at current point and crash `--update-entry'
                        ;; with "Must be on Org-mode heading.".
                        ((null marker)
                         (message "org-gcal: skipping entry %s — not found in any file"
                                  entry-id)
                         (deferred:succeed nil))
                        ((and (markerp marker)
                              (not (marker-buffer marker)))
                         (error "org-gcal: marker's buffer for entry %s has been killed"
                                entry-id))
                        (t
                         (org-with-point-at marker
                           ;; If skipping exports, just overwrite current entry's
                           ;; calendar data with what's been retrieved from the
                           ;; server. Otherwise, sync the entry at the current
                           ;; point.
                           (set-marker marker nil)
                           (if (and skip-export event)
                               (progn
                                 (org-gcal--update-entry calendar-id event
                                                         'update-existing inactive-p)
                                 (deferred:succeed nil))
                             (org-gcal-post-at-point nil skip-export
                                                     (org-gcal--sync-get-update-existing)))))))
                     ;; Log but otherwise ignore errors.
                     (deferred:error it
                                     (lambda (err)
                                       (message "org-gcal-sync: error: %s" err))))))
   (deferred:succeed nil)))

(defun org-gcal--sync-lock ()
  "Activate sync lock."
  (setq org-gcal--sync-lock t))

(defun org-gcal--sync-unlock ()
  "Deactivate sync lock in case of failed sync."
  (interactive)
  (setq org-gcal--sync-lock nil))

(defun org-gcal--sync-get-update-existing ()
  "Obtain value of 'org-gcal-managed-post-at-point-update-existing' for syncs."
  (if (equal org-gcal-managed-post-at-point-update-existing 'prompt)
      'never-push
    org-gcal-managed-post-at-point-update-existing))

;;;###autoload
(defun org-gcal-fetch ()
  "Fetch event data from google calendar."
  (interactive)
  (org-gcal-sync t))

;;;###autoload
(defun org-gcal-sync-buffer (&optional skip-export silent filter-date
                                       filter-managed)
  "Sync entries with Calendar events in currently-visible portion of buffer.

Updates events on the server unless SKIP-EXPORT is set. In this case, events
modified on the server will overwrite entries in the buffer.
Set SILENT to non-nil to inhibit notifications.
Set FILTER-DATE to only update events scheduled for later than
'org-gcal-up-days' and earlier than 'org-gcal-down-days'.
Set FILTER-MAANGED to only update events with 'org-gcal-managed-property' set
to “org”."
  (interactive)
  (when org-gcal--sync-lock
    (user-error "org-gcal sync locked. If a previous sync has failed, call 'org-gcal--sync-unlock' to reset the lock and try again."))
  (org-gcal--sync-lock)
  (let*
      ((name (or (buffer-file-name) (buffer-name))))
    (deferred:try
     (deferred:$
      (org-gcal--sync-buffer-inner skip-export silent filter-date
                                   filter-managed
                                   (point-min-marker))
      (deferred:nextc it
                      (lambda (_)
                        (org-gcal--notify "Completed syncing events in buffer."
                                          (concat "Events synced in\n" name)
                                          silent)
                        (deferred:succeed nil))))
     :finally
     (lambda ()
       (org-gcal--sync-unlock)))))

(defmacro org-gcal--with-point-at-no-widen (pom &rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY.

Based on 'org-with-point-at' but doesn't widen the buffer.
Signals an error if POM is a marker whose buffer has been killed."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (mpom)
    `(let ((,mpom ,pom))
       (save-excursion
         (when (markerp ,mpom)
           (unless (marker-buffer ,mpom)
             (error "org-gcal: marker’s buffer has been killed"))
           (set-buffer (marker-buffer ,mpom)))
         (goto-char (or ,mpom (point)))
         ,@body))))

(defun org-gcal--sync-buffer-inner
    (skip-export _silent filter-date filter-managed marker)
  "Inner loop of 'org-gcal-sync-buffer'."
  (while
      (not
       (catch 'block
         (deferred:$
          (deferred:succeed nil)
          (deferred:nextc it
                          ;; Returns (wrapped in deferred object):
                          ;; - marker within current headline if there are still headlines
                          ;;   left in the file.
                          ;; - nil if there are no more headlines.
                          (lambda (_)
                            (org-gcal--with-point-at-no-widen marker
                              ;; By default set next position of marker to nil. We'll set it below if
                              ;; there remains more to edit.
                              (setq marker nil)
                              (let* ((drawer-point
                                      (lambda ()
                                        (re-search-forward
                                         (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
                                         (point-max)
                                         'noerror)))
                                     (marker-for-post
                                      (cond
                                       ((eq major-mode 'org-mode)
                                        (when (funcall drawer-point)
                                          (setq marker (point-marker))
                                          marker))
                                       ((eq major-mode 'org-agenda-mode)
                                        (while (and (not marker) (not (eobp)))
                                          (when-let* ((agenda-marker (point-marker))
                                                     (org-marker (org-get-at-bol 'org-hd-marker)))
                                            (org-with-point-at org-marker
                                              (org-narrow-to-element)
                                              (when (funcall drawer-point)
                                                (setq marker agenda-marker)
                                                (point-marker)))))
                                        ;; If org-marker isn't found on this line, go to the next one.
                                        (forward-line 1))
                                       (t
                                        (user-error "Unsupported major mode %s in current buffer"
                                                    major-mode)))))
                                (if (and marker marker-for-post)
                                    (org-with-point-at marker-for-post
                                      (let* ((time-desc (org-gcal--get-time-and-desc))
                                             (start
                                              (plist-get time-desc :start))
                                             (start
                                              (and start
                                                   (org-gcal--parse-calendar-time-string start)))
                                             (end (plist-get time-desc :end))
                                             (end
                                              (and end
                                                   (org-gcal--parse-calendar-time-string end))))
                                        (if
                                            ;; Skip posting the headline under these
                                            ;; conditions
                                            (or
                                             ;; Don't sync events if 'filter-date' is set
                                             ;; and event is too far in the past or
                                             ;; future.
                                             (and filter-date
                                                  (or
                                                   (not start) (not end)
                                                   (time-less-p start (org-gcal--up-time))
                                                   (time-less-p (org-gcal--down-time) end)))
                                             ;; Don't sync if 'filter-managed' is set and
                                             ;; headline is not managed by Org (see
                                             ;; 'org-gcal-managed-property')
                                             (and filter-managed
                                                  (not
                                                   (string=
                                                    "org"
                                                    (org-entry-get
                                                     (point)
                                                     org-gcal-managed-property)))))
                                            (deferred:succeed marker)
                                          (deferred:try
                                           (deferred:$
                                            ;; Try to avoid hanging Emacs during
                                            ;; interactive use by waiting until Emacs is
                                            ;; idle.
                                            (deferred:wait-idle 1000)
                                            (deferred:nextc it
                                                            (lambda (_)
                                                              (org-with-point-at marker-for-post
                                                                (org-gcal-post-at-point nil skip-export
                                                                                        (org-gcal--sync-get-update-existing))))))
                                           :catch
                                           (lambda (err)
                                             (message "org-gcal-sync-buffer: at %S event %S: error: %s"
                                                      marker-for-post time-desc err))
                                           :finally
                                           (lambda (_)
                                             (deferred:succeed marker))))))
                                  (deferred:succeed nil))))))
          (deferred:nextc it
                          (lambda (m)
                            (when m
                              (setq marker m)
                              (throw 'block nil))
                            (deferred:succeed nil)))
          (deferred:error it
                          (lambda (err)
                            (message "org-gcal-sync-buffer: error: %s" err)))))))
  (deferred:succeed nil))

;;;###autoload
(defun org-gcal-fetch-buffer (&optional silent filter-date)
  "Fetch changes to events in the currently-visible portion of the buffer

Unlike 'org-gcal-sync-buffer', this will not push any changes to Google
Calendar. For SILENT and FILTER-DATE see 'org-gcal-sync-buffer'."
  (interactive)
  (org-gcal-sync-buffer t silent filter-date))

(defvar org-gcal-debug nil)
;;;###autoload
(defun org-gcal-toggle-debug ()
  "Toggle debugging flags for 'org-gcal'."
  (interactive)
  (cond
   (org-gcal-debug
    (setq
     debug-on-error (cdr (assq 'debug-on-error org-gcal-debug))
     debug-ignored-errors (cdr (assq 'debug-ignored-errors org-gcal-debug))
     deferred:debug (cdr (assq 'deferred:debug org-gcal-debug))
     deferred:debug-on-signal
     (cdr (assq 'deferred:debug-on-signal org-gcal-debug))
     request-log-level (cdr (assq 'request-log-level org-gcal-debug))
     request-log-buffer-name (cdr (assq 'request-log-buffer-name org-gcal-debug))
     org-gcal-debug nil)
    (message "org-gcal-debug DISABLED"))
   (t
    (setq
     org-gcal-debug
     `((debug-on-error . ,debug-on-error)
       (debug-ignored-errors . ,debug-ignored-errors)
       (deferred:debug . ,deferred:debug)
       (deferred:debug-on-signal . ,deferred:debug-on-signal)
       (request-log-level . ,request-log-level)
       (request-log-buffer-name . ,request-log-buffer-name))
     debug-on-error '(error)
     ;; These are errors that are thrown by various pieces of code that
     ;; don't mean anything.
     debug-ignored-errors (append debug-ignored-errors
                                  '(scan-error file-already-exists))
     deferred:debug t
     request-message-level 'debug
     request-log-level 'debug
     ;; Remove leading space so it shows up in the buffer list.
     request-log-buffer-name "*request-log*"
     deferred:debug-on-signal t)
    (message "org-gcal-debug ENABLED"))))

(defun org-gcal--headline ()
  "Get bare headline at current point."
  (substring-no-properties
   (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))

(defun org-gcal--filter (items)
  "Filter ITEMS on an AND of `org-gcal-fetch-event-filters' functions.
Run each element from ITEMS through all of the filters.  If any
filter returns NIL, discard the item."
  (if org-gcal-fetch-event-filters
      (cl-remove-if
       (lambda (item)
         (and (member nil
                      (mapcar (lambda (filter-func)
                                (funcall filter-func item)) org-gcal-fetch-event-filters))
              t))
       items)
    items))

(defun org-gcal--all-property-local-values (pom property literal-nil)
  "Return all values for PROPERTY in entry at point or marker POM.
Works like 'org--property-local-values', except that if multiple values of a
property whose key doesn't contain a '+' sign are present, this function will
return all of them. In particular, we wish to retrieve all local values of the
\"ID\" property. LITERAL-NIL also works the same way.

Does not preserve point."
  (org-with-point-at pom
    (org-gcal--back-to-heading)
    (let ((range (org-get-property-block)))
      (when range
        (goto-char (car range))
        (let* ((case-fold-search t)
               (end (cdr range))
               value)
          ;; Find values.
          (let* ((property+ (org-re-property
                             (concat (regexp-quote property) "\\+?") t t)))
            (while (re-search-forward property+ end t)
              (let ((v (match-string-no-properties 3)))
                (push (if literal-nil v (org-not-nil v)) value))))
          ;; Return final values.
          (and (not (equal value '(nil))) (nreverse value)))))))

(defun org-gcal--id-find (id &optional markerp)
  "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
  (or
   (org-generic-id-find org-gcal-entry-id-property id markerp
                        'cached)
   ;; Fallback for legacy "ID" property. Don't use 'org-id-find' directly
   ;; because it always run 'org-id-update-id-locations' if the ID isn't found,
   ;; which slows us down considerably, and tries to fall back to the current
   ;; buffer, which we don't want either.
   (when-let* ((file (org-gcal--find-id-file id)))
     (org-id-find-id-in-file id file markerp))))

(defun org-gcal--find-id-file (id)
  "Query the id database for the file in which this ID is located.

Like 'org-id-find-id-file', except that it doesn't fall back to the current
buffer if ID is not found in the id database, but instead returns nil.

Only needed for legacy entries that use \"ID\" to store entry IDs."
  (unless org-id-locations (org-id-locations-load))
  (or (and org-id-locations
           (hash-table-p org-id-locations)
           (gethash id org-id-locations))
      nil))

(defun org-gcal--get-id (pom)
  "Retrieve an entry ID at point-or-marker POM.

  Use 'org-gcal-entry-id-property', or \":ID:\" if not present (for backward
compatibility)."
  (org-gcal--event-id-from-entry-id
   (or (org-entry-get pom org-gcal-entry-id-property)
       (org-entry-get pom "ID"))))


(defun org-gcal--put-id (pom calendar-id event-id)
  "Store a canonical entry ID at point-or-marker POM.

Entry ID is generated from CALENDAR-ID and EVENT-ID and stored in the
'org-gcal-entry-id-property'.

This will also update the stored ID locations using
'org-generic-id-add-location'."
  (org-with-point-at pom
    (org-gcal--back-to-heading)
    (let ((entry-id (org-gcal--format-entry-id calendar-id event-id)))
      (org-entry-put (point) org-gcal-entry-id-property entry-id)
      (when-let* ((fname (buffer-file-name)))
        (org-generic-id-add-location org-gcal-entry-id-property entry-id
                                     fname)))))

(defun org-gcal--event-id-from-entry-id (entry-id)
  "Parse an ENTRY-ID created by 'org-gcal--format-entry-id' and return EVENT-ID."
  (when
      (and entry-id
           (string-match
            (rx-to-string
             '(and
               string-start
               (submatch-n 1
                 (1+ (not (any ?/ ?\n))))
               ?/
               (submatch-n 2 (1+ (not (any ?/ ?\n))))
               string-end))
            entry-id))
    (match-string 1 entry-id)))

(defun org-gcal--format-entry-id (calendar-id event-id)
  "Format CALENDAR-ID and ENTRY-ID into a canonical ID for an Org mode entry.

  Return nil if either argument is nil."
  (when (and calendar-id event-id)
    (format "%s/%s" event-id calendar-id)))

(defun org-gcal--back-to-heading ()
  "\
  Call 'org-back-to-heading' with the invisible-ok argument set to true.
  We always intend to go back to the invisible heading here."
  (org-back-to-heading 'invisible-ok))

(defun org-gcal--delete-body-content ()
  "Delete entry body content between end of metadata and next heading.
Point must be on a heading.  The body region is everything after all
drawers and planning lines, up to the next heading or end of subtree."
  (save-excursion
    (org-gcal--back-to-heading)
    (let ((body-start (save-excursion (org-end-of-meta-data t) (point)))
          (body-end (save-excursion (outline-next-heading) (point))))
      (when (< body-start body-end)
        (delete-region body-start body-end)))))

(defun org-gcal--get-time-and-desc ()
  "Get the timestamp and description of the event at point.

  Return a plist with :start, :end, and :desc keys. The value for a key is nil
  if not present."
  (let (start end desc tobj elem)
    (save-excursion
      (org-gcal--back-to-heading)
      (setq elem (org-element-at-point))
      ;; Find event time: check drawer first, then body.
      (save-excursion
        (let ((limit (save-excursion (outline-next-heading) (point))))
          ;; Try drawer.
          (when (re-search-forward
                 (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
                 limit 'noerror)
            (when (re-search-forward
                   "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                   limit 'noerror)
              (goto-char (match-beginning 0))
              (setq tobj (org-element-timestamp-parser))))
          ;; If no timestamp in drawer (body mode), try body.
          (unless tobj
            (org-gcal--back-to-heading)
            (org-end-of-meta-data t)
            ;; org-end-of-meta-data can move point at or past the next
            ;; heading on empty entries, so re-bound the search.
            (when (and (< (point) limit)
                       (re-search-forward
                        "[<\\[][0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                        limit 'noerror))
              (goto-char (match-beginning 0))
              (setq tobj (org-element-timestamp-parser))))))
      ;; Read description.
      (if (eq org-gcal-description-mode 'body)
          ;; Body mode: description is after all drawers/metadata and
          ;; timestamps, before the next heading.
          (save-excursion
            (org-gcal--back-to-heading)
            (org-end-of-meta-data t)
            ;; Skip past bare timestamp lines and the blank lines that
            ;; follow them.  Guard with `eobp' because at the end of the
            ;; buffer `forward-line' no longer advances and the regex still
            ;; matches, which would loop forever.
            (while (and (not (eobp))
                        (looking-at "^[<\\[].*[>\\]]\\s-*$"))
              (forward-line))
            (while (and (not (eobp))
                        (looking-at "^\\s-*$"))
              (forward-line))
            (let* ((body-start (point))
                   (body-end (save-excursion
                               (outline-next-heading) (point)))
                   (raw (string-trim
                         (buffer-substring-no-properties
                          body-start body-end))))
              (setq desc (if (string-empty-p raw) nil raw))))
        ;; Drawer mode: find the drawer.
        (when (re-search-forward
               (format "^[ \t]*:%s:[ \t]*$" org-gcal-drawer-name)
               (save-excursion (outline-next-heading) (point))
               'noerror)
          ;; Drawer mode: description follows the timestamp inside
          ;; the drawer.  Skip leading blank lines.
          (forward-line)
          (beginning-of-line)
          (re-search-forward
           "\\(?:^[ \t]*$\\)*\\([^z-a]*?\\)\n?[ \t]*:END:"
           (save-excursion (outline-next-heading) (point)))
          (setq desc (match-string-no-properties 1))
          (setq desc
                (if (string-match-p "\\`\n*\\'" desc)
                    nil
                  (replace-regexp-in-string
                   "^✱" "*"
                   (replace-regexp-in-string
                    "\\`\\(?: *<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*?>$\\)\n?\n?"
                    ""
                    (replace-regexp-in-string
                     " *:PROPERTIES:\n *\\(.*\\(?:\n.*\\)*?\\) *:END:\n+"
                     ""
                     desc))))))))
    ;; Prefer to read event time from the SCHEDULED property if present.
    (setq tobj (or (org-element-property :scheduled elem) tobj))
    (when tobj
      (when (plist-get (cadr tobj) :year-start)
        (setq
         start
         (org-gcal--format-org2iso
          (plist-get (cadr tobj) :year-start)
          (plist-get (cadr tobj) :month-start)
          (plist-get (cadr tobj) :day-start)
          (plist-get (cadr tobj) :hour-start)
          (plist-get (cadr tobj) :minute-start)
          (when (plist-get (cadr tobj) :hour-start) t))))
      (when (plist-get (cadr tobj) :year-end)
        (setq
         end
         (org-gcal--format-org2iso
          (plist-get (cadr tobj) :year-end)
          (plist-get (cadr tobj) :month-end)
          (plist-get (cadr tobj) :day-end)
          (plist-get (cadr tobj) :hour-end)
          (plist-get (cadr tobj) :minute-end)
          (when (plist-get (cadr tobj) :hour-end) t)))))
    (list :start start :end end :desc desc)))

(defun org-gcal--source-from-link-string (link)
  "Parse LINK, a link in Org format, to a Google Calendar API source object.

Returns an alist with ':url' for the link URL and ':title' for the link title,
or nil if no valid link is found."
  (with-temp-buffer
    (let ((org-inhibit-startup nil))
      (insert link)
      (org-mode)
      (goto-char (point-min))
      (when-let* ((link-element (org-element-link-parser)))
        (let ((link-title-begin (org-element-property :contents-begin link-element))
              (link-title-end (org-element-property :contents-end link-element)))
          (append
           `((url . ,(org-element-property :raw-link link-element)))
           (when (and link-title-begin link-title-end)
             `((title
                . ,(buffer-substring-no-properties
                    link-title-begin
                    link-title-end))))))))))

;;;###autoload
(defun org-gcal-post-at-point (&optional skip-import skip-export existing-mode)
  "Post entry at point to current calendar.

This overwrites the event on the server with the data from the entry, except if
the 'org-gcal-etag-property' is present and is out of sync with the server, in
which case the entry is overwritten with data from the server instead.

If SKIP-IMPORT is not nil, don't overwrite the entry with data from the server.
If SKIP-EXPORT is not nil, don't overwrite the event on the server.
For valid values of EXISTING-MODE see
'org-gcal-managed-post-at-point-update-existing'."
  (interactive)
  (save-excursion
    ;; Post entry at point in org-agenda buffer.
    (when (eq major-mode 'org-agenda-mode)
      (let ((m (org-get-at-bol 'org-hd-marker)))
        (set-buffer (marker-buffer m))
        (goto-char (marker-position m))))
    (end-of-line)
    (org-gcal--back-to-heading)
    (let* ((skip-import skip-import)
           (skip-export skip-export)
           (marker (point-marker))
           (elem (org-element-headline-parser (point-max) t))
           (smry (org-gcal--headline))
           (loc (org-entry-get (point) "LOCATION"))
           (source
            (when-let* ((link-string
                        (or (org-entry-get (point) "link")
                            (nth 0
                                 (org-entry-get-multivalued-property
                                  (point) "ROAM_REFS")))))
              (org-gcal--source-from-link-string link-string)))
           (transparency (or (org-entry-get (point) "TRANSPARENCY")
                             org-gcal-default-transparency))
           (recurrence (org-entry-get (point) "recurrence"))
           (event-id (org-gcal--get-id (point)))
           (etag (org-entry-get (point) org-gcal-etag-property))
           (managed (org-entry-get (point) org-gcal-managed-property))
           (calendar-id
            (org-entry-get (point) org-gcal-calendar-id-property)))
      ;; Set 'org-gcal-managed-property' if not present.
      (unless (and managed (member managed '("org" "gcal")))
        (let ((x
               (if (and calendar-id event-id)
                   org-gcal-managed-update-existing-mode
                 org-gcal-managed-create-from-entry-mode)))
          (org-entry-put (point) org-gcal-managed-property x)
          (setq managed x)))
      ;; Fill in Calendar ID if not already present.
      (unless calendar-id
        (setq calendar-id
              ;; Completes read with prompts like "CALENDAR-FILE (CALENDAR-ID)",
              ;; and then uses 'replace-regexp-in-string' to extract just
              ;; CALENDAR-ID.
              (replace-regexp-in-string
               ".*(\\(.*?\\))$" "\\1"
               (completing-read "Calendar ID: "
                                (mapcar
                                 (lambda (x) (format "%s (%s)" (org-gcal--calendar-file x) (car x)))
                                 org-gcal-fetch-file-alist))))
        (org-entry-put (point) org-gcal-calendar-id-property calendar-id))
      (when (equal managed "gcal")
        (unless existing-mode
          (setq existing-mode org-gcal-managed-post-at-point-update-existing))
        (pcase existing-mode
          ('never-push
           (setq skip-export t))
          ;; PROMPT and PROMPT-SYNC are handled identically here. When syncing
          ;; PROMPT is mapped to NEVER-PUSH in the calling function, while
          ;; PROMPT-SYNC is left unchanged.
          ;; Only when manually running 'org-gcal-post-at-point' should PROMPT
          ;; be seen here.
          ((or 'prompt 'prompt-sync)
           (unless (y-or-n-p (format "Push event to Google Calendar?\n\n%s\n\n"
                                     smry))
             (setq skip-export t)))
          ('always-push nil)
          (val
           (user-error "Bad value %S of EXISTING-MODE passed to 'org-gcal-post-at-point'. For valid values see 'org-gcal-managed-post-at-point-update-existing'."
                       val))))
      ;; Read currently-present start and end times and description. Fill in a
      ;; reasonable start and end time if either is missing.
      (let* ((time-desc (org-gcal--get-time-and-desc))
             (start (plist-get time-desc :start))
             (end (plist-get time-desc :end))
             (desc (plist-get time-desc :desc)))
        ;; Only prompt for a missing end time when actually pushing to the
        ;; server.  In sync (skip-export) paths the server data is the source
        ;; of truth and `org-gcal--post-event' falls back to a GET, so the
        ;; prompt would block deferred chains in non-interactive contexts.
        (unless (or end skip-export)
          (let* ((start-time (or start (org-read-date 'with-time 'to-time)))
                 (resolution 5)
                 (duration-default
                  (org-duration-from-minutes
                   (max
                    org-gcal-event-default-duration
                    ;; Round up to the nearest multiple of 'resolution' minutes.
                    (* resolution
                       (ceiling
                        (/ (- (org-duration-to-minutes
                               (or (org-element-property :EFFORT elem) "0:00"))
                              (org-clock-sum-current-item))
                           resolution))))))
                 (duration (read-from-minibuffer "Duration: " duration-default))
                 (duration-minutes (org-duration-to-minutes duration))
                 (duration-seconds (* 60 duration-minutes))
                 (end-time (time-add start-time duration-seconds)))
            (setq start (org-gcal--format-time2iso start-time)
                  end (org-gcal--format-time2iso end-time))))
        (org-gcal--post-event start end smry loc source desc calendar-id marker transparency etag
                              event-id nil skip-import skip-export)))))

;;;###autoload
(defun org-gcal-delete-at-point (&optional clear-gcal-info)
  "Delete entry at point to current calendar.

If called with prefix or with CLEAR-GCAL-INFO non-nil, will clear calendar info
from the entry even if deleting the event from the server fails.  Use this to
delete calendar info from events on calendars you no longer have access to."
  (interactive "P")
  (save-excursion
    ;; Delete entry at point in org-agenda buffer.
    (when (eq major-mode 'org-agenda-mode)
      (let ((m (org-get-at-bol 'org-hd-marker)))
        (set-buffer (marker-buffer m))
        (goto-char (marker-position m))))
    (end-of-line)
    (org-gcal--back-to-heading)
    (let* ((marker (point-marker))
           (smry (org-gcal--headline))
           (event-id (org-gcal--get-id (point)))
           (etag (org-entry-get (point) org-gcal-etag-property))
           (calendar-id
            (org-entry-get (point) org-gcal-calendar-id-property))
           (delete-error))
      (if (and event-id
               (y-or-n-p (format "Do you really want to delete event?\n\n%s\n\n" smry)))
          (deferred:try
           (org-gcal--delete-event calendar-id event-id etag (copy-marker marker))
           :catch
           (lambda (err)
             (message "Setting delete-error to %S" err)
             (setq delete-error err))
           :finally
           (lambda (_unused)
             ;; Only clear org-gcal from headline if successful or we were
             ;; forced to.
             (message "clear-gcal-info delete-error: %S %S"
                      clear-gcal-info delete-error)
             (when (or clear-gcal-info (null delete-error))
               (unless (marker-buffer marker)
                 (error "org-gcal: marker’s buffer has been killed"))
               ;; Delete :org-gcal: drawer after deleting event. This will preserve
               ;; the ID for links, but will ensure functions in this module don't
               ;; identify the entry as a Calendar event.
               (org-with-point-at marker
                 (when (re-search-forward
                        (format
                         "^[ \t]*:%s:[^z-a]*?\n[ \t]*:END:[ \t]*\n?"
                         (regexp-quote org-gcal-drawer-name))
                        (save-excursion (outline-next-heading) (point))
                        'noerror)
                   (replace-match "" 'fixedcase))
                 (org-entry-delete marker org-gcal-calendar-id-property)
                 (org-entry-delete marker org-gcal-entry-id-property))
               ;; Finally cancel and delete the event if this is configured.
               (org-with-point-at marker
                 (org-back-to-heading)
                 (org-gcal--handle-cancelled-entry)))
             (if delete-error
                 (error "org-gcal-delete-at-point: for %s %s: error: %S"
                        calendar-id event-id delete-error)
               (deferred:succeed nil))))
        (deferred:succeed nil)))))

(defun org-gcal--get-access-token (calendar-id)
  "Return the access token for the account owning CALENDAR-ID."
  (aio-wait-for
   (oauth2-auto-access-token (org-gcal--account calendar-id) 'org-gcal)))

(defun org-gcal--refresh-token (calendar-id)
  "Refresh OAuth access token for CALENDAR-ID and return it as a deferred."
  ;; FIXME: For now, we just synchronously wait for the refresh. Once the
  ;; project has been rewritten to use aio
  ;; (https://github.com/kidd/org-gcal.el/issues/191), we can wait for this
  ;; asynchronously as well.
  (let ((token
         (aio-wait-for
          (oauth2-auto-access-token (org-gcal--account calendar-id) 'org-gcal))))
    (deferred:succeed token)))

;;;###autoload
(defun org-gcal-sync-tokens-clear ()
  "Clear all Calendar API sync tokens.

  Use this to force retrieving all events in 'org-gcal-sync' or
  'org-gcal-fetch'."
  (interactive)
  (setq org-gcal--sync-tokens nil)
  (persist-save 'org-gcal--sync-tokens))

;; Internal
(defun org-gcal--archive-old-event ()
  "Archive old event at point."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (let ((properties (org-entry-properties)))
                                        ; Check if headline is managed by `org-gcal', and hasn't been archived
                                        ; yet. Only in that case, potentially archive.
        (when (and (assoc "ORG-GCAL-MANAGED" properties)
                   (not (assoc "ARCHIVE_TIME" properties)))

                                        ; Go to beginning of line to parse the headline
          (beginning-of-line)
          (let ((elem (org-element-headline-parser (point-max) t)))

                                        ; Go to next timestamp to parse it
            (condition-case nil
                (goto-char (cdr (org-gcal--timestamp-successor)))
              (error (error "Org-gcal error: Couldn't parse %s"
                            (buffer-file-name))))
            (let ((tobj (cadr (org-element-timestamp-parser))))
              (when (>
                     (time-to-seconds (time-subtract (current-time) (days-to-time org-gcal-up-days)))
                     (time-to-seconds (encode-time 0 (if (plist-get tobj :minute-end)
                                                         (plist-get tobj :minute-end) 0)
                                                   (if (plist-get tobj :hour-end)
                                                       (plist-get tobj :hour-end) 24)
                                                   (plist-get tobj :day-end)
                                                   (plist-get tobj :month-end)
                                                   (plist-get tobj :year-end))))
                (org-gcal--notify "Archived event." (org-element-property :title elem))
                (let ((kill-ring kill-ring)
                      (select-enable-clipboard nil))
                  (org-archive-subtree))))))))
    (save-buffer)))

(defun org-gcal--save-sexp (data file)
  "Print Lisp object DATA to FILE, creating it if necessary."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p (file-name-directory file))
      (make-directory dir)))
  (let* ((content (when (file-exists-p file)
                    (org-gcal--read-file-contents file))))
    (if (and content (listp content) (plist-get content :token))
        (setq content (plist-put content :token data))
      (setq content `(:token ,data :elem nil)))
    (with-temp-file file
      (pp content (current-buffer)))))

(defun org-gcal--read-file-contents (file)
  "Call 'read' on the contents of FILE, returning the resulting object."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case nil
        (read (current-buffer))
      (end-of-file nil))))

(defun org-gcal--json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read-from-string
     (decode-coding-string
      (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))))

(defun org-gcal--safe-substring (string from &optional to)
  "Call the `substring' function safely.
  No errors will be returned for out of range values of FROM and
  TO.  Instead an empty string is returned."
  (let* ((len (length string))
         (to (or to len)))
    (when (< from 0)
      (setq from (+ len from)))
    (when (< to 0)
      (setq to (+ len to)))
    (if (or (< from 0) (> from len)
            (< to 0) (> to len)
            (< to from))
        ""
      (substring string from to))))

(defun org-gcal--strip-html-p (calendar-id)
  "Return non-nil if HTML should be stripped for CALENDAR-ID.
Consult `org-gcal-strip-html-descriptions-overrides' first, falling
back to `org-gcal-strip-html-descriptions'."
  (if-let* ((override (assoc calendar-id
                              org-gcal-strip-html-descriptions-overrides
                              #'string=)))
      (cdr override)
    org-gcal-strip-html-descriptions))

(defun org-gcal--strip-html (string)
  "Strip HTML tags and decode entities in STRING.
Google Calendar returns event descriptions as HTML.  Convert to
plain text suitable for insertion into Org files."
  (thread-last string
    (replace-regexp-in-string "<br[^>]*>" "\n")
    (replace-regexp-in-string "<li[^>]*>" "\n- ")
    (replace-regexp-in-string "<[^>]+>" "")
    (replace-regexp-in-string "&amp;" "&")
    (replace-regexp-in-string "&lt;" "<")
    (replace-regexp-in-string "&gt;" ">")
    (replace-regexp-in-string "&nbsp;" " ")
    (replace-regexp-in-string "&quot;" "\"")
    (replace-regexp-in-string "&#39;" "'")
    (replace-regexp-in-string "\n\\{3,\\}" "\n\n")
    (string-trim)))

(defun org-gcal--alldayp (s e)
  (let ((slst (org-gcal--parse-date s))
        (elst (org-gcal--parse-date e)))
    (and
     (= (length s) 10)
     (= (length e) 10)
     ;; Check if end is exactly 1 day after start.  Use calendar
     ;; absolute days to avoid encode-time failures on pre-epoch dates.
     (= (- (calendar-absolute-from-gregorian
             (list (plist-get elst :mon)
                   (plist-get elst :day)
                   (plist-get elst :year)))
           (calendar-absolute-from-gregorian
             (list (plist-get slst :mon)
                   (plist-get slst :day)
                   (plist-get slst :year))))
        1))))

(defun org-gcal--parse-date (str)
  (list :year (string-to-number  (org-gcal--safe-substring str 0 4))
        :mon  (string-to-number (org-gcal--safe-substring str 5 7))
        :day  (string-to-number (org-gcal--safe-substring str 8 10))
        :hour (string-to-number (org-gcal--safe-substring str 11 13))
        :min  (string-to-number (org-gcal--safe-substring str 14 16))
        :sec  (string-to-number (org-gcal--safe-substring str 17 19))))

(defun org-gcal--parse-calendar-time (time)
  "Parse TIME, the start or end time object from Calendar API Events resource.
Return an Emacs time object from 'encode-time'."
  (org-gcal--parse-calendar-time-string
   (or (plist-get time :dateTime)
       (plist-get time :date))))

(defun org-gcal--parse-calendar-time-string (time-string)
  (condition-case nil
      (if (< 11 (length time-string))
          (parse-iso8601-time-string time-string)
        (apply #'encode-time
               ;; Full days have time strings with unknown hour, minute, and
               ;; second, which 'parse-time-string' will set to
               ;; nil. 'encode-time' can't tolerate that, so instead set the
               ;; time to 00:00:00.
               `(0 0 0 .
                 ,(nthcdr 3 (parse-time-string time-string)))))
    ;; Pre-epoch dates can fail on some platforms.  Return epoch as
    ;; fallback so callers that use this for filtering still work.
    (error (encode-time 0 0 0 1 1 1970))))

(defun org-gcal--down-time ()
  "Convert 'org-gcal-down-days' to Emacs time value."
  (time-add (current-time) (days-to-time org-gcal-down-days)))

(defun org-gcal--up-time ()
  "Convert 'org-gcal-up-days' to Emacs time value."
  (time-subtract (current-time) (days-to-time org-gcal-up-days)))

(defun org-gcal--time-zone (seconds)
  (current-time-zone (seconds-to-time seconds)))

(defun org-gcal--format-time2iso (time)
  "Format Emacs time value TIME to ISO format string."
  (format-time-string "%FT%T%z" time (car (org-gcal--time-zone 0))))

(defun org-gcal--rrule-parse (recurrence)
  "Parse RECURRENCE into an alist of RRULE parameters.
RECURRENCE is a vector of strings from the Google Calendar API.
Returns nil if no RRULE is found."
  (when-let* ((rules (append recurrence nil))
              (rrule-str (cl-find-if
                          (lambda (s) (string-prefix-p "RRULE:" s))
                          rules)))
    (mapcar (lambda (p)
              (let ((kv (split-string p "=")))
                (cons (car kv) (cadr kv))))
            (split-string (substring rrule-str 6) ";"))))

(defun org-gcal--rrule-to-repeater (recurrence)
  "Convert RECURRENCE to an Org repeater string, or nil if not mappable.

RECURRENCE is the value of the `recurrence' field from the Google Calendar API,
a vector of strings like [\"RRULE:FREQ=WEEKLY;INTERVAL=2\"].

Returns a string like \"+1w\", \"+2m\", etc., or nil if the rule is too complex
to represent as an Org repeater.  Multi-day BYDAY weekly rules return nil here;
use `org-gcal--rrule-expand-days' for those."
  (when-let* ((pairs (org-gcal--rrule-parse recurrence)))
    (let* ((freq (cdr (assoc "FREQ" pairs)))
           (interval (string-to-number (or (cdr (assoc "INTERVAL" pairs)) "1")))
           (byday (cdr (assoc "BYDAY" pairs)))
           (bymonthday (cdr (assoc "BYMONTHDAY" pairs)))
           (bysetpos (cdr (assoc "BYSETPOS" pairs)))
           (unit (pcase freq
                   ("DAILY" "d")
                   ("WEEKLY" "w")
                   ("MONTHLY" "m")
                   ("YEARLY" "y"))))
      ;; Bail out for complex rules that can't map to a single repeater.
      (when (and unit
                 (not bymonthday)
                 (not bysetpos)
                 ;; BYDAY with multiple days can't be a single repeater.
                 ;; A single BYDAY for weekly events just confirms the day.
                 (not (and byday
                           (or (not (string= freq "WEEKLY"))
                               (string-match-p "," byday)))))
        (format "+%d%s" interval unit)))))

(defvar org-gcal--day-of-week-alist
  '(("SU" . 0) ("MO" . 1) ("TU" . 2) ("WE" . 3)
    ("TH" . 4) ("FR" . 5) ("SA" . 6))
  "Map RRULE day abbreviations to `calendar-day-of-week' numbers (0=Sun).")

(defun org-gcal--rrule-expand-days (recurrence)
  "For multi-day BYDAY weekly rules, return a list of day offsets.

Each element is (DAY-ABBREV . OFFSET-DAYS) where OFFSET-DAYS is relative to
the event's start date.  Returns nil if the rule is not a multi-day weekly
BYDAY rule (i.e., it's already handled by `org-gcal--rrule-to-repeater')."
  (when-let* ((pairs (org-gcal--rrule-parse recurrence))
              ((string= (cdr (assoc "FREQ" pairs)) "WEEKLY"))
              (byday (cdr (assoc "BYDAY" pairs)))
              ((string-match-p "," byday))
              ((not (cdr (assoc "BYMONTHDAY" pairs))))
              ((not (cdr (assoc "BYSETPOS" pairs)))))
    (let ((days (split-string byday ",")))
      (mapcar (lambda (d) (cons d (cdr (assoc d org-gcal--day-of-week-alist))))
              days))))

(defun org-gcal--rrule-next-instance (cur-decoded freq interval byday)
  "Advance CUR-DECODED to the next RRULE instance.
FREQ is \"DAILY\", \"WEEKLY\", \"MONTHLY\", or \"YEARLY\".
INTERVAL is the repeat interval.  BYDAY is the BYDAY value
\(e.g., \"3FR\" for 3rd Friday) or nil."
  (pcase freq
    ("DAILY"
     (decoded-time-add cur-decoded (make-decoded-time :day interval)))
    ("WEEKLY"
     (decoded-time-add cur-decoded (make-decoded-time :day (* 7 interval))))
    ("MONTHLY"
     (if (and byday (string-match "\\([0-9]+\\)\\([A-Z]\\{2\\}\\)" byday))
         ;; BYDAY with ordinal: e.g., "3FR" = 3rd Friday.
         (let* ((nth (string-to-number (match-string 1 byday)))
                (day-abbr (match-string 2 byday))
                (dow (cdr (assoc day-abbr org-gcal--day-of-week-alist)))
                (next-month (decoded-time-add
                             cur-decoded
                             (make-decoded-time :month interval)))
                (year (decoded-time-year next-month))
                (month (decoded-time-month next-month))
                (first-of-month (encode-time
                                 0 (decoded-time-minute cur-decoded)
                                 (decoded-time-hour cur-decoded)
                                 1 month year))
                (first-dow (decoded-time-weekday
                            (decode-time first-of-month)))
                (days-to-first (mod (- dow first-dow) 7))
                (target-day (+ 1 days-to-first (* 7 (1- nth)))))
           (decode-time (encode-time
                         0 (decoded-time-minute cur-decoded)
                         (decoded-time-hour cur-decoded)
                         target-day month year)))
       ;; Simple monthly: same day, next month.
       (decoded-time-add cur-decoded (make-decoded-time :month interval))))
    ("YEARLY"
     (decoded-time-add cur-decoded (make-decoded-time :year interval)))
    (_ (decoded-time-add cur-decoded (make-decoded-time :day 1)))))

(defun org-gcal--instance-modified-p (instance-event parent-event)
  "Return non-nil if INSTANCE-EVENT differs from the parent RRULE prediction.
Compares summary, start time (against originalStartTime), and duration.
PARENT-EVENT is the master event plist from pass 1.
Returns t if modified, nil if unmodified.  When in doubt (missing
fields), returns nil to avoid creating unnecessary child headings."
  (condition-case nil
      (or
       ;; Summary changed.
       (not (equal (plist-get instance-event :summary)
                   (plist-get parent-event :summary)))
       ;; Start time differs from originalStartTime (time was moved).
       (when-let* ((orig-time (plist-get instance-event :originalStartTime))
                   (start (or (plist-get (plist-get instance-event :start) :dateTime)
                              (plist-get (plist-get instance-event :start) :date)))
                   (orig (or (plist-get orig-time :dateTime)
                             (plist-get orig-time :date))))
         (not (equal start orig)))
       ;; Duration differs from master.
       (let ((i-dur (time-subtract
                     (org-gcal--parse-calendar-time (plist-get instance-event :end))
                     (org-gcal--parse-calendar-time (plist-get instance-event :start))))
             (p-dur (time-subtract
                     (org-gcal--parse-calendar-time (plist-get parent-event :end))
                     (org-gcal--parse-calendar-time (plist-get parent-event :start)))))
         (not (equal i-dur p-dur))))
    (error nil)))

(defun org-gcal--build-exception-aware-timestamps
    (parent-event modified-instances cancelled-instances)
  "Build a list of inactive Org timestamp strings for a recurring event.
PARENT-EVENT is the master event plist.  MODIFIED-INSTANCES and
CANCELLED-INSTANCES are lists of instance event plists.
Returns a list of inactive timestamp strings, or nil if no
exceptions exist (meaning the parent's existing repeater suffices)."
  (when (or modified-instances cancelled-instances)
    (let* ((recurrence (plist-get parent-event :recurrence))
           (pairs (org-gcal--rrule-parse recurrence))
           (freq (cdr (assoc "FREQ" pairs)))
           (interval (string-to-number (or (cdr (assoc "INTERVAL" pairs)) "1")))
           (byday (cdr (assoc "BYDAY" pairs)))
           (until-str (cdr (assoc "UNTIL" pairs)))
           (until-time (when until-str
                         (org-gcal--parse-calendar-time-string until-str))))
      (when freq
        ;; Build lookup: date-key -> modified event or 'cancelled.
        (let ((special-dates (make-hash-table :test 'equal))
              (last-special-time nil))
          (dolist (inst modified-instances)
            (let* ((orig-time (org-gcal--parse-calendar-time
                               (plist-get inst :originalStartTime)))
                   (date-key (format-time-string "%Y-%m-%d" orig-time)))
              (puthash date-key inst special-dates)
              (when (or (null last-special-time)
                        (time-less-p last-special-time orig-time))
                (setq last-special-time orig-time))))
          (dolist (inst cancelled-instances)
            (let* ((orig-time (org-gcal--parse-calendar-time
                               (or (plist-get inst :originalStartTime)
                                   (plist-get inst :start))))
                   (date-key (format-time-string "%Y-%m-%d" orig-time)))
              (puthash date-key 'cancelled special-dates)
              (when (or (null last-special-time)
                        (time-less-p last-special-time orig-time))
                (setq last-special-time orig-time))))
          ;; Walk RRULE instances from master DTSTART to last-special-time.
          (let* ((master-start (org-gcal--parse-calendar-time
                                (plist-get parent-event :start)))
                 (master-end (org-gcal--parse-calendar-time
                              (plist-get parent-event :end)))
                 (master-dur (time-subtract master-end master-start))
                 (cur-decoded (decode-time master-start))
                 (result nil))
            ;; Multi-BYDAY: collect day offsets for multi-stream walk.
            (let* ((expand-days (org-gcal--rrule-expand-days recurrence))
                   (day-streams
                    (if (and expand-days (string= freq "WEEKLY"))
                        ;; Multiple day streams, each walks independently.
                        (let ((start-dow (decoded-time-weekday
                                          (decode-time master-start))))
                          (mapcar
                           (lambda (dp)
                             (let* ((offset (mod (- (cdr dp) start-dow) 7))
                                    (shifted (time-add master-start
                                                       (seconds-to-time
                                                        (* offset 86400)))))
                               (decode-time shifted)))
                           expand-days))
                      ;; Single stream.
                      (list cur-decoded))))
              ;; For each stream, walk to last-special-time, then add repeater.
              (dolist (stream-start day-streams)
                (let ((cur-d (copy-sequence stream-start)))
                  ;; Walk instances up to last-special-time.
                  (while (not (time-less-p last-special-time
                                           (encode-time cur-d)))
                    (let* ((cur-time (encode-time cur-d))
                           (date-key (format-time-string "%Y-%m-%d" cur-time))
                           (special (gethash date-key special-dates))
                           (past-until (and until-time
                                           (time-less-p until-time cur-time))))
                      (cond
                       (past-until nil)
                       ((eq special 'cancelled) nil)
                       ;; Modified instance: use its actual time.
                       (special
                        (let* ((inst-start (org-gcal--parse-calendar-time
                                            (plist-get special :start)))
                               (inst-end (org-gcal--parse-calendar-time
                                           (plist-get special :end)))
                               (s (format-time-string "%Y-%m-%d %a %H:%M"
                                                      inst-start))
                               (e (format-time-string "%H:%M" inst-end)))
                          (push (format "[%s-%s]" s e) result)))
                       ;; Regular instance: use master's time pattern.
                       (t
                        (let* ((s (format-time-string "%Y-%m-%d %a %H:%M"
                                                      cur-time))
                               (end-time (time-add cur-time master-dur))
                               (e (format-time-string "%H:%M" end-time)))
                          (push (format "[%s-%s]" s e) result)))))
                    (setq cur-d (org-gcal--rrule-next-instance
                                 cur-d freq interval byday)))
                  ;; Final timestamp: next instance after all specials.
                  (let ((final-time (encode-time cur-d)))
                    (unless (and until-time (time-less-p until-time final-time))
                      (let* ((s (format-time-string "%Y-%m-%d %a %H:%M"
                                                    final-time))
                             (end-time (time-add final-time master-dur))
                             (e (format-time-string "%H:%M" end-time))
                             (repeater (unless until-time
                                         (format " +%d%s" interval
                                                 (downcase
                                                  (substring freq 0 1))))))
                        (push (format "[%s-%s%s]" s e (or repeater ""))
                              result)))))))
            (nreverse result)))))))

(defun org-gcal--update-parent-timestamps (parent-marker timestamps)
  "Replace the timestamp(s) at PARENT-MARKER with TIMESTAMPS.
In body mode, replaces bare timestamp lines after metadata.
In drawer mode, replaces timestamps inside the :org-gcal: drawer."
  (org-with-point-at parent-marker
    (let ((sub-end (save-excursion
                     (if (outline-next-heading) (point) (point-max)))))
      (if (eq org-gcal-description-mode 'body)
          ;; Body mode: replace bare timestamp lines after metadata.
          (progn
            (org-end-of-meta-data t)
            (let ((ts-start (point)))
              ;; Delete existing timestamp lines.
              (while (and (< (point) sub-end)
                          (looking-at "^[<\\[].*[>\\]]\\s-*$"))
                (delete-region (point) (min (1+ (line-end-position)) sub-end)))
              ;; Insert new timestamps.
              (dolist (ts timestamps)
                (insert ts "\n"))))
        ;; Drawer mode: replace inside :org-gcal: drawer.
        (when (re-search-forward
               (format "^[ \t]*:%s:" (regexp-quote org-gcal-drawer-name))
               sub-end t)
          (forward-line)
          (let ((ts-start (point)))
            (when (re-search-forward "^[ \t]*:END:" sub-end t)
              (let ((ts-end (line-beginning-position)))
                (delete-region ts-start ts-end)
                (goto-char ts-start)
                (dolist (ts timestamps)
                  (insert ts "\n"))))))))))

(defun org-gcal--compact-instances (calendar-id calendar-file)
  "Process collected instances, inserting only modified ones as children.
Also prunes existing unmodified child headings and rebuilds parent
timestamps with exception-aware compaction."
  (maphash
   (lambda (recurring-event-id instances)
     (let* ((parent-event (gethash recurring-event-id
                                   org-gcal--parent-event-cache))
            (parent-entry-id (org-gcal--format-entry-id
                              calendar-id recurring-event-id))
            (parent-marker (org-gcal--id-find parent-entry-id 'markerp)))
       (when (and parent-event parent-marker)
         (let ((modified nil)
               (cancelled nil)
               (unmodified-count 0))
           ;; Classify instances.
           (dolist (event instances)
             (cond
              ((org-gcal--event-cancelled-p event)
               (push event cancelled))
              ((org-gcal--instance-modified-p event parent-event)
               (push event modified))
              (t (cl-incf unmodified-count))))
           (message "org-gcal compact %s: %d modified, %d cancelled, %d unmodified"
                    recurring-event-id
                    (length modified) (length cancelled) unmodified-count)
           ;; Rebuild parent timestamps with exception-aware compaction.
           (when (or modified cancelled)
             (let ((new-ts (org-gcal--build-exception-aware-timestamps
                            parent-event modified cancelled)))
               (when new-ts
                 (org-gcal--update-parent-timestamps parent-marker new-ts))))
           ;; Insert child headings only for modified instances that
           ;; don't already have a heading.
           (dolist (event modified)
             (let* ((entry-id (org-gcal--format-entry-id
                               calendar-id (plist-get event :id)))
                    (existing (org-gcal--id-find entry-id 'markerp)))
               (unless existing
                 (atomic-change-group
                   (org-with-point-at parent-marker
                     (let ((level (org-current-level)))
                       (org-end-of-subtree t t)
                       (unless (bolp) (insert "\n"))
                       (insert (make-string (1+ level) ?*) " \n"
                               ":PROPERTIES:\n:END:\n")
                       (forward-line -2)
                       (org-back-to-heading t)
                       (org-gcal--update-entry calendar-id event
                                               'newly-fetched)
                       (org-entry-put (point) org-gcal-managed-property
                                      org-gcal-managed-newly-fetched-mode))))
               (when existing (set-marker existing nil))))
           ;; Prune existing child headings for unmodified instances.
           (org-gcal--prune-unmodified-children
            parent-marker parent-event calendar-id instances)))
         (when parent-marker (set-marker parent-marker nil)))))
   org-gcal--instance-collector))

(defun org-gcal--prune-unmodified-children (parent-marker parent-event
                                                          calendar-id instances)
  "Delete child headings under PARENT-MARKER for unmodified instances.
Only deletes headings with `org-gcal-managed' property whose events
are in INSTANCES and are not modified relative to PARENT-EVENT."
  (let ((instance-ids (make-hash-table :test 'equal)))
    ;; Build lookup of all instance event IDs in this batch.
    (dolist (event instances)
      (puthash (plist-get event :id) event instance-ids))
    ;; Walk children in reverse order (to preserve positions).
    (org-with-point-at parent-marker
      (let* ((parent-level (org-current-level))
             (subtree-end (save-excursion (org-end-of-subtree t) (point)))
             (children nil))
        ;; Collect child positions.
        (save-excursion
          (while (and (outline-next-heading) (<= (point) subtree-end))
            (when (= (org-outline-level) (1+ parent-level))
              (push (point-marker) children))))
        ;; Process children (already in reverse order from push).
        (dolist (child-marker children)
          (org-with-point-at child-marker
            (when (org-entry-get nil org-gcal-managed-property)
              (let* ((child-entry-id (org-entry-get nil org-gcal-entry-id-property))
                     ;; Extract event ID from entry-id (format: "eventId/calendarId").
                     (child-event-id
                      (when child-entry-id
                        (car (split-string child-entry-id "/"))))
                     (child-event (when child-event-id
                                    (gethash child-event-id instance-ids))))
                (when (and child-event
                           (not (org-gcal--instance-modified-p
                                 child-event parent-event))
                           ;; Don't prune if child has user-added subheadings.
                           (let ((child-end (save-excursion
                                              (org-end-of-subtree t) (point))))
                             (save-excursion
                               (not (and (outline-next-heading)
                                         (< (point) child-end)
                                         (> (org-outline-level)
                                            (1+ parent-level)))))))
                  (delete-region (org-entry-beginning-position)
                                 (save-excursion
                                   (org-end-of-subtree t t) (point)))))))
          (set-marker child-marker nil))))))

(defun org-gcal--shift-iso-date (iso-str day-offset start-dow)
  "Shift ISO-STR forward so it falls on DAY-OFFSET (0=Sun..6=Sat).
START-DOW is the day-of-week of ISO-STR.  Returns a new ISO date string."
  (let ((delta (mod (- day-offset start-dow) 7)))
    (if (= delta 0)
        iso-str
      (let* ((plst (org-gcal--parse-date iso-str))
             (abs-day (calendar-absolute-from-gregorian
                       (list (plist-get plst :mon)
                             (plist-get plst :day)
                             (plist-get plst :year))))
             (new-date (calendar-gregorian-from-absolute (+ abs-day delta)))
             (m (nth 0 new-date))
             (d (nth 1 new-date))
             (y (nth 2 new-date)))
        (if (< 11 (length iso-str))
            ;; DateTime: replace the date portion, keep the time
            (format "%04d-%02d-%02d%s" y m d (substring iso-str 10))
          (format "%04d-%02d-%02d" y m d))))))

(defun org-gcal--expand-multi-day-events (events calendar-id)
  "Expand multi-BYDAY weekly recurring EVENTS into per-day synthetic events.
Returns a new vector with expanded events.  Non-expandable events are
passed through unchanged."
  (let (result)
    (cl-loop
     for event across events
     do
     (let ((expand-days (org-gcal--rrule-expand-days
                         (plist-get event :recurrence))))
       (if (not expand-days)
           (push event result)
         ;; Compute start day-of-week from the event's start date.
         (let* ((stime (or (plist-get (plist-get event :start) :dateTime)
                           (plist-get (plist-get event :start) :date)))
                (plst (org-gcal--parse-date stime))
                (start-dow (calendar-day-of-week
                            (list (plist-get plst :mon)
                                  (plist-get plst :day)
                                  (plist-get plst :year))))
                (pairs (org-gcal--rrule-parse (plist-get event :recurrence)))
                (interval (string-to-number
                           (or (cdr (assoc "INTERVAL" pairs)) "1")))
                (repeater (format "+%dw" interval)))
           (dolist (day-info expand-days)
             (let* ((day-abbrev (car day-info))
                    (target-dow (cdr day-info))
                    (new-id (format "%s_%s" (plist-get event :id) day-abbrev))
                    (shifted-start
                     (org-gcal--shift-iso-date stime target-dow start-dow))
                    ;; Shift end by the same delta as start.
                    (etime (or (plist-get (plist-get event :end) :dateTime)
                               (plist-get (plist-get event :end) :date)))
                    (shifted-end
                     (org-gcal--shift-iso-date etime target-dow start-dow))
                    ;; Build synthetic event with shifted times.
                    (new-event (copy-sequence event)))
               (plist-put new-event :id new-id)
               (plist-put new-event :org-gcal-repeater repeater)
               ;; Rebuild :start and :end with shifted dates.
               (let ((start-obj (copy-sequence (plist-get event :start)))
                     (end-obj (copy-sequence (plist-get event :end))))
                 (if (plist-get start-obj :dateTime)
                     (plist-put start-obj :dateTime shifted-start)
                   (plist-put start-obj :date shifted-start))
                 (if (plist-get end-obj :dateTime)
                     (plist-put end-obj :dateTime shifted-end)
                   (plist-put end-obj :date shifted-end))
                 (plist-put new-event :start start-obj)
                 (plist-put new-event :end end-obj))
               (push new-event result)))))))
    (vconcat (nreverse result))))

(defun org-gcal--format-iso2org (str &optional tz repeater inactive)
  "Format ISO date STR as an Org timestamp.
When INACTIVE is non-nil, use square brackets instead of angle brackets."
  (let* ((plst (org-gcal--parse-date str))
         (open (if inactive "[" "<"))
         (close (if inactive "]" ">"))
         (date-part
          (condition-case nil
              (let ((seconds (org-gcal--time-to-seconds plst)))
                (format-time-string
                 (if (< 11 (length str)) "%Y-%m-%d %a %H:%M" "%Y-%m-%d %a")
                 (seconds-to-time
                  (+ (if tz (car (org-gcal--time-zone seconds)) 0)
                     seconds))))
            ;; Fallback for dates that encode-time can't handle (e.g.
            ;; pre-epoch on Windows).  Compute day-of-week via calendar.
            (error
             (let* ((y (plist-get plst :year))
                    (m (plist-get plst :mon))
                    (d (plist-get plst :day))
                    (dow (calendar-day-name (list m d y) t)))
               (if (< 11 (length str))
                   (format "%04d-%02d-%02d %s %02d:%02d"
                           y m d dow
                           (plist-get plst :hour)
                           (plist-get plst :min))
                 (format "%04d-%02d-%02d %s" y m d dow)))))))
    (concat
     open date-part
     (if (and repeater (not (string= repeater ""))) (concat " " repeater) "")
     close)))

(defun org-gcal--make-link-string (url title)
  "Return an Org link string for URL and TITLE across Org versions."
  (cond
   ((fboundp 'org-link-make-string)
    (org-link-make-string url title))
   ((fboundp 'org-make-link-string)
    (org-make-link-string url title))
   (title
    (format "[[%s][%s]]" url title))
   (t
    (format "[[%s]]" url))))

(defun org-gcal--format-org2iso (year mon day &optional hour min tz)
  (condition-case nil
      (let ((seconds (time-to-seconds (encode-time 0
                                                   (or min 0)
                                                   (or hour 0)
                                                   day mon year))))
        (format-time-string
         (if (or hour min) "%Y-%m-%dT%H:%M:00Z" "%Y-%m-%d")
         (seconds-to-time
          (-
           seconds
           (if tz (car (org-gcal--time-zone seconds)) 0)))))
    ;; Fallback for pre-epoch dates that encode-time can't handle.
    (error
     (if (or hour min)
         (format "%04d-%02d-%02dT%02d:%02d:00Z" year mon day (or hour 0) (or min 0))
       (format "%04d-%02d-%02d" year mon day)))))

(defun org-gcal--iso-next-day (str &optional previous-p)
  (let ((format (if (< 11 (length str))
                    "%Y-%m-%dT%H:%M"
                  "%Y-%m-%d"))
        (plst (org-gcal--parse-date str))
        (prev (if previous-p -1 +1)))
    (format-time-string format
                        (seconds-to-time
                         (+ (org-gcal--time-to-seconds plst)
                            (* 60 60 24 prev))))))

(defun org-gcal--iso-previous-day (str)
  (org-gcal--iso-next-day str t))

(defun org-gcal--event-cancelled-p (event)
  "Has EVENT been cancelled?"
  (string= (plist-get event :status) "cancelled"))

(defun org-gcal--convert-time-to-local-timezone (date-time local-timezone)
  (if (and date-time
           local-timezone)
      (format-time-string "%Y-%m-%dT%H:%M:%S%z" (parse-iso8601-time-string date-time) local-timezone)
    date-time))

(defun org-gcal--update-entry (calendar-id event &optional update-mode inactive)
  "Update the entry at the current heading with information from EVENT.

EVENT is parsed from the Calendar API JSON response using 'org-gcal--json-read'.
CALENDAR-ID must be passed as well. Point must be located on an Org-mode heading
line or an error will be thrown. Point is not preserved.

If UPDATE-MODE is passed, then the functions in
'org-gcal-after-update-entry-functions' are called in order with the same
arguments as passed to this function and the point moved to the beginning of the
heading.

When INACTIVE is non-nil, use inactive timestamps (square brackets) and skip
SCHEDULED.  Used for master recurring events in `instances' mode."
  (unless (org-at-heading-p)
    (user-error "Must be on Org-mode heading."))
  (let* ((smry  (plist-get event :summary))
         (desc  (when-let* ((d (plist-get event :description)))
                  (let ((d (if (org-gcal--strip-html-p calendar-id)
                               (org-gcal--strip-html d)
                             d)))
                    (if org-gcal-description-filter-function
                        (funcall org-gcal-description-filter-function d)
                      d))))
         (loc   (plist-get event :location))
         (source (plist-get event :source))
         (transparency   (plist-get event :transparency))
         (_link  (plist-get event :htmlLink))
         (meet  (plist-get event :hangoutLink))
         (etag (plist-get event :etag))
         (event-id    (plist-get event :id))
         (stime (plist-get (plist-get event :start)
                           :dateTime))
         (etime (plist-get (plist-get event :end)
                           :dateTime))
         (sday  (plist-get (plist-get event :start)
                           :date))
         (eday  (plist-get (plist-get event :end)
                           :date))
         (start (if stime (org-gcal--convert-time-to-local-timezone stime org-gcal-local-timezone) sday))
         (end   (if etime (org-gcal--convert-time-to-local-timezone etime org-gcal-local-timezone) eday))
         (old-time-desc (org-gcal--get-time-and-desc))
         (old-start (plist-get old-time-desc :start))
         (old-end (plist-get old-time-desc :end))
         (recurrence (plist-get event :recurrence))
         (repeater (or (plist-get event :org-gcal-repeater)
                       (org-gcal--rrule-to-repeater recurrence)))
         (elem (org-element-at-point)))
    (when loc (setq loc (replace-regexp-in-string "\n" ", " loc)))
    (org-edit-headline
     (cond
      ;; Don't update headline if the new summary is the same as the CANCELLED
      ;; todo keyword.
      ((equal smry org-gcal-cancelled-todo-keyword) (org-gcal--headline))
      (smry smry)
      ;; Set headline to “busy” if there is no existing headline and no summary
      ;; from server.
      ((or (null (org-gcal--headline))
           (string-empty-p (org-gcal--headline)))
       "busy")
      (t (org-gcal--headline))))
    (org-entry-put (point) org-gcal-etag-property etag)
    (when recurrence (org-entry-put (point) "recurrence" (format "%s" recurrence)))
    (when loc (org-entry-put (point) "LOCATION" loc))
    (when source
      (let ((roam-refs
             (org-entry-get-multivalued-property (point) "ROAM_REFS"))
            (link (org-entry-get (point) "link")))
        (cond
         ;; ROAM_REFS can contain multiple references, but only bare URLs are
         ;; supported. To make sure we can round-trip between ROAM_REFS and
         ;; Google Calendar, only import to ROAM_REFS if there is no title in
         ;; the source, and if ROAM_REFS has at most one entry.
         ((and (null link)
               (<= (length roam-refs) 1)
               (or (null (plist-get source :title))
                   (string-empty-p (plist-get source :title))))
          (org-entry-put (point) "ROAM_REFS"
                         (plist-get source :url)))
         (t
          (org-entry-put (point) "link"
                         (org-gcal--make-link-string
                          (plist-get source :url)
                          (plist-get source :title)))))))
    (when transparency (org-entry-put (point) "TRANSPARENCY" transparency))
    (when meet
      (org-entry-put
       (point)
       "HANGOUTS"
       (format "[[%s][%s]]"
               meet
               "Join Hangouts Meet")))
    (org-entry-put (point) org-gcal-calendar-id-property calendar-id)
    (org-gcal--put-id (point) calendar-id event-id)
    ;; Erase existing drawer and body content.
    (org-gcal--back-to-heading)
    (save-excursion
      (when (re-search-forward
             (format
              "^[ \t]*:%s:[^z-a]*?\n[ \t]*:END:[ \t]*\n?"
              (regexp-quote org-gcal-drawer-name))
             (save-excursion (outline-next-heading) (point))
             'noerror)
        (replace-match "" 'fixedcase)))
    (org-gcal--delete-body-content)
    (unless (re-search-forward ":PROPERTIES:[^z-a]*?:END:"
                               (save-excursion (outline-next-heading) (point))
                               'noerror)
      (message "PROPERTIES not found: %s (%s) %d"
               (buffer-name) (buffer-file-name) (point)))
    (end-of-line)
    ;; Build timestamp(s).
    (let* ((expand-days
            (when (and inactive recurrence)
              (org-gcal--rrule-expand-days recurrence)))
           (open (if inactive "[" "<"))
           (close (if inactive "]" ">"))
           (make-ts
            (lambda (s e rep)
              (if (or (string= s e) (org-gcal--alldayp s e))
                  (org-gcal--format-iso2org s nil rep inactive)
                (if (and
                     (= (plist-get (org-gcal--parse-date s) :year)
                        (plist-get (org-gcal--parse-date e) :year))
                     (= (plist-get (org-gcal--parse-date s) :mon)
                        (plist-get (org-gcal--parse-date e) :mon))
                     (= (plist-get (org-gcal--parse-date s) :day)
                        (plist-get (org-gcal--parse-date e) :day)))
                    (format "%s%s-%s%s%s"
                            open
                            (org-gcal--format-date s "%Y-%m-%d %a %H:%M")
                            (org-gcal--format-date e "%H:%M")
                            (if rep (concat " " rep) "")
                            close)
                  (format "%s--%s"
                          (org-gcal--format-iso2org s nil rep inactive)
                          (org-gcal--format-iso2org
                           (if (< 11 (length e)) e
                             (org-gcal--iso-previous-day e))
                           nil nil inactive))))))
           (timestamps
            (if expand-days
                (let ((start-dow (plist-get (org-gcal--parse-date start) :dow)))
                  (mapcar
                   (lambda (day-pair)
                     (let ((shifted-s (org-gcal--shift-iso-date
                                       start (cdr day-pair) start-dow))
                           (shifted-e (org-gcal--shift-iso-date
                                       end (cdr day-pair) start-dow)))
                       (funcall make-ts shifted-s shifted-e "+1w")))
                   expand-days))
              (list (funcall make-ts start end repeater)))))
      ;; Insert timestamp(s) and description.
      (if (eq org-gcal-description-mode 'body)
          ;; Body mode: bare timestamps + description in body, no drawer.
          ;; Format: blank line, timestamp(s), blank line, description.
          (progn
            (if (and (not inactive) (org-element-property :scheduled elem))
                (let ((org-closed-keep-when-no-todo t))
                  (org-schedule nil (car timestamps)))
              (when (and inactive (org-element-property :scheduled elem))
                (save-excursion
                  (org-back-to-heading t)
                  (let ((org-closed-keep-when-no-todo t))
                    (org-schedule '(4)))))
              (insert "\n\n")
              (dolist (ts timestamps)
                (insert ts "\n")))
            (when desc
              (insert "\n" desc)
              (unless (string= "\n" (org-gcal--safe-substring desc -1))
                (insert "\n"))))
      ;; Drawer mode (default): timestamp + description inside drawer.
      (newline)
      (insert (format ":%s:" org-gcal-drawer-name))
      (newline)
      (if (and (not inactive) (org-element-property :scheduled elem))
          (let ((org-closed-keep-when-no-todo t))
            (org-schedule nil (car timestamps)))
        (when (and inactive (org-element-property :scheduled elem))
          (save-excursion
            (org-back-to-heading t)
            (let ((org-closed-keep-when-no-todo t))
              (org-schedule '(4)))))
        (dolist (ts timestamps)
          (insert ts)
          (newline))
        (when desc (newline)))
      (when desc
        (insert (replace-regexp-in-string "^\*" "✱" desc))
        (insert (if (string= "\n" (org-gcal--safe-substring desc -1)) "" "\n")))
      (insert ":END:")))
    (when (org-gcal--event-cancelled-p event)
      (save-excursion
        (org-back-to-heading t)
        (org-gcal--handle-cancelled-entry)))
    (when update-mode
      (cl-dolist (f org-gcal-after-update-entry-functions)
        (save-excursion
          (org-back-to-heading t)
          (funcall f calendar-id event update-mode))))
    ))


(defun org-gcal--handle-cancelled-entry ()
  "Perform actions to be done on cancelled entries."
  (unless (org-at-heading-p)
    (user-error "Must be on Org-mode heading"))
  (let ((already-cancelled
         (string= (nth 2 (org-heading-components))
                  org-gcal-cancelled-todo-keyword)))
    (unless already-cancelled
      (when (and org-gcal-update-cancelled-events-with-todo
                 (member org-gcal-cancelled-todo-keyword
                         org-todo-keywords-1))
        (let ((org-inhibit-logging t))
          (org-todo org-gcal-cancelled-todo-keyword))))
    (when (or org-gcal-remove-events-with-cancelled-todo
              (not already-cancelled))
      (org-gcal--maybe-remove-entry))))

(defun org-gcal--maybe-remove-entry ()
  "Maybe remove the entry at the current heading

Depends on the value of 'org-gcal-remove-api-cancelled-events'."
  (when-let* (((and org-gcal-remove-api-cancelled-events))
             (smry (org-gcal--headline))
             ((or (eq org-gcal-remove-api-cancelled-events t)
                  (y-or-n-p (format "Delete Org headline for cancelled event\n%s? "
                                    (or smry ""))))))
    (delete-region
     (save-excursion
       (org-back-to-heading t)
       (point))
     (save-excursion
       (org-end-of-subtree t t)
       (point)))))

(defun org-gcal--format-date (str format &optional tz)
  (let* ((plst (org-gcal--parse-date str))
         (seconds (org-gcal--time-to-seconds plst)))
    (concat
     (format-time-string format
                         (seconds-to-time
                          (+ (if tz (car (org-gcal--time-zone seconds)) 0)
                             seconds))))))

(defun org-gcal--param-date (str)
  (and str
       (if (< 11 (length str)) "dateTime" "date")))

(defun org-gcal--param-date-alt (str)
  (and str
       (if (< 11 (length str)) "dateTime" "date")))

(defun org-gcal--get-calendar-id-of-buffer ()
  "Find calendar id of current buffer."
  (or (cl-loop for entry in org-gcal-fetch-file-alist
               if (file-equal-p (org-gcal--calendar-file entry)
                                (buffer-file-name (buffer-base-buffer)))
               return (car entry))
      (user-error (concat "Buffer `%s' may not be related to google calendar; "
                          "please check/configure `org-gcal-fetch-file-alist'")
                  (buffer-name))))

(defun org-gcal--get-event (calendar-id event-id)
  "\
Retrieves a Google Calendar event given a CALENDAR-ID and EVENT-ID. If the
access token A-TOKEN is not specified, it is loaded from the token file.

Returns a 'deferred' function that on success returns a 'request-response'
object."
  (let ((a-token (org-gcal--get-access-token calendar-id)))
    (deferred:$
     (request-deferred
      (concat
       (org-gcal-events-url calendar-id)
       (concat "/" event-id))
      :type "GET"
      :headers
      `(("Accept" . "application/json")
        ("Authorization" . ,(format "Bearer %s" a-token)))
      :parser 'org-gcal--json-read)
     (deferred:nextc it
                     (lambda (response)
                       (let
                           ((_data (request-response-data response))
                            (status-code (request-response-status-code response))
                            (error-thrown (request-response-error-thrown response)))
                         (cond
                          ;; If there is no network connectivity, the response will not
                          ;; include a status code.
                          ((eq status-code nil)
                           (org-gcal--notify
                            "Got Error"
                            "Could not contact remote service. Please check your network connectivity.")
                           (error "Network connectivity issue"))
                          ((eq 401 (or (plist-get (plist-get (request-response-data response) :error) :code)
                                       status-code))
                           (org-gcal--notify
                            "Received HTTP 401"
                            "OAuth token expired. Now trying to refresh token.")
                           (deferred:$
                            (org-gcal--refresh-token calendar-id)
                            (deferred:nextc it
                                            (lambda (_unused)
                                              (org-gcal--get-event calendar-id event-id)))))
                          ;; Generic error-handler meant to provide useful information about
                          ;; failure cases not otherwise explicitly specified.
                          ((not (eq error-thrown nil))
                           (org-gcal--notify
                            (concat "Status code: " (number-to-string status-code))
                            (format "%s %s: %s"
                                    calendar-id
                                    event-id
                                    (pp-to-string error-thrown)))
                           (error "org-gcal--get-event: Got error %S for %s %s: %S"
                                  status-code calendar-id event-id error-thrown))
                          ;; Fetch was successful.
                          (t response))))))))

(defun org-gcal--post-event (start end smry loc source desc calendar-id marker transparency &optional etag event-id a-token skip-import skip-export)
  "\
Creates or updates an event on Calendar CALENDAR-ID with attributes START, END,
SMRY, LOC, DESC. The Org buffer and point from which the event is read is given
by MARKER.

If ETAG is provided, it is used to retrieve the event data from the server and
overwrite the event at MARKER if the event has changed on the server. MARKER is
destroyed by this function.

Returns a 'deferred' object that can be used to wait for completion."
  (let ((stime (org-gcal--param-date start))
        (etime (org-gcal--param-date end))
        (stime-alt (org-gcal--param-date-alt start))
        (etime-alt (org-gcal--param-date-alt end))
        (a-token (or a-token (org-gcal--get-access-token calendar-id))))
    (deferred:try
     (deferred:$
      (apply
       #'request-deferred
       (concat
        (org-gcal-events-url calendar-id)
        (when event-id
          (concat "/" (url-hexify-string event-id))))
       :type (cond
              (skip-export "GET")
              (event-id "PATCH")
              (t "POST"))
       :headers (append
                 `(("Content-Type" . "application/json")
                   ("Accept" . "application/json")
                   ("Authorization" . ,(format "Bearer %s" a-token)))
                 (cond
                  ((null etag) nil)
                  ((null event-id)
                   (error "org-gcal--post-event: %s %s %s: %s"
                          (point-marker) calendar-id event-id
                          "Event cannot have ETag set when event ID absent"))
                  (t
                   `(("If-Match" . ,etag)))))
       :parser 'org-gcal--json-read
       (unless skip-export
         (list
          :data (encode-coding-string
                 (json-encode
                  (append
                   `(("summary" . ,smry)
                     ("location" . ,loc)
                     ("source" . ,source)
                     ("transparency" . ,transparency)
                     ("description" . ,desc))
                   (if (and start end)
                       `(("start" (,stime . ,start) (,stime-alt . nil))
                         ("end" (,etime . ,(if (equal "date" etime)
                                               (org-gcal--iso-next-day end)
                                             end))
                          (,etime-alt . nil)))
                     nil)))
                 'utf-8))))
      (deferred:nextc it
                      (lambda (response)
                        (let
                            ((_temp (request-response-data response))
                             (status-code (request-response-status-code response))
                             (error-msg (request-response-error-thrown response)))
                          (cond
                           ;; If there is no network connectivity, the response will not
                           ;; include a status code.
                           ((eq status-code nil)
                            (org-gcal--notify
                             "Got Error"
                             "Could not contact remote service. Please check your network connectivity.")
                            (error "Network connectivity issue"))
                           ((eq 401 (or (plist-get (plist-get (request-response-data response) :error) :code)
                                        status-code))
                            (org-gcal--notify
                             "Received HTTP 401"
                             "OAuth token expired. Now trying to refresh-token")
                            (deferred:$
                             (org-gcal--refresh-token calendar-id)
                             (deferred:nextc it
                                             (lambda (_unused)
                                               (org-gcal--post-event start end smry loc source desc calendar-id
                                                                     marker transparency etag event-id nil
                                                                     skip-import skip-export)))))
                           ;; ETag on current entry is stale. This means the event on the
                           ;; server has been updated. In that case, update the event using
                           ;; the data from the server.
                           ((eq status-code 412)
                            (unless skip-import
                              (org-gcal--notify
                               "Received HTTP 412"
                               (format "ETag stale for %s\n%s\n\n%s"
                                       smry
                                       (org-gcal--format-entry-id calendar-id event-id)
                                       "Will overwrite this entry with event from server."))
                              (deferred:$
                               (org-gcal--get-event calendar-id event-id)
                               (deferred:nextc it
                                               (lambda (response)
                                                 (unless (marker-buffer marker)
                                                   (error "org-gcal: marker's buffer has been killed"))
                                                 (save-excursion
                                                   (with-current-buffer (marker-buffer marker)
                                                     (goto-char (marker-position marker))
                                                     (org-gcal--update-entry
                                                      calendar-id
                                                      (request-response-data response)
                                                      (if event-id 'update-existing 'create-from-entry))))
                                                 (deferred:succeed nil))))))
                           ;; Generic error-handler meant to provide useful information about
                           ;; failure cases not otherwise explicitly specified.
                           ((not (eq error-msg nil))
                            (org-gcal--notify
                             (concat "Status code: " (number-to-string status-code))
                             (pp-to-string error-msg))
                            (error "Got error %S: %S" status-code error-msg))
                           ;; Fetch was successful.
                           (t
                            (unless skip-export
                              (let* ((data (request-response-data response)))
                                (unless (marker-buffer marker)
                                  (error "org-gcal: marker's buffer has been killed"))
                                (save-excursion
                                  (with-current-buffer (marker-buffer marker)
                                    (goto-char (marker-position marker))
                                    ;; Update the entry to add ETag, as well as other
                                    ;; properties if this is a newly-created event.
                                    (org-gcal--update-entry calendar-id data
                                                            (if event-id
                                                                'update-existing
                                                              'create-from-entry))))
                                (org-gcal--notify "Event Posted"
                                                  (concat "Org-gcal post event\n  " (plist-get data :summary)))))
                            (deferred:succeed nil)))))))
     :finally
     (lambda (_)
       (set-marker marker nil)))))


(defun org-gcal--delete-event (calendar-id event-id etag marker &optional a-token)
  "\
Deletes an event on Calendar CALENDAR-ID with EVENT-ID. The Org buffer and
point from which the event is read is given by MARKER. MARKER is destroyed by
this function.

If ETAG is provided, it is used to retrieve the event data from the server and
overwrite the event at MARKER if the event has changed on the server.

Returns a 'deferred' object that can be used to wait for completion."
  (let ((a-token (or a-token (org-gcal--get-access-token calendar-id))))
    (deferred:try
     (deferred:$
      (request-deferred
       (concat
        (org-gcal-events-url calendar-id)
        (concat "/" event-id))
       :type "DELETE"
       :headers (append
                 `(("Content-Type" . "application/json")
                   ("Accept" . "application/json")
                   ("Authorization" . ,(format "Bearer %s" a-token)))
                 (cond
                  ((null etag) nil)
                  ((null event-id)
                   (error "Event cannot have ETag set when event ID absent"))
                  (t
                   `(("If-Match" . ,etag)))))

       :parser 'org-gcal--json-read)
      (deferred:nextc it
                      (lambda (response)
                        (let
                            ((_temp (request-response-data response))
                             (status-code (request-response-status-code response))
                             (error-msg (request-response-error-thrown response)))
                          (cond
                           ;; If there is no network connectivity, the response will not
                           ;; include a status code.
                           ((eq status-code nil)
                            (org-gcal--notify
                             "Got Error"
                             "Could not contact remote service. Please check your network connectivity.")
                            (error "Network connectivity issue"))
                           ((eq 401 (or (plist-get (plist-get (request-response-data response) :error) :code)
                                        status-code))
                            (org-gcal--notify
                             "Received HTTP 401"
                             "OAuth token expired. Now trying to refresh-token")
                            (deferred:$
                             (org-gcal--refresh-token calendar-id)
                             (deferred:nextc it
                                             (lambda (_unused)
                                               (org-gcal--delete-event calendar-id event-id
                                                                       etag marker nil)))))
                           ;; ETag on current entry is stale. This means the event on the
                           ;; server has been updated. In that case, update the event using
                           ;; the data from the server.
                           ((eq status-code 412)
                            (org-gcal--notify
                             "Received HTTP 412"
                             (format "ETag stale for entry %s\n\n%s"
                                     (org-gcal--format-entry-id calendar-id event-id)
                                     "Will overwrite this entry with event from server."))
                            (deferred:$
                             (org-gcal--get-event calendar-id event-id)
                             (deferred:nextc it
                                             (lambda (response)
                                               (unless (marker-buffer marker)
                                                 (error "org-gcal: marker's buffer has been killed"))
                                               (save-excursion
                                                 (with-current-buffer (marker-buffer marker)
                                                   (goto-char (marker-position marker))
                                                   (org-gcal--update-entry
                                                    calendar-id
                                                    (request-response-data response)
                                                    'update-existing)))
                                               (deferred:succeed nil)))))
                           ;; Generic error-handler meant to provide useful information about
                           ;; failure cases not otherwise explicitly specified.
                           ((not (eq error-msg nil))
                            (org-gcal--notify
                             (concat "Status code: " (number-to-string status-code))
                             (pp-to-string error-msg))
                            (error "Got error %S: %S" status-code error-msg))
                           ;; Fetch was successful.
                           (t
                            (org-gcal--notify "Event Deleted" "Org-gcal deleted event")
                            (deferred:succeed nil)))))))
     :finally
     (lambda (_)
       (set-marker marker nil)))))

(declare-function org-capture-goto-last-stored "org-capture" ())
(defun org-gcal--capture-post ()
  "Create gcal event for headline when captured or refiled into a gcal Org file."
  (when (not org-note-abort)
    (save-excursion
      (save-window-excursion
        (let ((inhibit-message t))
          (org-capture-goto-last-stored))
        (let ((matched nil))
          (dolist (i org-gcal-fetch-file-alist)
            (unless matched
              (when (and (buffer-file-name)
                         (string= (file-truename (org-gcal--calendar-file i))
                                  (file-truename (buffer-file-name)))
                         (org-gcal--entry-under-heading-p
                          (org-gcal--calendar-heading i)))
                (org-entry-put (point) org-gcal-calendar-id-property (car i))
                (org-gcal-post-at-point)
                (setq matched t)))))))))
(defun org-gcal--refile-post ()
  "Create gcal event for headline when refiled into a gcal Org file."
  (unless (or
           ;; Refile from capture is handled by 'org-gcal--capture-post'.
           (bound-and-true-p org-capture-is-refiling)
           ;; Don't POST unnecessarily if the headline being refiled is already
           ;; a gcal event.
           (and (org-entry-get (point) org-gcal-calendar-id-property)
                (org-entry-get (point) org-gcal-entry-id-property)))
    (save-excursion
      (save-window-excursion
        (let ((matched nil))
          (dolist (i org-gcal-fetch-file-alist)
            (unless matched
              (when (and (buffer-file-name)
                         (string= (file-truename (org-gcal--calendar-file i))
                                  (file-truename (buffer-file-name)))
                         (org-gcal--entry-under-heading-p
                          (org-gcal--calendar-heading i)))
                (org-entry-put (point) org-gcal-calendar-id-property (car i))
                (org-gcal-post-at-point)
                (setq matched t)))))))))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook 'org-gcal--capture-post))
(with-eval-after-load 'org-refile
  (add-hook 'org-after-refile-insert-hook 'org-gcal--refile-post))

(defun org-gcal--sync-tokens-valid ()
  "Is 'org-gcal--sync-tokens' in a valid format?"
  (and (listp org-gcal--sync-tokens)
       (json-alist-p org-gcal--sync-tokens)))

(defun org-gcal--timestamp-successor ()
  "Search for the next timestamp object.
Return value is a cons cell whose CAR is `timestamp' and CDR is
beginning position."
  (save-excursion
    (when (re-search-forward
           (concat org-ts-regexp-both
                   "\\|"
                   "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
                   "\\|"
                   "\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
           nil t)
      (cons 'timestamp (match-beginning 0)))))

(defun org-gcal--notify (title message &optional silent)
  "Send alert with TITLE and MESSAGE.

When SILENT is non-nil, silence messages even when 'org-gcal-notify-p' is
non-nil."
  (when (and org-gcal-notify-p (not silent))
    (if org-gcal-logo-file
        (alert message :title title :icon org-gcal-logo-file)
      (alert message :title title))
    (message "%s\n%s" title message)))

(defun org-gcal--time-to-seconds (plst)
  (time-to-seconds
   (encode-time
    (plist-get plst :sec)
    (plist-get plst :min)
    (plist-get plst :hour)
    (plist-get plst :day)
    (plist-get plst :mon)
    (plist-get plst :year))))


(defun org-gcal-reload-client-id-secret ()
  "Setup OAuth2 authentication after setting client ID and secret."
  (interactive)
  (add-to-list
   'oauth2-auto-additional-providers-alist
   `(org-gcal
     (authorize_url . "https://accounts.google.com/o/oauth2/auth")
     (token_url . "https://oauth2.googleapis.com/token")
     (scope . "https://www.googleapis.com/auth/calendar")
     (client_id . ,org-gcal-client-id)
     (client_secret . ,org-gcal-client-secret))))

(if (and org-gcal-client-id org-gcal-client-secret)
    (org-gcal-reload-client-id-secret)
  ;; Don't print warning during tests.
  (unless noninteractive
    (warn "org-gcal: must set 'org-gcal-client-id' and 'org-gcal-client-secret' for this package to work. Please run 'org-gcal-reload-client-id-secret' after setting these variables.")))

(provide 'org-gcal)

;;; org-gcal.el ends here
