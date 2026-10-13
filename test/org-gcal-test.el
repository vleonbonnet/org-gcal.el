;;; org-gcal-test.el --- Tests for org-gcal.el -*- lexical-binding: t -*-

;; Copyright (C) 2019 Robert Irelan
;; Package-Requires: ((org-gcal) (el-mock) (emacs "26") (load-relative "1.3"))

;; Author: Robert Irelan <rirelan@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for org-gcal.el

;;; Code:

;; Must set these variables before loading the package, but don’t reset them if
;; they’re already set.
(unless (and (boundp 'org-gcal-client-id) org-gcal-client-id
             (boundp 'org-gcal-client-secret) org-gcal-client-secret)
  (setq org-gcal-client-id "test_client_id"
        org-gcal-client-secret "test_client_secret"))

(require 'org-gcal)
(require 'cl-lib)
(require 'el-mock)
(require 'load-relative)
(unless (featurep 'org-test)
  (load-relative "org-test"))

(defconst org-gcal-test-calendar-id "foo@foobar.com")

(defconst org-gcal-test-event-json
  "\
{
 \"kind\": \"calendar#event\",
 \"etag\": \"\\\"12344321\\\"\",
 \"id\": \"foobar1234\",
 \"status\": \"confirmed\",
 \"htmlLink\": \"https://www.google.com/calendar/event?eid=foobareid1234\",
 \"hangoutsLink\": \"https://hangouts.google.com/my-meeting-id\",
 \"location\": \"Foobar's desk\",
 \"transparency\": \"opaque\",
 \"created\": \"2019-09-27T20:50:45.000Z\",
 \"updated\": \"2019-10-06T22:59:47.287Z\",
 \"summary\": \"My event summary\",
 \"description\": \"My event description\\n\\nSecond paragraph\",
 \"creator\": {
  \"email\": \"foo@foobar.com\",
  \"displayName\": \"Foo Foobar\"
 },
 \"organizer\": {
  \"email\": \"bar@foobar.com\",
  \"self\": true
 },
 \"start\": {
  \"dateTime\": \"2019-10-06T17:00:00-07:00\"
 },
 \"end\": {
  \"dateTime\": \"2019-10-06T21:00:00-07:00\"
 },
 \"reminders\": {
  \"useDefault\": true
 },
 \"source\": {
  \"url\": \"https://google.com\",
  \"title\": \"Google\"
 }
}
")

(defconst org-gcal-test-html-event-json
  (replace-regexp-in-string
   "My event description\\\\n\\\\nSecond paragraph"
   "<html-blob>Click the link:<br><a href=\\\\\"https://meet.jit.si/Weekly\\\\\">https://meet.jit.si/Weekly</a><br><br>Items:<br><ul><li>First</li><li>Second</li></ul>&amp; more info</html-blob>"
   org-gcal-test-event-json))

(defconst org-gcal-test-cancelled-event-json
  (replace-regexp-in-string "confirmed" "cancelled"
                            org-gcal-test-event-json))

(defconst org-gcal-test-full-day-event-json
  (replace-regexp-in-string
   "\"dateTime\": \"2019-10-06T17:00:00-07:00\""
   "\"date\": \"2019-10-06\""
   (replace-regexp-in-string
    "\"dateTime\": \"2019-10-06T21:00:00-07:00\""
    "\"date\": \"2019-10-07\""
    org-gcal-test-event-json)))

(defmacro org-gcal-test--with-temp-buffer (contents &rest body)
  "Create a ‘org-mode’ enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of the buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun org-gcal-test--stale-marker (contents)
  "Return a marker whose buffer was killed after inserting CONTENTS."
  (let ((buffer (generate-new-buffer " *org-gcal-stale-marker*")))
    (with-current-buffer buffer
      (org-mode)
      (insert contents)
      (goto-char (point-min))
      (prog1 (point-marker)
        (kill-buffer buffer)))))

(defmacro org-gcal-test--should-error-match (regexp &rest body)
  "Assert that BODY signals an error whose message matches REGEXP."
  (declare (indent 1) (debug t))
  `(let ((err (should-error (progn ,@body) :type 'error)))
     (should (string-match-p ,regexp (error-message-string err)))))

(defun org-gcal-test--json-read-string (json)
  "Wrap ‘org-gcal--json-read’ to parse a JSON string."
  (with-temp-buffer
    (insert json)
    (org-gcal--json-read)))

(defun org-gcal-test--title-to-string (elem)
  "Get :title from ELEM and convert to string."
  (let ((prop (org-element-property :title elem)))
    (cond
     ((listp prop) (car prop))
     ((stringp prop) prop)
     (t
      (user-error "org-gcal-test--title-to-string: unhandled type for :title prop of elem %S"
                  elem)))))

(defconst org-gcal-test-event
  (org-gcal-test--json-read-string org-gcal-test-event-json))

(defconst org-gcal-test-html-event
  (org-gcal-test--json-read-string org-gcal-test-html-event-json))

(defconst org-gcal-test-cancelled-event
  (org-gcal-test--json-read-string org-gcal-test-cancelled-event-json))

(defconst org-gcal-test-full-day-event
  (org-gcal-test--json-read-string org-gcal-test-full-day-event-json))

(ert-deftest org-gcal-test--save-sexp ()
  "Verify that org-gcal--save-sexp saves its data to the right place."
  (let* ((file
          ;; When I rescan ID locations, the symlink "/var" is resolved to
          ;; "/private/var" on macOS. The simplest way to fix this is just to
          ;; resolve the symlink manually at the start.
          ;;
          ;; Also need to run ‘abbreviate-file-name’ in case the temp file is
          ;; created under HOME.
          (abbreviate-file-name (file-truename (make-temp-file "org-gcal-test--save-sexp.")))))
    (unwind-protect
        (org-gcal-test--with-temp-buffer
         ""
         (let ((data '(:foo :bar)))
           (org-gcal--save-sexp data file)
           (should (string-equal (buffer-string)
                                 ""))
           (should (equal (org-gcal--read-file-contents file)
                          `(:token ,data :elem nil)))
           (setq data '(:baz :quux))
           (org-gcal--save-sexp data file)
           (should (equal (buffer-string)
                          ""))
           (should (equal (org-gcal--read-file-contents file)
                          `(:token ,data :elem nil))))))))

(ert-deftest org-gcal-test--update-empty-entry ()
  "Verify that an empty headline is populated correctly from a calendar event
object."
  (org-gcal-test--with-temp-buffer
      "* "
    (org-gcal--update-entry org-gcal-test-calendar-id
                            org-gcal-test-event)
    (org-back-to-heading)
    (let ((elem (org-element-at-point)))
      (should (equal (org-gcal-test--title-to-string elem)
                     "My event summary"))
      (should (equal (org-element-property :ETAG elem)
                     "\"12344321\""))
      (should (equal (org-element-property :LOCATION elem)
                     "Foobar's desk"))
      (should (equal (org-element-property :LINK elem)
                     "[[https://google.com][Google]]"))
      (should (equal (org-element-property :TRANSPARENCY elem)
                     "opaque"))
      (should (equal (org-element-property :CALENDAR-ID elem)
                     "foo@foobar.com"))
      (should (equal (org-element-property :ENTRY-ID elem)
                     "foobar1234/foo@foobar.com")))
    ;; Check contents of "org-gcal" drawer
    (re-search-forward ":org-gcal:")
    (let ((elem (org-element-at-point)))
      (should (equal (org-element-property :drawer-name elem)
                     "org-gcal"))
      (should (equal (buffer-substring-no-properties
                      (org-element-property :contents-begin elem)
                      (org-element-property :contents-end elem))
                     "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))))

(ert-deftest org-gcal-test--update-existing-entry ()
  "Verify that an existing headline is populated correctly from a calendar event
object."
  (org-gcal-test--with-temp-buffer
   "\
* Old event summary
:PROPERTIES:
:ETag:     \"9999\"
:LOCATION: Somewhere else
:link: [[https://yahoo.com][Yahoo!]]
:TRANSPARENCY: transparent
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<9999-10-06 Sun 17:00-21:00>

Old event description
:END:
"
   (org-gcal--update-entry org-gcal-test-calendar-id
                           org-gcal-test-event)
   (org-back-to-heading)
   (let ((elem (org-element-at-point)))
     (should (equal (org-gcal-test--title-to-string elem)
                    "My event summary"))
     (should (equal (org-element-property :ETAG elem)
                    "\"12344321\""))
     (should (equal (org-element-property :LOCATION elem)
                    "Foobar's desk"))
     (should (equal (org-element-property :LINK elem)
                    "[[https://google.com][Google]]"))
     (should (equal (org-element-property :TRANSPARENCY elem)
                    "opaque"))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "foo@foobar.com"))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "foobar1234/foo@foobar.com")))
   ;; Check contents of "org-gcal" drawer
   (re-search-forward ":org-gcal:")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :drawer-name elem)
                    "org-gcal"))
     (should (equal (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))
                    "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))))

(ert-deftest org-gcal-test--update-existing-entry-cancelled ()
  "Verify that an existing headline is populated correctly from a cancelled
  calendar event object."
  (let (
        (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
        (org-gcal-cancelled-todo-keyword "CANCELLED")
        (org-gcal-remove-api-cancelled-events nil)
        (buf "\
* Old event summary
:PROPERTIES:
:ETag:     \"9999\"
:LOCATION: Somewhere else
:link: [[https://yahoo.com][Yahoo!]]
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<9999-10-06 Sun 17:00-21:00>

Old event description
:END:
"))
    (let ((org-gcal-update-cancelled-events-with-todo t))
      (org-gcal-test--with-temp-buffer
       buf
       (org-gcal--update-entry org-gcal-test-calendar-id
                               org-gcal-test-cancelled-event)
       (org-back-to-heading)
       (let ((elem (org-element-at-point)))
         (should (equal (org-gcal-test--title-to-string elem)
                        "My event summary"))
         (should (equal (org-element-property :todo-keyword elem)
                        "CANCELLED"))
         (should (equal (org-element-property :ETAG elem)
                        "\"12344321\""))
         (should (equal (org-element-property :LOCATION elem)
                        "Foobar's desk"))
         (should (equal (org-element-property :LINK elem)
                        "[[https://google.com][Google]]"))
         (should (equal (org-element-property :CALENDAR-ID elem)
                        "foo@foobar.com"))
         (should (equal (org-element-property :ENTRY-ID elem)
                        "foobar1234/foo@foobar.com")))
       ;; Check contents of "org-gcal" drawer
       (re-search-forward ":org-gcal:")
       (let ((elem (org-element-at-point)))
         (should (equal (org-element-property :drawer-name elem)
                        "org-gcal"))
         (should (equal (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))
                        "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))))
    (let ((org-gcal-update-cancelled-events-with-todo nil))
      (org-gcal-test--with-temp-buffer
       buf
       (org-gcal--update-entry org-gcal-test-calendar-id
                               org-gcal-test-cancelled-event)
       (org-back-to-heading)
       (let ((elem (org-element-at-point)))
         (should (equal (org-gcal-test--title-to-string elem)
                        "My event summary"))
         (should (equal (org-element-property :todo-keyword elem)
                        nil))
         (should (equal (org-element-property :ETAG elem)
                        "\"12344321\""))
         (should (equal (org-element-property :LOCATION elem)
                        "Foobar's desk"))
         (should (equal (org-element-property :LINK elem)
                        "[[https://google.com][Google]]"))
         (should (equal (org-element-property :TRANSPARENCY elem)
                        "opaque"))
         (should (equal (org-element-property :CALENDAR-ID elem)
                        "foo@foobar.com"))
         (should (equal (org-element-property :ENTRY-ID elem)
                        "foobar1234/foo@foobar.com")))
       ;; Check contents of "org-gcal" drawer
       (re-search-forward ":org-gcal:")
       (let ((elem (org-element-at-point)))
         (should (equal (org-element-property :drawer-name elem)
                        "org-gcal"))
         (should (equal (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))
                        "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))))
    (let ((org-gcal-remove-api-cancelled-events t))
      (org-gcal-test--with-temp-buffer
       buf
       (org-gcal--update-entry org-gcal-test-calendar-id
                               org-gcal-test-cancelled-event)
       (should (equal (buffer-substring-no-properties
                       (point-min) (point-max))
                      ""))))))

(ert-deftest org-gcal-test--update-existing-entry-already-cancelled ()
  "Verify that an existing headline is modified correctly according to the \
  value of ‘org-gcal-remove-events-with-cancelled-todo’."
  (let (
        (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
        (org-gcal-cancelled-todo-keyword "CANCELLED")
        (org-gcal-remove-api-cancelled-events nil)
        (org-gcal-remove-events-with-cancelled-todo nil)
        (buf "\
* Old event summary
:PROPERTIES:
:ETag:     \"9999\"
:LOCATION: Somewhere else
:link: [[https://yahoo.com][Yahoo!]]
:TRANSPARENCY: transparent
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<9999-10-06 Sun 17:00-21:00>

Old event description
:END:
"))
    (let ((org-gcal-update-cancelled-events-with-todo t))
      (org-gcal-test--with-temp-buffer
       buf
       ;; First mark the event as cancelled.
       (org-gcal--update-entry org-gcal-test-calendar-id
                               org-gcal-test-cancelled-event)
       (org-back-to-heading)
       (let ((elem (org-element-at-point)))
         (should (equal (org-gcal-test--title-to-string elem)
                        "My event summary"))
         (should (equal (org-element-property :todo-keyword elem)
                        "CANCELLED"))
         (should (equal (org-element-property :ETAG elem)
                        "\"12344321\""))
         (should (equal (org-element-property :LOCATION elem)
                        "Foobar's desk"))
         (should (equal (org-element-property :LINK elem)
                        "[[https://google.com][Google]]"))
         (should (equal (org-element-property :TRANSPARENCY elem)
                        "opaque"))
         (should (equal (org-element-property :CALENDAR-ID elem)
                        "foo@foobar.com"))
         (should (equal (org-element-property :ENTRY-ID elem)
                        "foobar1234/foo@foobar.com")))
       ;; Check contents of "org-gcal" drawer
       (re-search-forward ":org-gcal:")
       (let ((elem (org-element-at-point)))
         (should (equal (org-element-property :drawer-name elem)
                        "org-gcal"))
         (should (equal (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))
                        "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))
       ;; Now check that the event isn’t removed when
       ;; ‘org-gcal-remove-events-with-cancelled-todo’ is nil.
       (setq org-gcal-remove-api-cancelled-events t
             org-gcal-remove-events-with-cancelled-todo nil)
       (org-back-to-heading)
       (org-gcal--update-entry org-gcal-test-calendar-id
                               org-gcal-test-cancelled-event)
       (org-back-to-heading)
       (let ((elem (org-element-at-point)))
         (should (equal (org-gcal-test--title-to-string elem)
                        "My event summary"))
         (should (equal (org-element-property :todo-keyword elem)
                        "CANCELLED"))
         (should (equal (org-element-property :ETAG elem)
                        "\"12344321\""))
         (should (equal (org-element-property :LOCATION elem)
                        "Foobar's desk"))
         (should (equal (org-element-property :LINK elem)
                        "[[https://google.com][Google]]"))
         (should (equal (org-element-property :TRANSPARENCY elem)
                        "opaque"))
         (should (equal (org-element-property :CALENDAR-ID elem)
                        "foo@foobar.com"))
         (should (equal (org-element-property :ENTRY-ID elem)
                        "foobar1234/foo@foobar.com")))
       ;; Check contents of "org-gcal" drawer
       (re-search-forward ":org-gcal:")
       (let ((elem (org-element-at-point)))
         (should (equal (org-element-property :drawer-name elem)
                        "org-gcal"))
         (should (equal (buffer-substring-no-properties
                         (org-element-property :contents-begin elem)
                         (org-element-property :contents-end elem))
                        "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))
       ;; Now check that the event is removed when
       ;; ‘org-gcal-remove-events-with-cancelled-todo’ is t.
       (setq org-gcal-remove-api-cancelled-events t
             org-gcal-remove-events-with-cancelled-todo t)
       (org-back-to-heading)
       (org-gcal--update-entry org-gcal-test-calendar-id
                               org-gcal-test-cancelled-event)
       (should (equal (buffer-substring-no-properties
                       (point-min) (point-max))
                      ""))))))

(ert-deftest org-gcal-test--update-existing-entry-scheduled ()
  "Same as ‘org-gcal-test--update-existing-entry’, but with SCHEDULED
property."
  (org-gcal-test--with-temp-buffer
   "\
* Old event summary
SCHEDULED: <9999-10-06 Sun 17:00-21:00>
:PROPERTIES:
:ETag:     \"9999\"
:LOCATION: Somewhere else
:link: [[https://yahoo.com][Yahoo!]]
:TRANSPARENCY: transparent
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
Old event description
:END:
"
   (org-gcal--update-entry org-gcal-test-calendar-id
                           org-gcal-test-event)
   (org-back-to-heading)
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property
                     :raw-value
                     (org-element-property :scheduled elem))
                    "<2019-10-06 Sun 17:00-21:00>"))
     (should (equal (org-gcal-test--title-to-string elem)
                    "My event summary"))
     (should (equal (org-element-property :ETAG elem)
                    "\"12344321\""))
     (should (equal (org-element-property :LOCATION elem)
                    "Foobar's desk"))
     (should (equal (org-element-property :LINK elem)
                    "[[https://google.com][Google]]"))
     (should (equal (org-element-property :TRANSPARENCY elem)
                    "opaque"))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "foo@foobar.com"))
     (should (equal (org-element-property :ENTRY-ID elem)
                    "foobar1234/foo@foobar.com")))
   ;; Check contents of "org-gcal" drawer
   (re-search-forward ":org-gcal:")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :drawer-name elem)
                    "org-gcal"))
     (should (equal (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))
                    "\
My event description

Second paragraph
")))))

(ert-deftest org-gcal-test--update-existing-entry-with-id ()
  "Verify that existing IDs in an existing headline will be preserved."
  (org-gcal-test--with-temp-buffer
   "\
* Old event summary
:PROPERTIES:
:LOCATION: Somewhere else
:link: [[https://yahoo.com][Yahoo!]]
:TRANSPARENCY: transparent
:calendar-id: foo@foobar.com
:entry-id:       ABCD-EFGH
:END:
:org-gcal:
<9999-10-06 Sun 17:00-21:00>

Old event description
:END:
"
   (org-gcal--update-entry org-gcal-test-calendar-id
                           org-gcal-test-event)
   (org-back-to-heading)
   (let ((elem (org-element-at-point)))
     (should (equal (org-gcal-test--title-to-string elem)
                    "My event summary"))
     (should (equal (org-element-property :ETAG elem)
                    "\"12344321\""))
     (should (equal (org-element-property :LOCATION elem)
                    "Foobar's desk"))
     (should (equal (org-element-property :LINK elem)
                    "[[https://google.com][Google]]"))
     (should (equal (org-element-property :TRANSPARENCY elem)
                    "opaque"))
     (should (equal (org-element-property :CALENDAR-ID elem)
                    "foo@foobar.com")))
   ;; The canonical ID should be that generated by org-gcal.
   (should (equal (org-gcal--all-property-local-values (point) org-gcal-entry-id-property nil)
                  '("foobar1234/foo@foobar.com")))
   (should (equal (org-entry-get (point) org-gcal-entry-id-property)
                  '"foobar1234/foo@foobar.com"))
   ;; Check contents of "org-gcal" drawer
   (re-search-forward ":org-gcal:")
   (let ((elem (org-element-at-point)))
     (should (equal (org-element-property :drawer-name elem)
                    "org-gcal"))
     (should (equal (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))
                    "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
")))))

(ert-deftest org-gcal-test--post-at-point-basic ()
  "Verify basic case of ‘org-gcal-post-to-point’."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" "foobar1234"
                                * * *))
    (let ((org-gcal-managed-post-at-point-update-existing 'always-push))
      (org-gcal-post-at-point)))))

(defun org-gcal-test--sync-update-stale-entry (refound)
  "Run `org-gcal--sync-update-entries' on one entry with a stale marker.
The entry is synced with `skip-export', so it is updated from
`org-gcal-test-event'.  `org-gcal--id-find', used to re-find the
entry after its cached marker went stale, is stubbed to return REFOUND."
  (cl-letf (((symbol-function 'org-gcal--id-find)
             (lambda (&rest _) refound)))
    (deferred:sync!
     (org-gcal--sync-update-entries
      org-gcal-test-calendar-id
      (list
       (org-gcal--event-entry-create
        :entry-id "foobar1234/foo@foobar.com"
        :marker (org-gcal-test--stale-marker "* My event summary\n")
        :event org-gcal-test-event))
      t))))

(ert-deftest org-gcal-test--sync-update-entries-stale-marker-refinds-entry ()
  "Verify sync re-finds an entry by ID when its cached marker is stale."
  (org-gcal-test--with-temp-buffer
      "\
* Old event summary
:PROPERTIES:
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
"
    (org-gcal-test--sync-update-stale-entry (point-marker))
    (goto-char (point-min))
    (let ((elem (org-element-at-point)))
      (should (equal (org-gcal-test--title-to-string elem)
                     "My event summary"))
      (should (equal (org-element-property :ETAG elem)
                     "\"12344321\"")))))

(ert-deftest org-gcal-test--sync-update-entries-stale-marker-not-found ()
  "Verify sync skips an entry whose stale marker cannot be re-found."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'org-gcal--update-entry)
               (lambda (&rest _)
                 (ert-fail "org-gcal--update-entry called for missing entry"))))
      (org-gcal-test--sync-update-stale-entry nil))
    (should
     (cl-some
      (lambda (message)
        (string-match-p "skipping entry foobar1234/foo@foobar.com.*not found"
                        message))
      messages))))

(ert-deftest org-gcal-test--sync-update-entries-refound-marker-killed ()
  "Verify sync logs a clear error when the re-found marker is also stale."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (org-gcal-test--sync-update-stale-entry
       (org-gcal-test--stale-marker "* My event summary\n")))
    (should
     (cl-some
      (lambda (message)
        (string-match-p
         "error:.*marker.*buffer.*foobar1234/foo@foobar.com.*killed"
         message))
      messages))))

(defconst org-gcal-test--update-entries-buffer
  "\
* Old event summary
:PROPERTIES:
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
* Second event
:PROPERTIES:
:calendar-id: foo@foobar.com
:entry-id:       second5678/foo@foobar.com
:END:
"
  "Two entries for `org-gcal--sync-update-entries' to update.")

(defun org-gcal-test--update-entries (done)
  "Update the entries of the current buffer, then call DONE.
The current buffer holds `org-gcal-test--update-entries-buffer'.  Run
`org-gcal--sync-update-entries' on both entries, and call DONE with no
argument once the deferred object it returns succeeds."
  (let ((entries
         (mapcar (lambda (entry-id)
                   (goto-char (point-min))
                   (re-search-forward (regexp-quote entry-id))
                   (org-back-to-heading t)
                   (org-gcal--event-entry-create
                    :entry-id entry-id
                    :marker (point-marker)
                    :event org-gcal-test-event))
                 '("foobar1234/foo@foobar.com" "second5678/foo@foobar.com"))))
    (deferred:sync!
     (deferred:nextc
      (org-gcal--sync-update-entries org-gcal-test-calendar-id entries t)
      (lambda (_) (funcall done))))))

(ert-deftest org-gcal-test--sync-update-entries-waits-for-updates ()
  "Verify the deferred object returned by an update succeeds after it.
The steps of a sync that follow the update of entries, such as the
compaction of recurring event instances, must not run before."
  (let ((log nil))
    (org-gcal-test--with-temp-buffer org-gcal-test--update-entries-buffer
      (cl-letf (((symbol-function 'org-gcal--update-entry)
                 (lambda (&rest _)
                   (push (org-entry-get nil org-gcal-entry-id-property) log))))
        (org-gcal-test--update-entries (lambda () (push 'done log)))))
    (should (equal (reverse log)
                   '("foobar1234/foo@foobar.com" "second5678/foo@foobar.com"
                     done)))))

(ert-deftest org-gcal-test--sync-update-entries-error-skips-entry ()
  "Verify an error updating an entry is logged, and the others are updated."
  (let ((log nil)
        (messages nil))
    (org-gcal-test--with-temp-buffer org-gcal-test--update-entries-buffer
      (cl-letf (((symbol-function 'org-gcal--update-entry)
                 (lambda (&rest _)
                   (let ((entry-id (org-entry-get nil org-gcal-entry-id-property)))
                     (when (equal entry-id "foobar1234/foo@foobar.com")
                       (error "Cannot update %s" entry-id))
                     (push entry-id log))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (org-gcal-test--update-entries (lambda () (push 'done log)))))
    (should (equal (reverse log) '("second5678/foo@foobar.com" done)))
    (should
     (cl-some
      (lambda (message)
        (string-match-p "error:.*Cannot update foobar1234/foo@foobar.com"
                        message))
      messages))))

(ert-deftest org-gcal-test--with-point-at-no-widen-stale-marker ()
  "Verify stale markers report the killed buffer before moving point."
  (org-gcal-test--should-error-match "marker.*buffer has been killed"
    (org-gcal--with-point-at-no-widen
        (org-gcal-test--stale-marker "* My event summary\n")
      (point))))

;;; Entry ID index

(defun org-gcal-test--write-entry-file (file seconds &optional entry-id)
  "Write FILE, holding an entry with ENTRY-ID if non-nil.
Set FILE's modification time to SECONDS after a fixed base time."
  (with-temp-file file
    (when entry-id
      (insert "* My event summary\n:PROPERTIES:\n:entry-id: " entry-id
              "\n:END:\n")))
  (set-file-times file (time-add (seconds-to-time 1767268800) seconds)))

(ert-deftest org-gcal-test--id-find-rescans-stale-index ()
  "Verify `org-gcal--id-find' rescans when the index records a moved entry.
Refiling an entry after the last scan leaves the index recording the
file it left; a fetch must still find the entry, or it inserts a
duplicate."
  (let* ((entry-id "foobar1234/foo@foobar.com")
         (old (abbreviate-file-name
               (file-truename (make-temp-file "old." nil ".org"))))
         (new (abbreviate-file-name
               (file-truename (make-temp-file "new." nil ".org"))))
         (org-generic-id-locations (org-generic-id--make-hash-table))
         (org-generic-id--file-fingerprints nil)
         (org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-agenda-files (list old new))
         (org-generic-id-extra-files nil)
         (org-generic-id-search-archives nil)
         (org-id-locations (make-hash-table :test #'equal)))
    (org-gcal-test--write-entry-file old 0 entry-id)
    (org-gcal-test--write-entry-file new 0)
    (org-generic-id-update-id-locations org-gcal-entry-id-property nil t)
    (should (equal (org-generic-id-find-id-file org-gcal-entry-id-property
                                                entry-id t)
                   old))
    ;; Refile the entry from OLD to NEW.
    (org-gcal-test--write-entry-file old 60)
    (org-gcal-test--write-entry-file new 60 entry-id)
    ;; Fetches run in the calendar file, which no longer holds the entry.
    (org-gcal-test--with-temp-buffer ""
      (should (equal (org-gcal--id-find entry-id) (cons new 1))))))

(ert-deftest org-gcal-test--put-id-indirect-buffer ()
  "Verify `org-gcal--put-id' indexes the file of an indirect buffer's base.
Org capture edits its target file through an indirect buffer."
  (let* ((base (find-file-noselect (make-temp-file "capture." nil ".org")))
         (file (buffer-file-name base))
         (added nil))
    (unwind-protect
        (let ((indirect (make-indirect-buffer
                         base (generate-new-buffer-name "*capture*") t)))
          (unwind-protect
              (with-current-buffer indirect
                (insert "* My event summary\n")
                (cl-letf (((symbol-function 'org-generic-id-add-location)
                           (lambda (&rest args) (push args added))))
                  (org-gcal--put-id (point-min) org-gcal-test-calendar-id
                                    "foobar1234")))
            (kill-buffer indirect)))
      (with-current-buffer base (set-buffer-modified-p nil))
      (kill-buffer base))
    (should (equal added
                   `((,org-gcal-entry-id-property "foobar1234/foo@foobar.com"
                                                  ,file))))))

(defmacro org-gcal-test--with-stubbed-sync (calls &rest body)
  "Run BODY with the ID scan and calendar fetch of a sync stubbed.
The stubs push `scan' and `fetch' onto CALLS as they run."
  (declare (indent 1) (debug t))
  `(let ((org-gcal-fetch-file-alist
          (list (cons org-gcal-test-calendar-id
                      (make-temp-file "calendar." nil ".org"))))
         (org-gcal-auto-archive nil)
         (org-gcal--sync-lock nil))
     (cl-letf (((symbol-function 'org-generic-id-update-id-locations)
                (lambda (&rest _) (push 'scan ,calls)))
               ((symbol-function 'org-generic-id-files) #'ignore)
               ((symbol-function 'org-gcal--sync-calendar)
                (lambda (&rest _)
                  (push 'fetch ,calls)
                  (deferred:succeed nil))))
       ,@body)))

(ert-deftest org-gcal-test--sync-scans-index-before-fetching ()
  "Verify `org-gcal-sync' updates the entry ID index before fetching.
Entries refiled since the last sync are otherwise looked up in the files
they left."
  (let ((calls nil))
    (org-gcal-test--with-stubbed-sync calls
      (org-gcal-sync t t)
      (deferred:flush-queue!)
      (should-not org-gcal--sync-lock))
    (should (equal (reverse calls) '(scan fetch scan)))))

(ert-deftest org-gcal-test--sync-scan-error-releases-lock ()
  "Verify a failed ID scan leaves the sync lock free."
  (let ((calls nil))
    (org-gcal-test--with-stubbed-sync calls
      (cl-letf (((symbol-function 'org-generic-id-update-id-locations)
                 (lambda (&rest _) (error "Scan failed"))))
        (should-error (org-gcal-sync t t)))
      (should-not org-gcal--sync-lock))
    (should-not calls)))

;;; Instances of recurring events

(defconst org-gcal-test--parent-event
  `(:kind "calendar#event" :etag "\"1\"" :id "parent123" :status "confirmed"
    :summary "My recurring event"
    :start (:dateTime "2026-09-30T08:00:00-07:00")
    :end (:dateTime "2026-09-30T13:00:00-07:00"))
  "Master of a recurring event, without the recurrence rule.
Compaction then leaves the parent's timestamps alone.")

(defconst org-gcal-test--moved-instance
  `(:kind "calendar#event" :etag "\"2\"" :id "parent123_20261014T150000Z"
    :status "confirmed" :summary "My recurring event"
    :recurringEventId "parent123"
    :originalStartTime (:dateTime "2026-10-14T08:00:00-07:00")
    :start (:dateTime "2026-10-16T08:00:00-07:00")
    :end (:dateTime "2026-10-16T13:00:00-07:00"))
  "Instance of `org-gcal-test--parent-event' moved to another day.")

(defconst org-gcal-test--parent-entry
  "* Calendar
** My recurring event
:PROPERTIES:
:calendar-id: foo@foobar.com
:entry-id: parent123/foo@foobar.com
:org-gcal-managed: gcal
:END:
"
  "Heading of `org-gcal-test--parent-event'.")

(defconst org-gcal-test--instance-entry
  "*** My recurring event
:PROPERTIES:
:ETag:     \"2\"
:calendar-id: foo@foobar.com
:entry-id: parent123_20261014T150000Z/foo@foobar.com
:org-gcal-managed: gcal
:END:

<2026-10-16 Fri 08:00-13:00>
"
  "Heading of `org-gcal-test--moved-instance', under its parent.")

(defconst org-gcal-test--instance-entry-id
  "parent123_20261014T150000Z/foo@foobar.com")

(defun org-gcal-test--write-file (file seconds &rest contents)
  "Write CONTENTS to FILE, modified SECONDS after a fixed base time."
  (with-temp-file file
    (apply #'insert contents))
  (set-file-times file (time-add (seconds-to-time 1767268800) seconds)))

(defmacro org-gcal-test--with-instance-files (instance-p &rest body)
  "Run BODY with a parent file and a calendar file visited.
The parent file holds `org-gcal-test--parent-entry', followed by
`org-gcal-test--instance-entry' if INSTANCE-P.  The ID index scans both
files, but records only the parent: it doesn't know about an instance
another computer added.  BODY runs in the calendar file's buffer, and
can refer to `parent-file', `parent-buffer' and `calendar-file'."
  (declare (indent 1) (debug t))
  `(let* ((parent-file (abbreviate-file-name
                        (file-truename (make-temp-file "parent." nil ".org"))))
          (calendar-file (abbreviate-file-name
                          (file-truename
                           (make-temp-file "calendar." nil ".org"))))
          (org-generic-id-locations (org-generic-id--make-hash-table))
          (org-generic-id--file-fingerprints nil)
          (org-generic-id--reported-duplicates
           (org-generic-id--make-hash-table))
          (org-generic-id-locations-file
           (make-temp-file "org-generic-id-locations."))
          (org-agenda-files (list parent-file calendar-file))
          (org-generic-id-extra-files nil)
          (org-generic-id-search-archives nil)
          (org-id-locations (make-hash-table :test #'equal))
          (org-gcal--parent-event-cache nil)
          (org-gcal--instance-collector nil)
          (parent-buffer nil)
          (calendar-buffer nil))
     (org-gcal-test--write-file parent-file 0 org-gcal-test--parent-entry
                                (if ,instance-p
                                    org-gcal-test--instance-entry
                                  ""))
     (org-gcal-test--write-file calendar-file 0 "")
     (unwind-protect
         (progn
           (setq parent-buffer (find-file-noselect parent-file)
                 calendar-buffer (find-file-noselect calendar-file))
           (org-generic-id-update-id-locations org-gcal-entry-id-property
                                               nil t)
           (remhash org-gcal-test--instance-entry-id
                    (gethash org-gcal-entry-id-property
                             org-generic-id-locations))
           (with-current-buffer calendar-buffer
             ,@body))
       (dolist (buffer (list parent-buffer calendar-buffer))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(defun org-gcal-test--count-instance-entries (buffer)
  "Return the number of entries for the moved instance in BUFFER."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (how-many (concat "^:entry-id: "
                       (regexp-quote org-gcal-test--instance-entry-id)
                       "$")))))

(ert-deftest org-gcal-test--id-find-instance-after-revert ()
  "Verify `org-gcal--id-find' finds an entry read by reverting its buffer.
Another computer added it to the file after the ID index was updated."
  (org-gcal-test--with-instance-files nil
    (should-not (org-gcal--id-find org-gcal-test--instance-entry-id))
    (org-gcal-test--write-file parent-file 60 org-gcal-test--parent-entry
                               org-gcal-test--instance-entry)
    (with-current-buffer parent-buffer
      (revert-buffer t t t))
    (let ((marker (org-gcal--id-find org-gcal-test--instance-entry-id
                                     'markerp)))
      (should (eq (marker-buffer marker) parent-buffer)))))

(ert-deftest org-gcal-test--compact-instances-finds-unindexed-instance ()
  "Verify compaction doesn't insert an instance already under its parent.
The ID index misses the instance, and the calendar file is not the
parent's.  Compaction returns the existing entry for an update."
  (org-gcal-test--with-instance-files t
    (let ((org-gcal--parent-event-cache (make-hash-table :test #'equal))
          (org-gcal--instance-collector (make-hash-table :test #'equal)))
      (puthash "parent123" org-gcal-test--parent-event
               org-gcal--parent-event-cache)
      (puthash "parent123" (list org-gcal-test--moved-instance)
               org-gcal--instance-collector)
      (let ((entries (org-gcal--compact-instances org-gcal-test-calendar-id
                                                  calendar-file)))
        (should (= (length entries) 1))
        (should (equal (org-gcal--event-entry-entry-id (car entries))
                       org-gcal-test--instance-entry-id))
        (should (eq (marker-buffer (org-gcal--event-entry-marker (car entries)))
                    parent-buffer))
        (should (eq (org-gcal--event-entry-event (car entries))
                    org-gcal-test--moved-instance))))
    (should (= (org-gcal-test--count-instance-entries parent-buffer) 1))
    (should (equal (org-generic-id-find-id-file
                    org-gcal-entry-id-property org-gcal-test--instance-entry-id
                    'no-fallback)
                   parent-file))))

(ert-deftest org-gcal-test--compact-instances-inserts-missing-instance ()
  "Verify compaction inserts a modified instance missing from its parent."
  (org-gcal-test--with-instance-files nil
    (let ((org-gcal--parent-event-cache (make-hash-table :test #'equal))
          (org-gcal--instance-collector (make-hash-table :test #'equal)))
      (puthash "parent123" org-gcal-test--parent-event
               org-gcal--parent-event-cache)
      (puthash "parent123" (list org-gcal-test--moved-instance)
               org-gcal--instance-collector)
      (should-not (org-gcal--compact-instances org-gcal-test-calendar-id
                                               calendar-file)))
    (should (= (org-gcal-test--count-instance-entries parent-buffer) 1))))

(ert-deftest org-gcal-test--sync-handle-events-finds-unindexed-instance ()
  "Verify an instance of an uncached parent already under it is not inserted.
Pass 2 inserts such an instance directly under its parent; it returns
the existing entry for an update instead."
  (org-gcal-test--with-instance-files t
    (let ((entries (org-gcal--sync-handle-events
                    org-gcal-test-calendar-id calendar-file
                    (vector org-gcal-test--moved-instance)
                    nil nil nil nil nil :instances)))
      (should (= (length entries) 1))
      (should (equal (org-gcal--event-entry-entry-id (car entries))
                     org-gcal-test--instance-entry-id))
      (should (eq (marker-buffer (org-gcal--event-entry-marker (car entries)))
                  parent-buffer)))
    (should (= (org-gcal-test--count-instance-entries parent-buffer) 1))))

(ert-deftest org-gcal-test--sync-handle-events-inserts-missing-instance ()
  "Verify an instance of an uncached parent is inserted under it."
  (org-gcal-test--with-instance-files nil
    (should-not (org-gcal--sync-handle-events
                 org-gcal-test-calendar-id calendar-file
                 (vector org-gcal-test--moved-instance)
                 nil nil nil nil nil :instances))
    (should (= (org-gcal-test--count-instance-entries parent-buffer) 1))))

(ert-deftest org-gcal-test--sync-calendar-events-updates-compacted-entries ()
  "Verify the entries returned by compaction are updated."
  (let ((updates nil)
        (org-gcal--instance-collector (make-hash-table :test #'equal)))
    (puthash "parent123" (list org-gcal-test--moved-instance)
             org-gcal--instance-collector)
    (cl-letf (((symbol-function 'org-gcal--sync-request-events)
               (lambda (&rest _) (deferred:succeed nil)))
              ((symbol-function 'org-gcal--sync-handle-response)
               (lambda (&rest _) []))
              ((symbol-function 'org-gcal--sync-handle-events)
               (lambda (&rest _) nil))
              ((symbol-function 'org-gcal--compact-instances)
               (lambda (&rest _) '(compacted)))
              ((symbol-function 'org-gcal--sync-update-entries)
               (lambda (_calendar-id entries skip-export)
                 (push (list entries skip-export) updates)
                 (deferred:succeed nil))))
      (org-gcal--sync-calendar-events
       (cons org-gcal-test-calendar-id "calendar.org") 'skip-export nil nil
       nil nil nil :instances)
      (deferred:flush-queue!))
    (should (member '((compacted) skip-export) updates))))

(ert-deftest org-gcal-test--sync-calendar-events-compacts-after-updates ()
  "Verify compaction runs after the fetched entries are updated.
Compaction can delete the heading of an entry: an update that runs
after it would rewrite the heading that follows instead."
  (let ((log nil)
        (org-gcal--instance-collector (make-hash-table :test #'equal)))
    (puthash "parent123" (list org-gcal-test--moved-instance)
             org-gcal--instance-collector)
    (org-gcal-test--with-temp-buffer org-gcal-test--update-entries-buffer
      (let ((entry (org-gcal--event-entry-create
                    :entry-id "foobar1234/foo@foobar.com"
                    :marker (point-marker)
                    :event org-gcal-test-event)))
        (cl-letf (((symbol-function 'org-gcal--sync-request-events)
                   (lambda (&rest _) (deferred:succeed nil)))
                  ((symbol-function 'org-gcal--sync-handle-response)
                   (lambda (&rest _) []))
                  ((symbol-function 'org-gcal--sync-handle-events)
                   (lambda (&rest _) (list entry)))
                  ((symbol-function 'org-gcal--update-entry)
                   (lambda (&rest _) (push 'update log)))
                  ((symbol-function 'org-gcal--compact-instances)
                   (lambda (&rest _) (push 'compact log) nil)))
          (org-gcal--sync-calendar-events
           (cons org-gcal-test-calendar-id "calendar.org") 'skip-export nil nil
           nil nil nil :instances)
          (deferred:flush-queue!))))
    (should (equal (reverse log) '(update compact)))))

(ert-deftest org-gcal-test--post-at-point-api-response ()
  "Verify that ‘org-gcal-post-to-point’ updates an event using the data
returned from the Google Calendar API."
  (org-gcal-test--with-temp-buffer
   "\
* Original summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Original location
:link: [[https://yahoo.com][Yahoo!]]
:TRANSPARENCY: transparent
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<2021-03-05 Fri 12:00-14:00>

Original description

Original second paragraph
:END:
"
   (defvar update-entry-hook-called nil)
   (setq update-entry-hook-called nil)
   (let (org-gcal-after-update-entry-functions)
     (defun update-entry-hook (calendar-id event update-mode)
       (message "update-entry-hook %S %S %S" calendar-id event update-mode)
       (setq update-entry-hook-called t))
     (add-hook 'org-gcal-after-update-entry-functions #'update-entry-hook)
     (with-mock
      (stub org-gcal--time-zone => '(0 "UTC"))
      (stub org-generic-id-add-location => nil)
      (stub org-gcal--get-access-token => "my_access_token")
      (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
      (stub request-deferred =>
            (deferred:succeed
             (make-request-response
              :status-code 200
              :data org-gcal-test-event)))
      (let ((org-gcal-managed-post-at-point-update-existing 'always-push))
        (org-gcal-post-at-point)
        (org-back-to-heading)
        (should (equal update-entry-hook-called t))
        (let ((elem (org-element-at-point)))
          (should (equal (org-gcal-test--title-to-string elem)
                         "My event summary"))
          (should (equal (org-element-property :ETAG elem)
                         "\"12344321\""))
          (should (equal (org-element-property :LOCATION elem)
                         "Foobar's desk"))
          (should (equal (org-element-property :LINK elem)
                         "[[https://google.com][Google]]"))
          (should (equal (org-element-property :TRANSPARENCY elem)
                         "opaque"))
          (should (equal (org-element-property :CALENDAR-ID elem)
                         "foo@foobar.com"))
          (should (equal (org-element-property :ENTRY-ID elem)
                         "foobar1234/foo@foobar.com")))
        ;; Check contents of "org-gcal" drawer
        (re-search-forward ":org-gcal:")
        (let ((elem (org-element-at-point)))
          (should (equal (org-element-property :drawer-name elem)
                         "org-gcal"))
          (should (equal (buffer-substring-no-properties
                          (org-element-property :contents-begin elem)
                          (org-element-property :contents-end elem))
                         "\
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
"))))))))

(ert-deftest org-gcal-test--post-event-stale-marker ()
  "Verify that post-event callbacks report stale markers clearly."
  (let ((marker (org-gcal-test--stale-marker "* My event summary\n")))
    (cl-letf (((symbol-function 'org-gcal--get-access-token)
               (lambda (_calendar-id) "my_access_token"))
              ((symbol-function 'request-deferred)
               (lambda (&rest _args)
                 (deferred:succeed
                  (make-request-response
                   :status-code 200
                   :data org-gcal-test-event)))))
      (org-gcal-test--should-error-match "marker.*buffer has been killed"
        (deferred:sync!
         (org-gcal--post-event
          "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
          "My event summary" "Foobar's desk"
          '((url . "https://google.com") (title . "Google"))
          "My event description"
          org-gcal-test-calendar-id
          marker "opaque" "\"12344321\"" "foobar1234"))))))

(ert-deftest org-gcal-test--post-at-point-managed-update-existing-gcal ()
  "Verify ‘org-gcal-post-at-point’ with ‘org-gcal-managed-update-existing-mode’
set to \"gcal\"."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (y-or-n-p *) => nil)
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" "foobar1234"
                                * * t))
    (let ((org-gcal-managed-update-existing-mode "gcal"))
      (org-gcal-post-at-point)))))

(ert-deftest org-gcal-test--post-at-point-managed-update-existing-org ()
  "Verify ‘org-gcal-post-at-point’ with ‘org-gcal-managed-update-existing-mode’
set to \"org\"."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" "foobar1234"
                                * * nil))
    (let ((org-gcal-managed-update-existing-mode "org"))
      (org-gcal-post-at-point)))))

(ert-deftest org-gcal-test--post-at-point-managed-create-from-entry-gcal ()
  "Verify ‘org-gcal-post-at-point’ with ‘org-gcal-managed-create-from-entry-mode’
set to \"gcal\"."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (y-or-n-p *) => nil)
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                nil
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" nil
                                * * t))
    (let ((org-gcal-managed-update-existing-mode "gcal")
          (org-gcal-managed-create-from-entry-mode "gcal"))
      (org-gcal-post-at-point)))))

(ert-deftest org-gcal-test--post-at-point-managed-create-from-entry-org ()
  "Verify ‘org-gcal-post-at-point’ with ‘org-gcal-managed-create-from-entry-mode’
set to \"org\"."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" nil
                                * * nil))
    (let ((org-gcal-managed-update-existing-mode "gcal")
          (org-gcal-managed-create-from-entry-mode "org"))
      (org-gcal-post-at-point)))))

(ert-deftest org-gcal-test--post-at-point-old-id-property ()
  "Verify that \":ID:\" property is read for event ID by \
‘org-gcal-post-to-point’ only if ‘org-gcal-entry-id-property’ is not present."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:ID:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" "foobar1234"
                                * * *))
    (let ((org-gcal-managed-post-at-point-update-existing 'always-push))
      (org-gcal-post-at-point))))
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:ID:             hello-world
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" "foobar1234"
                                * * *))
    (let ((org-gcal-managed-post-at-point-update-existing 'always-push))
      (org-gcal-post-at-point)))))


(ert-deftest org-gcal-test--post-at-point-no-id ()
  "Verify that ‘org-gcal-post-to-point’ doesn't send an ID to Calendar API if
an org-gcal Calendar Event ID can't be retrieved from the current entry."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" nil nil
                                * * *))
    (org-gcal-post-at-point)))
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:entry-id: ABCD-EFGH
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-06T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" nil nil
                                * * *))
    (org-gcal-post-at-point))))

(ert-deftest org-gcal-test--post-at-point-no-properties ()
  "Verify that ‘org-gcal-post-to-point’ fills in entries with no relevant
org-gcal properties with sane default values."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
"
   (with-mock
    (stub completing-read => "foo@foobar.com")
    (stub org-read-date => (encode-time 0 0 17 6 10 2019 nil nil t))
    (stub read-from-minibuffer => "4:00")
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00+0000" "2019-10-06T21:00:00+0000"
                                "My event summary" nil
                                nil nil
                                "foo@foobar.com"
                                * "opaque" nil nil
                                * * *))
    (org-gcal-post-at-point)))
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:Effort: 2:00
:END:
:LOGBOOK:
CLOCK: [2019-06-06 Thu 17:00]--[2019-06-06 Thu 18:00] => 1:00
:END:
"
   (with-mock
    (stub completing-read => "foo@foobar.com")
    (stub org-read-date => (encode-time 0 0 17 6 10 2019 nil nil t))
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (cl-letf
        (((symbol-function #'read-from-minibuffer)
          (lambda (_p initial-contents) initial-contents)))
      (mock (org-gcal--post-event "2019-10-06T17:00:00+0000" "2019-10-06T18:00:00+0000"
                                  "My event summary" nil
                                  nil nil
                                  "foo@foobar.com"
                                  * "opaque" nil nil
                                  * * *))
      (org-gcal-post-at-point)))))

(ert-deftest org-gcal-test--post-at-point-default-duration ()
  "Verify that missing end time uses ‘org-gcal-event-default-duration’."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
"
   (with-mock
    (stub completing-read => "foo@foobar.com")
    (stub org-read-date => (encode-time 0 0 17 6 10 2019 nil nil t))
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (let ((org-gcal-event-default-duration 30))
      (cl-letf (((symbol-function #'read-from-minibuffer)
                 (lambda (_prompt initial-contents)
                   (should (equal initial-contents "0:30"))
                   initial-contents)))
        (mock (org-gcal--post-event "2019-10-06T17:00:00+0000" "2019-10-06T17:30:00+0000"
                                    "My event summary" nil
                                    nil nil
                                    "foo@foobar.com"
                                    * "opaque" nil nil
                                    * * *))
        (org-gcal-post-at-point))))))

(ert-deftest org-gcal-test--post-at-point-default-duration-minimum ()
  "Verify ‘org-gcal-event-default-duration’ is a minimum duration."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:Effort: 2:00
:END:
:LOGBOOK:
CLOCK: [2019-06-06 Thu 17:00]--[2019-06-06 Thu 18:00] => 1:00
:END:
"
   (with-mock
    (stub completing-read => "foo@foobar.com")
    (stub org-read-date => (encode-time 0 0 17 6 10 2019 nil nil t))
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (let ((org-gcal-event-default-duration 90))
      (cl-letf (((symbol-function #'read-from-minibuffer)
                 (lambda (_prompt initial-contents)
                   (should (equal initial-contents "1:30"))
                   initial-contents)))
        (mock (org-gcal--post-event "2019-10-06T17:00:00+0000" "2019-10-06T18:30:00+0000"
                                    "My event summary" nil
                                    nil nil
                                    "foo@foobar.com"
                                    * "opaque" nil nil
                                    * * *))
        (org-gcal-post-at-point))))))

(ert-deftest org-gcal-test--post-at-point-etag-no-id ()
  "Verify that ‘org-gcal-post-to-point’ fails if an ETag is present but
an event ID is not."
  :expected-result :failed
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:LOCATION: Foobar's desk
:TRANSPARENCY: opaque
:ETag:     \"12344321\"
:calendar-id: foo@foobar.com
:END:
:org-gcal:
<2019-10-06 Sun 17:00-21:00>

My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (stub request-deferred => (deferred:succeed nil))
    (org-gcal-post-at-point))))

(ert-deftest org-gcal-test--post-at-point-time-date-range ()
  "Verify that entry with a time/date range for its timestamp is parsed by
‘org-gcal-post-to-point’ (see https://orgmode.org/manual/Timestamps.html)."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
SCHEDULED: <2019-10-06 Sun 17:00>--<2019-10-07 Mon 21:00>
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
My event description

Second paragraph
:END:
"
   (with-mock
    (stub org-gcal--time-zone => '(0 "UTC"))
    (stub org-generic-id-add-location => nil)
    (stub org-gcal--get-access-token => "my_access_token")
    (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
    (mock (org-gcal--post-event "2019-10-06T17:00:00Z" "2019-10-07T21:00:00Z"
                                "My event summary" "Foobar's desk"
                                `((url . "https://google.com") (title . "Google"))
                                "My event description\n\nSecond paragraph"
                                "foo@foobar.com"
                                * "opaque" "\"12344321\"" "foobar1234"
                                * * *))
    (let ((org-gcal-managed-post-at-point-update-existing 'always-push))
      (org-gcal-post-at-point)))))

(ert-deftest org-gcal-test--delete-at-point-delete-drawer ()
  "Verify that the org-gcal drawer is deleted by ‘org-gcal-delete-at-point’ if
and only if the event at the point is successfully deleted by the Google
Calendar API."
  (let ((org-gcal-remove-api-cancelled-events nil)
        (org-gcal-update-cancelled-events-with-todo nil)
        (buf "\
* My event summary
SCHEDULED: <2019-10-06 Sun 17:00>--<2019-10-07 Mon 21:00>
:PROPERTIES:
:ETag:     \"12344321\"
:LOCATION: Foobar's desk
:link: [[https://google.com][Google]]
:TRANSPARENCY: opaque
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
My event description

Second paragraph
:END:
"))
    (org-gcal-test--with-temp-buffer
     buf
     ;; Don’t delete drawer if we don’t receive 200.
     (with-mock
      (let ((deferred:debug t))
        (stub org-gcal--time-zone => '(0 "UTC")))
      (stub org-generic-id-add-location => nil)
      (stub org-gcal--get-access-token => "my_access_token")
      (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
      (stub y-or-n-p => t)
      (stub alert => t)
      (stub request-deferred =>
            (deferred:succeed
             (make-request-response
              :status-code 500
              :error-thrown '(error . nil))))
      (deferred:sync!
       (deferred:$
        (org-gcal-delete-at-point)
        (deferred:error it #'ignore)))
      (org-back-to-heading)
      (should (re-search-forward ":org-gcal:" nil 'noerror))))

    ;; Delete drawer if we do receive 200.
    (org-gcal-test--with-temp-buffer
     buf
     (with-mock
      (let ((deferred:debug t))
        (stub org-gcal--time-zone => '(0 "UTC"))
        (stub org-generic-id-add-location => nil)
        (stub org-gcal--get-access-token => "my_access_token")
        (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
        (stub y-or-n-p => t)
        (stub request-deferred =>
              (deferred:succeed
               (make-request-response
                :status-code 200)))
        (deferred:sync! (org-gcal-delete-at-point))
        (org-back-to-heading)
        (should-not (re-search-forward ":org-gcal:" nil 'noerror)))))

    ;; Delete the entire entry if configured to
    (org-gcal-test--with-temp-buffer
     buf
     (with-mock
      (let ((deferred:debug t)
            (org-gcal-remove-api-cancelled-events t))
        (stub org-gcal--time-zone => '(0 "UTC"))
        (stub org-gcal--get-access-token => "my_access_token")
        (stub org-gcal--refresh-token => (deferred:succeed "test_access_token"))
        (stub y-or-n-p => t)
        (stub request-deferred =>
              (deferred:succeed
               (make-request-response
                :status-code 200)))
        (deferred:sync! (org-gcal-delete-at-point))
        (should (equal (buffer-string) "")))))))

(ert-deftest org-gcal-test--delete-event-stale-marker-on-etag-conflict ()
  "Verify delete-event reports stale markers clearly after HTTP 412."
  (let ((marker (org-gcal-test--stale-marker "* My event summary\n")))
    (cl-letf (((symbol-function 'org-gcal--get-access-token)
               (lambda (_calendar-id) "my_access_token"))
              ((symbol-function 'org-gcal--notify) #'ignore)
              ((symbol-function 'request-deferred)
               (lambda (&rest _args)
                 (deferred:succeed
                  (make-request-response
                   :status-code 412))))
              ((symbol-function 'org-gcal--get-event)
               (lambda (_calendar-id _event-id)
                 (deferred:succeed
                  (make-request-response
                   :status-code 200
                   :data org-gcal-test-event)))))
      (org-gcal-test--should-error-match "marker.*buffer has been killed"
        (deferred:sync!
         (org-gcal--delete-event org-gcal-test-calendar-id
                                 "foobar1234"
                                 "\"12344321\""
                                 marker))))))

(ert-deftest org-gcal-test--delete-at-point-stale-marker-in-finally ()
  "Verify delete-at-point reports clearly if its source buffer is killed."
  (org-gcal-test--with-temp-buffer
   "\
* My event summary
:PROPERTIES:
:ETag:     \"12344321\"
:calendar-id: foo@foobar.com
:entry-id:       foobar1234/foo@foobar.com
:END:
:org-gcal:
My event description
:END:
"
   (cl-letf (((symbol-function 'org-gcal--get-access-token)
              (lambda (_calendar-id) "my_access_token"))
             ((symbol-function 'y-or-n-p)
              (lambda (&rest _args) t))
             ((symbol-function 'org-gcal--delete-event)
              (lambda (_calendar-id _event-id _etag marker &optional _a-token)
                (kill-buffer (marker-buffer marker))
                (deferred:succeed nil)))
             ((symbol-function 'error)
              (lambda (format-string &rest args)
                (let ((message (apply #'format format-string args)))
                  (when (string-match-p "marker.*buffer has been killed" message)
                    (throw 'stale-marker-error message))
                  (signal 'error (list message))))))
     (should
      (string-match-p "marker.*buffer has been killed"
                      (catch 'stale-marker-error
                        (deferred:sync! (org-gcal-delete-at-point))
                        nil))))))


(ert-deftest org-gcal-test--save-with-full-day-event ()
  "Verify that a full day event will get set correctly."
  (org-gcal-test--with-temp-buffer
   "* "
   (org-gcal--update-entry org-gcal-test-calendar-id
                           org-gcal-test-full-day-event)
   (org-back-to-heading)
   ;; Check contents of "org-gcal" drawer
   (re-search-forward ":org-gcal:")
   (let ((elem (org-element-at-point)))
     (should (equal (buffer-substring-no-properties
                     (org-element-property :contents-begin elem)
                     (org-element-property :contents-end elem))
                    "\
<2019-10-06 Sun>

My event description

Second paragraph
")))))

(ert-deftest org-gcal-test--save-with-full-day-event-and-local-timezone ()
  "Verify that a full day event will get set correctly when local-timezone is set."
  (let (
        (org-gcal-local-timezone "Europe/London"))
    (org-gcal-test--with-temp-buffer
     "* "
     (org-gcal--update-entry org-gcal-test-calendar-id
                             org-gcal-test-full-day-event)
     (org-back-to-heading)
     ;; Check contents of "org-gcal" drawer
     (re-search-forward ":org-gcal:")
     (let ((elem (org-element-at-point)))
       (should (equal (buffer-substring-no-properties
                       (org-element-property :contents-begin elem)
                       (org-element-property :contents-end elem))
                      "\
<2019-10-06 Sun>

My event description

Second paragraph
"))))))

(ert-deftest org-gcal-test--ert-fail ()
  "Test handling of ERT failures in deferred code. Should fail."
  :expected-result :failed
  (with-mock
   (stub request-deferred =>
         (deferred:$
          (deferred:succeed
           (ert-fail "Failure"))
          (deferred:nextc it
                          (lambda (_)
                            (deferred:succeed "Success")))))
   (should (equal
            (deferred:sync! (request-deferred))
            "Success"))))

(ert-deftest org-gcal-test--convert-time-to-local-timezone()
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-00:00" nil)
           "2021-03-03T11:30:00-00:00"))
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-00:00" "")
           "2021-03-03T11:30:00+0000"))
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "")
           "2021-03-03T19:30:00+0000"))
  (should (equal
           (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "Europe/London")
           "2021-03-03T19:30:00+0000")))
;; FIXME: Passed in local with Emacs 26.3 and 27.1, Failed in GitHub CI
;; (should (equal
;;          (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "Europe/Oslo")
;;          "2021-03-03T20:30:00+0100"))
;; (should (equal
;;          (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "America/New_York")
;;          "2021-03-03T14:30:00-0500"))
;; (should (equal
;;          (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "America/Los_Angeles")
;;          "2021-03-03T11:30:00-0800"))
;; (should (equal
;;          (org-gcal--convert-time-to-local-timezone "2021-03-03T11:30:00-08:00" "Asia/Shanghai")
;;          "2021-03-04T03:30:00+0800"))


;; TODO: fails with Cask, but succeeds interactively due to macro compilation issues.
;; (ert-deftest org-gcal-test--headline-archive-old-event ()
;;   "Check that `org-gcal--archive-old-event' parses headlines correctly.
;; Regression test for https://github.com/kidd/org-gcal.el/issues/172 .

;; Also tests that the `org-gcal--archive-old-event' function does
;; not loop over and over, archiving the same entry because it is
;; under another heading in the same file."
;;   (let ((org-archive-location "::* Archived")  ; Make the archive this same buffer
;;         (test-time "2022-01-30 Sun 01:23")
;;         (buf "\
;; #+CATEGORY: Test

;; * Event Title
;; :PROPERTIES:
;; :org-gcal-managed: something
;; :END:
;; <2021-01-01 Fri 12:34-14:35>
;; "))
;;     (org-test-with-temp-text-in-file
;;         buf
;;       (org-test-at-time (format "<%s>" test-time)
;;         ;; Ensure property drawer is not indented
;;         (setq-local org-adapt-indentation nil)
;;         (let* ((target-buf (format "\
;; #+CATEGORY: Test

;; * Archived

;; ** Event Title
;; :PROPERTIES:
;; :org-gcal-managed: something
;; :ARCHIVE_TIME: %s
;; :ARCHIVE_FILE: %s
;; :ARCHIVE_CATEGORY: Test
;; :END:
;; <2021-01-01 Fri 12:34-14:35>
;; "
;;                                    test-time
;;                                         ; The variable `file' is the current file
;;                                         ; name under the macro
;;                                         ; `org-test-with-temp-text-in-file'
;;                                    file)))
;;           (require 'org-archive)
;;           (org-gcal--archive-old-event)
;;           (let ((bufstr
;;                  (buffer-substring-no-properties (point-min) (point-max))))
;;             (should (string-equal bufstr target-buf))))))))

(ert-deftest org-gcal-test--strip-html ()
  "Verify that `org-gcal--strip-html' converts HTML to plain text."
  (should (equal (org-gcal--strip-html
                  "<html-blob>Click the link:<br><a href=\"https://meet.jit.si/Weekly\">https://meet.jit.si/Weekly</a><br></html-blob>")
                 "Click the link:\nhttps://meet.jit.si/Weekly"))
  ;; List items
  (should (equal (org-gcal--strip-html "<ul><li>First</li><li>Second</li></ul>")
                 "- First\n- Second"))
  ;; HTML entities
  (should (equal (org-gcal--strip-html "foo &amp; bar &lt;baz&gt; &quot;quux&quot; &#39;x&#39; &nbsp;")
                 "foo & bar <baz> \"quux\" 'x'"))
  ;; Consecutive blank lines collapsed
  (should (equal (org-gcal--strip-html "a<br><br><br><br>b")
                 "a\n\nb"))
  ;; Plain text passes through
  (should (equal (org-gcal--strip-html "No HTML here")
                 "No HTML here")))

(ert-deftest org-gcal-test--strip-html-p ()
  "Verify that `org-gcal--strip-html-p' respects global default and overrides."
  ;; Global default nil
  (let ((org-gcal-strip-html-descriptions nil)
        (org-gcal-strip-html-descriptions-overrides nil))
    (should-not (org-gcal--strip-html-p "cal@example.com")))
  ;; Global default t
  (let ((org-gcal-strip-html-descriptions t)
        (org-gcal-strip-html-descriptions-overrides nil))
    (should (org-gcal--strip-html-p "cal@example.com")))
  ;; Per-calendar override enables stripping despite global nil
  (let ((org-gcal-strip-html-descriptions nil)
        (org-gcal-strip-html-descriptions-overrides
         '(("cal@example.com" . t))))
    (should (org-gcal--strip-html-p "cal@example.com"))
    (should-not (org-gcal--strip-html-p "other@example.com")))
  ;; Per-calendar override disables stripping despite global t
  (let ((org-gcal-strip-html-descriptions t)
        (org-gcal-strip-html-descriptions-overrides
         '(("shared@group.calendar.google.com" . nil))))
    (should-not (org-gcal--strip-html-p "shared@group.calendar.google.com"))
    (should (org-gcal--strip-html-p "personal@gmail.com"))))

(ert-deftest org-gcal-test--update-entry-strip-html ()
  "Verify that `org-gcal--update-entry' strips HTML when enabled."
  ;; With stripping enabled, HTML is converted to plain text.
  (let ((org-gcal-strip-html-descriptions t)
        (org-gcal-strip-html-descriptions-overrides nil))
    (org-gcal-test--with-temp-buffer
        "* "
      (org-gcal--update-entry org-gcal-test-calendar-id
                              org-gcal-test-html-event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let* ((elem (org-element-at-point))
             (contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
        ;; Should not contain HTML tags (use regex that won't match Org timestamps)
        (should-not (string-match-p "</?[a-zA-Z][^>]*>" contents))
        ;; Should contain converted text
        (should (string-match-p "Click the link:" contents))
        (should (string-match-p "https://meet.jit.si/Weekly" contents))
        ;; Entity decoded
        (should (string-match-p "& more info" contents))))))

(ert-deftest org-gcal-test--update-entry-preserve-html ()
  "Verify that `org-gcal--update-entry' preserves HTML when stripping disabled."
  ;; With stripping disabled (default), HTML is preserved.
  (let ((org-gcal-strip-html-descriptions nil)
        (org-gcal-strip-html-descriptions-overrides nil))
    (org-gcal-test--with-temp-buffer
        "* "
      (org-gcal--update-entry org-gcal-test-calendar-id
                              org-gcal-test-html-event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let* ((elem (org-element-at-point))
             (contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
        ;; Should still contain HTML tags
        (should (string-match-p "<html-blob>" contents))
        (should (string-match-p "</a>" contents))
        ;; Entity NOT decoded
        (should (string-match-p "&amp;" contents))))))

(ert-deftest org-gcal-test--update-entry-preserve-html-per-calendar ()
  "Verify per-calendar override preserves HTML in `org-gcal--update-entry'."
  ;; Global default is t, but this specific calendar has stripping disabled.
  (let ((org-gcal-strip-html-descriptions t)
        (org-gcal-strip-html-descriptions-overrides
         `((,org-gcal-test-calendar-id . nil))))
    (org-gcal-test--with-temp-buffer
        "* "
      (org-gcal--update-entry org-gcal-test-calendar-id
                              org-gcal-test-html-event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let* ((elem (org-element-at-point))
             (contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
        (should (string-match-p "<html-blob>" contents))
        (should (string-match-p "</a>" contents))
        (should (string-match-p "&amp;" contents))))))

(ert-deftest org-gcal-test--update-entry-strip-html-per-calendar ()
  "Verify per-calendar override for HTML stripping in `org-gcal--update-entry'."
  ;; Global default is nil, but this specific calendar has stripping enabled.
  (let ((org-gcal-strip-html-descriptions nil)
        (org-gcal-strip-html-descriptions-overrides
         `((,org-gcal-test-calendar-id . t))))
    (org-gcal-test--with-temp-buffer
        "* "
      (org-gcal--update-entry org-gcal-test-calendar-id
                              org-gcal-test-html-event)
      (org-back-to-heading)
      (re-search-forward ":org-gcal:")
      (let* ((elem (org-element-at-point))
             (contents (buffer-substring-no-properties
                        (org-element-property :contents-begin elem)
                        (org-element-property :contents-end elem))))
        (should-not (string-match-p "</?[a-zA-Z][^>]*>" contents))
        (should (string-match-p "& more info" contents))))))

;;; OAuth2 provider registration

(defun org-gcal-test--resolved-aio-promise (value)
  "Return an `aio' promise already resolved to VALUE."
  (let ((promise (aio-promise)))
    (aio-resolve promise (lambda () value))
    promise))

(defmacro org-gcal-test--with-oauth2-stub (seen &rest body)
  "Run BODY with `oauth2-auto-access-token' stubbed.
The stub pushes the `org-gcal' provider info it sees onto SEEN and
returns a resolved promise for \"test_token\"."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'oauth2-auto-access-token)
              (lambda (_username provider)
                (push (oauth2-auto--provider-info provider) ,seen)
                (org-gcal-test--resolved-aio-promise "test_token"))))
     ,@body))

(ert-deftest org-gcal-test--get-access-token-registers-provider ()
  "Credentials set after load are registered before a token is fetched."
  (let ((oauth2-auto-additional-providers-alist nil)
        (org-gcal-client-id "late_client_id")
        (org-gcal-client-secret "late_client_secret")
        (seen nil))
    (org-gcal-test--with-oauth2-stub seen
      (should (equal (org-gcal--get-access-token org-gcal-test-calendar-id)
                     "test_token")))
    (should (= (length seen) 1))
    (should (equal (alist-get 'client_id (car seen)) "late_client_id"))
    (should (equal (alist-get 'client_secret (car seen))
                   "late_client_secret"))))

(ert-deftest org-gcal-test--refresh-token-registers-provider ()
  "`org-gcal--refresh-token' also registers the provider first."
  (let ((oauth2-auto-additional-providers-alist nil)
        (org-gcal-client-id "late_client_id")
        (org-gcal-client-secret "late_client_secret")
        (seen nil))
    (org-gcal-test--with-oauth2-stub seen
      (should (equal (deferred:sync!
                       (org-gcal--refresh-token org-gcal-test-calendar-id))
                     "test_token")))
    (should (= (length seen) 1))
    (should (equal (alist-get 'client_id (car seen)) "late_client_id"))))

(ert-deftest org-gcal-test--get-access-token-missing-credentials ()
  "A missing client ID or secret signals `user-error' before any request."
  (dolist (creds '((nil . "secret") ("id" . nil) (nil . nil)))
    (let ((oauth2-auto-additional-providers-alist nil)
          (org-gcal-client-id (car creds))
          (org-gcal-client-secret (cdr creds))
          (seen nil))
      (org-gcal-test--with-oauth2-stub seen
        (should-error (org-gcal--get-access-token org-gcal-test-calendar-id)
                      :type 'user-error))
      (should-not seen)
      (should-not (assq 'org-gcal oauth2-auto-additional-providers-alist)))))

(ert-deftest org-gcal-test--reload-client-id-secret-replaces-entry ()
  "Changed credentials replace the registered provider instead of stacking."
  (let ((oauth2-auto-additional-providers-alist
         (list (list 'other-provider (cons 'client_id "other"))))
        (org-gcal-client-id "old_id")
        (org-gcal-client-secret "old_secret"))
    (org-gcal-reload-client-id-secret)
    (setq org-gcal-client-id "new_id"
          org-gcal-client-secret "new_secret")
    (org-gcal-reload-client-id-secret)
    (should (= (cl-count 'org-gcal oauth2-auto-additional-providers-alist
                         :key #'car)
               1))
    (should (assq 'other-provider oauth2-auto-additional-providers-alist))
    (let ((info (oauth2-auto--provider-info 'org-gcal)))
      (should (equal (alist-get 'client_id info) "new_id"))
      (should (equal (alist-get 'client_secret info) "new_secret")))))

(ert-deftest org-gcal-test--load-without-credentials-does-not-warn ()
  "Loading org-gcal before credentials are set neither warns nor registers.
Runs in a child Emacs so a fresh load does not disturb this session."
  (let* ((form
          `(progn
             (setq load-path ',load-path)
             (advice-add 'display-warning :override
                         (lambda (&rest args)
                           (princ (format "WARNING: %S\n" args))))
             ;; The old load-time warning was skipped in batch mode.  Bind,
             ;; don't set: batch Emacs only exits after --eval when
             ;; `noninteractive' is non-nil.
             (let ((noninteractive nil))
               (require 'org-gcal))
             (princ (format "REGISTERED: %S\n"
                            (and (assq 'org-gcal
                                       oauth2-auto-additional-providers-alist)
                                 t)))
             (kill-emacs 0)))
         (output
          (with-temp-buffer
            (let ((status (call-process
                           (expand-file-name invocation-name
                                             invocation-directory)
                           nil t nil "--batch" "-Q"
                           "--eval" (prin1-to-string form))))
              (should (eql status 0)))
            (buffer-string))))
    (should (string-match-p "^REGISTERED: nil$" output))
    (should-not (string-match-p "^WARNING: " output))))

;;; TODO: Figure out mocking for POST/PATCH followed by GET
;;; - ‘mock‘ might work for this - the argument list must be specified up
;;;   front, but the wildcard ‘*’ can be used to match any value. If that
;;;   doesn’t work, use ‘cl-flet’.

;;; TODO: Figure out how to set up org-id for mocking (org-mode tests should help?)
;;; - There are actually no org-mode tests for this.
;;; - Set ‘org-id-locations’ (a hash table). This maps each ID to the file in
;;;   which the ID is found, so a temp file (not just a temp buffer) is needed.
