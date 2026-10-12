;;; org-generic-id.el --- Global identifiers for Org entries -*- lexical-binding: t; -*-
;;
;; Author: Robert Irelan <rirelan at gmail dot com>
;; Homepage: https://orgmode.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file generalizes ‘org-id' to allow using any property, not just :ID:,
;; to locate Org-mode entries. This library only handles storing IDs in entries,
;; locating entries with a given ID, and storing a map of IDs to files
;; containing such IDs. Generation of IDs must be handled by the user of the
;; library - unlike ‘org-id', no functions are provided to automatically
;; generate IDs.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'persist)

(declare-function message-make-fqdn "message" ())
(declare-function org-goto-location "org-goto" (&optional _buf help))

;;; Customization

(defgroup org-generic-id nil
  "Options concerning global entry identifiers in Org mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-generic-id-locations-file (convert-standard-filename
                                          (concat user-emacs-directory ".org-generic-id-locations"))
  "The file for remembering in which file an ID was defined."
  :group 'org-generic-id
  :type 'file)

(defcustom org-generic-id-locations-file-relative nil
  "Determines if org-generic-id-locations should be stored as relative links.
Non-nil means that links to locations are stored as links
relative to the location of where `org-generic-id-locations-file' is
stored.

Nil means to store absolute paths to files.

This customization is useful when folders are shared across
systems but mounted at different roots.  Relative path to
`org-generic-id-locations-file' still has to be maintained across
systems."
  :group 'org-generic-id
  :type 'boolean)

(defun org-generic-id--make-hash-table (&rest args)
  "Make hash table with ‘equal’ test and optional other ARGS."
  (apply #'make-hash-table :test #'equal args))

(defvar org-generic-id-locations (org-generic-id--make-hash-table)
  "Hashtable storing map of ID names to IDs to file containing them.

Example structure:

#s(hash-table size 31 test equal rehash-size 1.5 rehash-threshold 0.8125 data
              (\"entry-id\"
                #s(hash-table size 2 test equal rehash-size 1.5
                              rehash-threshold 0.8125 data
                                        (\"entry-id-1\" \"file1.org\"
                                         \"entry-id-2\" \"file2.org\")))
              (\"other-id\"
                #s(hash-table size 2 test equal rehash-size 1.5
                              rehash-threshold 0.8125 data
                                        (\"other-id-1\" \"file3.org\"
                                         \"other-id-2\" \"file4.org\"))))
")

(defvar org-generic-id--files
  (org-generic-id--make-hash-table :weakness 'value)
  "Hashtable mapping file names to buffers visiting the files.

The keys are file names - multiple keys may refer to the same buffer.  The
values are as follows:

- buffer: list containing the buffer corresponding to the file
- nil: list containing nil, signifying that the file has been determined to not
         be visited by a buffer
- `unknown’: when the buffer for a file has not been determined.

The table has weak values so that it does not cause buffers to be retained when
they would otherwise be garbage collected (after being killed, for example).")

(defun org-generic-id-files ()
  "Return a list of all files known to have IDs."
  (let ((tmp (org-generic-id--make-hash-table))
        res)
    (maphash
     (lambda (_id-name id-hash)
       (maphash
        (lambda (_id file)
          (puthash file t tmp))
        id-hash))
     org-generic-id-locations)
    (maphash
     (lambda (file _v) (push file res))
     tmp)
    res))

(persist-defvar
  org-generic-id--file-fingerprints nil
  "Modification times of files as of their last scan for IDs.

This is a plist mapping each ID-PROP, as a symbol, to an alist of
\(FILE . MODTIME).  ‘org-generic-id-update-id-locations’ reads a file
again only when its modification time differs from the one recorded
here, so a file without an entry is always read.  For documentation on
ID-PROP see ‘org-generic-id-find’.")

(defcustom org-generic-id-extra-files 'org-agenda-text-search-extra-files
  "Files to be searched for IDs, besides the agenda files.
When Org reparses files to remake the list of files and IDs it is tracking,
it will normally scan the agenda files, the archives related to agenda files,
any files that are listed as ID containing in the current register, and
any Org file currently visited by Emacs.
You can list additional files here."
  :group 'org-generic-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
            (file))))

(defcustom org-generic-id-search-archives t
  "Non-nil means search also the archive files of agenda files for entries.
This is a possibility to reduce overhead, but it means that entries moved
to the archives can no longer be found by ID."
  :group 'org-generic-id
  :type 'boolean)

;;; The API functions

;;;###autoload
(defun org-generic-id-get (&optional id-prop pom)
  "Get the ID-PROP property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
In any case, the ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil id-prop)))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (t nil)))))

;;;###autoload
(defun org-generic-id-find (id-prop id &optional markerp cached no-fallback)
  "Return the location of the entry with property ID-PROP, value ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker.

Normally, if an entry with ID is not found, this function will run
‘org-generic-id-update-id-locations' in order to pick up any updates to the
files, and then search again, before concluding an ID can’t be found. If
CACHED is passed, that function will not be run.

Normally the ID will be searched for in the current buffer before updating ID
locations. This behavior can be disabled with NO-FALLBACK."
  (cond
   ((symbolp id) (setq id (symbol-name id)))
   ((numberp id) (setq id (number-to-string id))))
  (let ((file (org-generic-id-find-id-file id-prop id no-fallback))
        org-agenda-new-buffers where)
    (when file
      (setq where (org-generic-id-find-id-in-file id-prop id file markerp)))
    (unless (or where cached)
      (org-generic-id-update-id-locations id-prop nil t)
      (setq file (org-generic-id-find-id-file id-prop id no-fallback))
      (when file
        (setq where (org-generic-id-find-id-in-file
                     id-prop id file markerp))))
    where))

;;; Internal functions

;; Storing ID locations (files)

;;;###autoload
(defun org-generic-id-update-id-locations (id-prop &optional files silent)
  "Scan relevant files for IDs.
Store the relation between files and corresponding IDs.
This will scan all agenda files, all associated archives, and all
files currently mentioned in `org-generic-id-locations'.
When FILES is given, scan also these files.

A file is read only when its text may have changed since it was last
read: its buffer has unsaved changes, or its modification time differs
from the one recorded then.  The IDs recorded for a file that is read
are replaced by the ones it now holds, so IDs deleted or moved out of
it are forgotten.  Files holding an ID that is also in another file are
read every time, until the duplicate is resolved.

Unless SILENT, report progress, and report once each ID found in more
than one file."
  (interactive "sID Property: ")
  (let* ((files
          (let ((raw (delete-dups
                      (mapcar #'abbreviate-file-name
                              (cl-remove-if-not
                               ;; Default `org-generic-id-extra-files' value
                               ;; contains `agenda-archives' symbol.
                               #'stringp
                               (append
                                ;; Agenda files and all associated archives.
                                (org-agenda-files t org-generic-id-search-archives)
                                ;; Explicit extra files.
                                (if (symbolp org-generic-id-extra-files)
                                    (symbol-value org-generic-id-extra-files)
                                  org-generic-id-extra-files)
                                ;; All files known to have IDs.
                                (org-generic-id-files)
                                ;; Additional files from function call.
                                files))))))
            ;; Deduplicate files that resolve to the same physical file
            ;; (e.g. network drive aliases like Z: vs ~/interceptor).
            ;; Use file-truename which resolves symlinks and drive mappings.
            (let ((seen (make-hash-table :test #'equal))
                  result)
              (dolist (f raw (nreverse result))
                (let ((key (ignore-errors (file-truename f))))
                  (unless (gethash (or key f) seen)
                    (puthash (or key f) t seen)
                    (push f result)))))))
         (nfiles (length files))
         (id-regexp (org-generic-id--id-regexp id-prop))
         (prop-key (intern id-prop))
         (old-prints (plist-get org-generic-id--file-fingerprints prop-key))
         (new-prints nil)
         ;; (FILE . IDS) for each file read, and the files skipped as
         ;; unchanged since they were last read.
         (scanned nil)
         (unchanged nil)
         (truename (org-generic-id--truename-function))
         (ndup 0)
         (dup-files nil)
         (i 0))
    (unless (gethash id-prop org-generic-id-locations)
      (puthash id-prop (org-generic-id--make-hash-table) org-generic-id-locations))
    (with-temp-buffer
      (delay-mode-hooks
        (org-mode)
        (dolist (file files)
          (condition-case err
              (when (file-exists-p file)
                (let* ((buf (org-generic-id--get-file-to-buf
                             org-generic-id--files file))
                       (buf (and buf (buffer-live-p buf) buf))
                       (print (org-generic-id--file-fingerprint file buf)))
                  (if (and print (equal print (cdr (assoc file old-prints))))
                      (progn
                        (push (cons file print) new-prints)
                        (push file unchanged))
                    (cl-incf i)
                    (unless silent
                      (message "Finding :%s: locations (%d/%d files): %s"
                               id-prop i nfiles file))
                    (push (cons file (org-generic-id--file-ids
                                      file buf id-prop id-regexp))
                          scanned)
                    ;; A buffer with unsaved changes gets no fingerprint, so
                    ;; it is read again once saved or reverted.
                    (when print
                      (push (cons file print) new-prints)))))
            (file-error
             (warn "org-generic-id-update-id-locations: file “%s”: %S"
                   file err))))
        (let ((result (org-generic-id--record-scan
                       id-prop (nreverse scanned) unchanged silent truename)))
          (setq ndup (car result)
                dup-files (cdr result)))))
    ;; Files holding a duplicated ID are read by every update until the
    ;; duplicate is resolved: the index records one file per ID, so once
    ;; that file drops the ID, the others must be read to record it again.
    (when (> (hash-table-count dup-files) 0)
      (setq new-prints
            (cl-remove-if (lambda (print)
                            (gethash (funcall truename (car print)) dup-files))
                          new-prints)))
    (setq org-generic-id--file-fingerprints
          (plist-put org-generic-id--file-fingerprints prop-key new-prints))
    ;; Save the new locations and reload to regenerate variables.
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    (when (and (not silent) (> ndup 0))
      (warn
       "WARNING: %d duplicate :%s: properties found, check *Messages* buffer"
       ndup id-prop))
    (message "%d files updated (out of %d), and %d :%s: IDs found."
             i nfiles
             (hash-table-count (gethash id-prop org-generic-id-locations))
             id-prop)
    org-generic-id-locations))

(defun org-generic-id--id-regexp (id-prop)
  "Return a regexp matching the lines that may set ID-PROP."
  (rx-to-string `(seq bol (0+ (any "\t "))
                  ,(format ":%s:" id-prop)
                  (1+ " ") (not (any " ")))))

(defun org-generic-id--file-fingerprint (file buf)
  "Return the modification time of the text a scan of FILE reads.
BUF is the buffer visiting FILE, or nil.  The scan reads BUF when it is
non-nil, so this is the modification time BUF last synced with FILE,
even if FILE has changed on disk since.  Return nil when BUF has unsaved
changes, as its text then matches no modification time."
  (cond
   ((null buf)
    (file-attribute-modification-time (file-attributes file)))
   ((buffer-modified-p buf) nil)
   (t (with-current-buffer buf (visited-file-modtime)))))

(defun org-generic-id--file-ids (file buf id-prop id-regexp)
  "Return the values of ID-PROP set in FILE, in buffer order.
Read BUF, the buffer visiting FILE, if non-nil, so unsaved changes
count; otherwise read FILE into the current buffer.  ID-REGEXP matches
the lines that may set ID-PROP."
  (save-excursion
    (if buf
        (set-buffer buf)
      (insert-file-contents file nil nil nil 'replace))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            ids)
        (while (re-search-forward id-regexp nil t)
          (when-let* (((org-at-property-p))
                      (id (org-entry-get (point) id-prop)))
            (push id ids)))
        (nreverse ids)))))

(defun org-generic-id--file-holds-id-p (file id-prop id)
  "Return non-nil if an entry in FILE sets ID-PROP to ID.
Read the buffer visiting FILE if any, otherwise read FILE into the
current buffer."
  (let ((buf (org-generic-id--get-file-to-buf org-generic-id--files file)))
    (ignore-errors
      (save-excursion
        (if (and buf (buffer-live-p buf))
            (set-buffer buf)
          (insert-file-contents file nil nil nil 'replace))
        (org-with-wide-buffer
         (org-find-property id-prop id))))))

(defun org-generic-id--truename-function ()
  "Return a function mapping a file name to its true name, memoized."
  (let ((memo (org-generic-id--make-hash-table)))
    (lambda (file)
      (or (gethash file memo)
          (puthash file (or (ignore-errors (file-truename file)) file)
                   memo)))))

(defvar org-generic-id--reported-duplicates (org-generic-id--make-hash-table)
  "Duplicated IDs reported so far, as keys (ID-PROP . ID).
A duplicate is reported once, and again only if it recurs after it was
resolved.")

(defun org-generic-id--record-scan (id-prop scanned unchanged silent truename)
  "Record the IDs found by a scan for ID-PROP in `org-generic-id-locations'.
SCANNED lists (FILE . IDS) for each file read, in scan order, and
UNCHANGED the files skipped because their text did not change since
they were last read, or t if every file not in SCANNED is unchanged.
TRUENAME maps a file name to its true name.
Forget the IDs recorded for the files in SCANNED, then map each of
their IDs to its file; for an ID found more than once, the last file
wins.

An ID is duplicated when it appears twice in SCANNED, or when a file in
UNCHANGED still records and holds it.  Unless SILENT, report each
duplicate not reported before.  Return (NDUP . FILES): NDUP is the
number of duplicates reported, and FILES a hash table whose keys are the
true names of the files holding duplicated IDs.

Must be called with an Org buffer current that may be overwritten."
  (let ((table (gethash id-prop org-generic-id-locations))
        (rescanned (org-generic-id--make-hash-table))
        (kept (org-generic-id--make-hash-table))
        ;; Each ID found, mapped to the true name of its first file.
        (seen (org-generic-id--make-hash-table))
        (dups (org-generic-id--make-hash-table))
        (dup-files (org-generic-id--make-hash-table))
        (ndup 0))
    (dolist (s scanned)
      (puthash (funcall truename (car s)) t rescanned))
    (unless (eq unchanged t)
      (dolist (file unchanged)
        (puthash (funcall truename file) t kept)))
    ;; Compare true names: an ID may have been recorded under another
    ;; name for the same file, e.g. by `org-generic-id-add-location'.
    (maphash (lambda (id file)
               (when (gethash (funcall truename file) rescanned)
                 (remhash id table)))
             table)
    (dolist (s scanned)
      (let ((file (funcall truename (car s))))
        (dolist (id (cdr s))
          (let* ((other (gethash id table))
                 (twin
                  (or (gethash id seen)
                      ;; A record can outlive its entry, e.g. when the
                      ;; buffer was reverted after the ID was added, so
                      ;; check the unchanged file still holds the ID.
                      (and other
                           (let ((other-true (funcall truename other)))
                             (if (eq unchanged t)
                                 (not (gethash other-true rescanned))
                               (gethash other-true kept)))
                           (org-generic-id--file-holds-id-p other id-prop id)
                           (funcall truename other)))))
            (when twin
              (puthash id t dups)
              (puthash twin t dup-files)
              (puthash file t dup-files))
            (unless (gethash id seen)
              (puthash id file seen))
            (puthash id (car s) table)))))
    ;; Report new duplicates, and forget resolved ones so that they are
    ;; reported again if they recur.
    (maphash (lambda (id _)
               (let ((key (cons id-prop id)))
                 (cond
                  ((not (gethash id dups))
                   (remhash key org-generic-id--reported-duplicates))
                  ((or silent
                       (gethash key org-generic-id--reported-duplicates)))
                  (t
                   (puthash key t org-generic-id--reported-duplicates)
                   (message "Duplicate :%s: property %S" id-prop id)
                   (cl-incf ndup)))))
             seen)
    (cons ndup dup-files)))

(defun org-generic-id--tracked-name (id-prop file)
  "Return the name under which the index for ID-PROP tracks FILE, or nil.
The index tracks the files the last update read or skipped as unchanged,
and the files it records an ID in."
  (let ((names (org-generic-id--make-hash-table))
        (table (gethash id-prop org-generic-id-locations)))
    (dolist (print (plist-get org-generic-id--file-fingerprints
                              (intern id-prop)))
      (puthash (car print) t names))
    (when table
      (maphash (lambda (_id name) (puthash name t names)) table))
    (if (gethash file names)
        file
      ;; FILE may be tracked under another name for the same file.
      (let ((base (file-name-nondirectory file))
            (true (ignore-errors (file-truename file))))
        (catch 'found
          (maphash
           (lambda (name _)
             (when (and true
                        (eq t (compare-strings
                               base nil nil
                               (file-name-nondirectory name) nil nil t))
                        (equal (ignore-errors (file-truename name)) true))
               (throw 'found name)))
           names)
          nil)))))

(defun org-generic-id--recorded-ids (id-prop file truename)
  "Return the sorted IDs that the index for ID-PROP records in FILE.
TRUENAME maps a file name to its true name."
  (let ((true (funcall truename file))
        ids)
    (when-let* ((table (gethash id-prop org-generic-id-locations)))
      (maphash (lambda (id name)
                 (when (equal (funcall truename name) true)
                   (push id ids)))
               table))
    (sort ids #'string<)))

(defun org-generic-id--update-file (id-prop file buf)
  "Record the values of ID-PROP that FILE holds now.
FILE is the name under which the index tracks the file, and BUF the
buffer visiting it.  This is `org-generic-id-update-id-locations' for
one file: the other files count as unchanged, and duplicates are left
for that function to report.  Return non-nil if the IDs recorded in
FILE changed."
  (let* ((truename (org-generic-id--truename-function))
         (prop-key (intern id-prop))
         (before (org-generic-id--recorded-ids id-prop file truename))
         (ids (org-generic-id--file-ids
               file buf id-prop (org-generic-id--id-regexp id-prop)))
         ;; Checking that another file holds an ID may read that file into
         ;; the current buffer, so make it a temporary one.
         (dup-files
          (with-temp-buffer
            (delay-mode-hooks
              (org-mode)
              (cdr (org-generic-id--record-scan
                    id-prop (list (cons file ids)) t t truename)))))
         ;; Files holding a duplicated ID lose their fingerprint so that the
         ;; next update reads them, and reports the duplicate.
         (prints (cl-remove-if
                  (lambda (print)
                    (or (equal (car print) file)
                        (gethash (funcall truename (car print)) dup-files)))
                  (plist-get org-generic-id--file-fingerprints prop-key)))
         (print (unless (gethash (funcall truename file) dup-files)
                  (org-generic-id--file-fingerprint file buf))))
    (when print
      (push (cons file print) prints))
    (setq org-generic-id--file-fingerprints
          (plist-put org-generic-id--file-fingerprints prop-key prints))
    (not (equal before (org-generic-id--recorded-ids id-prop file truename)))))

(defun org-generic-id--update-visited-file ()
  "Record the IDs of the file visited by the current buffer.
Do so for each ID property indexed, if the index tracks the file and
last read other text from it.  Run when a buffer reads its file, by
reverting it or visiting it again: the update at the start of an
operation such as a sync has not seen that text, so IDs that another
program added to the file would be missing from the index until the
next update."
  (let ((buf (or (buffer-base-buffer) (current-buffer))))
    (when-let* (((hash-table-p org-generic-id-locations))
                (fname (buffer-file-name buf))
                ((with-current-buffer buf (derived-mode-p 'org-mode))))
      (with-demoted-errors "org-generic-id: recording IDs of a reread file: %S"
        (let ((file (abbreviate-file-name fname))
              changed)
          (maphash
           (lambda (id-prop _)
             (when-let* ((name (org-generic-id--tracked-name id-prop file))
                         ((not (equal
                                (org-generic-id--file-fingerprint name buf)
                                (cdr (assoc name
                                            (plist-get
                                             org-generic-id--file-fingerprints
                                             (intern id-prop))))))))
               (when (org-generic-id--update-file id-prop name buf)
                 (setq changed t))))
           org-generic-id-locations)
          (when changed
            (org-generic-id-locations-save)))))))

(defun org-generic-id-locations-save ()
  "Save `org-generic-id-locations' in `org-generic-id-locations-file'."
  (when (and org-generic-id-locations)
    (let ((out (if (hash-table-p org-generic-id-locations)
                   (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                 org-generic-id-locations)))
      (when (and org-generic-id-locations-file-relative out)
        (setq out (mapcar
                   (lambda (item)
                     (if (file-name-absolute-p (car item))
                         (cons (file-relative-name
                                (car item) (file-name-directory
                                            org-generic-id-locations-file))
                               (cdr item))
                       item))
                   out)))
      (with-temp-file org-generic-id-locations-file
        (let ((print-level nil)
              (print-length nil))
          (pp out (current-buffer)))))))

;;;###autoload
(defun org-generic-id-locations-load ()
  "Read the data from `org-generic-id-locations-file'."
  (setq org-generic-id-locations nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents org-generic-id-locations-file)
          (setq org-generic-id-locations (read (current-buffer)))
          (let ((loc (file-name-directory org-generic-id-locations-file)))
            (mapc (lambda (id-item)
                    (mapc
                     (lambda (item)
                       (unless (file-name-absolute-p (car item))
                         (setf (car item) (expand-file-name (car item) loc))))
                     (cdr id-item)))
                  org-generic-id-locations)))
      (error
       (message "Could not read org-generic-id-values from %s.  Setting it to nil."
                org-generic-id-locations-file))))
  (setq org-generic-id-locations (org-generic-id--locations-alist-to-hash org-generic-id-locations)))

;;;###autoload
(defun org-generic-id-add-location (id-prop id file)
  "Add the ID with location FILE to the database of ID locations."
  ;; Only when the buffer has a file
  (unless file
    (error "bug: ‘org-generic-id-add-locations' expects a file-visiting buffer"))
  (let ((afile (abbreviate-file-name file)))
    (when id
      (let ((id-prop-hash (gethash id-prop org-generic-id-locations
                                   (org-generic-id--make-hash-table))))
        (puthash id-prop id-prop-hash org-generic-id-locations)
        (puthash id afile id-prop-hash))
      (when (eq 'unknown (gethash afile org-generic-id--files 'unknown))
        (puthash afile (find-buffer-visiting afile) org-generic-id--files)))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'org-generic-id-locations-save))

(defun org-generic-id--locations-hash-to-alist (hash)
  "Turn an org-generic-id hash into an alist, so it can be written to a file."
  (let (res)
    (maphash
     (lambda (k v)
       (let ((v (org-generic-id--hash-to-alist v)))
         (setf (alist-get k res) v)))
     hash)
    (setq res (sort res (lambda (x y) (string< (car x) (car y)))))
    res))

(defun org-generic-id--hash-to-alist (hash)
  "Turn a hash into an alist while reversing keys and values.

Create an alist with each key being the distinct values in the original hash,
and each value a list of all original hash keys that map to the key of the
alist. For example, a hash

    {’id1’: ’file1’, ’id2’: ’file2’, ’id3’: ’file1’}

is turned into an alist like this:

    ((’file1’ . (’id1’ ’id3’)) (’file2’ . (’id2’)))"
  (let (res x)
    (maphash
     (lambda (k v)
       (if (setq x (assoc v res))
           (setcdr x (cons k (cdr x)))
         (push (list v k) res)))
     hash)
    (mapc (lambda (x)
            (setcdr x (sort (cdr x) #'string<)))
          res)
    (setq res (sort res (lambda (x y) (string< (car x) (car y)))))
    res))

(defun org-generic-id--locations-alist-to-hash (list)
  "Turn an org-generic-id location list into a hash table."
  (let ((res (org-generic-id--make-hash-table
              :size (apply '+ (mapcar 'length list)))))
    (mapc
     (lambda (x)
       (puthash (car x) (org-generic-id--alist-to-hash (cdr x)) res))
     list)
    res))

(defun org-generic-id--alist-to-hash (list)
  "Reverse the transformation made in ‘org-generic-id--hash-to-alist’."
  (let ((res (org-generic-id--make-hash-table
              :size (apply '+ (mapcar 'length list))))
        f)
    (mapc
     (lambda (x)
       (setq f (car x))
       (mapc (lambda (i) (puthash i f res)) (cdr x)))
     list)
    res))

;; Finding entries with specified id

;;;###autoload
(defun org-generic-id-find-id-file (id-prop id &optional no-fallback)
  "Query the id database for the file in which this ID is located.

If NO-FALLBACK is set, don’t fall back to current buffer if not found in
‘org-generic-id-locations’."
  (or (and org-generic-id-locations
           (hash-table-p org-generic-id-locations)
           ;; Guard for errors in the ‘gethash’ call after this
           (gethash id-prop org-generic-id-locations)
           (gethash id
                    (gethash id-prop org-generic-id-locations)))
      ;; Fall back on current buffer
      (and (not no-fallback)
           (buffer-file-name (or (buffer-base-buffer (current-buffer))
                                 (current-buffer))))))

(defun org-generic-id-find-id-in-file (id-prop id file &optional markerp)
  "Return the position of the entry ID in FILE.

If that files does not exist, or if it does not contain this ID,
return nil.

The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a new marker."
  (cond
   ((not file) nil)
   ((not (file-exists-p file)) nil)
   (t
    (let* ((visiting (find-buffer-visiting file))
           (buffer (or visiting (find-file-noselect file))))
      (unwind-protect
          (with-current-buffer buffer
            (let ((pos (org-with-wide-buffer (org-find-property id-prop id))))
              (cond
               ((null pos) nil)
               (markerp (move-marker (make-marker) pos buffer))
               (t (cons file pos)))))
        ;; Remove opened buffer in the process.
        (unless (or visiting markerp) (kill-buffer buffer)))))))

(defun org-generic-id--get-file-to-buf (file-to-buf file)
  "Get buffer visiting FILE, or nil if no such buffer.

If FILE or ‘(abbreviate-file-name FILE)’ is present in FILE-TO-BUF, use that.
Otherwise, find the buffer visiting FILE if any, and cache the result in
FILE-TO-BUF, whose format is documented at ‘org-generic-id--files’."
  (let ((b (gethash file file-to-buf 'unknown)))
    (if (not (eq 'unknown b))
        b
      (let* ((tmp (or (get-file-buffer file) (find-buffer-visiting file)))
             (buf (when tmp (if-let* ((base (buffer-base-buffer tmp))) base tmp))))
        (org-generic-id--files-buffer-hook-impl
         file-to-buf file buf)
        buf))))

(defun org-generic-id--files-find-file-hook ()
  "Update ‘org-generic-id--files’ after a file is loaded.
Also record the IDs the file holds if they may have changed since the
index last read it."
  (org-generic-id--files-buffer-hook-impl
   org-generic-id--files (buffer-file-name) (current-buffer))
  (org-generic-id--update-visited-file))

(defun org-generic-id--files-kill-buffer-hook ()
  "Update ‘org-generic-id--files’ after a buffer is killed."
  (org-generic-id--files-buffer-hook-impl
   org-generic-id--files (buffer-file-name) nil))

(defun org-generic-id--files-buffer-hook-impl (file-to-buf fname buf)
  "Update FILE-TO-BUF to associate FNAME with BUF.
FILE-TO-BUF has a format like ‘org-generic-id--files’."
  (when fname
    (let ((true-fname (abbreviate-file-name fname)))
      (puthash fname buf file-to-buf)
      (when (not (equal fname true-fname))
        (puthash true-fname buf file-to-buf)))))

(add-hook 'find-file-hook #'org-generic-id--files-find-file-hook)
(add-hook 'kill-buffer-hook #'org-generic-id--files-kill-buffer-hook)
(add-hook 'after-revert-hook #'org-generic-id--update-visited-file)


(unless (featurep 'org-generic-id)
  (unless (and (hash-table-p org-generic-id-locations)
               (> (hash-table-count org-generic-id-locations) 0))
    (message "Loading org-generic-id-locations on first load.")
    (org-generic-id-locations-load)))

(provide 'org-generic-id)

;;; org-generic-id.el ends here
