;;; org-generic-id-test.el --- Tests for org-generic-id.el -*- lexical-binding: t -*-

(require 'org-generic-id)
(require 'cl-lib)
(require 'el-mock)

(defconst org-generic-id-test--source-file
  (expand-file-name "../org-generic-id.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the org-generic-id source file.")

(defun org-generic-id-test--load-source-as-first-load ()
  "Load org-generic-id as if it has not already provided its feature."
  (let ((old-features features))
    (unwind-protect
        (progn
          (setq features (remove 'org-generic-id features))
          (load-file org-generic-id-test--source-file))
      (setq features old-features))))

(ert-deftest org-generic-id--round-trip ()
  "Verify that ‘org-generic-id-locations-save’ and
‘org-generic-id-locations-load' round-trip."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("ID"
             ("/tmp/bar" "id1" "id2" "id3")
             ("/tmp/foo" "fid"))
            ("SN"
             ("/tmp/bar" "sn1" "sna"))))
         (org-generic-id-locations
          (org-generic-id--locations-alist-to-hash
           org-generic-id-locations-alist)))
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--first-load-loads-empty-location-cache ()
  "Verify first load reads persisted locations when memory cache is empty."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("entry-id"
             ("/tmp/calendar.org" "event-1/calendar@example.com"))))
         (org-generic-id-locations (org-generic-id--make-hash-table)))
    (with-temp-file org-generic-id-locations-file
      (let ((print-level nil)
            (print-length nil))
        (pp org-generic-id-locations-alist (current-buffer))))
    (org-generic-id-test--load-source-as-first-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--first-load-keeps-populated-location-cache ()
  "Verify first load preserves an already-populated memory cache."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("entry-id"
             ("/tmp/memory.org" "event-1/calendar@example.com"))))
         (org-generic-id-locations
          (org-generic-id--locations-alist-to-hash
           org-generic-id-locations-alist)))
    (with-temp-file org-generic-id-locations-file
      (let ((print-level nil)
            (print-length nil))
        (pp '(("entry-id"
               ("/tmp/disk.org" "event-2/calendar@example.com")))
            (current-buffer))))
    (org-generic-id-test--load-source-as-first-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--find ()
  "Verify ‘org-generic-id-find’.
‘org-generic-id-locations-load' round-trip."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-file
          ;; When I rescan ID locations, the symlink "/var" is resolved to
          ;; "/private/var" on macOS. The simplest way to fix this is just to
          ;; resolve the symlink manually at the start.
          (abbreviate-file-name (file-truename (make-temp-file "my.org."))))
         (org-agenda-files (list org-file))
         (org-generic-id-extra-files nil))
    ;; Reset various variables by saving and reloading.
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    ;; We run the test in a temp buffer because ‘org-generic-id-find’ will by
    ;; default search for the ID in the current buffer if it’s not found in
    ;; ‘org-generic-id-locations’.  We want to exercise the ID search/update, so
    ;; we don’t want to fall back to the current buffer.  We thus need to switch
    ;; buffer to ‘org-file’ to update it.
    (with-temp-buffer
      (with-current-buffer (find-file-noselect org-file)
        (org-mode)
        (insert "\
* id1
:PROPERTIES:
:ID: id1
:SN: sn1
:END:"))
      (org-generic-id-update-id-locations "ID")
      (org-generic-id-update-id-locations "SN")
      ;; Round trip variables
      (org-generic-id-locations-save)
      (org-generic-id-locations-load)
      (let (m)
        (setq m (org-generic-id-find-id-in-file
                 "ID" "id1" org-file))
        (should (equal m
                       `(,org-file . 1)))
        (setq m (org-generic-id-find "ID" "id1"))
        (should (equal m
                       `(,org-file . 1)))
        (with-current-buffer (get-file-buffer org-file)
          (goto-char (point-max))
          (insert "
* id2
:PROPERTIES:
:ID: id2
:END:"))
        (setq m (org-generic-id-find "ID" "id2" nil t t))
        (should (equal m nil))
        (setq m (org-generic-id-find "ID" "id2"))
        (should (equal m
                       `(,org-file . 44)))

        (setq m (org-generic-id-find-id-in-file
                 "SN" "sn1" org-file))
        (should (equal m
                       `(,org-file . 1)))
        (setq m (org-generic-id-find "SN" "sn1"))
        (should (equal m
                       `(,org-file . 1)))
        (with-current-buffer (get-file-buffer org-file)
          (goto-char (point-max))
          (insert "
* sn2
:PROPERTIES:
:SN: sn2
:END:"))
        (setq m (org-generic-id-find "SN" "sn2" nil t t))
        (should (equal m nil))
        (setq m (org-generic-id-find "SN" "sn2"))
        (should (equal m
                       `(,org-file . 78)))))))

;;; Incremental updates

(defconst org-generic-id-test--time (seconds-to-time 1767268800)
  "Base modification time for files written by the update tests.")

(defmacro org-generic-id-test--with-index (files &rest body)
  "Run BODY with an empty ID index scanning only FILES.
FILES is evaluated to a list of file names, used as the agenda files."
  (declare (indent 1) (debug t))
  `(let ((org-generic-id-locations (org-generic-id--make-hash-table))
         (org-generic-id--file-fingerprints nil)
         (org-generic-id--reported-duplicates (org-generic-id--make-hash-table))
         (org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-agenda-files ,files)
         (org-generic-id-extra-files nil)
         (org-generic-id-search-archives nil))
     ,@body))

(defun org-generic-id-test--temp-file ()
  "Return the name of a new empty Org file, as the ID scan names it."
  ;; `file-truename' resolves symlinks such as /var -> /private/var on macOS.
  (abbreviate-file-name (file-truename (make-temp-file "my.org." nil ".org"))))

(defun org-generic-id-test--write (file seconds &rest ids)
  "Write an entry for each of IDS to FILE, modified SECONDS after the base time.
Each entry sets the ID property to its ID."
  (with-temp-file file
    (dolist (id ids)
      (insert (format "* %s\n:PROPERTIES:\n:ID: %s\n:END:\n" id id))))
  (set-file-times file (time-add org-generic-id-test--time seconds)))

(defun org-generic-id-test--location (id)
  "Return the file the index records for ID."
  (gethash id (gethash "ID" org-generic-id-locations)))

(defun org-generic-id-test--update ()
  "Update the ID index and return the warnings it raised."
  (let (warnings)
    (cl-letf (((symbol-function 'warn)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) warnings))))
      (org-generic-id-update-id-locations "ID"))
    warnings))

(ert-deftest org-generic-id--update-forgets-moved-ids ()
  "IDs deleted or moved out of a reread file are no longer recorded in it."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0 "id1" "id2")
      (org-generic-id-test--write b 0)
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id1") a))
      (should (equal (org-generic-id-test--location "id2") a))
      ;; Refile id1 to b and delete id2.
      (org-generic-id-test--write a 60)
      (org-generic-id-test--write b 60 "id1")
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id1") b))
      (should-not (org-generic-id-test--location "id2")))))

(ert-deftest org-generic-id--update-rereads-file-with-older-modtime ()
  "A file is reread when its modification time changes, even backwards.
A file written from another machine can carry a modification time
behind the clock of the machine scanning it."
  (let ((a (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--update)
      (org-generic-id-test--write a -3600 "id2")
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id2") a))
      (should-not (org-generic-id-test--location "id1")))))

(ert-deftest org-generic-id--update-skips-unchanged-files ()
  "Files whose text did not change since they were last read are skipped."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file))
        (file-ids (symbol-function 'org-generic-id--file-ids))
        (read nil))
    (cl-letf (((symbol-function 'org-generic-id--file-ids)
               (lambda (file &rest args)
                 (push file read)
                 (apply file-ids file args))))
      (org-generic-id-test--with-index (list a b)
        (org-generic-id-test--write a 0 "id1")
        (org-generic-id-test--write b 0 "id2")
        (org-generic-id-test--update)
        (should (equal (sort read #'string<) (sort (list a b) #'string<)))
        (setq read nil)
        (org-generic-id-test--update)
        (should-not read)
        (org-generic-id-test--write b 60 "id2" "id3")
        (org-generic-id-test--update)
        (should (equal read (list b)))
        (should (equal (org-generic-id-test--location "id1") a))
        (should (equal (org-generic-id-test--location "id3") b))))))

(ert-deftest org-generic-id--update-reads-unsaved-changes ()
  "A buffer with unsaved changes is read, and reread once saved."
  (let* ((a (org-generic-id-test--temp-file))
         (buf (find-file-noselect a)))
    (unwind-protect
        (org-generic-id-test--with-index (list a)
          (with-current-buffer buf
            (insert "* id1\n:PROPERTIES:\n:ID: id1\n:END:\n"))
          (org-generic-id-test--update)
          (should (equal (org-generic-id-test--location "id1") a))
          (with-current-buffer buf
            (erase-buffer)
            (insert "* id2\n:PROPERTIES:\n:ID: id2\n:END:\n")
            (save-buffer))
          (org-generic-id-test--update)
          (should (equal (org-generic-id-test--location "id2") a))
          (should-not (org-generic-id-test--location "id1")))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest org-generic-id--update-reports-duplicates-across-updates ()
  "An ID read from a changed file is a duplicate if an unchanged file holds it."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--write b 0)
      (should-not (org-generic-id-test--update))
      (org-generic-id-test--write b 60 "id1")
      (let ((warnings (org-generic-id-test--update)))
        (should (= (length warnings) 1))
        (should (string-match-p "1 duplicate :ID:" (car warnings)))))))

(ert-deftest org-generic-id--update-ignores-stale-record-for-duplicates ()
  "An ID recorded in an unchanged file that no longer holds it is no duplicate.
This happens when a buffer is reverted after an ID was added to it."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0)
      (org-generic-id-test--write b 0)
      (org-generic-id-test--update)
      (org-generic-id-add-location "ID" "id1" a)
      (org-generic-id-test--write b 60 "id1")
      (should-not (org-generic-id-test--update))
      (should (equal (org-generic-id-test--location "id1") b)))))

(ert-deftest org-generic-id--update-keeps-duplicate-after-removing-copy ()
  "Removing the recorded copy of a duplicated ID records the remaining one.
The index records one file per ID, so the other file must be read again
even though it did not change."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--write b 0 "id1")
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id1") b))
      (org-generic-id-test--write b 60)
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id1") a)))))

(ert-deftest org-generic-id--update-keeps-duplicate-across-updates-after-removing-copy ()
  "Same as the test above, for a duplicate found across two updates."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--write b 0)
      (org-generic-id-test--update)
      (org-generic-id-test--write b 60 "id1")
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id1") b))
      (org-generic-id-test--write b 120)
      (org-generic-id-test--update)
      (should (equal (org-generic-id-test--location "id1") a)))))

(ert-deftest org-generic-id--update-reports-duplicate-once ()
  "A duplicate is reported once, and again only if it recurs once resolved."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--write b 0 "id1")
      (should (= (length (org-generic-id-test--update)) 1))
      (should-not (org-generic-id-test--update))
      (org-generic-id-test--write b 60)
      (should-not (org-generic-id-test--update))
      (org-generic-id-test--write b 120 "id1")
      (should (= (length (org-generic-id-test--update)) 1)))))

;;; Rereading a visited file

(defmacro org-generic-id-test--visiting (var file &rest body)
  "Run BODY with VAR bound to a buffer visiting FILE, killed afterwards."
  (declare (indent 2) (debug t))
  `(let ((,var (find-file-noselect ,file)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var (set-buffer-modified-p nil))
         (kill-buffer ,var)))))

(defun org-generic-id-test--revert (buf)
  "Revert BUF from its file and return the warnings raised."
  (let (warnings)
    (cl-letf (((symbol-function 'warn)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) warnings))))
      (with-current-buffer buf
        (revert-buffer t t t)))
    warnings))

(defmacro org-generic-id-test--counting-reads (var &rest body)
  "Run BODY, pushing onto VAR each file whose IDs are read."
  (declare (indent 1) (debug t))
  `(let ((file-ids (symbol-function 'org-generic-id--file-ids)))
     (cl-letf (((symbol-function 'org-generic-id--file-ids)
                (lambda (file &rest args)
                  (push file ,var)
                  (apply file-ids file args))))
       ,@body)))

(ert-deftest org-generic-id--revert-records-added-ids ()
  "Reverting a visited file records the IDs added to it on disk.
An update reads the buffer of a visited file, so an ID that another
program added to the file is only seen once the buffer is reverted."
  (let ((a (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--visiting buf a
        (org-generic-id-test--update)
        (org-generic-id-test--write a 60 "id1" "id2")
        (org-generic-id-test--update)
        (should-not (org-generic-id-test--location "id2"))
        (should-not (org-generic-id-test--revert buf))
        (should (equal (org-generic-id-test--location "id1") a))
        (should (equal (org-generic-id-test--location "id2") a))))))

(ert-deftest org-generic-id--revert-forgets-removed-ids ()
  "Reverting a visited file forgets the IDs removed from it on disk."
  (let ((a (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1" "id2")
      (org-generic-id-test--visiting buf a
        (org-generic-id-test--update)
        (org-generic-id-test--write a 60 "id1")
        (org-generic-id-test--revert buf)
        (should (equal (org-generic-id-test--location "id1") a))
        (should-not (org-generic-id-test--location "id2"))))))

(ert-deftest org-generic-id--revert-records-fingerprint ()
  "The next update skips a file read when its buffer was reverted."
  (let ((a (org-generic-id-test--temp-file))
        (read nil))
    (org-generic-id-test--counting-reads read
      (org-generic-id-test--with-index (list a)
        (org-generic-id-test--write a 0 "id1")
        (org-generic-id-test--visiting buf a
          (org-generic-id-test--update)
          (org-generic-id-test--write a 60 "id2")
          (org-generic-id-test--revert buf)
          (should (equal read (list a a)))
          (setq read nil)
          (org-generic-id-test--update)
          (should-not read))))))

(ert-deftest org-generic-id--revert-skips-unchanged-text ()
  "Reverting a buffer to the text the index last read reads nothing."
  (let ((a (org-generic-id-test--temp-file))
        (read nil))
    (org-generic-id-test--counting-reads read
      (org-generic-id-test--with-index (list a)
        (org-generic-id-test--write a 0 "id1")
        (org-generic-id-test--visiting buf a
          (org-generic-id-test--update)
          (setq read nil)
          (org-generic-id-test--revert buf)
          (should-not read))))))

(ert-deftest org-generic-id--revert-leaves-duplicates-to-update ()
  "A duplicate read by a revert is reported by the next update."
  (let ((a (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--visiting buf a
        (org-generic-id-test--update)
        (org-generic-id-test--write a 60 "id1" "id1")
        (should-not (org-generic-id-test--revert buf))
        (let ((warnings (org-generic-id-test--update)))
          (should (= (length warnings) 1))
          (should (string-match-p "1 duplicate :ID:" (car warnings))))))))

(ert-deftest org-generic-id--revert-leaves-duplicates-across-files-to-update ()
  "A duplicate of an ID held by another file is reported by the next update.
The other file is read to check that it still holds the ID, which must
not happen in the reverted buffer."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0)
      (org-generic-id-test--write b 0 "id1")
      (org-generic-id-test--visiting buf a
        (org-generic-id-test--update)
        (org-generic-id-test--write a 60 "id1")
        (should-not (org-generic-id-test--revert buf))
        (should (equal (with-current-buffer buf (buffer-string))
                       (with-temp-buffer
                         (insert-file-contents a)
                         (buffer-string))))
        (let ((warnings (org-generic-id-test--update)))
          (should (= (length warnings) 1))
          (should (string-match-p "1 duplicate :ID:" (car warnings))))))))

(ert-deftest org-generic-id--revert-records-moved-id ()
  "An ID moved into a reverted file from another file is no duplicate."
  (let ((a (org-generic-id-test--temp-file))
        (b (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a b)
      (org-generic-id-test--write a 0)
      (org-generic-id-test--write b 0 "id1")
      (org-generic-id-test--visiting buf a
        (org-generic-id-test--update)
        (org-generic-id-test--write a 60 "id1")
        (org-generic-id-test--write b 60)
        (should-not (org-generic-id-test--revert buf))
        (should (equal (org-generic-id-test--location "id1") a))
        (should (assoc b (plist-get org-generic-id--file-fingerprints 'ID)))
        (should-not (org-generic-id-test--update))
        (should (equal (org-generic-id-test--location "id1") a))))))

(ert-deftest org-generic-id--revert-ignores-untracked-files ()
  "Reverting a file that updates don't read records nothing."
  (let ((a (org-generic-id-test--temp-file))
        (c (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--write c 0)
      (org-generic-id-test--visiting buf c
        (org-generic-id-test--update)
        (org-generic-id-test--write c 60 "id9")
        (org-generic-id-test--revert buf)
        (should-not (org-generic-id-test--location "id9"))))))

(ert-deftest org-generic-id--find-file-reread-records-ids ()
  "Visiting a changed file again rereads its buffer, which records its IDs.
`find-file-noselect' rereads without asking when the file matches
`revert-without-query'."
  (let ((a (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--visiting buf a
        (org-generic-id-test--update)
        (org-generic-id-test--write a 60 "id2")
        (let ((revert-without-query '(".")))
          (should (eq (find-file-noselect a) buf)))
        (should (equal (org-generic-id-test--location "id2") a))
        (should-not (org-generic-id-test--location "id1"))))))

(ert-deftest org-generic-id--visit-records-ids-changed-since-update ()
  "Visiting a file that changed since the index read it records its IDs.
This covers a buffer killed and visited again between two updates."
  (let ((a (org-generic-id-test--temp-file)))
    (org-generic-id-test--with-index (list a)
      (org-generic-id-test--write a 0 "id1")
      (org-generic-id-test--update)
      (org-generic-id-test--write a 60 "id2")
      (org-generic-id-test--visiting buf a
        (should (string-match-p "id2" (with-current-buffer buf
                                        (buffer-string))))
        (should (equal (org-generic-id-test--location "id2") a))
        (should-not (org-generic-id-test--location "id1"))))))
