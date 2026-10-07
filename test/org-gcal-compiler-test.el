;;; org-gcal-compiler-test.el --- Offline compiler cleanup checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-gcal)

(ert-deftest org-gcal-compiler-multi-day-expansion-keeps-occurrences ()
  (let* ((events [(:id "weekly" :summary "Meeting"
                       :start (:dateTime "2026-10-05T09:00:00Z")
                       :end (:dateTime "2026-10-05T10:00:00Z")
                       :recurrence ["RRULE:FREQ=WEEKLY;BYDAY=MO,WE"])])
         (expanded (org-gcal--expand-multi-day-events events "calendar")))
    (should (= 2 (length expanded)))
    (should (equal "2026-10-05T09:00:00Z"
                   (plist-get (plist-get (aref expanded 0) :start) :dateTime)))
    (should (equal "2026-10-07T09:00:00Z"
                   (plist-get (plist-get (aref expanded 1) :start) :dateTime)))))

(ert-deftest org-gcal-compiler-body-timestamps-retain-prose ()
  (with-temp-buffer
    (org-mode)
    (insert "* Event\n:PROPERTIES:\n:END:\n<2026-10-05 Mon 09:00>\nBody\n")
    (goto-char (point-min))
    (let ((org-gcal-description-mode 'body))
      (org-gcal--update-parent-timestamps
       (point-marker) '("<2026-10-07 Wed 10:00>")))
    (should (string-match-p "<2026-10-07 Wed 10:00>\nBody" (buffer-string)))
    (should-not (string-match-p "2026-10-05" (buffer-string)))))
