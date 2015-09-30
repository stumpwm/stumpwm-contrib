(in-package #:winner-mode)

(defvar *current-ids* (make-hash-table))
(defvar *max-ids* (make-hash-table))
(defvar *tmp-folder* #p"/tmp/")
