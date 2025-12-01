;; Advent of Code Utilities
(defun aoc-read-input (filename)
  "Read input file as string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun aoc-read-lines (filename)
  "Read input file as list of lines."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun aoc-read-numbers (filename)
  "Read input file as list of numbers."
  (mapcar #'string-to-number (aoc-read-lines filename)))

(defun aoc-time (func)
  "Time the execution of FUNC."
  (let ((start (current-time)))
    (prog1 (funcall func)
      (message "Time: %.6f sec" (float-time (time-since start))))))

(provide 'common)
