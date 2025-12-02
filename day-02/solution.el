;;; solution.el --- Day 1: Puzzle Title

;;; Commentary:
;; Solutions for Advent of Code Day 1

;;; Code:


;; Load utilities
;; Set up load path
(let ((utils-dir (expand-file-name 
                  "../utils" 
                  (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path utils-dir))
(require 'common)

(require 'cl-lib)
(require 'seq)

(defun day-02-count-invalid-ids (input regexp)
  "Count invalid IDs based on REGEXP pattern."
  (cl-loop with invalid-id-count = 0
           for range in (split-string (replace-regexp-in-string "[[:space:]\n\r]+" "" input) "," t)
           for (start end) = (mapcar #'string-to-number (split-string range "-"))
           do (cl-loop for n from start to end
                       for n-str = (number-to-string n)
                       when (string-match-p regexp n-str)
                       do (cl-incf invalid-id-count n)
                       do (sit-for 0.001))
           finally return invalid-id-count))

(defun day-02-part1 (input)
  "Solve part 1."
  (day-02-count-invalid-ids input "^\\([0-9]+\\)\\1$"))

(defun day-02-part2 (input)
  "Solve part 2."
  (day-02-count-invalid-ids input "^\\([0-9]+\\)\\1+$"))

;;; Tests and Execution
(defun test-day-02 ()
  "Test with sample input."
  (let* ((test-input (aoc-read-input "test.txt"))
         (part1 (day-02-part1 test-input))
         (part2 (day-02-part2 test-input))) 
    (message "Part 1 test: %s" part1)
    (message "Part 2 test: %s" part2)))

(defun run-day-02 ()
  "Run with actual input."
  (let* ((input (aoc-read-input "input.txt"))
         (part1 (aoc-time (lambda () (day-02-part1 input))))
         (part2 (aoc-time (lambda () (day-02-part2 input)))))
    (message "Part 1: %s" part1)
    (message "Part 2: %s" part2))
  )

;; Uncomment to run:
(test-day-02)          
(run-day-02)

(provide 'solution)
