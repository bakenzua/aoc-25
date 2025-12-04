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

(defun day-x-part1 (input)
  "Solve part 1."
  )

(defun day-x-part2 (input)
  "Solve part 2."
  )

;;; Tests and Execution
(defun test-day-x ()
  "Test with sample input."
  (let* ((test-input (aoc-read-input "test.txt"))
         (part1 (day-x-part1 test-input))
         (part2 (day-x-part2 test-input))) 
    (message "Part 1 test: %s" part1)
    (message "Part 2 test: %s" part2)))

(defun run-day-x ()
  "Run with actual input."
  (let* ((input (aoc-read-input "input.txt"))
         (part1 (aoc-time (lambda () (day-x-part1 input))))
         (part2 (aoc-time (lambda () (day-x-part2 input)))))
    (message "Part 1: %s" part1)
    (message "Part 2: %s" part2))
  )

;; Uncomment to run:
;; (test-day-x)          
;; (run-day-x)

(provide 'solution)
