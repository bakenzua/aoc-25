;;; solution.el --- Day 1: Puzzle Title

;;; Commentary:
;; Solutions for Advent of Code Day 1

;;; Code:


(require 'cl-lib)
(require 'seq)

(defun day-01-part1 (input)
  "Solve part 1."
  (let ((lines (split-string input "\n" t)))
    ;; Your solution here
    ))

(defun day-01-part2 (input)
  "Solve part 2."
  ;; Your solution here
  )

;;; Tests and Execution
(defun test-day-01 ()
  "Test with sample input."
  (let* ((test-input "test input here")
         (part1 (day-x-part1 test-input))
         (part2 (day-x-part2 test-input)))
    (message "Part 1 test: %s" part1)
    (message "Part 2 test: %s" part2)))

(defun run-day-01 ()
  "Run with actual input."
  (let* ((input (aoc-read-input "input.txt"))
         (part1 (aoc-time (lambda () (day-01-part1 input))))
         (part2 (aoc-time (lambda () (day-01-part2 input)))))
    (message "Part 1: %s" part1)
    (message "Part 2: %s" part2)))

;; Uncomment to run:
;; (test-day-x)
;; (run-day-x)

(provide 'solution)
;;; solution.el ends here
