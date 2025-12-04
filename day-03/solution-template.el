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


(defun find-first-largest-digit-and-index (digit-list)
  "Finds the largest number in a list of digits and returns the number 
  and the first index it occurs at 
  Example: (find-largest-digit-and-indices '(1 9 4 9 2)) => (9 1)"
  (let ((max-digit nil)
        (max-index nil)
        (index 0))
    
    ;; Iterate through the list, keeping track of the current index
    (dolist (digit digit-list)
      
      ;; 1. Check if the current digit is greater than the current maximum
      (cond 
       ((null max-digit)
        ;; First element: Initialize max-digit and max-indices
        (setq max-digit digit)
        (setq max-index index))
       
       ((> digit max-digit)
        ;; New largest digit found: Update max-digit and reset max-indices
        (setq max-digit digit)
        (setq max-index index)))
       
      
      ;; Increment the index for the next element
      (setq index (1+ index)))
    
    ;; Return a list containing the largest digit and the list of indices
      (list max-digit max-index)))

(defun string-of-digits-to-list-of-digits (s)
  "Converts a string of digits into a list of single-character strings."
  (mapcar (lambda (i) (string-to-number (substring s i (1+ i))))
          (number-sequence 0 (1- (length s))))
  )

(defun find-largest-n-digit-number (sd &optional n)
  "Iterates through sd finding the largest n digit number"

  (let* (
        (n (or n 2))
        (digit-list (string-of-digits-to-list-of-digits sd))
        (start 0)
        (digit-list-length (length digit-list))
        (joltage 0)
        )
    
    (dotimes (i n)
      (setq end (+ i (- digit-list-length n) 1))
      (setq result (find-first-largest-digit-and-index (sublist digit-list start end)))
      ;; (message "list: %s start: %s  end: %s result: %s" (sublist digit-list start end) start end result)
      (setq joltage (+ joltage (* (expt 10 (1- (- n i))) (car result))))
      (setq start (+ (cadr result) 1 start))
   
      )
    joltage
    ))

(defun day-03-solve (input &optional n)
  "Solve puzzle for INPUT with N-digit numbers (default 2)."
  (let ((n (or n 2))
        (total-joltage 0))
    (dolist (line input total-joltage)
      (let ((joltage (find-largest-n-digit-number line n)))
        (setq total-joltage (+ total-joltage joltage))
        total-joltage))
    )
  )

;; Then define part1/part2 as thin wrappers:
(defun day-03-part1 (input)
  (day-03-solve input 2))

(defun day-03-part2 (input)
  (day-03-solve input 12))


;;; Tests and Execution
(defun test-day-03 ()
  "Test with sample input."
  (let* ((test-input (aoc-read-lines "test.txt"))
         (part1 (day-03-part1 test-input))
         (part2 (day-03-part2 test-input))) 
    (message "Part 1 test (357): %s" part1)
    (message "Part 2 test (3121910778619): %s" part2)))

(defun run-day-03 ()
  "Run with actual input."
  (let* ((input (aoc-read-lines "input.txt"))
         (part1 (aoc-time (lambda () (day-03-part1 input))))
         (part2 (aoc-time (lambda () (day-03-part2 input)))))
    (message "Part 1: %s" part1)
    (message "Part 2: %s" part2))
  )

;; Uncomment to run:
(test-day-03)
(run-day-03)

(provide 'solution)
