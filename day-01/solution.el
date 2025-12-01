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

(defun day-01-part1 (input)
  "Solve part 1."
  (cl-loop with position = 50
           with zero-count = 0
           for line in (split-string input "\n" t)
           for direction = (substring line 0 1)
           for steps = (string-to-number (substring line 1))
           do (setq position
                    (let ((new-pos (if (string= direction "R")
                                       (+ position steps)
                                     (- position steps))))
                      (mod (+ new-pos 100) 100)))
           when (= position 0)
           do (cl-incf zero-count)
           finally return (or zero-count 0)))

(defun day-01-part2 (input)
  (cl-loop with position = 50
           with zero-count = 0
           for line in (split-string input "\n" t)
           for direction = (substring line 0 1)
           for steps = (string-to-number (substring line 1))
           for new-pos = (if (string= direction "L")
                             (- position steps)
                             (+ position steps))
           for wrapped-pos = (mod (+ new-pos 100) 100)
           
           ;; Count full circles from step count
           do (cl-incf zero-count (floor (/ steps 100)))
           ;; count crossings
           do (let* ((old-mod position)
                     (new-mod wrapped-pos))
                ;; check if we crossed 0 in the remaining partial movement
                (when (or (and (< old-mod new-mod)  ; wrap backward across 0
                               (string= direction "L")
                               (/= old-mod 0)       ; not already on 0
                               (<= new-mod 99))
                          (and (> old-mod new-mod)  ; wrap forward across 0
                               (string= direction "R")
                               (/= new-mod 0)       ; don't land on zero
                               (<= old-mod 99)))
                 (cl-incf zero-count))
                )
           ;; Update position with wrap-around
           do (setq position wrapped-pos)
           ;; detect landing on zero - part1
           when (= position 0)
           do (cl-incf zero-count)
           finally return (or zero-count 0)
           ))

;;; Tests and Execution
(defun test-day-01 ()
  "Test with sample input."
  (let* ((test-input (aoc-read-input "test.txt"))
         (part1 (day-01-part1 test-input))
         (part2 (day-01-part2 test-input))) 
    (message "Part 1 test: %s" part1)
    (message "Part 2 test: %s" part2)))

(defun run-day-01 ()
  "Run with actual input."
  (let* ((input (aoc-read-input "input.txt"))
         (part1 (aoc-time (lambda () (day-01-part1 input))))
         (part2 (aoc-time (lambda () (day-01-part2 input)))))
    (message "Part 1: %s" part1)
    (message "Part 2: %s" part2))
  )

;; Uncomment to run:
(test-day-01)          
(run-day-01)

(provide 'solution)
