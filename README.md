
# Advent of Code 2025 – Emacs Lisp Solutions

This repository contains my solutions for [Advent of Code 2025](https://adventofcode.com/2025), implemented in Emacs Lisp. The goal is to solve the daily programming puzzles while learning and improving my Emacs Lisp skills.

## About Advent of Code

[Advent of Code](https://adventofcode.com/2025) is an annual series of programming puzzles created by **Eric Wastl**. Each day from December 1st to 25th, a new puzzle is released, challenging participants with algorithmic and logical problems. Huge thanks to Eric for creating and maintaining this wonderful event!

## Why Emacs Lisp?

I’m using this as an opportunity to:
- Deepen my understanding of Emacs Lisp
- Explore functional programming patterns in a Lisp environment
- Integrate puzzle-solving directly into my Emacs workflow

## How to Run

1. Open any `solution.el` file in Emacs.
2. Evaluate the buffer (`M-x eval-buffer`).
3. Call the main solving function, e.g., `(day-01-part-1)`.

Alternatively, use `M-x ielm` or `M-x eval-expression` to run functions interactively.

## Repository Structure

```
advent-of-code-2025/
├── README.md
├── init.el
├── utils
│   └── common.el
├── day-01/
│    └──  solution.el
├── day-02/
│    └── solution.el
└── ...
```

Each day’s directory contains:
- `solution.el`: The Emacs Lisp code solving the puzzle
- `input.txt`: My personalized puzzle input (not shared publicly)

## Learning Resources

- [Advent of Code 2025](https://adventofcode.com/2025) – The official puzzle site
- [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/) – Comprehensive Emacs Lisp documentation
- [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/) – Great for beginners

## Notes

- Solutions are written for clarity and learning, not necessarily for optimal performance.
- Input files are excluded via `.gitignore` to respect Advent of Code guidelines.
- Feedback and suggestions are welcome, especially regarding Emacs Lisp best practices!

Happy hacking, and may your Emacs sessions be ever productive!
