;; Add project to load path
(add-to-list 'load-path (expand-file-name "."))
(add-to-list 'load-path (expand-file-name "utils"))

;; Require common utilities
(require 'common)

;; Optional: Useful packages
(require 'dash)           ; Functional programming helpers
(require 's)              ; String manipulation
(require 'f)              ; File/directory functions
