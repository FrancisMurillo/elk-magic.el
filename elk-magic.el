;;; elk-magic.el --- An Emacs Lisp source code analyzer
;;
;; Filename: elk-magic.el
;; Description: Just something to analyze Emacs Lisp code
;; Author: Francis Murillo
;; Maintainer:
;; Created: Thu Aug 25 14:21:44 2016 (+0800)
;; Version: 0.1.0
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'dash)

(require 'elk)

;;* Package
(defgroup elk-magic nil
  "Parse Emacs Lisp source code"
  :prefix "elk-magic-"
  :group 'tools
  :link (list 'url-link
           :tag "Github" "https://github.com/FrancisMurillo/elk-magic.el"))


;;* Private
(defun elk-magic--discard-filler (tokens)
  "Disregard comments and whitespace with the tokens"
  (let* ((filterer (lambda (token)
                     (let ((type (plist-get token :type)))
                       (pcase type
                         ((or `whitespace `comment) nil)
                         (_ t)))))
         (recurser (lambda (token)
                     (let ((type (plist-get token :type)))
                       (pcase type
                         ((or `expression `quote)
                          (let* ((sub-tokens (plist-get token :tokens)))
                            (plist-put (-copy token) :tokens (elk-magic--discard-filler sub-tokens))))
                         (_ token)))))
         (pipeline (-compose
                    (-partial #'-map recurser)
                    (-partial #'-filter filterer))))
    (funcall pipeline tokens)))


(defun elk-magic--flatten-tokens (tokens)
  "Flatten nested tokens as one token list"
  (funcall (-compose
            (-partial #'apply #'append)
            (-partial #'-map (lambda (token)
                               (let ((type (plist-get token :type)))
                                 (pcase type
                                   ((or `expression `quote)
                                    (let ((sub-tokens (plist-get token :tokens)))
                                      (elk-magic--flatten-tokens sub-tokens)))
                                   (_ (list token)))))))
           tokens))

(defun elk-magic--select-type (type tokens)
  "Filter tokens by a specified type"
  (funcall (-compose
            (-partial #'-filter
                      (lambda (token)
                        (eq (plist-get token :type) type)))
            #'elk-magic--flatten-tokens)
           tokens))

(defun elk-magic--extract-atoms (tokens)
  "Get atoms in tokens"
  (funcall (-compose
            (-partial #'-map (-rpartial #'plist-get :text))
            (-partial #'elk-magic--select-type 'atom))
           tokens))

(defun elk-magic--extract-text (tokens)
  "Get text in tokens"
  (funcall (-compose
            (-partial #'-map (-rpartial #'plist-get :text))
            (-partial #'elk-magic--select-type 'text))
           tokens))

(defun elk-magic--default-atom-filter (atom)
  "Filter an atom if it is not built-in or redundant"
  (funcall (-andfn (-not
                    (-compose
                     #'special-form-p
                     #'intern-soft))
                   (-not (-compose
                          #'subrp
                          #'symbol-function
                          #'intern-soft)))
           atom))


;;* Interface
(defun elk-magic-summarize-atoms (tokens)
  "Report what atoms are used more likely"
  (funcall (-compose
            (-partial #'-sort (-on #'> #'cdr))
            (-partial #'-map (lambda (repeating-tokens)
                               (cons (-first-item repeating-tokens)
                                     (1- (length repeating-tokens)))))
            (-partial #'-group-by #'identity)
            (-partial #'-filter #'elk-magic--default-atom-filter)
            #'elk-magic--extract-atoms)
           tokens))

(defun elk-magic--nearest-top-expression-at-point ()
  "Get token expression that is nearest to the highest point"
  (interactive)
  (let* ((source-text (buffer-substring-no-properties (point-min) (point-max)))
         (tokens (elk-parse source-text))
         (expression-token (-first (lambda (token)
                                     (and (= (plist-get token :level) 1)
                                          (eq (plist-get token :type) 'expression)
                                          (<= (plist-get token :start-pos) (point))
                                          (>= (plist-get token :end-pos) (point))))
                                   tokens)))
    (if expression-token
        (goto-char (1+ (plist-get expression-token :start-pos)))
      (message "No near top level expression at point"))))

(defun elk-magic--create-token-table (tokens)
  "Create an hash table on the TOKENS based on their ID.  This is for manipulating tokens inplace."
  (letrec ((table (make-hash-table
                   :size (length tokens)
                   :rehash-size 2.0))
           (recurser
            (lambda (table tokens)
              (mapc (lambda (token)
                      (puthash (plist-get token :id) token table)
                      (funcall recurser table (plist-get token :tokens)))
                    tokens)
              table)))
    (funcall recurser table tokens)))

(defun elk-magic--select-top-level-expressions (tokens)
  "Extract top level expressions from a list of TOKENS."
  (-filter (lambda (token)
             (and (= (plist-get token :level) 1)
                (eq (plist-get token :type) 'expression)))
           tokens))

(defun elk-magic--token-depth (token)
  "Find out the TOKEN depth or the maximum number of level it has."
  (letrec ((recurser
       (lambda (token)
         (let* ((token-level (plist-get token :level))
             (sub-tokens (plist-get token :tokens))
             (sub-token-depths
              (-map recurser sub-tokens)))
           (if sub-token-depths
               (-max sub-token-depths)
             token-level)))))
    (funcall recurser token)))

(defun elk-magic--token-atoms (token)
  "Find out the TOKEN child atoms up to the last depth."
  (letrec ((recurser
       (lambda (token)
         (let* ((token-type (plist-get token :type))
             (sub-tokens (plist-get token :tokens)))
           (if (eq token-type 'atom)
               (list token)
             (apply #'append (-map recurser sub-tokens)))))))
    (funcall recurser token)))

(defun elk-magic--token-complexity (token)
  "Compute expression or TOKEN complexity."
  (let* ((atoms (elk-magic--token-atoms token))
      (root-level (plist-get token :level))
      (atom-complexity (/ (float (length atoms)))))
    (-sum
     (-map (lambda (atom)
             (let* ((depth (- (plist-get atom :level) root-level))
                 (depth-complexity depth))
               (* atom-complexity depth-complexity)))
           atoms))))


;;* Interface


(provide 'elk-magic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk-magic.el ends here
