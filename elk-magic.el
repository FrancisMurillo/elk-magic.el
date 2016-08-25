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

(require 'elk)

;;* Package
(defgroup elk-magic nil
  "Parse Emacs Lisp source code"
  :prefix "elk-magic-"
  :group 'tools
  :link (list 'url-link
              :tag "Github" "https://github.com/FrancisMurillo/elk-magic.el"))

(defun elk--discard-filler (tokens)
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
                            (plist-put (-copy token) :tokens (elk--discard-filler sub-tokens))))
                         (_ token)))))
         (pipeline (-compose
                    (-partial #'-map recurser)
                    (-partial #'-filter filterer))))
    (funcall pipeline tokens)))


(defun elk--attach-source (text tokens)
  "Label atoms based on their source text"
  (lexical-let* ((source-text text)
                 (recurser (lambda (token)
                             (let ((type (plist-get token :type)))
                               (pcase type
                                 ((or `atom `text `comment `whitespace)
                                  (let ((start-pos (plist-get token :start-pos))
                                        (end-pos  (plist-get token :end-pos))
                                        (new-token (-copy token)))
                                    (when (= end-pos -1)
                                      (setf end-pos (length source-text))
                                      (plist-put new-token :end-pos end-pos))
                                    (plist-put new-token :text
                                              (substring-no-properties source-text
                                                                       start-pos
                                                                       end-pos))))
                                 ((or `expression `quote)
                                  (let* ((sub-tokens (plist-get token :tokens)))
                                    (plist-put(-copy token) :tokens (elk--attach-source source-text sub-tokens))))
                                 (_ token))))))
    (-map recurser tokens)))

(defun elk--leveler (level tokens)
  "Recurser of elk--attach-level"
  (-map (lambda (token)
          (let ((type (plist-get token :type))
                (leveled-token (plist-put (-copy token) :level level)))
            (pcase type
              ((or `expression `quote)
               (let ((sub-tokens (plist-get leveled-token :tokens)))
                 (plist-put (-copy leveled-token)
                            :tokens (elk--leveler (1+ level) sub-tokens))))
              (_ leveled-token))))
        tokens))

(defun elk--attach-level (tokens)
  "Attach a level value for the"
  (elk--leveler 0 tokens))

(defun elk--incremental-sequence (&optional start)
  "An quick implementation of an increasing sequence"
  (lexical-let ((seed (or start 0)))
    (lambda ()
      (prog1
          seed
        (setf seed (1+ seed))))))

(defun elk--marker (parent-id generator tokens)
  "Recurser of elk--attach-token-id"
  (-map (lambda (token)
          (let ((type (plist-get token :type))
                (marked-token (plist-put (-copy token) :id (funcall generator))))
            (setf marked-token (plist-put marked-token :parent-id parent-id))
            (pcase type
              ((or `expression `quote)
               (let ((sub-tokens (plist-get marked-token :tokens)))
                 (plist-put (-copy marked-token)
                            :tokens (elk--marker
                                     (plist-get marked-token :id)
                                     generator sub-tokens))))
              (_ marked-token))))
        tokens))

(defun elk--attach-token-id (tokens)
  "Attach an id for each token, useful when the tokens are flattned"
  (elk--marker 0 (elk--incremental-sequence 1) tokens))

(defun elk--indexer (tokens)
  "Recurser of elk--attach-expression-index"
  (-map-indexed (lambda (index token)
                  (let ((type (plist-get token :type))
                        (indexed-token (plist-put (-copy token) :index index)))
                    (pcase type
                      ((or `expression `quote)
                       (let ((sub-tokens (plist-get indexed-token :tokens)))
                         (plist-put (-copy indexed-token)
                                    :tokens (elk--indexer sub-tokens))))
                      (_ indexed-token))))
                tokens))

(defun elk--attach-expression-index (tokens)
  "Attach indices to expression to determine what position it is in"
  (elk--indexer tokens))


(defun elk--flatten-tokens (tokens)
  "Flatten nested tokens as one token list"
  (funcall (-compose
            (-partial #'apply #'append)
            (-partial #'-map (lambda (token)
                               (let ((type (plist-get token :type)))
                                 (pcase type
                                   ((or `expression `quote)
                                    (let ((sub-tokens (plist-get token :tokens)))
                                      (elk--flatten-tokens sub-tokens)))
                                   (_ (list token)))))))
           tokens))

(defun elk--select-type (type tokens)
  "Filter tokens by a specified type"
  (funcall (-compose
            (-partial #'-filter
                      (lambda (token)
                        (eq (plist-get token :type) type)))
            #'elk--flatten-tokens)
           tokens))

(defun elk--extract-atoms (tokens)
  "Get atoms in tokens"
  (funcall (-compose
            (-partial #'-map (-rpartial #'plist-get :text))
            (-partial #'elk--select-type 'atom))
           tokens))

(defun elk--extract-text (tokens)
  "Get text in tokens"
  (funcall (-compose
            (-partial #'-map (-rpartial #'plist-get :text))
            (-partial #'elk--select-type 'text))
           tokens))

(defun elk--default-atom-filter (atom)
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


(defun elk--summarize-atoms (tokens)
  "Report what atoms are used more likely"
  (funcall (-compose
            (-partial #'-sort (-on #'> #'cdr))
            (-partial #'-map (lambda (repeating-tokens)
                               (cons (-first-item repeating-tokens)
                                     (1- (length repeating-tokens)))))
            (-partial #'-group-by #'identity)
            (-partial #'-filter #'elk--default-atom-filter)
            #'elk--extract-atoms)
           tokens))





(defun elk--nearest-top-expression-at-point ()
  "Get token expression that is nearest to the highest point"
  (interactive)
  (let* ((source-text (buffer-substring-no-properties (point-min) (point-max)))
         (tokens (elk--tokenize source-text))
         (expression-token (-first (lambda (token)
                                     (and (= (plist-get token :level) 0)
                                          (eq (plist-get token :type) 'expression)
                                          (<= (plist-get token :start-pos) (point))
                                          (>= (plist-get token :end-pos) (point))))
                                   tokens)))
    (if expression-token
        (goto-char (1+ (plist-get expression-token :start-pos)))
      (message "No near top level expression at point"))))


(provide 'elk-magic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elk-magic.el ends here
