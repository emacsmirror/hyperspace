;;; hyperspace.el --- Search Quick                       -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; URL: https://github.com/ieure/hyperspace-el
;; Version: 0.8
;; Package-Requires: ((emacs "25") (s "1.12.0"))
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hyperspace is a way to get nearly anywhere from wherever you are,
;; whether that's somewhere inside Emacs or on the web.  It's
;; somewhere in between Quicksilver and keyword URLs.

;; When you invoke Hyperspace (suggested keybinding: H-SPC),

;;; Code:

(require 's)

 ;; Action helpers

(defun hyperspace-action->browse-url-pattern (pattern query)
  "Browse a URL former from PATTERN and QUERY."
  (browse-url (format pattern query)))

(defun hyperspace--mu4e-context (query)
  "Return a mu4e context for QUERY.

   If the first word of QUERY matches the beginning of a mu4e
   context, return its name.  Otherwise, return NIL."
  (loop with parts = (s-split-up-to "\\s-+" query 1)
        with possible-context = (car parts)
        with possible-query = (cadr parts)
        for context in mu4e-contexts
        for context-name = (mu4e-context-name context)
        if (s-starts-with? possible-context context-name)
        return (cons context-name possible-query)))

(defun hyperspace-action->mu4e (&optional query)
  "Search mu4e with QUERY.

   If QUERY is unspecified, use the first bookmark in variable
   ‘mu4e-bookmarks’ and update mail and index."

  (mu4e-headers-search (or query (caar mu4e-bookmarks)))
  (unless query
    (mu4e-update-mail-and-index)))

(defun hyperspace-action->mu4e-context (&optional query)
  "Look for a mu4e context in the first word of QUERY.

   A valid context is one which matches a left-anchored substring of
   all defined mu4e contexts.

   If found, switch to it, then call `hyperspace-action->mu4e' with
   the remainder of QUERY.  Otherwise, call with the entire QUERY,
   without switching the context."

  (thread-first
      (pcase (hyperspace--mu4e-context query)
        (`(context . query)
         (mu4e-context-switch nil context)
         query))
    (or query)
    (hyperspace-action->mu4e)))

(defun hyperspace-action->info (node &optional query)
  "Open an Info buffer for NODE.

   If QUERY is present, look it up in the index."
    (info node)
    (when query
      (Info-index query)))

 ;; Package definitions

(defgroup hyperspace nil
  "Options for Hyperspace"
  :prefix "hyperspace-"
  :group 'applications)

(defcustom hyperspace-actions
  '(("ddg" . "https://duckduckgo.com/?q=%s")
    ("dis" . "https://duckduckgo.com/?q=%s&iax=images&ia=images")
    ("wp"  . "https://en.wikipedia.org/wiki/%s")
    ("gg"  . "https://www.google.com/search?q=%s")
    ("gis" . "https://www.google.com/search?tbm=isch&q=%s")
    ("ggm" . "https://www.google.com/maps/search/%s")
    ("clp" . "https://portland.craigslist.org/search/sss?query=%s")
    ("eb" .  "https://www.ebay.com/sch/i.html?_nkw=%s")
    ("bb" . #'bbdb-search-name)
    ("m4" . #'hyperspace-action->mu4e)
    ("m4c" . #'hyperspace-action->mu4e-context)
    ("el" . (apply-partially #'hyperspace-action->info "(elisp)Top"))
    ("av" . #'apropos-variable)
    ("ac" . #'apropos-command)
    ("af" . (lambda (query) (apropos-command query t))))

  "Actions for Hyperspace.

   Hyperspace actions a cons of (KEYWORD . DISPATCHER).  When
   Hyperspace is invoked, the keyword is extracted and looked up
   in this alist; the remainder of the string is passed to the
   dispatcher as a QUERY argument.

   DISPATCHER can be a function which performs the action.

   DISPATCHER can also be an expression which returns a function
   to perform the action.

   Finally, DISPATCHER can be a string with a URL pattern containing
   '%s'.  The '%s' will be replaced with the query, and the URL browsed."

  :group 'hyperspace
  :type '(alist :key-type (string :tag "Keyword")
                :value-type (choice
                             (function :tag "Function")
                             (sexp :tag "Expression")
                             (string :tag "URL Pattern"))))

(defcustom hyperspace-default-action
  (caar hyperspace-actions)
  "Default action."
  :group 'hyperspace
  :type `(radio
          ,@(mapcar (lambda (action) (list 'const (car action))) hyperspace-actions)))

(defvar hyperspace-history nil
  "History of Hyperspace actions.")

(defun hyperspace--initial-text ()
  "Return the initial text.

   If a region of the buffer is selected, it will be prefilled as
   the Hyperspace query."
  (when (region-active-p)
    (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun hyperspace--initial-query ()
  "Return the initial query.

   If a region of the buffer is selected, it will be prefilled as
   the Hyperspace query."
  (if-let ((text (hyperspace--initial-text)))
      (cons (concat " " text) 1)))

(defun hyperspace--query ()
  "Ask the user for the Hyperspace action and query.

   If the region isn't active, the user is prompted for the action and query.

   If the region is active, its text is used as the initial value
   for the query, and the user enters the action.

   If a prefix argument is specified and the region is active,
   `HYPERSPACE-DEFAULT-ACTION' is chosen without prompting."
  (let ((initial (hyperspace--initial-query)))
    (if (and initial current-prefix-arg)
        (list hyperspace-default-action (string-trim (car initial)))
    (s-split-up-to
     "\\s-+"
     (read-from-minibuffer "HS: " initial nil nil 'hyperspace-history) 1))))

(defun hyperspace--evalable-p (form)
  "Can FORM be evaluated?"
  (and (listp form)
       (or (functionp (car form))
           (subrp (car form)))))

(defun hyperspace--dispatch (action &optional query)
  "Execute ACTION, with optional QUERY argument."
  (pcase action
    ((pred functionp) (funcall action query))
    ((pred hyperspace--evalable-p) (funcall (eval action) query))
    ((pred stringp) (hyperspace-browse-url-pattern action query))
    (_ (error "Unknown action"))))

;;;###autoload
(defun hyperspace (keyword &optional query)
  "Execute action for keyword KEYWORD, with optional QUERY."
  (interactive (hyperspace--query))
  (if-let ((action (cdr (assoc keyword hyperspace-actions))))
      (hyperspace--dispatch action query)
    (error "No action defined for keyword `%s'" keyword)))

;;;###autoload
(defun hyperspace-enter (&optional query)
  "Enter Hyperspace, sending QUERY to the default action.

   If the region is active, use that as the query for
   ‘hyperspace-default-action’.  Otherwise, prompt the user."
  (interactive (list (hyperspace--initial-text)))
  (hyperspace
   hyperspace-default-action
   (or query
       (read-from-minibuffer (format "HS: %s " hyperspace-default-action)))))

(defvar hyperspace-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "H-SPC") #'hyperspace)
    (define-key kmap (kbd "C-s") #'hyperspace)))

(provide 'hyperspace)

;;; hyperspace.el ends here
