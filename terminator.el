;;; terminator.el --- multi-view terminal emulation
;;
;; Copyright (C) 2009 Fabián Ezequiel Gallina
;;
;; Author: Fabián Ezequiel Gallina (fabian@gnu.org.ar)
;; Created: 25 November 2009
;; Keywords: window, terminal, convenience

;; This file is NOT part of GNU Emacs.

;; terminator.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; terminator.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;; --------------------------------------------------------------------

;;; Commentary:
;;
;; This package mimics the behavior found in a program called
;; terminator (https://launchpad.net/terminator/) which provides the
;; posibility to work on several terminal emulators in just one window
;; (in Emacs we call these frames)
;;
;; By default terminator.el provides a function called
;; `terminator-basic-setup' which creates four basic templates. It's
;; recommended you try it the first time you install this
;; package. Here is a minimal terminator.el setup:
;;
;; (require 'terminator)
;; (terminator-global-mode 1)
;; (terminator-basic-setup)
;;
;; After adding that to your .emacs file you should be able to start
;; playing with terminator.el. By default the basic four templates
;; created by `terminator-basic-setup' are bound to the following
;; keybindings:
;;
;; * C-c t 1: Opens template 0
;; * C-c t 2: Opens template 1
;; * C-c t 3: Opens template 2
;; * C-c t 4: Opens template 3
;;
;; You also could open a template by just using C-c t t and you will
;; be asked for the template number to open in the minibuffer
;;
;; If you want to use custom templates (I encourage you to do so) you
;; have available the `terminator-add-template' function. It will
;; append the created template to the terminator-templates list. Here
;; is an example:
;;
;; ;; Erase all templates in case you used `terminator-basic-setup'
;; (setq terminator-templates (list))
;; (terminator-add-template 2 3 (lambda () (message "template opened")))
;;
;; The first argument is the number of horizontal splits while the
;; second is the number of vertical ones. These are mandatory.  The
;; last parameter is optional and should be a function to execute
;; after the splits and terminals have been created. This will allow
;; you to lot's of nifty things. For instance you could define a
;; template which will render like this:
;;
;;                    --------------
;;                    |      |     |
;;                    |      |     |
;;                    |      |------    (* the hook removed the vertical
;;                    | *    |     |    split)
;;                    |      |     |
;;                    --------------
;;

;; Requirements:
;;
;; You will need term.el which comes with GNU/Emacs. Also you will
;; need multi-term (http://www.emacswiki.org/emacs/MultiTerm) and
;; finally subdivide.el (http://github.com/fgallina/subdivide)

;; Installation:
;;
;; Put the following line in your `.emacs' file:
;;
;; (require 'terminator)
;; (terminator-global-mode t)
;;
;; and optionally you could enable the default templates
;;
;; (terminator-basic-setup)
;;

;; Acknowledgements:
;;

;;; Code:
(require 'subdivide)
(require 'term)
(require 'multi-term)


(defvar terminator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t t") 'terminator-open-template)
    (define-key map (kbd "C-c t 1") (lambda () (interactive) (terminator-open-template 0)))
    (define-key map (kbd "C-c t 2") (lambda () (interactive) (terminator-open-template 1)))
    (define-key map (kbd "C-c t 3") (lambda () (interactive) (terminator-open-template 2)))
    (define-key map (kbd "C-c t 4") (lambda () (interactive) (terminator-open-template 3)))
    (define-key map (kbd "C-c t k") 'terminator-close-all)
    map))


(defvar terminator-templates (list)
  "LIST containing template objects")


(defun terminator-make-template(hsplit vsplit &optional hook)
  "Creates a template with HSPLIT horizontal divisions and VSPLIT
vertical divisions.

If HOOK is defined it is run after the buffer is divided by
HSPLIT and VSPLIT."
  (list hsplit vsplit hook))


(defun terminator--get-template-width (template)
  "Returns the width of TEMPLATE"
  (nth 0 (nth template terminator-templates)))


(defun terminator--get-template-height (template)
  "Returns the height of TEMPLATE"
  (nth 1 (nth template terminator-templates)))


(defun terminator--get-template-hook (template)
  "Returns the hook of TEMPLATE"
  (nth 2 (nth template terminator-templates)))


(defun terminator-add-template(hsplit vsplit &optional hook)
  "Adds a template to the terminator-templates list"
  (setq terminator-templates
        (append terminator-templates
                (list (terminator-make-template hsplit vsplit hook)))))


(defun terminator-basic-setup ()
  "Creates 4 basic templates"

  ;; |-------------|
  ;; |*            |
  ;; |             |
  ;; |             |
  ;; |-------------|
  (terminator-add-template  1 1)

  ;; |-------------|
  ;; |      |*     |
  ;; |------|      |
  ;; |      |      |
  ;; |-------------|
  (terminator-add-template  2 2 (lambda ()
                                  (terminator-windmove 'right 2)
                                  (delete-other-windows-vertically)))

  ;; |-------------|
  ;; |*     |      |
  ;; |-------------|
  ;; |             |
  ;; |-------------|
  (terminator-add-template  1 2 (lambda ()
                                  (terminator-windmove 'up 1)
                                  (split-window-horizontally)))

  ;; |-------------|
  ;; |*            |
  ;; |-------------|
  ;; |             |
  ;; |-------------|
  (terminator-add-template  1 2))


;; This will do all the magic we need. After each split
;; balance-windows should be applied so we can use all the available
;; space in the buffer
(defun terminator-open-template (template)
  (interactive "nTemplate Number: ")
  (if (nth template terminator-templates)
      (let ((width (terminator--get-template-width template))
            (height (terminator--get-template-height template))
            (hook (terminator--get-template-hook template))
            (term-number 0)
            (cterminal))
        (subdivide-frame width height)
        (dotimes (i height)
          (dotimes (j width)
            (setq term-number (+ 1 term-number))
            (setq cterminal (format "*terminal<%s>*" term-number))
            (if (get-buffer cterminal)
                (switch-to-buffer cterminal)
              (get-buffer-create (multi-term)))
            (subdivide-windmove 'right))
          (subdivide-windmove 'left width)
          (subdivide-windmove 'down))
        (subdivide-windmove 'left width)
        (subdivide-windmove 'up height)
        (when (functionp hook)
          (funcall hook)))
    (message (format "Template %s does not exist" template))))


(defun terminator-close-all (&optional sure)
  "Closes all opened terminals at once."
  (interactive
   (list
    (y-or-n-p "Are you sure?: ")))
  (let ((i 1))
    (delete-other-windows)
    (when sure
      (while (get-buffer (format "*terminal<%s>*" i))
        (kill-buffer (format "*terminal<%s>*" i))
        (setq i (+ 1 i)))
      (message "All terminals killed"))))


;; Inspired in this article:
;; http://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
(defmacro terminator-windmove (direction &optional times)
  "Macro to execute windmove commands safely. Will return nil
when is not possible to move to DIRECTION"
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval
                   (progn
                     (dotimes (i ,times)
                       (funcall
                        (intern-soft
                         (concat
                          "windmove-" (prin1-to-string ,direction)))))))
           ('error
            (setq retval nil)))
         (windowp retval))
     nil))


;;;###autoload
(define-minor-mode terminator-mode
  "Toggle Terminator mode.

Key bindings:
\\{terminator-mode-map}"
  nil
  " trmntr"
  :group 'terminator
  (when terminator-mode
    (when (not (assq 'terminator-mode minor-mode-map-alist))
      (push (cons 'terminator-mode mweb-mode-map) minor-mode-map-alist))))


(defun terminator-mode-on ()
  "Turns on the globalized major mode"
  (terminator-mode t))


(define-globalized-minor-mode terminator-global-mode
  terminator-mode terminator-mode-on
  :group 'terminator
  :require 'terminator)


(provide 'terminator)
;;; terminator.el ends here
