;;; faster-whichkey.el --- Makes startup with whichkey faster -*- lexical-binding: t; -*-

;; Copyright (C) 2025 john
;;
;; Author: john <https://github.com/jgrey4296>
;; Created: October 29, 2025
;; Modified: October 29, 2025
;; Version: 1.0.0
;; Keywords:
;; Homepage: https://github.com/jgrey4296/faster-whichkey
;; Package-Requires: ((emacs "30.2") (general) (which-key))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:
(require 'general)
(require 'which-key)

(defvar faster-whichkey-active nil)

(defvar faster-whichkey--current-bindings nil)

(defvar faster-whichkey-ignores-fns '(digit-argument))

(defun faster-whichkey-toggle ()
  " Toggle whether faster-whichkey is activated or not"
  (interactive)
  (cond (faster-whichkey-active
         (advice-remove 'general-extended-def-:which-key  #'faster-whichkey-general-head-ad)
         (advice-remove 'which-key--get-keymap-bindings   #'faster-whichkey--get-keymap-bindings-ad)
         (advice-remove 'which-key--get-keymap-bindings-1 #'faster-whichkey--get-keymap-bindings-1-ad)
         (advice-remove 'which-key--compute-binding       #'faster-whichkey--compute-binding-ad)
         (advice-remove 'which-key--evil-operator-filter  #'faster-whichkey--evil-operator-filter-ad)
         (setq faster-whichkey-active nil)
         )
        (t
         (advice-add 'general-extended-def-:which-key  :override #'faster-whichkey-general-head-ad)
         (advice-add 'which-key--get-keymap-bindings   :around   #'faster-whichkey--get-keymap-bindings-ad)
         (advice-add 'which-key--get-keymap-bindings-1 :override #'faster-whichkey--get-keymap-bindings-1-ad)
         (advice-add 'which-key--compute-binding       :override #'faster-whichkey--compute-binding-ad)
         (advice-add 'which-key--evil-operator-filter  :override #'faster-whichkey--evil-operator-filter-ad)
         (setq faster-whichkey-active t)
         )
        )
)

(defun faster-whichkey-general-head-ad (_state keymap key edef kargs)
  " An alternative which-key implementation for General.
  Use as overriding advice on general-extended-def-:which-key

Add a which-key description for KEY.
If :major-modes is specified in EDEF, add the description for the corresponding
major mode. KEY should not be in the kbd format (kbd should have already been
run on it)."
    (let* ((wk (general--getf2 edef :which-key :wk))
           (keymaps (plist-get kargs :keymaps))
           (key (key-description key))
           (prefix (plist-get kargs :prefix))
           (binding (or (when (and (plist-get edef :def)
                                   (not (plist-get edef :keymp)))
                          (plist-get edef :def))
                        (when (and prefix
                                   (string= key prefix))
                          (plist-get kargs :prefix-command))))
           (replacement (cond ((consp wk) (cdr wk))
                              (t wk)))
           )
      (condition-case-unless-debug err
          (mapc #'(lambda (keymap-sym)
                    (cond ((and (boundp keymap-sym)
                                (keymapp (symbol-value keymap-sym)))
                           (faster-whichkey-add-evil-keymap-replacement _state (symbol-value keymap-sym)
                                                                          key `(,replacement . ,binding)))
                          ((and (boundp (intern (format "%s-map" keymap-sym)))
                                (keymapp (symbol-value (intern (format "%s-map" keymap-sym)))))
                           (faster-whichkey-add-evil-keymap-replacement _state (symbol-value (intern (format "%s-map" keymap-sym)))
                                                                          key `(,replacement . ,binding))
                           )
                          ))
                keymaps
                )
         (error (message "Binding Update Error for: (%s : %s : %s : %s) : %s" keymap key binding replacement err))
         )
    )
  )

(defun faster-whichkey--get-keymap-bindings-ad (fn keymap &optional start &rest args)
  " :around Advice for which-key--get-keymap-bindings.
Initializes 'faster-whichkey--current-bindings to 'start',
and filters resulting bindings that are nil or empty afterwards
  "
  (setq faster-whichkey--current-bindings start)
  (apply fn keymap start args)
  (-filter #'(lambda (x)
               (and (cdr-safe x)
                    (not (string-equal "" (cdr-safe x)))))
           faster-whichkey--current-bindings)
  )

(defun faster-whichkey--get-keymap-bindings-1-ad (keymap start &optional prefix filter all ignore-commands)
  " :override advice for 'which-key--get-keymap-bindings-1'
    Gets bindings from a keymap, preferring faster-whichkey's pseudo-maps over the raw keymap
"
  (let ((prefix-map (if prefix (lookup-key keymap prefix) keymap)))
    ;; Prefer which-key pseudo-maps:
    (when (and (keymapp prefix-map) (keymapp (lookup-key prefix-map [which-key])))
      (which-key--get-keymap-bindings-1 (lookup-key prefix-map [which-key]) nil nil filter all ignore-commands))

    (when (keymapp prefix-map)
        (map-keymap (-partial #'faster-whichkey--handle-binding prefix filter all ignore-commands) prefix-map))
    faster-whichkey--current-bindings
    )
  )

(defun faster-whichkey--compute-binding-ad (binding)
  "Replace BINDING with remapped binding if it exists.

Requires `which-key-compute-remaps' to be non-nil"
  (let (remap)
    (cond ((and which-key-compute-remaps (setq remap (command-remapping binding)))
           (copy-sequence (symbol-name remap)))
          (t
           (copy-sequence (symbol-name binding))))))

(defun faster-whichkey--evil-operator-filter-ad (binding)
  (let ((def (cdr binding)))
    (and (functionp def)
         (not (evil-get-command-property def :suppress-operator))))
  )

(defun faster-whichkey--handle-binding (prefix filter all ignore-commands ev def)
  " main discriminator to add bindings to faster-whichkey--current-bindings
adds binding text into faster-whichkey--current-bindings instead of returning a value
 "
  (let* ((key (vconcat prefix (list ev)))
         (key-desc (key-description key)))
    (cond
     ;; ignore raw binding that have already been set
     ((assoc (key-description (list ev)) faster-whichkey--current-bindings))
     ;; ignore bindings that have already been set
     ((assoc key-desc faster-whichkey--current-bindings))
     ;; add empty entry for ignored commands
     ((and (listp ignore-commands) (symbolp def) (memq def ignore-commands))
      (push (cons key-desc "") faster-whichkey--current-bindings)
      )
     ((and (symbolp def) (memq def faster-whichkey-ignores-fns))
      (push (cons key-desc "") faster-whichkey--current-bindings)
      )
     ;; ignoring extra stuff
     ((or (string-match-p which-key--ignore-non-evil-keys-regexp key-desc) (eq ev 'menu-bar))
      nil )
     ;; ignoring evil states
     ((and (keymapp def) (string-match-p which-key--evil-keys-regexp key-desc))
      nil)
     ;; event 27 is escape, so this will pick up meta
     ((and (keymapp def) (or all (and (numberp ev) (= ev 27))))
      (which-key--get-keymap-bindings-1 def nil key filter all ignore-commands))
     ;; ignore menu items (which-key--get-menu-item-binding def)
     ((eq 'menu-item (car-safe def)) nil)
     ;; An actual definition
     (def
      (let ((binding (cons key-desc (faster-whichkey--handle-def def))))
        (when (and binding
                   (or (null filter)
                       (and (functionp filter)
                            (funcall filter (cons key-desc def)))))
          (push binding faster-whichkey--current-bindings))
        )
      )
     )
    )
  )

(defun faster-whichkey--handle-def (def)
  " handler for actual binding definitions to convert to text
returns a string"
  (cond
   ;; ignore which-keys that are submaps without names
   ((and (eq (car-safe def) 'which-key) (keymapp (cdr-safe def)))
    nil)
   ;; ++submap name
   ((and (eq (car-safe def) 'which-key) (not (caddr def)))
    (concat "++" (cadr def)))
   ;; described binding
   ((eq (car-safe def) 'which-key)
    (cadr def))
   ;; remapped binding
   ((symbolp def)
    (which-key--compute-binding def))
   ;; unnamed submap
   ((keymapp def) "prefix")
   ;; unnamed lambda
   ((eq 'lambda (car-safe def)) "+lambda")
   ;; unnamed closure
   ((eq 'closure (car-safe def)) "+closure")
   ((stringp def) def)
   ((vectorp def) (key-description def))
   ;; looking for (STRING . DEFN)
   ((and (consp def) (stringp (car def)))
    (concat (when (keymapp (cdr-safe def)) "group:")
            (car def)))
   (t "unknown"))
  )

(defun faster-whichkey-add-keymap-replacement (state keymap key replacement &rest more)
  " Alt implementation of which-key-add-keymap-based-replacements
that uses evil-define-key, allowing state bindings

Mainly this is useful for a keymap-based-replacement implementation
of general-extended-def-:which-key
"
  (cl-assert (keymapp keymap))
  (while key
    (let* ((string (if (stringp replacement)
                       replacement
                     (car-safe replacement)))
           (command (cdr-safe replacement))
           (pseudo-key (faster-whichkey--pseudo-key (kbd key)))
           (bind (faster-whichkey--build-pseudo-binding string command))
           )
      ;;(message "adding replacement: %s : %s" pseudo-key bind)
      (if state
          (evil-define-key* state keymap pseudo-key bind)
        (define-key keymap pseudo-key bind)
        ))
    (setq key (pop more)
          replacement (pop more)))
  )

(defun faster-whichkey--pseudo-key (key &optional prefix)
  " create a pseudo-keystring to target which-key information
ie: [SPC d f] -> [SPC d whichkey f]
 "
  (let ((seq (listify-key-sequence key)))
    (vconcat (or prefix (butlast seq)) [which-key] (last seq))))

(defun faster-whichkey--build-pseudo-binding (desc bind)
  "create a pseudo binding to hold a which-key description
literally just a list with the `which-key` symbol at the head.
"
  (list 'which-key desc bind)
  )

(defun faster-whichkey-add-evil-keymap-replacement (state keymap key replacement &rest more)
  " Alt implementation of faster-whichkey-add-keymap-based-replacements
that uses evil-define-key, allowing state bindings

Mainly this is useful for a keymap-based-replacement implementation
of general-extended-def-:which-key
"
  (if (not (keymapp keymap))
      (error "Symbol is not a keymap" keymap))
  (while key
    (let* ((string (if (stringp replacement)
                       replacement
                     (car-safe replacement)))
           (command (cdr-safe replacement))
           (pseudo-key (faster-whichkey--pseudo-key (kbd key)))
           (bind `(which-key ,string ,command))
           )
      (if state
          (evil-define-key* state keymap pseudo-key bind)
        (define-key keymap pseudo-key bind)
        ))
    (setq key (pop more)
          replacement (pop more)))
  )

(defalias 'faster-whichkey-add-description-to-keymap #'faster-whichkey-add-keymap-replacement)

(provide 'faster-whichkey)
;;; faster-whichkey.el ends here
