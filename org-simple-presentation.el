;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defvar org-simple-presentation-mode nil)
(defvar org-simple-presentation-mode-map nil)

(if (not (assq 'org-simple-presentation-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(org-simple-presentation-mode "Org Simple Presentation")
                minor-mode-alist)))

(defun org-simple-presentation-define-mode-map ()
  "キーマップ `skk-j-mode-map' を定義する。"
  (unless (keymapp org-simple-presentation-mode-map)
    (setq org-simple-presentation-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'org-simple-presentation-mode org-simple-presentation-mode-map)
                minor-mode-map-alist))))

;; (unless (assq 'org-simple-presentation-mode minor-mode-map-alist)
;;   (setq minor-mode-map-alist
;;         (cons (cons 'org-simple-presentation-mode org-simple-presentation-mode-map)
;;               (minor-mode-map-alist))))



(defun org-simple-presentation-mode (&optional arg)
  "Org Simple Presentation minor-mode"
  (interactive)
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq org-simple-presentation-mode nil))
   (arg
    (setq org-simple-presentation-mode t))
   (t
    (setq org-simple-presentation-mode (not org-simple-presentation-mode))))
  (if org-simple-presentation-mode
      (org-simple-presentation-define-mode-map)
    nil))


(defun org-simple-presentation-start ()
  (interactive)
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
    (spec '())
	beg end level heading ibuf)
    (goto-char (point-min))
    (setq beg (point))
    (save-excursion
      (org-end-of-subtree t t)
      (if (org-on-heading-p) (backward-char 1))
      (setq end (point)))
      (add-to-invisibility-spec 'spec)
      (overlay-put (make-overlay (point-min) beg) 'invisible 'spec)
      (overlay-put (make-overlay (+ 1 end) (point-max)) 'invisible 'spec)))

(defun org-simple-presentation-stop ()
  (interactive)
  (let ((ol (overlays-in (point-min) (point-max))))
    (mapcar '(lambda (x)
               (if (overlay-get x 'invisible)
                   (delete-overlay x))) ol)))

(defun org-simple-presentation-goto-next ()
  (interactive)
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
    (spec '())
	beg end level heading ibuf)
    (org-end-of-subtree t t)
    (setq beg (point))
    (backward-char -1)
    (org-end-of-subtree t t)
    (if (org-on-heading-p) (backward-char 1))
    (setq end (point))
    (org-presentation-stop)
    (overlay-put (make-overlay (point-min) beg) 'invisible 'spec)
    (overlay-put (make-overlay (+ 1 end) (point-max)) 'invisible 'spec)))

(defun org-simple-presentation-goto-prebious ()
  (interactive)
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
    (spec '())
	beg end level heading ibuf)
    (if (org-on-heading-p) (backward-char 1))
    (org-back-to-heading t)
    (setq beg (point))
    (save-excursion
      (org-end-of-subtree t t)
      (if (org-on-heading-p) (backward-char 1))
      (setq end (point)))
    (org-presentation-stop)
    (overlay-put (make-overlay (point-min) beg) 'invisible 'spec)
    (overlay-put (make-overlay (+ 1 end) (point-max)) 'invisible 'spec)))

(global-set-key (kbd "M-n") 'org-presentation-goto-next)
(global-set-key (kbd "M-p") 'org-presentation-goto-prebious)
(define-key org-simple-presentation-mode-map "C-cn" 'org-simple-presentation-goto-next)
(define-key org-simple-presentation-mode-map "C-cp" 'org-simple-presentation-goto-previous)

(provide 'org-simple-presentation-mode)