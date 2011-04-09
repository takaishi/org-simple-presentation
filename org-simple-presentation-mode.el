
(defvar org-simple-presentation-mode nil)
(defvar org-simple-presentation-mode-map nil)

(if (not (assq 'org-simple-presentation-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(org-simple-presentation-mode "Org Simple Presentation")
                minor-mode-alist)))

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

(defun org-simple-presentation-define-mode-map ()
  "キーマップ `org-simple-presentation-define-mode-map' を定義する。"
  (unless (keymapp org-simple-presentation-mode-map)
    (setq org-simple-presentation-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'org-simple-presentation-mode org-simple-presentation-mode-map)
                minor-mode-map-alist))))


(defun org-simple-presentation-invisible-without-region (beg end)
  "begとend以外の領域を不可視にする"
  (org-simple-presentation-visible-buffer)
  (overlay-put (make-overlay (point-min) beg) 'invisible 'spec)
  (overlay-put (make-overlay (+ 1 end) (point-max)) 'invisible 'spec))

(defun org-simple-presentation-visible-buffer ()
  "バッファの全てを可視にする"
  (let ((ol (overlays-in (point-min) (point-max))))
    (mapcar '(lambda (x)
               (if (overlay-get x 'invisible)
                   (delete-overlay x))) ol)))

(defun org-simple-presentation-start ()
  "バッファ中の最初のヘッダに移動して，プレゼンテーションを開始する．"
  (interactive)
  (let ((cbuf (current-buffer))
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

(defun org-simple-presentation-end ()
  "プレゼンテーションを終了する"
  (interactive)
  (org-simple-presentation-visible-buffer))

(defun org-simple-presentation-goto-next ()
  "次のヘッダに切り替える"
  (interactive)
  (let ((cbuf (current-buffer))
	(pos (point))
    (spec '())
	beg end level heading ibuf)
    (org-end-of-subtree t t)
    (setq beg (point))
    (backward-char -1)
    (org-end-of-subtree t t)
    (if (org-on-heading-p) (backward-char 1))
    (setq end (point))
    (org-back-to-heading t)
    (org-simple-presentation-invisible-without-region beg end)))

  
(defun org-simple-presentation-goto-previous ()
  "前のヘッダに切り替える"
  (interactive)
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
    (spec '())
	beg end level heading ibuf)
    (if (org-on-heading-p) (backward-char 1))
    (org-back-to-heading t)
    (org-back-to-heading t)
    (setq beg (point))
    (save-excursion
      (org-end-of-subtree t t)
      (if (org-on-heading-p) (backward-char 1))
      (setq end (point)))
    (org-simple-presentation-invisible-without-region beg end)))

(define-key org-simple-presentation-mode-map "\C-cs" 'org-simple-presentation-start)
(define-key org-simple-presentation-mode-map "\C-ce" 'org-simple-presentation-end)
(define-key org-simple-presentation-mode-map "\C-cn" 'org-simple-presentation-goto-next)
(define-key org-simple-presentation-mode-map "\C-cp" 'org-simple-presentation-goto-previous)

(provide 'org-simple-presentation-mode)