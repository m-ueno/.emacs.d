;;;;
;;;; M-x / keybind で呼び出す気の利いたコマンド群
;;;;

;; id:rubikitch
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-t") 'other-window-or-split)

;; defkey -- merge 'kbd' macro
;; Emacs LISP TB
;; 覚え方: 引数は "" と 'command
;; 覚えにくい...
;; global-set-key + kbd or [ ] の方がよさげ
(defmacro defkey (keymap key command)
  `(define-key ,keymap ,(read-kbd-macro key) ,command))
(defmacro gdefkey (key command)
  `(define-key global-map ,(read-kbd-macro key) ,command))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (Message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

;; C-x k を置き換える C-x / でもどす
;; http://d.hatena.ne.jp/kitokitoki/20100608/p2
;; 注意 recentf に登録されない
(defvar my-killed-file-name-list nil)
(defun my-push-killed-file-name-list ()
  (when (buffer-file-name)
    (push (expand-file-name (buffer-file-name)) my-killed-file-name-list)))
(defun my-pop-killed-file-name-list ()
  (interactive)
  (unless (null my-killed-file-name-list)
    (find-file (pop my-killed-file-name-list))))
(add-hook 'kill-buffer-hook 'my-push-killed-file-name-list)
(global-set-key "\C-xk" (lambda() (interactive)(kill-buffer (buffer-name))))
(global-set-key "\C-x\/" 'my-pop-killed-file-name-list)

;; recursive byte-compile
;; http://labs.unoh.net/2008/07/emacstips_1.html
;; cf. #'byte-recompile-directory
(defun my-byte-compile-directory ()
  (interactive)
  (defun byte-compile-directories (dir)
    (if (file-directory-p dir)
        (byte-compile-directory-r (mapcar (function (lambda (f) (concat dir "/" f)))
                                          (directory-files dir)))))
  (defun byte-compile-directory-r (file-list)
    (cond ((null (car file-list))
           nil)
          ((and (file-directory-p (car file-list))
                (not (string-match "/\.\.?$" (car file-list))))
           (byte-compile-directories (car file-list))
           (if (not (null (cdr file-list)))
               (progn
                 (byte-compile-directories (cadr file-list))
                 (byte-compile-directory-r (cdr file-list)))))
          ((string-match "\.el$" (car file-list))
           (progn
             (byte-compile-file (car file-list))
             (byte-compile-directory-r (cdr file-list))))
          (t
           (if (not (null (cdr file-list)))
               (byte-compile-directory-r (cdr file-list))))))
  (byte-compile-directories (replace-regexp-in-string "/$" "" default-directory)))

;; カーソル動かさずにスクロール
;; http://norainu.net/mt/archives/2006/11/emacs_elisp.html
                                        ;(unless (fboundp 'scroll-up-line)
(defun scroll-up-line (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))
(defun scroll-down-line (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))
(global-set-key "\M-p" 'scroll-up-line)
(global-set-key "\M-n" 'scroll-down-line)

;; for cl-memo
(fset 'memo
   [?\M-< S-f5 ?\C-j ?\C-q ?\C-i?* ?  ?\C-o ?\C-o])

(defvar my-save-buffer-hook nil)
;(defvar my-save-buffer-hook #'delete-trailing-whitespace)
(defun save-buffer-wrapper ()
  (interactive)
  (let ((tostr (concat "$Last update: " (format-time-string "%Y/%m/%d %k:%M:%S") " $")))
    (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "\\$Last update\\([0-9/: ]*\\)?\\$" nil t)
          (replace-match tostr nil t)))
    (delete-trailing-whitespace)
    (run-hooks 'my-save-buffer-hook)
    (save-buffer)))

(global-set-key "\C-x\C-s" 'save-buffer-wrapper)

;; my-explorer-open [C-x C-@]
;; http://www.jaist.ac.jp/~n-yoshi/tips/junk_elisp.html#explorer
(defun my-explorer-open ()
  (interactive)
  (shell-command "explorer /e,."))
(global-set-key "\C-x\C-@" 'my-explorer-open)

;; insert date/time [F5 / S-F5]
(defun my-get-date-gen (form) (insert (format-time-string form)))
(defun my-get-date () (interactive) (my-get-date-gen "%Y-%m-%d"))
(defun my-get-time () (interactive) (my-get-date-gen "%H:%M"))
(defun my-get-dtime () (interactive) (my-get-date-gen "%Y-%m-%d %H:%M"))
;;(global-set-key [f5] 'my-get-date)
(global-set-key (kbd "S-<f5>") 'my-get-dtime)

;; *scratch*を消さない
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))


;; 分割した画面を入れ替える [f2]
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; yank 時に次の yank 候補を minibuffer に表示
(defun my-yank-display ()
  (unless (or (eq kill-ring-yank-pointer nil) ;; kill-ringが空だったり
              (= (aref (buffer-name) 0) ? )) ;; minibuf で yank しようとしていなければ
    (if (eq (cdr kill-ring-yank-pointer) nil)
        (message "end of kill-ring")
      (message (car (cdr kill-ring-yank-pointer))))))

(defun my-yank (arg)
  "Yank with displaying next"
  (interactive "*P")
  (yank arg)
  (my-yank-display)
  (setq this-command 'yank))

(defun my-yank-pop (arg)
  "Yank-pop with displaying next"
  (interactive "*p")
  (yank-pop arg)
  (my-yank-display)
  (setq this-command 'yank))

(global-set-key "\C-y" 'my-yank)
(global-set-key "\M-y" 'my-yank-pop)



;; 2011-09-05
;; git checkout/merge/pull用
;; auto-revert-mode, global-auto-revert-modeでもいいが、
;; こっちはundoが効くので安心
(defun reopen-file ()
  (interactive)
  (let ((file-name (buffer-file-name))
        (old-supersession-threat
         (symbol-function 'ask-user-about-supersession-threat))
        (point (point)))
    (when file-name
      (fset 'ask-user-about-supersession-threat (lambda (fn)))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file file-name)
            (set-visited-file-modtime)
            (goto-char point))
        (fset 'ask-user-about-supersession-threat
              old-supersession-threat)))))
(global-set-key (kbd "C-x C-r") 'reopen-file)
