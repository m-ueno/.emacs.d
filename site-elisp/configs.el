;; custom
(setq custom-file (expand-file-name "~/.emacs.d/site-elisp/custom.el"))

;; whoami
(setq user-full-name "m-ueno")
(setq user-mail-address "m-ueno@github")

(setq global-mode-string nil)
(setq frame-title-format
      '("-%z:%*%+  %b  "
        mode-line-modes
        which-func-format
        "--"
        invocation-name
        "@"
        system-name))

(which-func-mode)

(blink-cursor-mode -1)

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(mouse-wheel-mode 1)

(xterm-mouse-mode 1)

(fringe-mode 0)

;; (display-time)

(line-number-mode t)

(column-number-mode t)

(set-variable 'truncate-partial-width-windows nil)

(auto-image-file-mode)

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

;; transient-mark
(setq transient-mark-mode t)

(setq search-highlight t)
(setq query-replace-highlight t)
;;(setq isearch-lazy-highlight-initial-delay 0) ; obsolate
(setq lazy-highlight-initial-delay 0)

(setq backup-by-copying t)

(setq gc-cons-threshold 1000000)

(setq inhibit-startup-message t)

(setq visible-bell nil)

(setq font-lock-maximum-size nil)

;; ;; fast-lock
;; (setq font-lock-support-mode 'fast-lock-mode)
;; (setq fast-lock-cache-directories '("~/.emacs.d/emacs-flc"))

(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")

(setq require-final-newline t)

(setq temporary-file-directory "~/.emacs.d/tmp")
;; (setq temporary-file-directory "/dev/shm")

(setq scroll-step 1)
(setq scroll-conservatively 1)

(setq next-line-add-newlines nil)

(setq fill-column 79)
(add-hook 'c-mode-common-hook (lambda () (auto-fill-mode 1)))

(setq message-log-max 200)

(auto-compression-mode t)

(setq apropos-do-all t)

;;(delete-selection-mode t)
;; or...
;;(pc-selection-mode t)

;; abbrev
; (read-abbrev-file "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)

;; version control
;; (setq vc-follow-symlinks t)
;; (setq vc-suppress-confirm t)
;; (setq vc-command-messages t)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq cursor-in-non-selected-windows nil)

(setq-default indicate-empty-lines t)

;; (setq-default line-spacing 0)

(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8-mac)
; (set-clipboard-coding-system 'utf-8-unix)
(setq default-process-coding-system '(undecided . utf-8-unix))

(dolist (h '(emacs-lisp-mode-hook
	     c-mode-common-hook
	     ;ruby-mode-hook ;; ruby-mode has ruby-electirc
	     perl-mode-hook
	     php-mode-hook
	     haskell-mode-hook
	     coffee-mode-hook
;	     csharp-mode-hook
	     go-mode-hook))
  (add-hook h
	    (lambda ()
	      (electric-pair-mode t)
	      (electric-layout-mode t))))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "Stroustrup")
	    (c-toggle-hungry-state 1)
	    (setq indent-tabs-mode nil)
	    (setq c-basic-offset 4)
	    (add-to-list 'electric-layout-rules '(?{ . after))))

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-j" 'newline)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-z" 'scroll-down)
(global-set-key "\M-V" 'scroll-other-window-down)

;; リージョン選択の解除（デフォルトでM-ESC ESC | ESC ESC ESC）
;; http://dev.ariel-networks.com/articles/emacs/part4/
(global-set-key (kbd "C-M-g") 'keyboard-escape-quit)

;; clipboard
(global-set-key (kbd "S-<insert>") 'clipboard-yank)
(global-set-key (kbd "C-c r") 'query-replace)


;; > Emacs 24
;; monokai-like
(load-theme 'wombat)

; (setq my-font-family "Ricty Diminished")
; (setq my-font-family "源ノ角ゴシック Code JP R")
(setq my-font-family "Source Code Pro")
;; tips: scaleup with C-x C-+
(if window-system
    (progn
;      (set-face-attribute 'default nil :family "源ノ角ゴシック Code JP R" :height 130)
      (set-face-attribute 'default nil :family my-font-family :height 140)
      (set-fontset-font
       (frame-parameter nil 'font)
       'japanese-jisx0208
       (font-spec :family my-font-family))

;     (set-fontset-font "fontset-default" 'japanese-jisx0208 '("源ノ角ゴシック Code JP R" . "iso10646-*"))
      ;; (setq default-frame-alist
      ;;       (append
      ;;        (list '(foreground-color . "black")
      ;;              '(background-color . "alice blue")
      ;;              '(border-color . "azure")
      ;;              '(cursor-color . "tomato3")
      ;;              '(mouse-color . "orange")
      ;;              '(vertical-scroll-bars . nil)
      ;;              ;; '(alpha . 65)
      ;;              )
      ;;        default-frame-alist))
))

;;; conf-tips.el --- default function (short configure, just setq)

;; id:tomoya 20091015
;; [indent multi-line]
(setq comment-style 'multi-line)

;; http://d.hatena.ne.jp/khiker/20100114
;; 左フリンジの上下にマークをつける
(setq-default indicate-buffer-boundaries 'left)
;; 右フリンジの上下にマークをつける
(setq-default indicate-buffer-boundaries 'right)

;; 左フリンジの上と右フリンジの下にマークをつける
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
;; 右フリンジの上と左フリンジの下にマークをつける
(setq-default indicate-buffer-boundaries '((top . right) (t . left)))
;; 右フリンジの上にのみマークをつける
(setq-default indicate-buffer-boundaries '((top . right) (t . nil)))

;; C-UP/C-DOWN でペインをリサイズ
;; http://d.hatena.ne.jp/khiker/20100118
(global-set-key [(ctrl up)] '(lambda (arg) (interactive "p")
                               (shrink-window arg)))
(global-set-key [(ctrl down)] '(lambda (arg) (interactive "p")
                                 (shrink-window (- arg))))

;; mode line
(display-time)
(global-hl-line-mode t)
;; (set-face-background 'hl-line "Tomato3")
(setq hl-line-face 'underline)
(display-battery-mode t)

;; URLをC-x C-fで開く
(ffap-bindings)

;; 最近使ったファイル
(recentf-mode)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 100)
;; Trampのリモートファイルを除く
;; http://homepage.mac.com/zenitani/elisp-j.html
(setq recentf-exclude '("^/[^/:]+:"))

;; Emacs23には最初から入っている
(global-linum-mode t)

;; show-paren-mode
(show-paren-mode t)

;; diredで
(setq ls-lisp-dirs-first t)

;; 前回編集していた場所を記憶させるには ― saveplace
(load "saveplace")
(setq-default save-place t)

;; コンパイルウインドウの高さを制限
(setq compilation-window-height 15)

;; ファイルを開くとき
;; http://www.bookshelf.jp/soft/meadow_23.html
(if (<= emacs-major-version 23)
    (partial-completion-mode t))

; S-Arrowでウインドウ移動
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; http://infolab.stanford.edu/~manku/dotemacs.html
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; window 分割時、画面外に出る文章を折り返したい
(setq truncate-partial-width-windows nil)

;; kill-ringに重複を許さない
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;;Emacs の動作を軽快にしたい
(setq gc-cons-threshold 5242880)

;; 1 行ずつスクロールさせる
;; http://www.bookshelf.jp/soft/meadow_31.html#SEC428
(setq scroll-conservatively 35
       scroll-margin 0
       scroll-step 1)
(setq comint-scroll-show-maximum-output t)

;; kill-lineで行末の改行文字も削除
(setq kill-whole-line t)

;; 開始時にホームディレクトリに移動
(cd (expand-file-name "~"))

;; 起動時の画面はいらない
(setq inhibit-startup-message t
      startup-echo-area-message ""
      initial-scratch-message "")

(fset 'yes-or-no-p 'y-or-n-p)

;; ヴィジブルベルを抑制
(setq visible-bell nil)

;; ビープ音を抑制
(setq ring-bell-function '(lambda ()))

;; 自動セーブの設定
; (setq auto-save-default nil)

;; バックアップの設定
; (setq make-backup-files nil)

;; 自動保存したファイルを削除するか
(setq delete-auto-save-files t)

;; auto-insert template
(setq auto-insert-directory "~/.emacs.d/template/")
(auto-insert-mode 1)
;; テンプレート挿入時に尋ねない。デフォルトは 'function
(setq auto-insert-query nil)
;; 指定したファイルを挿入する
(setq auto-insert-alist
      (append
       '(
         ;; モード名で指定
;         (yahtml-mode . "test.html")
         ;; ファイル名で指定
         ("\\.c$" . "template.c")
         ("\\.pl$" . "template.pl")
         ("\\.pm$" . "template.pl")
         ("\\.py$" . "template.py")
         ("\\.html$" . "template.html")
         ("\\.tex$" . "template.tex")
         )
       auto-insert-alist))

;; elisp括弧を薄くする
(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey20"))
    (((class color) (background light))
     (:foreground "grey80")))
  "Face used to dim parentheses.")
(add-hook 'emacs-lisp-mode-hook
 	  (lambda ()
 	    (font-lock-add-keywords nil
 				    '(("(\\|)" . 'paren-face)))))

;; ----------------------------------------------------------------
;; key binding
;; ----------------------------------------------------------------
;; http://www.bookshelf.jp/soft/meadow_12.html
;; 基本は (define-key map-name kbd function)
;; global-set-key はこれの global-map 版のマクロ
;; map-name には c-mode-map, lisp-mode-map などが入る
;; 普通はhook時にやるので、
;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (define-key c-mode-map "\C-c>" 'indent-region)))
;; とするか、あるいは c-mode-map を書かずに
;; (add-hook 'c-mode-hook
;;           '(lambda ()
;; 	     (local-set-key "\C-c>" 'indent-region)))
;; とする
;; #'global-unset-key もある
