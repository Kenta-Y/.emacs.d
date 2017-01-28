;;=========================================================================
;;
;;	PATH関連
;;
;;=========================================================================

;;
;; load-path
;; Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;-------------------------------------------------------

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elpa")

;;
;; emacsの拡張リポジトリ関連
;;-------------------------------------------------------

;;;emacsリポジトリ追加
(require 'package)

;; 初期化
(package-initialize)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;;=========================================================================
;;
;;	el関連
;;
;;=========================================================================


;;
;; tabbar
;; (install-elisp "http://www.emacswiki.org/emacs/download/tabbar.el")
;; -------------------------------------------------------

(require 'tabbar)
(tabbar-mode 1)

;; Tab(bufferを簡単に切り替えれるようにしたもの)
(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)
(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)

;; タブ上でマウスホイールを使わない
(tabbar-mwheel-mode nil)

;; グループを使わない
(setq tabbar-buffer-groups-function nil)

;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

;; 外観変更
(setq tabbar-separator '(1.3)) ; タブの長さ

 ;(set-face-attribute ; 幅設定
 ;  'tabbar-separator nil
 ;   :height 1.1)

 (set-face-attribute ; バー自体の色
   'tabbar-default nil
    :background "LightSlateBlue"
    ;:family "Monaco"
	:height 1.1
	)
 (set-face-attribute ; アクティブなタブ
   'tabbar-selected nil
    :background "LightSlateBlue"
	:foreground "yellow"
    :weight 'bold
	;:box '(:line-width 1 :color "white" :style pressed-button)
	:box nil
	 )
 (set-face-attribute ; 非アクティブなタブ
   'tabbar-unselected nil
    :background "LightSlateBlue"
    :foreground "midnight blue"
	:weight 'bold
	;:box '(:line-width 1 :color "white" :style released-button)
    :box nil
	)
 (set-face-attribute ; 変更のあったファイル
   'tabbar-modified nil
  	:background "LightslateBlue"
  	:foreground "green"
  	:weight 'bold
  	;:box '(:line-width 1 :color "white" :style released-button)
  	:box nil
  )

;; タブに表示させるバッファの設定
(defun my-tabbar-buffer-list ()
  (delq nil
    (mapcar #'
	  (lambda (b)(cond
        ;; Always include the current buffer.
        ((eq (current-buffer) b) b)
        ((buffer-file-name b) b)
        ((char-equal ?\  (aref (buffer-name b) 0)) nil)
		((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
		((equal "*ansi-term*" (buffer-name b)) b) ; *ansi-term*バッファは表示する
		((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
        ((buffer-live-p b) b)))
(buffer-list))))

(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;(require 'tabbar-ruler)


;;=========================================================================
;;
;;	基本設定
;;
;;=========================================================================

;;対応する括弧をハイライト
(show-paren-mode t)

;;行番号の表示
(global-linum-mode t)

;;tabの文字数の設定
(setq-default tab-width 4	;;タブ幅
			  c-basic-offset 4	;;基本インデント量
			  indent-tabs-mode t)	;;インデントをタブ(t)/スペース(nil)で行う

;;tabキーでtab文字を入力できるように変更
;;元々のtabのキーバインドは260行目付近，"元々こちら"で検索！
(define-key global-map "\C-i" '(lambda	()
	(interactive)
	(insert "\t")))

;; .xxx~(バックアップファイル)の生成場所の指定
(setq backup-directory-alist
	  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
			backup-directory-alist))

;; .xxx#(自動セーブ機能)の生成場所の指定
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/autosave/") t)))

;;起動時の画面を消す
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;ゆっくりマウススクロールする
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse(setq scroll-conservatively 1)

;; Monaco 12pt をデフォルトにする
(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 100)
;; 日本語をヒラギノ角ゴProNにする
;(set-fontset-font "fontset-default"
;                  'japanese-jisx0208
;                  '("Hiragino Maru Gothic ProN"))
;; 半角カナをヒラギノ角ゴProNにする
;(set-fontset-font "fontset-default"
;                  'katakana-jisx0201
;                  '("Hiragino Maru Gothic ProN"))

(setq dviprint-command-format "dvipdfmx %s")

;;ステータスバーの色変更
(set-face-background 'mode-line "LightSlateBlue")

;;nyan-cat
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

;;; 追加ここまで

;;=========================================================================
;;
;;	Red Hat Linux default .emacs initialization file
;;
;;=========================================================================


;(set-default-coding-systems 'japanese-shift-jis)
(set-default-coding-systems 'utf-8)

;; Carbon Emacsの設定で入れられた. メニューを隠したり．
(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 )

;;Color
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 90)
   ))

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
       (global-font-lock-mode t)
))

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Enable wheelmouse support by default
(cond (window-system
       (mwheel-install)
))

(and (eq window-system 'mac)
     (functionp #'mac-toggle-max-window)
     (add-hook 'window-setup-hook
	       (lambda ()
		 (mac-toggle-max-window)
		 (toggle-tool-bar nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; マクロサーチパスの追加
;;; ~/lib/emacs 以下にユーザ用の *.el, *.elc を置くことができます
;;(setq load-path (append '("~/lib/emacs") load-path))

;;; C-h と Del の入れ替え
;;; Help が Shift + Ctrl + h および Del に割当てられ、
;;; 前一文字削除が Ctrl + h に割当てられます
;; (load-library "term/keyswap")
;; (if (eq window-system 'x)
;;    (progn
;;       (define-key function-key-map [delete] [8])
;;       (put 'delete 'ascii-character 8)))
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)

;;; ステータスラインに時間を表示する
;;(display-time)

;;; mule/emacs -nw で起動した時にメニューバーを消す
;;(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;;; スクロールバーを右側に表示する
;;(set-scroll-bar-mode 'right)

;;; rpm-mode の読み込み
;;; rpm-mode.el は spec ファイルの作成に便利です。
;;;   ~/lib/emacs に /usr/doc/rpm/rpm-mode.el をコピーして以下の設定を
;;; 行ってください。

;(setq auto-mode-alist (nconc '(("\\.spec" . rpm-mode)) auto-mode-alist))
;(autoload 'rpm-mode "rpm-mode" "Major mode for editing SPEC file of RPM." t) 
;(setq packager "Vine User <vine@hoge.fuga>");自分の名前
;      (setq buildrootroot "/tmp");BuildRootの場所
;      (setq projectoname "Project Vine");プロジェクト名 

;;;印刷設定
;(setq-default lpr-switches '("-Pepson"))
(setq-default lpr-switches '("-2P"))
(setq-default lpr-command "mpage")

;; ps-print
(setq ps-multibyte-buffer 'non-latin-printer)

;;;バッファの最後でnewlineで新規行を追加するのを禁止する

(setq next-line-add-newlines nil)


;;; 最終更新日の自動挿入
;;;   ファイルの先頭から 8 行以内に Time-stamp: <> または
;;;   Time-stamp: " " と書いてあれば、セーブ時に自動的に日付が挿入されます
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks
          (cons 'time-stamp write-file-hooks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; このファイルに間違いがあった場合に全てを無効にします
(put 'eval-expression 'disabled nil)


;;; c-q でウインドウ移動
(global-set-key "\C-q" 'other-window)

;;; M-q でフレーム移動
(global-set-key "\M-q" 'other-frame)

;;; c-x c-e でコンパイル
(global-set-key "\C-x\C-e" 'compile)

;;; c-x c-i でgoto-line
(global-set-key "\C-x\C-i" 'goto-line)

;;; c-^ でウインドウ拡大
(global-set-key "\C-^" 'enlarge-window)

;;; c-- でウインドウ縮小
;;;(global-set-key "\C-]" 'shrink-window)

;;; c-i でfill-paragraph
;;(define-key ctl-x-map "p" 'fill-paragraph)
;;(define-key global-map "\C-i" 'fill-paragraph)	//元々こちら

;;;補完
(define-key global-map "\C-j" 'dabbrev-expand)
(define-key global-map "\C-o" 'dabbrev-expand)

;;; 改良 kill-line
(setq kill-whole-line t)

;;; コンパイルウインドウの大きさを制限する
(setq compilation-window-height 12)      ;; 高さ
;; scroll mouse
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

;; デフォルト翻訳サイトの設定  
(setq text-translator-default-engine "excite.co.jp_enja")

;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)
;; グローバルキーを設定
(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; このファイルに間違いがあった場合に全てを無効にします
(put 'eval-expression 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
