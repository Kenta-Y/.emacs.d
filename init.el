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

;;
;; PATH
;; UpTeX関連含
;;-------------------------------------------------------

;(setenv "PATH"
;		(concat (getenv "PATH") ":/Applications/UpTeX.app/Contents/Resources/texbin:/usr/local/Cellar"))

(setenv "PATH"
		(concat (getenv "PATH") ":/usr/local/Cellar
								 :/usr/local/lib
								 :/Applications/UpTeX.app/Contents/Resources/texbin
								 :/usr/local/Cellar/ghostscript/9.16/bin
								 :/usr/local/Cellar/ghostscript/9.16/lib"))

(setenv "TMPDIR"
		(concat (getenv "TMPDIR") ":/private/tmp"))

(setenv "BIBINPUTS"
		(concat (getenv "BIBINPUTS") ":/Users/KentaYamagishi/Documents/tex/bibtex"))

;(shellenv/setenv "PATH" 'bash)



;;
;;	el関連
;;
;;=========================================================================

;;
;; tabbar
;; (install-elisp "http://www.emacswiki.org/emacs/download/tabbar.el")
;; -------------------------------------------------------
(require 'tabbar)
;(tabbar-mode)
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

 (set-face-attribute ; バー自体の色
    'tabbar-default nil
    ;:background "LightSlateBlue"
    :background "white"
    ;:family "Inconsolata"
    :family "white"
    :height 1.0)
 (set-face-attribute ; アクティブなタブ
   'tabbar-selected nil
    :background "navy"
    :foreground "white"
    :weight 'bold
    :box nil)
 (set-face-attribute ; 非アクティブなタブ
   'tabbar-unselected nil
    ;:background "LightSlateBlue"
    :background "white"
    :foreground "white"
    :box nil)

(require 'tabbar-ruler)

(require 'save-visited-files)
;;; tramp(remote)ファイルは復元しない
;(setq save-visited-files-ignore-tramp-files t)
;(turn-on-save-visited-files-mode)



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
                    :height 120)
;; 日本語をヒラギノ角ゴProNにする
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Maru Gothic ProN"))
;; 半角カナをヒラギノ角ゴProNにする
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Hiragino Maru Gothic ProN"))

(setq dviprint-command-format "dvipdfmx %s")

;; yatex_preview
;;(setq dvi2-command "open -a Preview")
;;(defvar YaTeX-dvi2-command-ext-alist
;;  '(("xdvi" . ".dvi")
;;    ("ghostview\\|gv" . ".ps")
;;    ("acroread\\|pdf\\|Preview\\|open" . ".pdf"))

;;ウィンドウの半透明化
;(if window-system
;	(progn
;	  (set-frame-parameter nil 'alpha80)))

;;ステータスバーの色変更
;(set-face-background 'mode-line "MediumPurple2")
(set-face-background 'mode-line "LightSlateBlue")

;;; 追加ここまで








;;; 以下竹村氏提供のものを使用


;; Red Hat Linux default .emacs initialization file

;;(set-default-coding-systems 'japanese-shift-jis)
(set-default-coding-systems 'utf-8)

;; (when (eq window-system 'mac)
;;  (add-hook 'window-setup-hook
;;            (lambda ()
;;              (set-frame-parameter nil 'fullscreen 'fullboth))))

;; (defun mac-toggle-max-window ()
;;   (interactive)
;;   (if (frame-parameter nil 'fullscreen)
;;       (set-frame-parameter nil 'fullscreen nil)
;;     (set-frame-parameter nil 'fullscreen 'fullboth)))

 (setq exec-path (cons "/usr/local/bin" exec-path))
 (setenv "PKG_CONFIG_PATH"
   (concat '"/usr/local/lib/pkgconfig:" (getenv "PKG_CONFIG_PATH")))
 (setenv "TMPDIR" (concat "/private/tmp" (getenv "TMPDIR")))


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
   (set-frame-parameter nil 'alpha 80)
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


;;;;;;;;;; 使用する辞書の設定 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SKK-JISYO.L をメモリ上に読み込んで利用する場合
;;(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")

;;; SKK-JISYO.M をメモリ上に読み込み、
;;; 見付からない場合は skkserv を起動して SKK-JISYO.L から検索する場合
;;; (skkexdic パッケージが必要です)
;;(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.M")
;;(setq skk-aux-large-jisyo "/usr/share/skk/SKK-JISYO.L")
;;(setq skk-server-portnum 1178)
;;(setq skk-server-host "localhost")
;;(setq skk-server-prog "/usr/libexec/skkserv")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key "\C-x\C-j" 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)
;; (autoload 'skk-mode "skk" nil t)
;; (autoload 'skk-auto-fill-mode "skk" nil t)
;; (autoload 'skk-tutorial "skk-tut" nil t)
;; (autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
;; (autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
;; (add-hook 'isearch-mode-hook
;; 	  (function (lambda ()
;; 		      (and (boundp 'skk-mode) skk-mode
;; 			   (skk-isearch-mode-setup) ))))
;; (add-hook 'isearch-mode-end-hook
;; 	  (function (lambda ()
;; 		      (and (boundp 'skk-mode) skk-mode
;; 			   (skk-isearch-mode-cleanup)
;; 			   (skk-set-cursor-color-properly) ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; X 版 Emacs/Mule で色を付ける
;;; color-mate の設定読み込み
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(load "~/.emacs-color.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaTeX 1.67
;;   [La]TeX 入力モード
;;   M-x yatex とするか、.tex で終わるファイルを読み込むと起動します
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX-mode
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;;(setq load-path (cons (expand-file-name "/usr/local/lib/mule/site-lisp/yatex/") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp/yatex") load-path))

(setq dvi2-command ""
      tex-command "/Applications/UpTeX.app/Contents/Resources/texbin/platex"
	  dviprint-command-format "/Applications/UpTeX.app/Contents/Resources/texbin/dvipdfmx %s"
      YaTeX-kanji-code 4)

(defun -acrobat ()
   "Call Acrobat Reader with a pdf file made from the tex file of current buffer"
   (interactive)
   (let ((filename (concat (file--sans-extension (buffer-name)) ".pdf")))
         (if (file-exists-p filename) 
              (call-process "acroread" nil nil nil filename)
              (message "Cannot file pdf file"))))

(setq yatex-mode-load-hook
     '(lambda() (YaTeX-define-key "p" 'call-acrobat)))

;;(defvar YaTeX-dvi2-command-ext-alist
;  '(("[agx]dvi\\|dviout\\|emacsclient" . ".dvi")
;   ("ghostview\\|gv" . ".ps")
;   ("acroread\\|pdf\\|Preview\\|TeXShop\\|Skim\\|evince\\|apvlv" . ".pdf")))

;; Add library path
;(add-to-list 'load-path "~/.emacs.d/lisp/yatex")
;; YaTeX mode
;(setq auto-mode-alist
;    (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;(setq tex-command "platex")
;(setq dviprint-command-format "dvipdfmx %s")
;; use Preview.app
;(setq dvi2-command "open -a Preview")
;(setq bibtex-command "pbibtex")

;; YaHtml-mode
;(setq auto-mode-alist
;      (cons (cons "\\.html$" 'yahtml-mode) auto-mode-alist))
;(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
;(setq yahtml-www-browser "netscape")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dic (eldic.el)
;;   Mule 上で dic を利用するための設定です
;;   ~/lib/emacs に /usr/doc/dic/eldic.el をコピーして
;;   dic-shell-file-name の辺りを適切に設定してください。
;;   C-c C-c C-e で英和、C-c C-c C-j で和英が引けます。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key "\C-c\C-c\C-e" 'lookup-edic)
;;(global-set-key "\C-c\C-c\C-j" 'lookup-jdic)
;;(autoload 'lookup-edic "eldic" nil t)
;;(autoload 'lookup-jdic "eldic" nil t)



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

;;Matlabモード
;;(autoload 'matlab-mode "~/.matlab.el" "Enter Matlab mode." t)
;;(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;;(autoload 'matlab-shell "~/.matlab.el" "Interactive Matlab mode." t)

;; require でも autoload でもおすきな方をどうぞ.
;; 最近だと, require の方が推奨されているとのことです.

;;(autoload 'text-translator "text-translator" "Text Translator" t)
;; (require 'text-translator)
;; (global-set-key "\C-xt" 'text-translator)
;; (global-set-key "\C-x\M-T" 'text-translator-translate-last-string)

;; プリフィックスキーを変更する場合.
;; (setq text-translator-prefix-key "¥M-n")

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
