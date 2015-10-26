;;; installed packages (mainly by auto-install)
;; auto-install
;;   http://www.emacswiki.org/emacs/download/auto-install.el
;; redo+
;;   http://www.emacswiki.org/emacs/download/redo+.el
;; color-theme
;;   http://code.google.com/p/gnuemacscolorthemetest/

;;; About elpa
;; M-x list-packages 

;;; 定数について
(defvar windows?
  (or (string-equal (system-name) "PC-GRANDMOTHER") (string-equal (system-name) "PC-B012"))
  "If your computer is Windows, windows? is true.")
(defvar linux?
  (string-equal (system-name) "ibako")
  "If your computer is Ubuntu, linux? is true.")


;;; load-path について
;; user-emacs-directory の定義(v23より前バージョン)
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

;; load-pathを追加する関数の定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "public_repos" "elisp" "elpa" "auth")


;;; パッケージについて
;; auto-install
;; M-x install-elisp RET URL RET -> C-c C-c でelispをインストール
;; 常時有効化したい場合は、init.elに以下のような設定を書くこと
(when (require 'auto-install nil t)
  ;; インストールディレクトリの指定
  (setq auto-install-directory (concat user-emacs-directory "/elisp/"))
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; redo+
(when (require 'redo+ nil t)
  ;; C-.にリドゥを割り当てる
  (global-set-key (kbd "C-.") 'redo))

;; package
;; Emacs 24以降は標準搭載。それ以前は落としてくること
;; M-x package-install RET hoge RET でパッケージをインストール
;; auto-installと違ってパッケージ管理が出来る他、init.elに設定を書く必要も(基本)無い
(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
	       '("ELPA" . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;; auto-complete
;; 入力補完をしてくれるすごいやつ
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "/elpa/auto-complete-1.4/dict"))
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; tramp
;; ローカルのEmacsでリモートサーバのファイルを編集(/plink:user@hostname#port:filepath)
(when windows?
  (setenv "PATH" (concat "C:\\Program Files (x86)\\PuTTY" ";" (getenv "PATH"))) 
  (when (require 'tramp nil t)
    (setq tramp-default-method "pscp")))

;; iedit
;; リファクタリングに利用．C-; で対応部分同時編集モードに入る．
;; このモード中に M-H で対象を関数内に限る．
(when (require 'iedit nil t)
  (setq iedit-case-sensitive-default nil))


;;; 表示とか見た目について
;; カラーテーマの変更(v24以降は標準のやつ、それより前はcolor-themeを利用)
(if (> emacs-major-version 23)
  (load-theme 'manoj-dark t)
  (when (require 'color-theme nil t)
    (color-theme-initialize)
    (color-theme-tty-dark)))

;; フォント設定
;; Windows
(when windows?
  (set-face-attribute 'default nil :family "Consolas" :height 140)
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
	       '(".*Hiragino Kaku Gothic ProN.*" . 1.2)))
;; Ubuntu(ibako)
(when linux?
  (add-to-list 'default-frame-alist '(font . "ricty-13.5")))

;; メニューバーおよびツールバーを非表示に
(tool-bar-mode 0)
(menu-bar-mode 0)

;; 対応するカッコのハイライト
(setq show-paren-delay 0) ; 表示までの秒数
(show-paren-mode t) ; 表示を有効化
(setq show-paren-style 'expression) ; カッコ内も強調表示
(set-face-background 'show-paren-match-face nil) ; 強調表示の背景をナシに
(set-face-underline-p 'show-paren-match-face "yellow") ; 強調表示部に下線を引く

;; ハイライト部分を赤色に(デフォルトは灰色で背景と被って見づらい)
(set-face-background 'highlight "red")

;; 左側に行表示
(when (require 'hlinum nil t)
  (custom-set-variables '(global-linum-mode t)))

;; スクロールをスムースに
(when (require 'smooth-scroll nil t)
  (smooth-scroll-mode t))


;;; モードラインについて
;; フォント設定
(when windows?
  (set-face-font 'mode-line "Consolas-14")
  (set-face-font 'mode-line-inactive "Consolas-14")
  (set-face-font 'mode-line-buffer-id "Consolas-15"))
(when linux?
  (set-face-font 'mode-line "ricty-14")
  (set-face-font 'mode-line-inactive "ricty-14")
  (set-face-font 'mode-line-buffer-id "ricty-15"))

;; カラム番号を表示
(column-number-mode t)

;; 時計を表示
(setq display-time-string-forms 
      '((format "%s/%s(%s) %s:%s"
		month day dayname 24-hours minutes)))
(display-time-mode t)

;; バッテリー残量を表示
(display-battery-mode t)


;;; キーバインドについて
;; 行の折り返し表示を切り替える
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; ウィンドウ切り替え。(C-x o)と同じ
(define-key global-map (kbd "C-t") 'other-window)

;; 背景透過設定
;; M-x alpha で数値を入力すれば背景透過設定できる．デフォルトは 80
(set-frame-parameter nil 'alpha (cons 80 '(90)))
(defun alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; バックアップファイルの作成を無効化(Trampで接続時にエラー音がうるさい)
(setq make-backup-files nil)
(setq auto-save-default nil) 


;;; Ruby
;; Ruby ファイルの関連付け
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; インデントをいい感じにする
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; end に対応する部分をピックアップする
(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  ;; 対応部分にハイライトかつミニバッファに表示
  (setq ruby-block-highlight-toggle t))

;; end を自動で補完
(when (require 'ruby-electric nil t)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil))
