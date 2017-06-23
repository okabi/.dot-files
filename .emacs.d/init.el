;;; installed packages (mainly by auto-install)
;; auto-install
;;   http://www.emacswiki.org/emacs/download/auto-install.el
;; redo+
;;   http://www.emacswiki.org/emacs/download/redo+.el
;; color-theme
;;   http://code.google.com/p/gnuemacscolorthemetest/

;;; About elpa
;; M-x list-packages

;;; 定数とロードパスの追加
(defvar windows?
  (or (string-equal (system-name) "PC-GRANDMOTHER") (string-equal (system-name) "PC-A246") (string-equal (system-name) "DESKTOP-U40CCUV"))
  "If your computer is Windows, windows? is true.")
(defvar home?
  (or (string-equal (system-name) "PC-GRANDMOTHER") (string-equal (system-name) "PC-A246"))
  "If your computer is yours, home? is true.")
(defvar lab?
  (string-equal (system-name) "DESKTOP-U40CCUV")
  "If your computer is Lab's, lab? is true.")

;; user-emacs-directory の定義(v23より前バージョン)
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

;; last-command-char が消されている環境があるらしいので
(define-obsolete-variable-alias 'last-command-char 'last-command-event "at least 19.34")


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
(add-to-load-path "elisp" "elpa")


;;; パッケージについて
;; auto-install
;; M-x install-elisp RET URL RET -> C-c C-c でelispをインストール
;; 常時有効化したい場合は、init.elに以下のような設定を書くこと
(when (require 'auto-install nil t)
  ;; インストールディレクトリの指定
  (setq auto-install-directory (concat user-emacs-directory "/elisp/"))
  ;; EmacsWikiに登録されているelispの名前を取得する
  ;; (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

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
(when (and home? windows?)
  (setenv "PATH" (concat "C:\\Program Files (x86)\\PuTTY" ";" (getenv "PATH")))
  (when (require 'tramp nil t)
    (setq tramp-default-method "pscp")))
(when (and lab? windows?)
  (setenv "PATH" (concat "D:\\okabi\\Programs\\PuTTY" ";" (getenv "PATH")))
  (when (require 'tramp nil t)
    (setq tramp-default-method "pscp")))

;; iedit
;; リファクタリングに利用．C-; で対応部分同時編集モードに入る．
;; このモード中に M-H で対象を関数内に限る．
(when (require 'iedit nil t)
  (setq iedit-case-sensitive-default nil))

;; git-gutter-fringe
;; git diff の結果を表示する
(when (require 'git-gutter-fringe nil t)
  (global-git-gutter-mode t))

;; バックアップファイルの作成を無効化(Trampで接続時にエラー音がうるさい)
(setq make-backup-files nil)
(setq auto-save-default nil)


;;; 表示とか見た目について
;; カラーテーマの変更
(if (> emacs-major-version 23)
  (load-theme 'manoj-dark t)
  (when (require 'color-theme nil t)
    (color-theme-initialize)
    (color-theme-tty-dark)))

;; 背景透過設定
;; M-x alpha で数値を入力すれば背景透過設定できる．デフォルトは 80
(set-frame-parameter nil 'alpha (cons 80 '(90)))
(defun alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; フォント設定
;; Windows
(when windows?
  (set-face-attribute 'default nil :family "Consolas" :height 140)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Kaku Gothic ProN.*" . 1.2)))
;; Ubuntu
;; (when linux?
;;   (add-to-list 'default-frame-alist '(font . "ricty-13.5")))

;; メニューバーおよびツールバーを非表示に
(tool-bar-mode 0)
(menu-bar-mode 0)

;; 対応するカッコのハイライト
(setq show-paren-delay 0) ; 表示までの秒数
(show-paren-mode t) ; 表示を有効化
(setq show-paren-style 'expression) ; カッコ内も強調表示
(set-face-background 'show-paren-match-face nil) ; 強調表示の背景をナシに
(set-face-underline-p 'show-paren-match-face "yellow") ; 強調表示部に下線を引く

;; 空白・タブを強調
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         ))
(global-whitespace-mode 1)

(set-face-attribute 'whitespace-tab nil
                    :background "Red"
                    :foreground nil
                    :underline nil)

;; 文字コードをutf-8に
(prefer-coding-system 'utf-8)

;; ハイライト部分を赤色に(デフォルトは灰色で背景と被って見づらい)
(set-face-background 'highlight "red")

;; 左側に行表示
(when (require 'hlinum nil t)
  (custom-set-variables '(global-linum-mode t)))

;; スクロールをスムースに
(when (require 'smooth-scroll nil t)
  (smooth-scroll-mode t))

;; 最終行に必ず一行挿入する
(setq require-final-newline t)


;;; モードラインについて
;; フォント設定
(when windows?
  (set-face-font 'mode-line "Consolas-14")
  (set-face-font 'mode-line-inactive "Consolas-14")
  (set-face-font 'mode-line-buffer-id "Consolas-15"))
;; (when linux?
;;   (set-face-font 'mode-line "ricty-14")
;;   (set-face-font 'mode-line-inactive "ricty-14")
;;   (set-face-font 'mode-line-buffer-id "ricty-15"))

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
;; redo+ (C-. でリドゥ)
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-.") 'redo))

;; ウィンドウ切り替え。(C-x o)と同じ
(define-key global-map (kbd "C-t") 'other-window)

;; C-h をバックスペースに
(define-key global-map (kbd "C-h") 'delete-backward-char)

;; C-: を指定行ジャンプに
(define-key global-map (kbd "C-:") 'goto-line)

;; C-x m を compile に
(define-key global-map "\C-xm" 'compile)
(setq compilation-scroll-output t)

;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; C-RET で矩形選択モードに入る
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; タブを入れないようにする
(setq-default indent-tabs-mode nil)

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


;;; Haskell
;; Haskell ファイルの関連付け
(autoload 'haskell-mode "haskell-mode"
  "Mode for editing haskelk source files" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; インデント方式の変更
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))


;;; HTML
;; HTML 用メジャーモード
(when (require 'web-mode nil t)
  ;; emacs 23以下の互換
  (when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode))

  ;; 適用する拡張子
  (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

  ;; インデント数
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-offset 2))
  (add-hook 'web-mode-hook 'web-mode-hook))
