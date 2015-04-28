;;; installed packages (mainly by auto-install)
;; auto-install
;;   http://www.emacswiki.org/emacs/download/auto-install.el
;; redo+
;;   http://www.emacswiki.org/emacs/download/redo+.el
;; hown
;;   http://howm.sourceforge.jp/
;; color-theme
;;   http://code.google.com/p/gnuemacscolorthemetest/

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

;; twitter用
(when (require 'twittering-mode nil t)
  ;; 認証情報を読み込む
  (let ((path "twitter-auth"))
    (load path t)))

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

;; egg
;; GitをEmacs上で使えるようにする
;; C-x v s で git status 相当、sでステージ・アンステージ(add)、cでコミット
(when (executable-find "git")
  (require 'egg nil t))

;; multi-term
;; Emacs上でターミナルを起動
;; M-x multi-term
(when (string-equal (system-name) "ibako")
    (when (require 'multi-term nil t)
      ;; 使用するシェルを指定
      (setq multi-term-program "/usr/local/bin/bash")))

;; howm
;; メモ・情報整理用
;; C-c , , または M-x howm-menu
;; howmメモ保存場所
(setq howm-directory (concat user-emacs-directory "/howm"))
;; howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
;; howmメモを1日1ファイルにする
; (setq howm-file-name-format "%Y/%m/%Y-%m-%d".howm")
;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  (define-key global-map (kbd "C-c , ,") 'howm-menu))

;;; 表示とか見た目について
;; カラーテーマの変更(v24以降は標準のやつ、それより前はcolor-themeを利用)
(if (> emacs-major-version 23)
  (load-theme 'manoj-dark t)
  (when (require 'color-theme nil t)
    (color-theme-initialize)
    (color-theme-tty-dark)))

;; フォント設定
;; Windows
(when (or (string-equal (system-name) "PC-GRANDMOTHER") (string-equal (system-name) "PC-B012"))
  (set-face-attribute 'default nil :family "Consolas" :height 140)
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    (font-spec :family "Hiragino Kaku Gothic ProN"))
  (add-to-list 'face-font-rescale-alist
	       '(".*Hiragino Kaku Gothic ProN.*" . 1.2)))
;; Ubuntu(ibako)
(when (string-equal (system-name) "ibako")
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


;;; モードラインについて
;; フォント設定
(when (or (string-equal (system-name) "PC-GRANDMOTHER") (string-equal (system-name) "PC-B012"))
  (set-face-font 'mode-line "Consolas-14")
  (set-face-font 'mode-line-inactive "Consolas-14")
  (set-face-font 'mode-line-buffer-id "Consolas-15"))
(when (string-equal (system-name) "ibako")
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

;; 矩形編集を便利に。C-RETで開始
(cua-mode t)
(setq cua-enable-cua-keys nil) ; t をセットすると、C-cでコピーとか出来る
