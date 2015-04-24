;;; installed packages (mainly by auto-install)
;; auto-install
;;   http://www.emacswiki.org/emacs/download/auto-install.el
;; redo+
;;   http://www.emacswiki.org/emacs/download/redo+.el

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
(add-to-load-path "elist" "conf" "public-repos" "elisp")


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
;;(when (require 'multi-term nil t)
;;  ;; 使用するシェルを指定
;;  (setq multi-term-program "/usr/local/bin/bash"))


;;; 表示とか見た目について
;; カラーテーマの変更(v24以降)
(when (> emacs-major-version 23)
  (load-theme 'manoj-dark t))

;; フォント設定
(set-face-attribute 'default nil :family "Consolas" :height 140)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))
(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

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
(set-face-font 'mode-line "Consolas-14")
(set-face-font 'mode-line-inactive "Consolas-14")
(set-face-font 'mode-line-buffer-id "Consolas-15")

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
