;;; ディレクトリについて
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
(add-to-load-path "elist" "conf" "public-repos")


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
