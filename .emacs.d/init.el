;; ~/.emacs.d/elisp をロードパスに追加する
(add-to-list 'load_path "~/.emacs.d/elisp")

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
