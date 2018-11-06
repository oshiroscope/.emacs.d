;; init.elをreloadする方法
;; http://moonstruckdrops.github.io/blog/2014/11/09/reload-config-emacs/

;;; 環境ごとの初期設定

;; https://qiita.com/j8takagi/items/504ccb86921695bdec13
;; windows setting

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (equal system-type 'windows-nt)
  ;; set default directory: HOME
  ;; https://qiita.com/t2psyto/items/05776f010792ba967152
  (setq default-directory "~/") 
  (setq command-line-default-directory "~/")  

  ;; 日本語IME周りの設定
  ;; 日本語IMEパッチは↓を使っている
  ;; https://github.com/mhatta/emacs-26-x86_64-win-ime
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  ;; 日本語入力時にカーソルの色を変える設定
  (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
  (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "black")))

  ;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
  (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

  ;; isearch に移行した際に日本語入力を無効にする
  (add-hook 'isearch-mode-hook '(lambda ()
                                  (deactivate-input-method)
                                  (setq w32-ime-composition-window (minibuffer-window))))
  (add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))

  ;; helm 使用中に日本語入力を無効にする
  (advice-add 'helm :around '(lambda (orig-fun &rest args)
                               (let ((select-window-functions nil)
                                     (w32-ime-composition-window (minibuffer-window)))
                                 (deactivate-input-method)
                                 (apply orig-fun args))))

  ;; ;; IMEが効かない問題に対処
  ;; https://github.com/chuntaro/NTEmacs64/issues/3
  (menu-bar-open)
  (defun send-esc ()
    (start-process "my-proc" nil "cscript.exe"
                   (expand-file-name "~/.emacs.d/sendesc.js")))
  (add-hook 'emacs-startup-hook 'send-esc))

;; linux setting
(when (equal system-type 'gnu/linux))


;; ;; magit 
;; ;; https://qiita.com/ignorant/items/86d353e3ada299f12836
;; (use-package magit
;;   :ensure t
;;   :defer t
;;   :init
;;   (when (eq system-type 'windows-nt)
;;     (setq magit-git-executable "D:/Programs/Git/bin/git.exe"))
;;   :bind ("C-x g" . magit-status)
;;   :config
;;   (setq magit-refs-show-commit-count 'all
;;         magit-log-buffer-file-locked t
;;         magit-revision-show-gravatars nil
;;         magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
;;   )

;; el-getの使い方
;; https://masutaka.net/chalow/2015-06-17-1.html
;; el-getのinstall
;; https://kiririmode.hatenablog.jp/entry/20180822/1534949626
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; bind-key
;; http://emacs.rubikitch.com/bind-key/
(el-get-bundle bind-key)

;; magit
;; https://qiita.com/ignorant/items/86d353e3ada299f12836
(el-get-bundle magit)
(bind-key "C-x g" 'magit-status)

;; auto-complete
(el-get-bundle auto-complete)  
	     
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
