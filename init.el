;; init.elをreloadする方法
;; http://moonstruckdrops.github.io/blog/2014/11/09/reload-config-emacs/
;; M-x eval-buffer


;;; 環境ごとの初期設定

;; https://qiita.com/j8takagi/items/504ccb86921695bdec13

;; windows setting

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
  ;; (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
  ;; (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "black")))

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



;;; 共通の初期設定

;; 日本語の文字コード設定
;; http://yohshiy.blog.fc2.com/blog-entry-273.html
(prefer-coding-system 'utf-8)

;; color theme
;; https://aoe-tk.hatenablog.com/entry/20130210/1360506829
(load-theme 'misterioso t)

;; 特定の色が見にくい場合
;; https://qiita.com/AnchorBlues/items/91026c4f1c0745f5b851

;; 背景の透過
;; https://sakashushu.blog.so-net.ne.jp/2014-04-27
(set-frame-parameter nil 'alpha 100)

;; 文字サイズの設定
;; https://qiita.com/suuungwoo/items/46fb78c3ab7904a30905
;; https://qiita.com/melito/items/238bdf72237290bc6e42
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))
(set-face-attribute 'default nil :height 150)

;;; パッケージ管理

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
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))
;; (el-get 'sync)
;; ↑初期化処理をel-get-bundleで書いていくタイプの人間は削除するらしい
;; http://semper-fi.hatenablog.com/entry/2016/07/18/161957

;;; パッケージ

;; bind-key
;; http://emacs.rubikitch.com/bind-key/
(el-get-bundle bind-key)

;; magit
;; https://qiita.com/ignorant/items/86d353e3ada299f12836
;; Linuxの場合はtexinfoをインストールしておく必要がある
;; https://github.com/dimitri/el-get/issues/2251
;; sudo apt-get install texinfo
(el-get-bundle magit)
(bind-key "C-x g" 'magit-status)

;; auto-complete
;; http://keisanbutsuriya.hateblo.jp/entry/2015/02/08/175005
(el-get-bundle auto-complete)  
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'latex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ 

;; Windowの場合はPuTTYをインストールする
;; https://www.putty.org/

;; tramp
;; https://qiita.com/Tats_U_/items/cb7ee924541a58c66946
(when (equal system-type 'gnu/linux)
  (setq tramp-default-method "ssh"))
(when (equal system-type 'windows-nt)
  (setq tramp-default-method "plink"))

;; ssh
(el-get-bundle ssh)
;; ssh-modeでのファイルパス補完の有効化
;; https://qiita.com/fujimotok/items/86c665fba6fdcf62aeca

;; ssh-modeでplink使うよう設定
(when (equal system-type 'windows-nt)
  ;;(setq ssh-program "/c/Program Files/PuTTY/plink.exe"))
  (setq ssh-program "plink"))

;; ファイルパスの補完有効化
(add-hook 'ssh-mode-hook
          (lambda ()
            (setq ssh-directory-tracking-mode t)
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))

;; undo-tree
;; https://www.emacswiki.org/emacs/UndoTree
(el-get-bundle undo-tree)

;; nyan-mode
;; https://github.com/TeMPOraL/nyan-mode
(el-get-bundle nyan-mode)
(nyan-mode 1)

;; helm
;; https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c
(el-get-bundle helm)
(require 'helm-config)
(helm-mode 1)

;; madhat2r-theme
;; register el-get directory as custom color-theme
;; (add-to-list 'custom-theme-load-path (locate-user-emacs-file "el-get/madhat2r-theme"))
;; (el-get-bundle madhat2r-theme)
;; (load-theme 'madhat2r t)



;;; cache directory

;; http://d.hatena.ne.jp/sandai/20120309/p1
;; https://qiita.com/ShingoFukuyama/items/19b02cd1679a6ea0bfdb

;; auto-complete
(setq ac-comphist-file (locate-user-emacs-file "cache/auto-complete/ac-comphist.dat"))

;; auto-save-list
(setq auto-save-list-file-prefix (locate-user-emacs-file "cache/auto-save-list/.saves-"))

;; nsm data
;; https://ayatakesi.github.io/emacs/25.1/Network-Security.html
(setq nsm-settings-file (locate-user-emacs-file "cache/network-security.data"))

;; tramp
(setq tramp-persistency-file-name (locate-user-emacs-file "cache/tramp"))


;;;  package selected package 

;; http://extra-vision.blogspot.com/2016/10/emacs25-package-selected-packages.html
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

