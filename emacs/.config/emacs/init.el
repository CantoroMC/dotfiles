(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indicate-empty-lines nil)
 '(package-selected-packages
   '(haskell-mode edit-server markdown-mode org-bullets org-plus-contrib all-the-icons-ivy-rich counsel ivy command-log-mode company rainbow-delimiters doom-modeline jbeans-theme evil-commentary evil-surround evil-collection evil goto-chg undo-tree key-chord auto-compile auto-package-update diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Configure package.el
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;
(package-initialize)

;; Download description of all configured ELPA packages, if not already done
(unless package-archive-contents
  (package-refresh-contents))

;; If use-package is not install, then do it.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(org-babel-load-file "~/.config/emacs/emacs_configuration.org")
