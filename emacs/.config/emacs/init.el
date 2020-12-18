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
