;;; packages.el --- w3m layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Kuroi Mato <venmos@fuck.gfw.es>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq w3m-packages
      '(
        w3m
        helm-w3m
        ))

(defun w3m/init-helm-w3m ()
  "Initializes helm-w3m and adds keybindings for its exposed functionalities."
  (use-package helm-w3m
    :commands (helm-w3m-bookmarks)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'w3m-mode
        "bb" 'helm-w3m-bookmarks))))

(defun w3m/init-w3m()
  "Initializes w3m and adds keybindings for its exposed functionalities."
  (use-package w3m
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "aw" 'w3m)
      )
    :config
    (progn
      (sk/init-w3m)
      (setq
       w3m-confirm-leaving-secure-page nil
       w3m-default-display-inline-images t)
      ;; (evilified-state-evilify-map w3m-mode-map
      ;;   :mode w3m-mode
      ;;   :eval-after-load w3m
      ;;   :bindings
      ;;   "f" 'ace-link-eww
      ;;   "{" 'evil-backward-paragraph
      ;;   "}" 'evil-forward-paragraph
      ;;   (kbd "C-v") 'evil-visual-block)
      (spacemacs/set-leader-keys-for-major-mode 'w3m-mode
        ","  'w3m-browse-url
        "o"  'w3m-browse-url
        "O"  'sk/w3m-browse-url-new-session
        "ba" 'w3m-bookmark-add-current-url
        "be" 'w3m-bookmark-edit
        "bv" 'w3m-bookmark-view
        "s"  'w3m-search
        "S"  'w3m-search-new-session
        "S"  'w3m-search-new-session
        "x"  'w3m-delete-buffer
        "t"  'w3m-view-this-url-new-session
        "&"  'sk/w3m-external-view-current-url
        "y"  'sk/w3m-copy-link
        "p"  'v/w3m-player-movie
        "r"  'w3m-redisplay-this-page
        "R"  'w3m-reload-this-page
        )
      (evil-define-key '(normal motion) w3m-mode-map
        (kbd "RET") 'w3m-view-this-url
        "gt" 'w3m-next-buffer
        "gT" 'w3m-previous-buffer
        )
      )))

;;; packages.el ends here
