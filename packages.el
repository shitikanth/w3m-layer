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

(defun v/w3m-save-buffer-to-file ()
  (interactive)
  (let* ((curr (buffer-file-name))
         (new (read-file-name
               "Save to file: " nil nil nil
               (and curr (file-name-nondirectory curr))))
         (mustbenew (if (and curr (file-equal-p new curr)) 'excl t)))
    (if (use-region-p)
        (write-region (region-beginning) (region-end) new nil nil nil mustbenew)
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) new nil nil nil mustbenew)))))

(defun v/w3m-player-movie ()
  (interactive)
  (let ((link (w3m-anchor)))
    (if (not link)
        (message "The point is not link.")
   (cond ((string-match "/\\/www\\.youtube\\.com\\/watch\/?" link)
          (message (concat "loading from youtube..." link))
          (call-process "mpv" nil nil nil link))
         ((string-match "/\\/www\\.bilibili\\.com\\/video\/" link)
            (message (concat "loading from bilibili..." link))
            (call-process "bilidan" nil nil nil link)))
   (message "Sorry, Playback error. please check url."))))

(defun v/w3m-copy-link ()
  (interactive)
  (let ((link (w3m-anchor)))
    (if (not link)
        (message "The point is not link.")
      (kill-new link)
      (message "Copy \"%s\" to clipboard." link))))

(defun w3m/init-helm-w3m ()
  "Initializes helm-w3m and adds keybindings for its exposed functionalities."
  (use-package helm-w3m
    :commands (helm-w3m-bookmarks)
    :init
    (progn
      (spacemacs/set-leader-keys
        "awb" 'helm-w3m-bookmarks))))

(defun v/w3m-open-url (url)
  "Opens url in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):" nil nil "google.com" nil )))
  (w3m-goto-url
   (concat "http://" url)))

(defun v/w3m-open-url-new-session (url)
  "Opens url in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):" nil nil "google.com" nil )))
  (w3m-goto-url-new-session
   (concat "http://" url)))

(defun w3m/init-w3m()
  "Initializes w3m and adds keybindings for its exposed functionalities."
  (use-package w3m
    :defer t
    :init
    (spacemacs/set-leader-keys
      "awo" 'v/w3m-open-url
      "awf" 'w3m-find-file
      "aws" 'w3m-search
      )
    (progn
        (evilified-state-evilify-map w3m-mode-map
          :mode w3m-mode
          :eval-after-load w3m
          :bindings
          "0" 'evil-digit-argument-or-evil-beginning-of-line
          "$" 'evil-end-of-line
          "f" 'evil-find-char
          "F" 'evil-find-char-backward
          "o" 'ace-link-eww
          "wp" 'v/w3m-player-movie
          "wy" 'v/w3m-copy-link
          "wf" 'w3m-find-file
          "wo" 'v/w3m-open-url
          "wO" 'v/w3m-open-url-new-session
          "wt" 'w3m-view-this-url-new-session
          "wT" 'w3m-create-empty-session
          "ws" 'w3m-search
          "wS" 'w3m-search-new-session
          "wl" 'w3m-next-buffer
          "wh" 'w3m-previous-buffer
          "wx" 'w3m-delete-buffer
          "wd" 'v/w3m-save-buffer-to-file
          "wD" 'w3m-save-buffer
          "we" 'w3m-bookmark-edit
          "wa" 'w3m-bookmark-add-current-url
          "wm" 'w3m-view-url-with-external-browser
          "wb" 'helm-w3m-bookmarks
          "wB" 'w3m-bookmark-view
        ))))

(with-eval-after-load 'w3m
  (define-key w3m-mode-map (kbd "C-f") 'evil-scroll-page-down)
  (define-key w3m-mode-map (kbd "C-b") 'evil-scroll-page-up)
  (define-key w3m-mode-map (kbd "SPC") 'evil-evilified-state))

(defun v/init-w3m ()
  (use-package w3m
    :init
    (progn
      (setq browse-url-browser-function 'w3m-goto-url-new-session
            w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533."
            w3m-coding-system 'utf-8
            w3m-file-coding-system 'utf-8
            w3m-file-name-coding-system 'utf-8
            w3m-input-coding-system 'utf-8
            w3m-output-coding-system 'utf-8
            w3m-terminal-coding-system 'utf-8))))

(defun sk/init-w3m ()
  (use-package w3m
    :init
    (progn
      (setq w3m-filter-configuration
            (append sk/w3m-additional-filters w3m-filter-configuration)))))

(defun sk/w3m-filter-livemint (url)
  "Filter cruft for livemint.com"
  (w3m-filter-delete-regions
   url
   "<div class=\"lg-device\">"
   "<div class=\"clearfix wrapper\""
   nil t)
  (w3m-filter-delete-regions
   url
   "<div class=\"subscribe-box\">"
   "</div>"))

(defun sk/w3m-filter-ndtv (url)
  "Filter cruft for ndtv.com"
  (w3m-filter-delete-regions
   url
   "<div class=\"nglobalnav\">"
   "</div>")
  (w3m-filter-delete-regions
   url
   "<div class=\"gnavigation\">"
   "</div>")
  (w3m-filter-delete-regions
   url
   "<div class=\"topnav_cont\">"
   "</div>")
  (w3m-filter-delete-regions
   url
   "<div class=\"topmenu\""
   "<div class=\"hide_cont"
   nil t))

(defun sk/w3m-filter-youtube (url)
  "Filter cruft from youtube.com"
  (w3m-filter-delete-regions
   url
   "<ytd-masthead"
   "/ytd-masthead>"))

(defun sk/w3m-filter-python-docs (url)
  "Filter cruft from youtube.com"
  (w3m-filter-delete-regions
   url
   "<div class=\"related\""
   "<div class=\"document\""
   nil t))

(setq sk/w3m-additional-filters
      '((t
         "Filter for Livemint"
         "\\`https?://www\\.livemint\\.com/" sk/w3m-filter-livemint)
        (t
         "Filter for NDTV"
         "\\`https?://www\\.ndtv\\.com/" sk/w3m-filter-ndtv)
        (t
         "Filter for Python docs"
         "\\`https?://docs\\.python\\.org/" sk/w3m-filter-python-docs)
        (t
         "Filter for Youtube"
         "\\`https?://www\\.youtube\\.com/" sk/w3m-filter-youtube)))

;;; packages.el ends here
