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
          (start-process "youtube" nil "mpv" link))
         ((string-match "/\\/www\\.bilibili\\.com\\/video\/" link)
            (message (concat "loading from bilibili..." link))
            (call-process "bilidan" nil nil nil link))))))

(defun v/w3m-copy-link ()
  (interactive)
  (let ((link (w3m-anchor)))
    (if (not link)
        (message "The point is not link.")
      (kill-new link)
      (message "Copy \"%s\" to clipboard." link))))

(defun sk/w3m-copy-link ()
  (interactive)
  (let ((link (or (w3m-anchor) w3m-current-url)))
    (kill-new link)
    (message "Copy \"%s\" to clipboard." link)))

(defun v/w3m-open-url (url)
  "Opens url in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):" nil nil "google.com" nil )))
  (w3m-goto-url
   (if (string-match-p "^https?://" url)
       url
     (concat "http://" url))))

(defun v/w3m-open-url-new-session (url)
  "Opens url in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):" nil nil "google.com" nil )))
  (w3m-goto-url-new-session
   (concat "http://" url)))

(defun sk/w3m-browse-url-new-session ()
  "Opens url in new w3m session."
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u
    (call-interactively 'w3m-browse-url)))


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
  (with-eval-after-load 'w3m-filter
    (setq
     w3m-filter-configuration
     (append sk/w3m-additional-filters w3m-filter-configuration))))

(defun sk/w3m-filter-livemint (url)
  "Filter cruft for livemint.com"
  (w3m-filter-delete-regions
   url
   "<div class=\"lg-device\">"
   "<div class=\"clearfix wrapper\" id=\"main-content\""
   nil t)
  (w3m-filter-delete-regions
   url
   "<div class=\"subscribe-box\">"
   "</div>"))

(defun sk/w3m-filter-ndtv (url)
  "Filter cruft for ndtv.com"
  (w3m-filter-delete-regions
   url
   "<div class=\"neweleccont\""
   "<div class=\"newcont\""
   nil t)
  (w3m-filter-delete-regions
   url
   "<div class=\"nhm_main"
   "<script type=\"text/javascript\""
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

(defun sk/w3m-filter-github (url)
  "Filter cruft from github.com"
  (w3m-filter-delete-regions
   url
   "<div class=\"position-relative"
   "<div role=\"main"
   nil t)
  (w3m-filter-delete-regions
   url
   "<ul class=\"pagehead-actions"
   "/ul>")
  (w3m-filter-delete-regions
   url
   "<div class=\"file-navigation"
   "<div class=\"file-wrap"
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
         "\\`https?://www\\.youtube\\.com/" sk/w3m-filter-youtube)
        (t
         "Filter for Github"
         "\\`https?://github\\.com/" sk/w3m-filter-github)))

(defun sk/w3m-external-view-current-url ()
  (interactive)
  (if w3m-current-url
      (funcall shr-external-browser w3m-current-url)))
