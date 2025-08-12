(with-current-buffer gnus-summary-buffer
  (let ((header (gnus-summary-article-header)))
    (when header
      (mail-header-id header))))
