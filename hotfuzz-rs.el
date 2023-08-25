;;; hotfuzz-rs --- rust implementation of hotfuzz -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (hotfuzz "0"))

;;; Commentary:

;; TODO: build steps

;;; Code:

(require 'hotfuzz)
(require 'hotfuzz-rs-module)

(defgroup hotfuzz-rs nil
  "rust implementation of hotfuzz."
  :group 'minibuffer)

;;;###autoload
(define-minor-mode hotfuzz-rs-mode
  "Minor mode to eanble rust hotfuzz."
  :global t
  (if hotfuzz-rs-mode
      (advice-add #'hotfuzz-filter :override #'hotfuzz-rs-module-filter)
    (advice-remove #'hotfuzz-filter #'hotfuzz-rs-module-filter)))

(provide 'hotfuzz-rs)
;;; hotfuzz-rs.el ends here
