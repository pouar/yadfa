(in-package :net.didierverna.declt)
(defun render-docstring (item)
  "Render ITEM's documentation string.
Rendering is done on *standard-output*."
  (when-let ((docstring (docstring item)))
    (write-string docstring *standard-output*)))
