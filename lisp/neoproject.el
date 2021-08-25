;;; a project manager

(require 'hierarchy)

(setq animals (hierarchy-new))

(let ((parentfn
       ;; Given an item, return its parent
       (lambda (item)
         (cl-case item
           (dove 'bird)
           (pigeon 'bird)
           (bird 'animal)
           (dolphin 'animal)
           (cow 'animal)))))
  (hierarchy-add-tree animals 'dove parentfn)
  (hierarchy-add-tree animals 'pigeon parentfn)
  (hierarchy-add-tree animals 'dolphin parentfn)
  (hierarchy-add-tree animals 'cow parentfn))

(hierarchy-sort animals)

(hierarchy-children animals 'animal)

(with-temp-buffer
  (hierarchy-map
   (hierarchy-labelfn-indent
    (lambda (animal _) (insert (symbol-name animal) "\n")))
   animals)
  (buffer-substring (point-min) (point-max)))

(switch-to-buffer
 (hierarchy-tabulated-display
  animals
  (hierarchy-labelfn-indent
   (hierarchy-labelfn-button
    (lambda (item _) (insert (symbol-name item)))
    (lambda (item _) (message "You clicked on: %s" item))))))

(switch-to-buffer
 (hierarchy-tree-display
  animals
  (lambda (item _) (insert (symbol-name item)))))

(defun hierarchy-examples-fs-directory-p (file)
  "Return non-nil if FILE is a directory and not . or ..."
  (and (not (string-suffix-p "/." file))
       (not (string-suffix-p "/.." file))
       (file-directory-p file)))

(defun hierarchy-examples-fs-children (folder)
  "Return sub-directories of FOLDER as absolute paths."
  (when (file-directory-p folder)
    (seq-filter #'hierarchy-examples-fs-directory-p (directory-files folder t))))

(defun hierarchy-examples-fs-parent (folder)
  "Return parent of FOLDER."
  (when (not (string= folder "/"))
    (directory-file-name (file-name-directory folder))))

(defun hierarchy-examples-fs-build-fs-hierarchy (folder)
  "Return hierarchy of FOLDER."
  (let* ((folder (expand-file-name folder))
         (parentfn #'hierarchy-examples-fs-parent)
         (childrenfn (lambda (file) (when (string-prefix-p folder file)
                                 (hierarchy-examples-fs-children file))))
         (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy folder parentfn childrenfn)
    (hierarchy-sort hierarchy)
    hierarchy))

(defun hierarchy-examples-fs-labelfn (folder _)
  "Insert name of FOLDER at current position.
_ is ignored."
  (insert (if (string= folder "/")
              "/"
            (file-name-nondirectory folder))))

(defun hierarchy-examples-fs-display-filesystem (&optional folder)
  "Display hierarchy of FOLDER in a tabulated list."
  (let* ((hierarchy (hierarchy-examples-fs-build-fs-hierarchy folder))
         (buffer (hierarchy-tabulated-display
                  hierarchy
                  (hierarchy-labelfn-indent
                   (hierarchy-labelfn-button
                    #'hierarchy-examples-fs-labelfn (lambda (item _) (dired item)))))))
    (switch-to-buffer buffer)))

(defun hierarchy-examples-fs-display-filesystem-tree (&optional folder)
  "Display hierarchy of FOLDER in a tree widget."
  (require 'tree-widget)
  (let* ((hierarchy (hierarchy-examples-fs-build-fs-hierarchy folder)))
    (switch-to-buffer (hierarchy-tree-display hierarchy #'hierarchy-examples-fs-labelfn))))

;; Execute one of the following lines to show the .emacd.d hierarchy
;; in either a tabulated list or a tree widget. This takes around 4
;; seconds on my computer.
;;
;; (hierarchy-examples-fs-display-filesystem "~/.emacs.d")
;;
;; (hierarchy-examples-fs-display-filesystem-tree "~/neo")
