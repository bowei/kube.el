;;; kube.el --- Kubernetes kubectl for emacs -*- lexical-binding: t; -*-
;;;
;;; Author: Bowei Du <bowei@google.com>
;;;
;;; Package-Requires: ((emacs "24"))
;;; Version: 1.0
;;; Keywords: kubernetes, utility
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; Copyright 2016 The Kubernetes Authors.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; Commentary:
;;;
;;; M-x kube-status launches the status page
;;;
(require 'cl)
(require 'ert)
(require 'json)

;; ---------------------------------------------------------------------------
;; Customize
(defgroup kube nil
  "kubectl interface"
  :group 'extensions
  :group 'convenience
  :link '(emacs-library-link :tag "Lisp File" "kube.el"))

(defcustom kube-kubectl-exec "kubectl"
  "Location of the kubectl executable"
  :type 'string
  :group 'kube)

(defcustom kube-gcloud-exec "gcloud"
  "Location of the gcloud executable"
  :type 'string
  :group 'kube)

(defcustom kube-terminal "/usr/bin/gnome-terminal"
  "Location of gnome terminal"
  :type 'string
  :group 'kube)

;; ---------------------------------------------------------------------------
;; Mode
(defvar kube-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Kubel")))
    (define-key map "?" 'kube--debug-at-point)
    (define-key map "L" 'kube--stream-logs)
    (define-key map "d" 'kube--delete-obj)
    (define-key map "g" 'kube--refresh-status)
    (define-key map "l" 'kube--show-logs)
    (define-key map "n" 'kube--select-namespace)
    (define-key map "r" 'kube--select-resource)
    (define-key map "t" 'kube--launch-terminal)
    (define-key map "x" 'kube--execute-cmd)
    (define-key map (kbd "RET") 'kube--view)
    map)
  "Keymap for kub.el")

(define-derived-mode kube-mode nil "Kubel"
  "Major mode for kub.el

\\{kube-mode-map}

Turning on kube mode runs the normal hook `kube-mode-hook'."
  (use-local-map kube-mode-map))

;; ---------------------------------------------------------------------------
;; Local vars
(defvar kube--namespace
  nil
  "Current namespace used for status. nil means no namespace if applicable")

(defvar kube--resource
  "pod"
  "Current resource used for status, string")

(defvar kube--status
  nil
  "Current status that was loaded")

(defvar kube--filter-expr
  nil
  "Filter expression to use for objects that are displayed. This
is either a regular expression or a column specific regexes, with
columns denoted by COLNAME: REGEXP")

;; ---------------------------------------------------------------------------
;; Constants
(defconst kube-status-help
  "----
RET  : show object
 d   : delete object
 g   : refresh
 l/L : show/stream logs
 n   : select namespace
 r   : select resource
 t   : launch terminal")

(defconst kube--res-ops
  (let ((table (make-hash-table))
	(entries
	 '(((name . "delete-obj"))
	   ((name . "execute-cmd"))
	   ((name . "get-details"))
	   ((name . "show-logs")))))
    (mapcar (lambda (x)
	      (puthash (intern (alist-get 'name x)) x table)) entries)
    table)
  "A list of all valid operations")

;; ---------------------------------------------------------------------------
;; Structs
(defstruct kube-status
  "Status page information"
  kind					; string
  namespace                             ; string or nil
  columns                               ; kube-col
  objs)					; seq of kube-obj

(defstruct kube-res
  "Metadata about a resource type"
  kind		      ; resource type (e.g. "pod", "node")
  columns	      ; kube-col description
  (has-namespace t)   ; whether or not resource is part of a namespace
  (ops '(delete-obj get-details))) ; valid operations to perform

(defstruct kube-col
  "Column information"
  name				      ; name of the column
  width				      ; width of the column (computed)
  json-path			      ; path to extract value
  (format "s")			      ; format character
  (alignment "l")		      ; alignment (left,right)
  (extra-attribs '()))		      ; additional face properties

(defstruct kube-obj-list
  "A list of objects"
  items)

(defstruct kube-obj
  "An entry for object (e.g. pod, namespace)"
  kind
  namespace
  name
  val)

;; ---------------------------------------------------------------------------
;; Public
(defun kube-status (prefix)
  "Get the status of a Kubernetes cluster. If run with a prefix
command, will prompt for resource type and namespace. Empty
namespace denotes --all-namespaces."
  (interactive "P")
  (let ((resource
	 (if prefix
	     (completing-read "Resource: " kube-res-types nil t nil nil "pod")
	   "pod"))
	(namespace
	 (if prefix
	     (let ((ns (read-from-minibuffer "Namespace: ")))
	       (if (equal ns "") nil ns))
	   nil)))
    (kube--status-impl resource namespace)))

;; ---------------------------------------------------------------------------
;; Private
(defun kube--debug-at-point ()
  (interactive)
  (message "%S" (text-properties-at (point))))

(defun kube--delete-obj ()
  (interactive)
  (let ((obj (kube--is-valid-obj-op kube--resource 'delete-obj)))
    (if obj
	(let* ((json (kube-obj-val obj))
	       (namespace (kube--json-path '(metadata namespace) json))
	       (name (kube--json-path '(metadata name) json))
	       (yn (yes-or-no-p (format "Delete %s %s %s? "
					kube--resource namespace name)))
	       (cmdline (nconc (list kube-kubectl-exec "delete")
			       (if namespace
				   (list "--namespace" namespace) nil)
			       (list (format "%s/%s" kube--resource name)))))
	  (message "Delete %S" cmdline)
	  (make-process :name "kubectl-delete"
			:buffer nil
			:command cmdline)))))

(defun kube--refresh-status ()
  (interactive)
  (kube--status-impl)
  (message "*kube-status* refreshed"))

(defun kube--stream-logs ()
  (interactive)
  (let ((obj (kube--is-valid-obj-op kube--resource 'show-logs)))
    (if obj
	(multiple-value-bind
	    (namespace name container)
	    (kube--prompt-container-from-pod obj)
	  (kube--logs namespace name container "--tail" "10" "-f")))))

(defun kube--show-logs (prefix)
  (interactive "P")
  (let ((obj (kube--is-valid-obj-op kube--resource 'show-logs)))
    (if obj
	(multiple-value-bind
	    (namespace name container)
	    (kube--prompt-container-from-pod obj)
	  (let ((tail-count
		 (if prefix (read-from-minibuffer "Lines: " "1000") "1000")))
	    (kube--logs namespace name container "--tail" tail-count))))))

(defun kube--logs (namespace name container &rest args)
  "Show logs with the given arguments"
  (let* ((buffer-name (format "*kube-logs %s %s %s*" namespace name container))
	 (cmdline (nconc (list "kube-logs" buffer-name kube-kubectl-exec "logs"
			       "--namespace" namespace "-c" container)
			 args
			 (list name))))
    (message "Running %S" cmdline)
    (apply 'start-process cmdline)
    (switch-to-buffer buffer-name)))

(defun kube--select-namespace ()
  (interactive)
  (let ((ns (read-from-minibuffer "Namespace: ")))
    (setq kube--namespace (if (equal ns "") nil ns))
    (kube--status-impl)))

(defun kube--select-resource ()
  (interactive)
  (kube--status-impl
   (completing-read "Resource: " kube-res-types nil t nil nil "pod")))

(defun kube--launch-terminal ()
  (interactive)
  (cond ((equal kube--resource "pod") (kube--launch-terminal-in-pod))
	((equal kube--resource "node") (kube--launch-terminal-in-node))
	(t (message "Cannot launch terminal in resource %s" kube--resource))))

(defun kube--execute-cmd ()
  (interactive)
  (cond ((equal kube--resource "pod") (kube--launch-terminal-in-pod))
	((equal kube--resource "node") (kube--launch-terminal-in-node))
	(t (message "Cannot launch terminal in resource %s" kube--resource))))

(defun kube--view ()
  (interactive)
  (if (kube--obj-at-point)
      (let* ((obj (kube--obj-at-point))
	     (resource kube--resource)
	     (namespace (kube--json-path '(metadata namespace) (kube-obj-val obj)))
	     (name (kube--json-path '(metadata name) (kube-obj-val obj)))
	     (cmdline (nconc (list kube-kubectl-exec "get")
			     (if namespace (list "--namespace" namespace) '())
			     (list (format "%s/%s" resource name)
				   "-o" "yaml")))
	     (buffer-name (format "*kubectl view %s %s %s*"
				  resource namespace name)))
	(make-process :name "kubectl-view"
		      :buffer buffer-name
		      :command cmdline
		      :sentinel (lambda (process event)
				  (message
				   "kubectl-view: %s"
				   (replace-regexp-in-string "\n$" "" event))
				  (switch-to-buffer buffer-name)
				  (yaml-mode)
				  (goto-char 0)
				  (setq buffer-undo-list '()))))))

(defun kube--launch-terminal-in-pod ()
  "Launch a terminal session if the `point' is currently on a pod"
  (if (kube--obj-at-point)
      (multiple-value-bind
	  (namespace name container)
	  (kube--prompt-container-from-pod (kube--obj-at-point))
	(let* ((cmd (concat kube-kubectl-exec " exec -it --namespace "
			    namespace " -c " container " " name " sh"))
	       (cmdline (list kube-terminal "-t"
			      (format "pod %s %s %s" namespace name container)
			      "-x" "bash"
			      "-c" cmd)))
	  (message "Launching terminal in %s %s %s" namespace name container)
	  (make-process :name "kubectl"
			:buffer nil
			:command cmdline)))))

(defun kube--make-ssh-cmdline (provider-url)
  (cond ((string-prefix-p "gce://" provider-url)
	 (save-match-data
	   (string-match "gce://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)"
			 provider-url)
	   (let ((project (substring provider-url
				     (match-beginning 1) (match-end 1)))
		 (zone (substring provider-url
				  (match-beginning 2) (match-end 2)))
		 (host (substring provider-url
				  (match-beginning 3) (match-end 3))))
	     (list kube-terminal "-t" (format "node %s %s" zone host) "-x"
		   kube-gcloud-exec "compute" "ssh" "--zone" zone host))))
	(t (message "Unsupported provider: %s" provider-url))))

(defun kube--launch-terminal-in-node ()
  "Launch a terminal session if the `point' is currently on a node"
  (if (kube--obj-at-point)
      (let* ((obj (kube--obj-at-point))
	     (json (kube-obj-val obj))
	     (external-ip (kube--res-node-external-ip json))
	     (cmdline (kube--make-ssh-cmdline
		       (kube--json-path '(spec providerID) json))))
	(message "Launching terminal: %S" cmdline)
	(make-process :name "kube-ssh" :buffer nil :command cmdline))))

(defun kube--obj-at-point ()
  (plist-get (text-properties-at (point)) 'kube-obj))

(defun kube--is-valid-obj-op (res op)
  "Return non-nil if op is valid for res and the point is over a valid object"
  (let* ((res (gethash (intern res) kube-res-metadata))
	 (ops (kube-res-ops res)))
    (and (seq-find (lambda (x) (equal op x)) ops)
	 (kube--obj-at-point))))

(defun kube--exec-command (args)
  (message "Running kubectl%s" (loop for x in args concat (concat " " x)))
  (let* ((buffer (get-buffer-create "*kube-command-output*"))
	 (process-args
	  (nconc (list kube-kubectl-exec nil buffer nil) args)))
    (with-current-buffer buffer
      (erase-buffer)
      (if (not (= (apply 'call-process process-args) 0))
	  (error "Error executing %S" args)))))

(defun kube--exec-get (resource namespace has-namespace)
  (let ((cmd (nconc (list "get" resource "-o" "json")
		    (cond ((not has-namespace) nil)
			  (namespace (list "--namespace" namespace))
			  (t (list "--all-namespaces"))))))
    (kube--exec-command cmd)))

(defun kube--status-impl (&optional resource namespace)
  "Does the actual kube status update"
  (let* ((resource (or resource kube--resource))
	 (namespace (or namespace kube--namespace))
	 (res-metadata (gethash (intern resource) kube-res-metadata)))
    (message "%S %S" resource namespace)
    (if (not (kube-res-has-namespace res-metadata))
	(setq namespace nil))
    (kube--exec-get resource namespace (kube-res-has-namespace res-metadata))
    (let ((buffer (get-buffer-create "*kube-status*"))
	  (status (make-kube-status
		   :kind resource
		   :namespace namespace
		   :columns (kube-res-columns
			     (gethash (intern resource) kube-res-metadata))
		   :objs (kube--json-to-obj
			  (kube--buffer-to-json "*kube-command-output*")))))
      (switch-to-buffer buffer)
      ;; These variables are buffer local.
      (setq kube--namespace namespace)
      (setq kube--resource resource)
      (setq kube--status status)
      (kube--render-status buffer status)
      (kube-mode)
      (goto-char 0))))

(defun kube--insert-with-properties (fmt properties &rest args)
  (let ((start (point)))
    (insert (apply 'format (cons fmt args)))
    (add-text-properties start (point) properties)))

(defun kube--json-to-status (json)
  (let* ((kind (kube--json-path '(kind) json))
	 (namespace (kube--json-path '(metadata namespace) json))
	 (name (kube--json-path '(metadata name) json))
	 (metadata (gethash (intern kind) kube-kind-metadata))
	 (columns (plist-get metadata 'columns)))
    (make-kube-object
     :kind kind
     :namespace namespace
     :name name
     :attributes (loop
		  for col in columns
		  collect (list (intern (kube-col-name col))
				(kube--json-path
				 (kube-col-json-path col) json))))))

(defun kube--prompt-container-from-pod (obj)
  "Prompt for container from a pod object, resolves to ns, name, container"
  (let* ((json (kube-obj-val obj))
	 (namespace (kube--json-path '(metadata namespace) json))
	 (name (kube--json-path '(metadata name) json))
	 (container-status (kube--json-path '(status containerStatuses) json))
	 (container-names
	  (seq-map (lambda (x) (alist-get 'name x)) container-status))
	 (container (completing-read "Container: " container-names nil t
				     (car container-names) nil
				     (car container-names))))
    (values namespace name container)))

(defun kube--render-status (buffer status)
  "Render the status page."
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (if kube--resource
	(progn
	  (kube--insert-with-properties
	   "Resource:  " '(kube-action kind face bold))
	  (kube--insert-with-properties
	   "%s\n" '(kube-action kind) kube--resource)))
    (if kube--namespace
	(progn
	  (kube--insert-with-properties
	   "Namespace: " '(kube-action kind face bold))
	  (kube--insert-with-properties
	   "%s\n" '(kube-action kind) kube--namespace)))
    (if kube--filter-expr
	(progn
	  (kube--insert-with-properties
	   "Filter:    " '(kube-action kind face bold))
	  (kube--insert-with-properties
	   "%s\n" '(kube-action kind) kube--filter-expr)))
    (insert "\n")
    (kube--render-table
     (kube--objs-to-table (kube-obj-list-items (kube-status-objs status))))
    (insert "\n")
    (insert kube-status-help)
    (setq buffer-read-only t)))

(defun kube--objs-to-table (objs)
  (if (= (length objs) 0)
      '((row ((val "no objects"))))
    (let* ((kind (kube-obj-kind (elt objs 0)))
	   (metadata (gethash kind kube-res-metadata)))
      (cons
       (list 'row
	     (loop for col in (kube-res-columns metadata)
		   collect (list 'val (kube-col-name col)
				 'prop '(face (bold underline)))))
       (loop for obj in objs
	     collect (kube--obj-to-row obj metadata))))))

(defun kube--obj-to-row (obj metadata)
  (list 'row
	(let ((cols (kube-res-columns metadata)))
	  (loop
	   for col in cols
	   collect (let ((val (kube--json-path
			       (kube-col-json-path col) (kube-obj-val obj)))
			 (prop (nconc (list 'kube-obj obj)
				      (copy-seq (kube-col-extra-attribs col)))))
		     (list 'val val 'prop prop))))))

(defun kube--render-table (table)
  "Render table given a matrix of cells. A table consists of a
  list of rows, each containing a list of cells.

((row ((val 1) (val 2))
      prop (face bold))
 (row ((val 3 align left) (val 4)))
 )"
    (let* ((cells-per-row (length (plist-get (elt table 0) 'row)))
	 (widths (make-vector cells-per-row 0)))
    ; calculate widths
    (loop for row in table
	  do (loop for i from 0 to (- (length (plist-get row 'row)) 1)
		   do (let* ((cell (elt (plist-get row 'row) i))
			     (width (length (plist-get cell 'val)))
			     (minwidth (or (plist-get cell 'minwidth) 0)))
			(setf (elt widths i) (max width minwidth (elt widths i)))
			)))
    ; render cells
    (loop for row in table
	  do (let ((start (point)))
	       (loop for i from 0 to (- (length (plist-get row 'row)) 1)
		     do (let* ((start (point))
			       (cell (elt (plist-get row 'row) i))
			       (align (plist-get cell 'align))
			       (fmt (format "%%%s%ds  "
					    (cond ((equal align 'right) "")
                                                  (t "-"))
					    (elt widths i))))
			  (insert (format fmt (plist-get cell 'val)))
			  (if (plist-get cell 'prop)
			      (add-text-properties start (point) (plist-get cell 'prop)))))
	       (if (plist-get row 'prop)
		   (add-text-properties start (point) (plist-get row 'prop)))
	       (insert "\n")))))

(defun kube--json-to-obj (json)
  "Create a kube object from an alist JSON"
  (let ((kind (alist-get 'kind json)))
    (if (equal kind "List")
        (let ((objects (alist-get 'items json)))
          (make-kube-obj-list
           :items (loop for idx from 0 to (- (length objects) 1)
                        collect (kube--json-to-obj (elt objects idx)))))
      (let ((metadata (alist-get 'metadata json)))
        (make-kube-obj
         :kind (intern (downcase (alist-get 'kind json)))
         :namespace (alist-get 'namespace metadata)
         :name (alist-get 'name metadata)
         :val json)))))

(defun kube--buffer-to-json (buffer)
  "Read a buffer and turn it to JSON"
  (with-current-buffer buffer
    (goto-char 0)
    (json-read)))

(defun kube--json-path (path json)
  "Return the element from json named by path. JSON should be
represented in encoded in alist format."
  (let ((ret json))
    (dolist (elt path ret)
      (cond ((functionp elt) (setq ret (apply elt (list ret))))
            ((numberp elt) (setq ret (elt ret elt)))
	    ((symbolp elt) (setq ret (alist-get elt ret)))
            (t (error "invalid type for JSON path element"))))))

;; ---------------------------------------------------------------------------
;; JSON extraction functions
(defun kube--get-pred-seq (pred extractor seq)
  (cl-some
   (lambda (x)
     (if (funcall pred x) (funcall extractor x) nil))
   seq))

(defun kube--get-kv-seq (key key-value field seq)
  (kube--get-pred-seq
   (lambda (x) (equal (alist-get key x) key-value))
   (lambda (x) (alist-get field x))
   seq))

;; ---------------------------------------------------------------------------
;; Resource definitions
(defconst kube--res-clusters
  (make-kube-res
   :kind 'clusters
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-componentstatus
  (make-kube-res
   :kind 'componentstatus
   :columns (list (make-kube-col :name "name" :json-path '(metadata name)))
   :has-namespace nil))

(defconst kube--res-configmap
  (make-kube-res
   :kind 'configmap
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-daemonset
  (make-kube-res
   :kind 'daemonset
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-deployment
  (make-kube-res
   :kind 'deployment
   :columns (list (make-kube-col :name "namespace"
				 :json-path '(metadata namespace)
				 :extra-attribs '(face italic))
		  (make-kube-col :name "name"
				 :json-path '(metadata name))
		  (make-kube-col :name "Rep"
				 :json-path (list 'spec 'replicas (lambda (x) (format "%s" x))))
		  (make-kube-col :name "Upd"
				 :json-path (list 'status 'updatedReplicas (lambda (x) (format "%s" x))))
		  (make-kube-col :name "Gen"
				 :json-path (list 'status 'observedGeneration (lambda (x) (format "%s" x)))))))

(defconst kube--res-endpoints
  (make-kube-res
   :kind 'endpoints
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-event
  (make-kube-res
   :kind 'event
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-horizontalpodautoscaler
  (make-kube-res
   :kind 'horizontalpodautoscaler
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-ingress
  (make-kube-res
   :kind 'ingress
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-job
  (make-kube-res
   :kind 'job
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-limitrange
  (make-kube-res
   :kind 'limitrange
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defun kube--res-namespace-age (ts)
  (kube--human-duration
   (round (- (float-time) (float-time (date-to-time ts))))))

(defconst kube--res-namespace
  (make-kube-res
   :kind 'namespace
   :columns (list (make-kube-col :name "name"
				 :json-path '(metadata name))
		  (make-kube-col :name "age"
				 :json-path (list 'metadata 'creationTimestamp
						  #'kube--res-namespace-age)))
   :ops '(delete-obj get-details)))

(defconst kube--res-networkpolicy
  (make-kube-res
   :kind 'networkpolicy
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defun kube--res-node-external-ip (json)
  (kube--json-path
   (list 'status 'addresses
	 (lambda (x) (kube--get-kv-seq 'type "ExternalIP" 'address x)))
   json))

(defconst kube--res-node
  (make-kube-res
   :kind 'node
   :columns (list (make-kube-col
		   :name "name"
		   :json-path '(metadata name)
		   :extra-attribs '())
		  (make-kube-col :name "pod cidr" :json-path '(spec podCIDR))
		  (make-kube-col
		   :name "S"
		   :json-path '(spec unschedulable (lambda (x) (if x "n" "y"))))
		  (make-kube-col
		   :name "ip"
		   :json-path (list 'status 'addresses
				    (lambda (x)
				      (kube--get-kv-seq 'type "InternalIP" 'address x))))
		  (make-kube-col
		   :name "external ip"
		   :json-path (list 'status 'addresses
				    (lambda (x)
				      (kube--get-kv-seq 'type "ExternalIP" 'address x)))))
   :has-namespace nil
   :ops '(execute-cmd get-details)))

(defconst kube--res-persistentvolumeclaim
  (make-kube-res
   :kind 'persistentvolumeclaim
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-persistentvolume
  (make-kube-res
   :kind 'persistentvolume
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defun kube--res-pod-restarts (statuses)
  (number-to-string
   (seq-reduce #'+ (seq-map (lambda (x) (alist-get 'restartCount x)) statuses) 0)))

(defun kube--human-duration (seconds)
  (cond ((< seconds 60) (format "%02d" seconds))
	((< seconds (* 60 60))
	 (concat (format "%02d:" (/ seconds 60))
		 (kube--human-duration (% seconds 60))))
	((< seconds (* 24 60 60))
	 (concat (format "%02d:" (/ seconds (* 60 60)))
		 (kube--human-duration (% seconds (* 60 60)))))
	(t (concat (format "%d " (/ seconds (* 24 60 60)))
		   (kube--human-duration (% seconds (* 60 60 24)))))))

(defun kube--res-pod-age (ts)
  (kube--human-duration
   (round (- (float-time) (float-time (date-to-time ts))))))

(defun kube--join-string (l sep)
  (apply 'concat
	 (cons (format "%s" (first l))
	       (mapcar (lambda (x) (format "%s%s" sep x)) (rest l)))))

(defconst kube--res-pod
  (make-kube-res
   :kind 'pod
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace)
				 :extra-attribs '(face italic))
		  (make-kube-col :name "name" :json-path '(metadata name)
				 :extra-attribs '())
		  ;;(make-kube-col :name "status" :json-path '(status phase))
		  (make-kube-col :name "R"
				 :json-path (list 'status 'containerStatuses #'kube--res-pod-restarts))
		  (make-kube-col :name "age"
				 :json-path (list 'status 'startTime #'kube--res-pod-age))
		  (make-kube-col :name "IP" :json-path '(status podIP))
		  (make-kube-col :name "node" :json-path '(spec nodeName)))
   :ops '(delete-obj execute-cmd get-details show-logs)))

(defconst kube--res-podsecuritypolicy
  (make-kube-res
   :kind 'podsecuritypolicy
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-podtemplate
  (make-kube-res
   :kind 'podtemplate
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-replicaset
  (make-kube-res
   :kind 'replicaset
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-replicationcontroller
  (make-kube-res
   :kind 'replicationcontroller
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-resourcequota
  (make-kube-res
   :kind 'resourcequota
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-secret
  (make-kube-res
   :kind 'secret
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube--res-serviceaccount
  (make-kube-res
   :kind 'serviceaccount
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))


(defun kube--res-service-info-format-cluster-ip (spec)
  (let ((port-spec (mapcar
		    (lambda (x) (format ":%s/%s[:%s]"
					(or (kube--json-path '(port) x) "-")
					(or (kube--json-path '(protocol) x) "-")
					(or (kube--json-path '(targetPort) x) "-")))
		    (kube--json-path '(ports) spec))))
    (kube--join-string port-spec ", ")))

(defun kube--res-service-info (spec)
  (let ((type (kube--json-path '(type) spec)))
    (cond ((or (equal type "ClusterIP") (equal type "NodePort"))
	   (kube--res-service-info-format-cluster-ip spec))
	  (t "TODO"))))

(defconst kube--res-service
  (make-kube-res
   :kind 'service
   :columns (list (make-kube-col :name "namespace"
				 :json-path '(metadata namespace)
				 :extra-attribs '(face italic))
		  (make-kube-col :name "name"
				 :json-path '(metadata name))
		  (make-kube-col :name "type"
				 :json-path '(spec type))
		  (make-kube-col :name "IP"
				 :json-path '(spec clusterIP))
		  (make-kube-col :name "ports"
				 :json-path (list 'spec #'kube--res-service-info)))))

(defconst kube--res-storageclass
  (make-kube-res
   :kind 'storageclass
   :columns (list (make-kube-col :name "namespace" :json-path '(metadata namespace))
		  (make-kube-col :name "name" :json-path '(metadata name)))))

(defconst kube-res-metadata
  (let ((table (make-hash-table))
	(resources
	 (list kube--res-clusters
	       kube--res-componentstatus
	       kube--res-configmap
	       kube--res-daemonset
	       kube--res-deployment
	       kube--res-endpoints
	       kube--res-event
	       kube--res-horizontalpodautoscaler
	       kube--res-ingress
	       kube--res-job
	       kube--res-limitrange
	       kube--res-namespace
	       kube--res-networkpolicy
	       kube--res-node
	       kube--res-persistentvolumeclaim
	       kube--res-persistentvolume
	       kube--res-pod
	       kube--res-podsecuritypolicy
	       kube--res-podtemplate
	       kube--res-replicaset
	       kube--res-replicationcontroller
	       kube--res-resourcequota
	       kube--res-secret
	       kube--res-serviceaccount
	       kube--res-service
	       kube--res-storageclass)))
    (mapcar (lambda (x) (puthash (kube-res-kind x) x table))
	    resources)
    table)
  "Metadata for how to render the resources.")

(defconst kube-res-types
  (let ((ret '()))
    (maphash (lambda (k v) (setq ret (cons (symbol-name k) ret)))
	     kube-res-metadata)
    ret)
  "Types of resources that can be `get'")

;; ---------------------------------------------------------------------------
(provide 'kube)
