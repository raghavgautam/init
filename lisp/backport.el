;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements.  See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership.  The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License.  You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Author: Raghav Kumar Gautam <raghavgautam@gmail.com>

(if (not (fboundp 'defvar-local))
    (progn
      (defmacro declare (&rest _specs)
	"Do not evaluate any arguments, and return nil.
If a `declare' form appears as the first form in the body of a
`defun' or `defmacro' form, SPECS specifies various additional
information about the function or macro; these go into effect
during the evaluation of the `defun' or `defmacro' form.

The possible values of SPECS are specified by
`defun-declarations-alist' and `macro-declarations-alist'.

For more information, see info node `(elisp)Declare Form'."
	;; FIXME: edebug spec should pay attention to defun-declarations-alist.
	nil)
      (defmacro defvar-local (var val &optional docstring)
  "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
  (declare (debug defvar) (doc-string 3))
  ;; Can't use backquote here, it's too early in the bootstrap.
  (list 'progn (list 'defvar var val docstring)
        (list 'make-variable-buffer-local (list 'quote var))))))

(if (not (fboundp 'read-only-mode))
    (defun read-only-mode ()
      "make the buffer readonly"
      (setq buffer-read-only (not buffer-read-only))))

(provide 'backport)
