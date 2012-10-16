Emacs Lisp Platform Bindings
============================

This directory contains elisp bindings to the DNAnexus Platform. To get started
using them:

    (add-to-list 'load-path (expand-file-name "~/path/to/dx-toolkit/src/elisp"))
    (require 'dx)
    (setq dx-auth-token "YOUR_TOKEN")
