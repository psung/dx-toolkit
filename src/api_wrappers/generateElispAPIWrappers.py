#!/usr/bin/env python

import json
import re
import sys

preamble = ''';;; dx-api.el --- Low-level API wrappers for the DNAnexus Platform

;; Copyright (C) 2012 DNAnexus, Inc.

;; Author: Phil Sung <psung@dnanexus.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dx-http)

'''

class_method_template = '''(defun dx-api-{method_name} (input callback &optional cbargs)
  (dx-http-request "{route}" input callback cbargs))
'''

object_method_template = '''(defun dx-api-{method_name} (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/{method_route}") input callback cbargs))
'''

app_object_method_template = '''(defun dx-api-{method_name} (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/{method_route}") input callback cbargs))
(defun dx-api-{method_name}-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/{method_route}") input callback cbargs))
'''

postamble = '''
(provide 'dx-api)
;;; dx-api.el ends here
'''

def lispify_name(name):
    return re.sub("[A-Z]+", lambda m: "-" + m.group(0).lower(), name, 0)

print preamble

for method in json.loads(sys.stdin.read()):
    route, signature, opts = method
    method_name = lispify_name(signature.split("(")[0])
    if (opts['objectMethod']):
        root, oid_route, method_route = route.split("/")
        if oid_route == 'app-xxxx':
            print app_object_method_template.format(method_name=method_name, method_route=method_route)
        else:
            print object_method_template.format(method_name=method_name, method_route=method_route)
    else:
        print class_method_template.format(method_name=method_name, route=route)

print postamble
