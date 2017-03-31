;;; dx-api.el --- Low-level API wrappers for the DNAnexus Platform

;; Copyright (C) 2012-2014 DNAnexus, Inc.

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


(defun dx-api-analysis-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-analysis-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-analysis-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-analysis-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-analysis-terminate (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/terminate") input callback cbargs))

(defun dx-api-app-add-authorized-users (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/addAuthorizedUsers") input callback cbargs))
(defun dx-api-app-add-authorized-users-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/addAuthorizedUsers") input callback cbargs))

(defun dx-api-app-add-categories (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/addCategories") input callback cbargs))
(defun dx-api-app-add-categories-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/addCategories") input callback cbargs))

(defun dx-api-app-add-developers (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/addDevelopers") input callback cbargs))
(defun dx-api-app-add-developers-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/addDevelopers") input callback cbargs))

(defun dx-api-app-add-tags (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/addTags") input callback cbargs))
(defun dx-api-app-add-tags-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/addTags") input callback cbargs))

(defun dx-api-app-delete (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/delete") input callback cbargs))
(defun dx-api-app-delete-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/delete") input callback cbargs))

(defun dx-api-app-describe (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/describe") input callback cbargs))
(defun dx-api-app-describe-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/describe") input callback cbargs))

(defun dx-api-app-get (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/get") input callback cbargs))
(defun dx-api-app-get-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/get") input callback cbargs))

(defun dx-api-app-install (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/install") input callback cbargs))
(defun dx-api-app-install-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/install") input callback cbargs))

(defun dx-api-app-list-authorized-users (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/listAuthorizedUsers") input callback cbargs))
(defun dx-api-app-list-authorized-users-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/listAuthorizedUsers") input callback cbargs))

(defun dx-api-app-list-categories (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/listCategories") input callback cbargs))
(defun dx-api-app-list-categories-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/listCategories") input callback cbargs))

(defun dx-api-app-list-developers (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/listDevelopers") input callback cbargs))
(defun dx-api-app-list-developers-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/listDevelopers") input callback cbargs))

(defun dx-api-app-publish (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/publish") input callback cbargs))
(defun dx-api-app-publish-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/publish") input callback cbargs))

(defun dx-api-app-remove-authorized-users (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/removeAuthorizedUsers") input callback cbargs))
(defun dx-api-app-remove-authorized-users-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/removeAuthorizedUsers") input callback cbargs))

(defun dx-api-app-remove-categories (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/removeCategories") input callback cbargs))
(defun dx-api-app-remove-categories-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/removeCategories") input callback cbargs))

(defun dx-api-app-remove-developers (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/removeDevelopers") input callback cbargs))
(defun dx-api-app-remove-developers-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/removeDevelopers") input callback cbargs))

(defun dx-api-app-remove-tags (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/removeTags") input callback cbargs))
(defun dx-api-app-remove-tags-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/removeTags") input callback cbargs))

(defun dx-api-app-run (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/run") input callback cbargs))
(defun dx-api-app-run-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/run") input callback cbargs))

(defun dx-api-app-uninstall (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/uninstall") input callback cbargs))
(defun dx-api-app-uninstall-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/uninstall") input callback cbargs))

(defun dx-api-app-update (app-id-or-name input callback &optional cbargs)
  (dx-http-request (concat "/" app-id-or-name "/update") input callback cbargs))
(defun dx-api-app-update-with-alias (app-id alias input callback &optional cbargs)
  (dx-http-request (concat "/" app-id "/" alias "/update") input callback cbargs))

(defun dx-api-app-new (input callback &optional cbargs)
  (dx-http-request "/app/new" input callback cbargs))

(defun dx-api-applet-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-applet-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-applet-get (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/get") input callback cbargs))

(defun dx-api-applet-get-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/getDetails") input callback cbargs))

(defun dx-api-applet-list-projects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listProjects") input callback cbargs))

(defun dx-api-applet-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-applet-rename (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/rename") input callback cbargs))

(defun dx-api-applet-run (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/run") input callback cbargs))

(defun dx-api-applet-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-applet-new (input callback &optional cbargs)
  (dx-http-request "/applet/new" input callback cbargs))

(defun dx-api-container-clone (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/clone") input callback cbargs))

(defun dx-api-container-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-container-destroy (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/destroy") input callback cbargs))

(defun dx-api-container-list-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listFolder") input callback cbargs))

(defun dx-api-container-move (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/move") input callback cbargs))

(defun dx-api-container-new-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/newFolder") input callback cbargs))

(defun dx-api-container-remove-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeFolder") input callback cbargs))

(defun dx-api-container-remove-objects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeObjects") input callback cbargs))

(defun dx-api-container-rename-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/renameFolder") input callback cbargs))

(defun dx-api-file-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-file-add-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTypes") input callback cbargs))

(defun dx-api-file-close (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/close") input callback cbargs))

(defun dx-api-file-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-file-download (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/download") input callback cbargs))

(defun dx-api-file-get-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/getDetails") input callback cbargs))

(defun dx-api-file-list-projects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listProjects") input callback cbargs))

(defun dx-api-file-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-file-remove-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTypes") input callback cbargs))

(defun dx-api-file-rename (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/rename") input callback cbargs))

(defun dx-api-file-set-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setDetails") input callback cbargs))

(defun dx-api-file-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-file-set-visibility (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setVisibility") input callback cbargs))

(defun dx-api-file-upload (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/upload") input callback cbargs))

(defun dx-api-file-new (input callback &optional cbargs)
  (dx-http-request "/file/new" input callback cbargs))

(defun dx-api-gtable-add-rows (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addRows") input callback cbargs))

(defun dx-api-gtable-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-gtable-add-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTypes") input callback cbargs))

(defun dx-api-gtable-close (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/close") input callback cbargs))

(defun dx-api-gtable-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-gtable-get (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/get") input callback cbargs))

(defun dx-api-gtable-get-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/getDetails") input callback cbargs))

(defun dx-api-gtable-list-projects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listProjects") input callback cbargs))

(defun dx-api-gtable-next-part (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/nextPart") input callback cbargs))

(defun dx-api-gtable-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-gtable-remove-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTypes") input callback cbargs))

(defun dx-api-gtable-rename (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/rename") input callback cbargs))

(defun dx-api-gtable-set-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setDetails") input callback cbargs))

(defun dx-api-gtable-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-gtable-set-visibility (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setVisibility") input callback cbargs))

(defun dx-api-gtable-new (input callback &optional cbargs)
  (dx-http-request "/gtable/new" input callback cbargs))

(defun dx-api-job-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-job-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-job-get-log (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/getLog") input callback cbargs))

(defun dx-api-job-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-job-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-job-terminate (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/terminate") input callback cbargs))

(defun dx-api-job-new (input callback &optional cbargs)
  (dx-http-request "/job/new" input callback cbargs))

(defun dx-api-notifications-get (input callback &optional cbargs)
  (dx-http-request "/notifications/get" input callback cbargs))

(defun dx-api-notifications-mark-read (input callback &optional cbargs)
  (dx-http-request "/notifications/markRead" input callback cbargs))

(defun dx-api-org-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-org-find-members (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/findMembers") input callback cbargs))

(defun dx-api-org-find-projects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/findProjects") input callback cbargs))

(defun dx-api-org-find-apps (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/findApps") input callback cbargs))

(defun dx-api-org-invite (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/invite") input callback cbargs))

(defun dx-api-org-remove-member (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeMember") input callback cbargs))

(defun dx-api-org-set-member-access (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setMemberAccess") input callback cbargs))

(defun dx-api-org-update (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/update") input callback cbargs))

(defun dx-api-org-new (input callback &optional cbargs)
  (dx-http-request "/org/new" input callback cbargs))

(defun dx-api-project-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-project-clone (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/clone") input callback cbargs))

(defun dx-api-project-decrease-permissions (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/decreasePermissions") input callback cbargs))

(defun dx-api-project-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-project-destroy (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/destroy") input callback cbargs))

(defun dx-api-project-invite (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/invite") input callback cbargs))

(defun dx-api-project-leave (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/leave") input callback cbargs))

(defun dx-api-project-list-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listFolder") input callback cbargs))

(defun dx-api-project-move (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/move") input callback cbargs))

(defun dx-api-project-new-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/newFolder") input callback cbargs))

(defun dx-api-project-remove-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeFolder") input callback cbargs))

(defun dx-api-project-remove-objects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeObjects") input callback cbargs))

(defun dx-api-project-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-project-rename-folder (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/renameFolder") input callback cbargs))

(defun dx-api-project-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-project-transfer (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/transfer") input callback cbargs))

(defun dx-api-project-update (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/update") input callback cbargs))

(defun dx-api-project-update-sponsorship (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/updateSponsorship") input callback cbargs))

(defun dx-api-project-new (input callback &optional cbargs)
  (dx-http-request "/project/new" input callback cbargs))

(defun dx-api-record-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-record-add-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTypes") input callback cbargs))

(defun dx-api-record-close (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/close") input callback cbargs))

(defun dx-api-record-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-record-get-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/getDetails") input callback cbargs))

(defun dx-api-record-list-projects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listProjects") input callback cbargs))

(defun dx-api-record-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-record-remove-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTypes") input callback cbargs))

(defun dx-api-record-rename (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/rename") input callback cbargs))

(defun dx-api-record-set-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setDetails") input callback cbargs))

(defun dx-api-record-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-record-set-visibility (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setVisibility") input callback cbargs))

(defun dx-api-record-new (input callback &optional cbargs)
  (dx-http-request "/record/new" input callback cbargs))

(defun dx-api-system-describe-data-objects (input callback &optional cbargs)
  (dx-http-request "/system/describeDataObjects" input callback cbargs))

(defun dx-api-system-describe-executions (input callback &optional cbargs)
  (dx-http-request "/system/describeExecutions" input callback cbargs))

(defun dx-api-system-describe-projects (input callback &optional cbargs)
  (dx-http-request "/system/describeProjects" input callback cbargs))

(defun dx-api-system-find-affiliates (input callback &optional cbargs)
  (dx-http-request "/system/findAffiliates" input callback cbargs))

(defun dx-api-system-find-apps (input callback &optional cbargs)
  (dx-http-request "/system/findApps" input callback cbargs))

(defun dx-api-system-find-data-objects (input callback &optional cbargs)
  (dx-http-request "/system/findDataObjects" input callback cbargs))

(defun dx-api-system-resolve-data-objects (input callback &optional cbargs)
  (dx-http-request "/system/resolveDataObjects" input callback cbargs))

(defun dx-api-system-find-executions (input callback &optional cbargs)
  (dx-http-request "/system/findExecutions" input callback cbargs))

(defun dx-api-system-find-analyses (input callback &optional cbargs)
  (dx-http-request "/system/findAnalyses" input callback cbargs))

(defun dx-api-system-find-jobs (input callback &optional cbargs)
  (dx-http-request "/system/findJobs" input callback cbargs))

(defun dx-api-system-find-projects (input callback &optional cbargs)
  (dx-http-request "/system/findProjects" input callback cbargs))

(defun dx-api-system-find-users (input callback &optional cbargs)
  (dx-http-request "/system/findUsers" input callback cbargs))

(defun dx-api-system-find-project-members (input callback &optional cbargs)
  (dx-http-request "/system/findProjectMembers" input callback cbargs))

(defun dx-api-system-find-orgs (input callback &optional cbargs)
  (dx-http-request "/system/findOrgs" input callback cbargs))

(defun dx-api-system-global-search (input callback &optional cbargs)
  (dx-http-request "/system/globalSearch" input callback cbargs))

(defun dx-api-system-greet (input callback &optional cbargs)
  (dx-http-request "/system/greet" input callback cbargs))

(defun dx-api-system-headers (input callback &optional cbargs)
  (dx-http-request "/system/headers" input callback cbargs))

(defun dx-api-system-shorten-url (input callback &optional cbargs)
  (dx-http-request "/system/shortenURL" input callback cbargs))

(defun dx-api-system-whoami (input callback &optional cbargs)
  (dx-http-request "/system/whoami" input callback cbargs))

(defun dx-api-user-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-user-update (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/update") input callback cbargs))

(defun dx-api-workflow-add-stage (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addStage") input callback cbargs))

(defun dx-api-workflow-add-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTags") input callback cbargs))

(defun dx-api-workflow-add-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/addTypes") input callback cbargs))

(defun dx-api-workflow-close (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/close") input callback cbargs))

(defun dx-api-workflow-describe (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/describe") input callback cbargs))

(defun dx-api-workflow-dry-run (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/dryRun") input callback cbargs))

(defun dx-api-workflow-get-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/getDetails") input callback cbargs))

(defun dx-api-workflow-is-stage-compatible (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/isStageCompatible") input callback cbargs))

(defun dx-api-workflow-list-projects (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/listProjects") input callback cbargs))

(defun dx-api-workflow-move-stage (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/moveStage") input callback cbargs))

(defun dx-api-workflow-overwrite (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/overwrite") input callback cbargs))

(defun dx-api-workflow-remove-stage (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeStage") input callback cbargs))

(defun dx-api-workflow-remove-tags (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTags") input callback cbargs))

(defun dx-api-workflow-remove-types (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/removeTypes") input callback cbargs))

(defun dx-api-workflow-rename (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/rename") input callback cbargs))

(defun dx-api-workflow-run (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/run") input callback cbargs))

(defun dx-api-workflow-set-details (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setDetails") input callback cbargs))

(defun dx-api-workflow-set-properties (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setProperties") input callback cbargs))

(defun dx-api-workflow-set-stage-inputs (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setStageInputs") input callback cbargs))

(defun dx-api-workflow-set-visibility (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/setVisibility") input callback cbargs))

(defun dx-api-workflow-update (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/update") input callback cbargs))

(defun dx-api-workflow-update-stage-executable (object-id input callback &optional cbargs)
  (dx-http-request (concat "/" object-id "/updateStageExecutable") input callback cbargs))

(defun dx-api-workflow-new (input callback &optional cbargs)
  (dx-http-request "/workflow/new" input callback cbargs))


(provide 'dx-api)
;;; dx-api.el ends here

