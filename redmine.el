(require 'json)

(defun plist-merge (base new)
  "Merges two plists. The keys of the second one will overwrite the old ones."
  (let ((key (car new))
        (val (cadr new))
        (new (cddr new)))
    (while (and key val)
      (setq base (plist-put base key val))
      (setq key (car new))
      (setq val (cadr new))
      (setq new (cddr new)))
    base))

(defvar redmine/host nil
  "The default host of the redmine.")

(defvar redmine/api-key nil
  "The default API key for the redmine")

(defun redmine/get (plist key &rest keys)
  "Execute `plist-get` recursively for `plist`.

Example:
  (setq plist '(:a 3
                :b (:c 12
                    :d (:e 31))))

  (redmine/get plist \"a\")
      ;; => 3
  (redmine/get plist :b)
      ;; => (:c 12 :d (:e 31))
  (redmine/get plist :b :c)
      ;; => 12
  (redmine/get plist :b :d :e)
      ;; => 31
  (redmine/get plist :b :a)
      ;; => nil
  (redmine/get plist :a :c)
      ;; => nil"
  (save-match-data
    (let ((ret (plist-get plist key)))
      (while (and keys ret)
        (if (listp ret)
            (progn
              (setq ret (redmine/get ret (car keys)))
              (setq keys (cdr keys)))
          (setq ret nil)))
      ret)))

;;---------------------------------------------
;; HTTP functions using Emacs URL package
;;---------------------------------------------
(defun redmine/build-url (resource pairs)
  "Creates a URL from a relative url, a list of queries key-value-pairs and
the dynamically bound `redmine-api-key` and `redmine-host` variables."
  (let* ((pairs (cons :key (cons redmine-api-key pairs)))
         (query-string (redmine/api-build-query-string pairs)))
    (concat (s-chop-suffix "/" redmine-host) resource query-string)))

(defun redmine/api-build-query-string (plist)
  (if plist
      (let (query-pairs)
        (while plist
          (let ((key (url-hexify-string
                      (substring (format "%s" (car plist)) 1 nil)))
                (val (url-hexify-string (format "%s" (cadr plist)))))
            (unless (or (null key) (null val))
              (setq query-pairs (cons (format "%s=%s" key val) query-pairs)))
            (setq plist (cddr plist))))
        (concat "?" (s-join "&" query-pairs)))
    ""))

(defun redmine/api-get (resource &rest query)
  "Does an http GET request and returns the body of the response."
  (let ((url (redmine/build-url resource query))
        (url-request-method "GET"))
    (message "GET Request: %s" url)
    (redmine/api-response-body (url-retrieve-synchronously url))))

(defun redmine/api-post (url data)
  "Does an http POST request and returns the body of the response."
  (let ((url-request-method "POST")
        (url-request-data (encode-coding-string data 'utf-8)))
    (message "POST Request: %s" url)
    (url-retrieve-synchronously (concat redmine-host url))))

(defun redmine/api-put (url data)
  "Does an http PUT request and returns the body of the response."
  (let ((url-request-method "PUT")
        (url-request-data (encode-coding-string data 'utf-8)))
    (message "PUT Request: %s" url)
    (url-retrieve-synchronously url)))

(defun redmine/api-delete (url)
  "Does an http DELETE request and returns the body of the response."
  (let ((url-request-method "DELETE"))
    (message "PUT Request: %s" url)
    (url-retrieve-synchronously url)))

(defun redmine/api-response-body (response-buffer)
  "Retrieves the body from a url response buffer."
  (let ((response-body ""))
    (save-excursion
      (switch-to-buffer response-buffer)
      (beginning-of-buffer)
      (when (re-search-forward "^$" nil t)
        (forward-char 1)
        (kill-region (point-min) (point)))
      (setq response-body (url-unhex-string (buffer-string)))
      (kill-buffer))
    response-body))

(defun redmine/api-get-all (resource &rest filters)
  (let* ((issue-list nil)
         (response-object (redmine/api-parse-json
                           (apply #'redmine/api-get resource filters)))
         (offset (redmine/get response-object :offset))
         (limit (redmine/get response-object :limit))
         (total-count (redmine/get response-object :total_count))
         (issue-list (redmine/get response-object :issues)))
    (if (< (+ offset limit) total-count)
        (append issue-list (apply #'redmine/api-get-all
                                  resource
                                  (plist-put filters :offset (+ offset limit))))
      issue-list)))

(defun redmine/api-parse-json (json-string)
  "Parses a JSON string and returns an object. Per default JSON objects are
going to be hashtables and JSON arrays are going to be lists."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (condition-case err
        (json-read-from-string json-string)
      (json-readtable-error
       (message "%s: Could not parse json-string into an object. See %s"
                (error-message-string err) json-string)))))

;;---------------------------------------------
;; API functions to retrieve data from redmine
;;---------------------------------------------
(defun redmine/get-issues (&rest filters)
  "Get a list of issues."
  (let ((filters (plist-merge '(:limit 100 :status_id "open") filters)))
    (apply #'redmine/api-get-all "/issues.json" filters)))

(defun redmine/get-issue (id)
  "Get a specific issue via id."
  (redmine/api-get (concat "/issues/"
                           (format "%s" id)
                           ".json")))

(defun redmine/create-issue (attributes)
  "Create an issue.

You can create an issue with giving each of its parameters or simply passing
an issue object to this function."
  )

(defun redmine/update-issue (id issue)
  "Update an issue. The object passed to this function gets updated."
  )

(defun redmine/delete-issue (id)
  "")

(defun redmine/get-time-entries (issue-id)
  "")

(defun redmine/get-attachments (issue-id)
  "")

(defun redmine/get-relations (issue-id)
  "")

(defun redmine/set-assignee (id assignee_id)
  "")

(defun redmine/add-issue-to-version ()
  "")

(defun redmine/get-project-issues (project &rest filters)
  "Get all issues for a specific project."
  (let ((filters (plist-merge '(:limit 100 :status_id "open") filters)))
    (message "\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\nFILTERS: %S" filters)
    (apply #'redmine/api-get-all
           (format "/projects/%s/issues.json" project)
           filters)))

(defun redmine/get-projects (&rest filters)
  "")

(defun redmine/create-project (project)
  "")

(defun redmine/get-project (project)
  ""
  )

(defun redmine/update-project (project &rest attributes)
  "")

(defun redmine/delete-project (project)
  "")

(defun redmine/create-version (&rest attributes)
  "")

(defun redmine/get-issue-statuses ()
  "")

(defun redmine/get-trackers ()
  "Gets a list of tracker names and their IDs.")

(defun redmine/get-issue-priorities ()
  "Gets a list of issue priorities and their IDs.")

(defun redmine/get-time-entry-activities ()
  "Gets a list of time entry activities and their IDs.")

(defun redmine/get-custom-fields ()
  "Gets a list of available custom fields.")

(defun redmine/get-categories ()
  "")
