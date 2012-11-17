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

(defvar elmine/host nil
  "The default host of the redmine.")

(defvar elmine/api-key nil
  "The default API key for the redmine")

(defun elmine/get (plist key &rest keys)
  "Execute `plist-get` recursively for `plist`.

Example:
  (setq plist '(:a 3
                :b (:c 12
                    :d (:e 31))))

  (elmine/get plist \"a\")
      ;; => 3
  (elmine/get plist :b)
      ;; => (:c 12 :d (:e 31))
  (elmine/get plist :b :c)
      ;; => 12
  (elmine/get plist :b :d :e)
      ;; => 31
  (elmine/get plist :b :a)
      ;; => nil
  (elmine/get plist :a :c)
      ;; => nil"
  (save-match-data
    (let ((ret (plist-get plist key)))
      (while (and keys ret)
        (if (listp ret)
            (progn
              (setq ret (elmine/get ret (car keys)))
              (setq keys (cdr keys)))
          (setq ret nil)))
      ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP functions using Emacs URL package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmine/make-key (string)
  (make-symbol (format ":%s" (s-dashed-words string))))

(defun elmine/ensure-string (object)
  "Return a string representation of OBJECT."
  (cond ((stringp object) object)
        ((keywordp object) (substring (format "%s" object) 1 nil))
        ((symbolp object) (symbol-name object))
        ((numberp object) (number-to-string object))
        (t (pp-to-string object))))

(defun elmine/api-build-query-string (plist)
  "Builds a query string from a given plist."
  (if plist
      (let (query-pairs)
        (while plist
          (let ((key (url-hexify-string (elmine/ensure-string (car plist))))
                (val (url-hexify-string (elmine/ensure-string (cadr plist)))))
            (setq query-pairs (cons (format "%s=%s" key val) query-pairs))
            (setq plist (cddr plist))))
        (concat "?" (s-join "&" query-pairs)))
    ""))

(defun elmine/api-build-url (path params)
  "Creates a URL from a relative PATH, a plist of query PARAMS and
the dynamically bound `redmine-api-key` and `redmine-host` variables."
  (let ((host (s-chop-suffix "/" redmine-host))
        (query-str (elmine/api-build-query-string params)))
    (concat host path query-str)))

(defun elmine/api-raw (method path data params)
  "Perform a raw HTTP request with given METHOD, a relative PATH and a
plist of PARAMS for the query."
  (let ((url (elmine/api-build-url path params))
        (url-request-method method)
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("X-Redmine-API-Key" . ,redmine-api-key)))
        (url-request-data data)
        header-end status header body)
    (message "HTTP %s Request %s with data: %s" method url data)
    (save-excursion
      (switch-to-buffer (url-retrieve-synchronously url))
      (beginning-of-buffer)
      (setq header-end (save-excursion
                         (if (re-search-forward "^$" nil t)
                             (progn
                               (forward-char)
                               (point))
                           (point-max))))
      (when (re-search-forward "^HTTP/\\(1\\.0\\|1\\.1\\) \\([0-9]+\\) \\([A-Za-z ]+\\)$" nil t)
        (setq status (plist-put status :code (string-to-number (match-string 2))))
        (setq status (plist-put status :text (match-string 3))))
      (while (re-search-forward "^\\([^:]+\\): \\(.*\\)" header-end t)
        (setq header (cons (match-string 1) (cons (match-string 2) header))))
      (unless (eq header-end (point-max))
        (setq body (url-unhex-string
                    (buffer-substring header-end (point-max)))))
      (kill-buffer))
    `(:status ,status
      :header ,header
      :body ,body)))

(defun elmine/api-get (element path &rest params)
  "Perform an HTTP GET request and return a PLIST with the request information.
It returns the "
  (let* ((params (if (listp (car params)) (car params) params))
         (response (elmine/api-raw "GET" path nil params))
         (object (elmine/api-decode (plist-get response :body)))
         )
    (if element
        (plist-get object element)
      object)))

(defun elmine/api-post (element object path &rest params)
  "Does an http POST request and returns response status as symbol."
  (let* ((params (if (listp (car params)) (car params) params))
         (data (elmine/api-encode `(,element ,object)))
         (response (elmine/api-raw "POST" path data params))
         (object (elmine/api-decode (plist-get response :body))))
    object))

(defun elmine/api-put (element object path &rest params)
  "Does an http PUT request and returns the response status as symbol.
Either :ok or :unprocessible."
  (let* ((params (if (listp (car params)) (car params) params))
         (data (elmine/api-encode `(,element ,object)))
         (response (elmine/api-raw "PUT" path data params))
         (object (elmine/api-decode (plist-get response :body))))
    object))

(defun elmine/api-delete (path &rest params)
  "Does an http DELETE request and returns the body of the response."
  (let* ((params (if (listp (car params)) (car params) params))
         (response (elmine/api-raw "DELETE" path nil params))
         (status (elmine/get response :status :code)))
    (cond ((eq status 200) t)
          ((eq status 404)
           (signal 'no-such-resource `(:response ,response))))))

(defun elmine/api-response-body (response-buffer)
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

(defun elmine/api-get-all (element path &rest filters)
  (let* ((response-object (apply #'elmine/api-get nil path filters))
         (offset (elmine/get response-object :offset))
         (limit (elmine/get response-object :limit))
         (total-count (elmine/get response-object :total_count))
         (issue-list (elmine/get response-object element)))
    (if (< (+ offset limit) total-count)
        (append issue-list (apply #'elmine/api-get-all element path
                                  (plist-put filters :offset (+ offset limit))))
      issue-list)))

(defun elmine/api-decode (json-string)
  "Parses a JSON string and returns an object. Per default JSON objects are
going to be hashtables and JSON arrays are going to be lists."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (condition-case err
        (json-read-from-string json-string)
      (json-readtable-error
       (message "%s: Could not parse json-string into an object. See %s"
                (error-message-string err) json-string)))))

(defun elmine/api-encode (object)
  "Return a JSON representation from the given object."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (condition-case err
        (json-encode object)
      (json-readtable-error
       (message "%s: Could not encode object into JSON string. See %s"
                (error-message-string err) object)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API functions to retrieve data from redmine ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmine/get-issues (&rest filters)
  "Get a list of issues."
  (let ((filters (plist-merge '(:limit 100 :status_id "open") filters)))
    (apply #'elmine/api-get-all :issues "/issues.json" filters)))

(defun elmine/get-issue (id)
  "Get a specific issue via id."
  (elmine/api-get :issue (format "/issues/%s.json" id)))

(defun elmine/create-issue (&rest params)
  "Create an issue.

You can create an issue with giving each of its parameters or simply passing
an issue object to this function."
  (let ((object (if (listp (car params)) (car params) params)))
    (elmine/api-post :issue object "/issues.json")))

(defun elmine/update-issue (object)
  "Update an issue. The object passed to this function gets updated."
  (elmine/api-put :issue object "/issues.json"))

(defun elmine/delete-issue (id)
  "Deletes an issue with a specific id."
  (elmine/api-delete (format "/issues/%s.json" id)))

(defun elmine/get-time-entries (&rest filters)
  "Gets all time entries.

Returns a list of plists with the following properties:

'((:issue_id \"\"))"
  (let ((filters (plist-merge '(:limit 100) filters)))
    (apply #'elmine/api-get-all :time_entries filters)))

(defun elmine/get-attachments (issue-id)
  "")

(defun elmine/get-relations (issue-id)
  "")

(defun elmine/set-assignee (id assignee_id)
  "")

(defun elmine/add-issue-to-version ()
  "")

(defun elmine/get-project-issues (project &rest filters)
  "Get all issues for a specific project."
  (let ((filters (plist-merge '(:limit 100 :status_id "open") filters)))
    (message "\\n\\n\\n\\n\\n\\n\\n\\n\\n\\n\\nFILTERS: %S" filters)
    (apply #'elmine/api-get-all :issues filters)))

(defun elmine/get-projects (&rest filters)
  "")

(defun elmine/create-project (project)
  "")

(defun elmine/get-project (project)
  "")

(defun elmine/update-project (project &rest attributes)
  "")

(defun elmine/delete-project (project)
  "")

(defun elmine/create-version (&rest attributes)
  "")

(defun elmine/get-issue-statuses ()
  "")

(defun elmine/get-trackers ()
  "Gets a list of tracker names and their IDs.")

(defun elmine/get-issue-priorities ()
  "Gets a list of issue priorities and their IDs.")

(defun elmine/get-time-entry-activities ()
  "Gets a list of time entry activities and their IDs.")

(defun elmine/get-custom-fields ()
  "Gets a list of available custom fields.")

(defun elmine/get-categories ()
  "")
