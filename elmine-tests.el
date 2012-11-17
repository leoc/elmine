;; I´m currently not very confident writing tests. Especially when in
;; the need for mocking URL requests.

;; Currently this file only contains expressions that I ran against my
;; personal redmine to verify the functionality.

;; TODO: Write real tests.

(elmine/api-raw "GET" "/issues/93.json" nil nil)

(elmine/api-raw "POST" "/projects/personal/issues.json"
                "{\"issue\": {\"abc\":\"test\"}}" nil)

(elmine/api-raw "DELETE" "/issues/102.json" nil nil)

(elmine/api-post :issue '(:subject "It´s not blank")
                 "/projects/personal/issues.json")

(elmine/api-get :issue "/issues/93.json")

(elmine/get-issues)

(elmine/get-issue 105)

(elmine/create-issue '(:subject "Some Title" :project_id "personal"))

(elmine/create-issue :subject "Some Other Title" :project_id "personal")

(elmine/update-issue '(:id 105 :subject "Changed Title 2"))

(elmine/delete-issue 101)

(elmine/get-issue-time-entries 4)

(elmine/get-issue-relations 99)

(elmine/get-projects)

(elmine/get-project "personal")

(elmine/create-project :name "Ein Test-Project" :identifier "test-project")

(elmine/update-project '(:identifier "test-project" :name "Ein neuer Test-Projekt-Name"))

(elmine/delete-project "test-project")

(elmine/get-project-categories "personal")

(elmine/get-project-issues "personal")

(elmine/get-project-versions "personal")

(elmine/get-version 16)

(elmine/create-version :project_id "personal" :name "ABCDEFG")

(elmine/update-version :id 18 :name "XYZ")

(elmine/get-issue-statuses)

(elmine/get-trackers)

(elmine/get-issue-priorities)

(elmine/get-time-entries)

(elmine/get-time-entry 1)

(elmine/create-time-entry :issue_id 93 :activity_id 1 :hours "2.0")

(elmine/update-time-entry :id 5 :hours "3.0")

(elmine/delete-time-entry 5)
