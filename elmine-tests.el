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

(elmine/get-time-entries)

(elmine/get-time-entries)
