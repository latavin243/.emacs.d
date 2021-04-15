;;; package --- Summary
;;; Commentary:
;;; generate functions to search online
;;; Code:

(defun searchit-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
	   (url-hexify-string
	    (if mark-active
		(buffer-substring (region-beginning) (region-end))
	      (read-string prompt))))))

(defmacro searchit-create-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, create the interactive command to search online."
  `(defun ,(intern (format "searchit-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (searchit-search ,search-engine-url ,search-engine-prompt)))

(searchit-create-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(searchit-create-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(searchit-create-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(searchit-create-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(provide 'init-search-online)
;;; init-search-online ends here
