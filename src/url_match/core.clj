(ns url-match.core)
(use 'clojure.string)

(defn get-string-between
  "Returns substring of original string between given match groups"
  [original-str, start-m-group, end-m-group]
  (get (re-find (re-pattern (str start-m-group "(.*)" end-m-group)) original-str) 2))

(defn split-first [s re]
  (split s re 2))

(defn bypassing-nil
  [f arg1 arg2]
  (if (nil? arg1)
    nil
    (f arg1 arg2)))

(defn host-component-parser
  "Host component parser"
  [host-pattern]
  {:host host-pattern})

(defn path-component-parser
  "URL path component parser"
  [path-pattern]
  {:path (split path-pattern #"/")})

(defn queryparam-component-parser
  "Queryparam component parser"
  [queryparam-pattern]
  (def query-param-def (split queryparam-pattern #"="))
  {:queryparam {(keyword (get query-param-def 0)) (get query-param-def 1)}})

(defn parse-component
  "Component pattern parser factory"
  [component-pattern]
  (defn get-string-between
    "Returns substring of original string between given match groups"
    [original-str, start-m-group, end-m-group]
    (get (re-find (re-pattern (str start-m-group "(.*)" end-m-group)) original-str) 2))

  (def component-prefix (get (split component-pattern #"\(") 0))

  (case component-prefix
    "host" (host-component-parser (get-string-between component-pattern "(host\\()" "(\\);)"))
    "path" (path-component-parser (get-string-between component-pattern "(path\\()" "(\\);)"))
    "queryparam" (queryparam-component-parser (get-string-between component-pattern "(queryparam\\()" "(\\);)"))))

(defn parse-url
  [url]
  (def schemaless-url (get (split url #"//") 1))
  (def url-by-components (split schemaless-url #"\?"))
  (def host-path (split-first (get url-by-components 0) #"/"))
  (def queryparams (apply merge-with merge (map queryparam-component-parser (bypassing-nil split (get url-by-components 1) #"\&"))))
  (def host (host-component-parser (get host-path 0)))
  (def path (path-component-parser (get host-path 1)))

  (merge queryparams host path))

(defn validate
  [parsed-pattern, parsed-url]

  (defn path-matches?
    [pattern, actual]
    (if (or (= (first pattern) (first actual)) (starts-with? (first pattern) "?"))
      (do
        (if (empty? pattern)
          true
          (path-matches? (rest pattern) (rest actual))))
      false))

  (defn host-valid?
    [pattern, actual]
    (= pattern actual))
  (defn path-valid?
    [pattern, actual]
    (and (== (count pattern) (count actual)) (path-matches? pattern actual)))
  (defn queryparams-valid?
    [pattern, actual]
    (every? actual (keys pattern)))
  (def validators {:host host-valid? :path path-valid? :queryparam queryparams-valid?})
  (every? true? (map (fn [key]
                       ((key validators) (key parsed-pattern) (key parsed-url)))
                     (keys parsed-pattern))))

(defn construct-binds
  [parsed-pattern, parsed-url]
  (defn get-path-binds
    [pattern, actual]
    (map (fn [bind]
           [(keyword (subs (get bind 0) 1)), (get bind 1)]) (filter (fn [element]
                                                                      (starts-with? (get element 0) "?")) (map vector pattern actual))))
  (defn get-queryparams-binds
    [pattern, actual]
    (select-keys actual (keys pattern)))
  (def binds-extractors {:path get-path-binds :queryparam get-queryparams-binds})
  (mapcat (fn [key]
            ((key binds-extractors) (key parsed-pattern) (key parsed-url)))
          (keys binds-extractors)))

(defn new-pattern
  "Transforms pattern string to a map."
  [pattern]
  (apply merge-with merge (map parse-component (map trim (split pattern #"\s")))))

(defn recognize
  "Recognizes the given url by pattern"
  [pattern, url]
  (def parsed-url (parse-url url))

  (if (validate pattern parsed-url)
    (construct-binds pattern parsed-url)))

(defn -main []
  (def twitter (new-pattern "host(twitter.com); path(?user/status/?id);"))
  (println (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392"))

  (def dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
  (println (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (println (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (println (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"))
  (def dribbble2 (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))
  (println (recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")))
