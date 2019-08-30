(ns url-match.core)
(use 'clojure.string)

(defn get-string-between
  "Returns substring of original string between given match groups"
  [original-str, start-m-group, end-m-group]
  (get (re-find (re-pattern (str start-m-group "(.*)" end-m-group)) original-str) 2))

(defn split-first [s re]
  (split s re 2))

(defn flip [f]
  "Wraps given function to accept args in reverse order"
  (fn [& args]
    (apply f (clojure.core/reverse args))))

(defn bypassing-nil [f]
  "Wraps given function to bypass nil if all params are nil"
  (fn [& args]
    (if (every? identity (map nil? args))
      nil
      (apply f args))))

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
  (let [query-param-def (split queryparam-pattern #"=")]
    {:queryparam {(keyword (first query-param-def)) (second query-param-def)}}))

(defn parse-component
  "Component pattern parser factory"
  [component-pattern]
  (let [component-prefix (first (split component-pattern #"\("))]
    (case component-prefix
      "host" (host-component-parser (get-string-between component-pattern "(host\\()" "(\\);)"))
      "path" (path-component-parser (get-string-between component-pattern "(path\\()" "(\\);)"))
      "queryparam" (queryparam-component-parser (get-string-between component-pattern "(queryparam\\()" "(\\);)")))))

(defn parse-url
  [url]
  (let [url-by-components (split (second (split url #"//")) #"\?")]
    (let [host-path (split-first (first url-by-components) #"/")
          queryparams (apply merge-with merge
                             (map queryparam-component-parser
                                  ((bypassing-nil (partial (flip split) #"\&")) (second url-by-components))))]
      (merge queryparams
             (host-component-parser (first host-path))
             (path-component-parser (second host-path))))))

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

  (let [validators {:host       host-valid?
                    :path       path-valid?
                    :queryparam queryparams-valid?}]
    (every? true? (map
                    (fn [key] ((get validators key) (get parsed-pattern key) (get parsed-url key)))
                    (keys parsed-pattern)))))

(defn construct-binds
  [parsed-pattern, parsed-url]

  (defn get-path-binds
    [pattern, actual]
    (map (fn [bind] [(keyword (subs (first bind) 1)), (second bind)])
         (filter (fn [element] (starts-with? (first element) "?"))
                 (map vector pattern actual))))
  (defn get-queryparams-binds
    [pattern, actual]
    (select-keys actual (keys pattern)))

  (let [binds-extractors {:path       get-path-binds
                          :queryparam get-queryparams-binds}]
    (mapcat (fn [key] (let [bind-extractor (get binds-extractors key)]
                        (bind-extractor (get parsed-pattern key) (get parsed-url key))))
            (keys binds-extractors))))

(defn new-pattern
  "Transforms pattern string to a map."
  [pattern]
  (apply merge-with merge (map parse-component (map trim (split pattern #"\s")))))

(defn recognize
  "Recognizes the given url by pattern"
  [pattern, url]
  (let [parsed-url (parse-url url)]
    (if (validate pattern parsed-url)
      (construct-binds pattern parsed-url))))

(defn -main []
  (def twitter (new-pattern "host(twitter.com); path(?user/status/?id);"))
  (println (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392"))

  (def dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
  (println (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (println (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (println (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"))
  (def dribbble2 (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))
  (println (recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")))
