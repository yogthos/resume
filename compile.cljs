(ns compile
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.walk :refer [prewalk postwalk]]
   ["fs" :as fs]
   ["path" :as path]
   ["puppeteer$default" :as puppeteer]))

;;; Hiccup parser ;;;

(defn normalize-body [body]
  (if (coll? body) (apply str (doall body)) (str body)))

(defn as-str
  [& xs]
  (apply str (map normalize-body xs)))

(defn escape-html
  [text]
  (-> (as-str text)
      (string/replace #"&" "&amp;")
      (string/replace #"<" "&lt;")
      (string/replace #">" "&gt;")
      (string/replace #"'" "&apos;")))

(defn xml-attribute [id value]
  (str " " (as-str (name id)) "=\"" (escape-html value) "\""))

(defn render-attribute [[name value]]
  (cond
    (true? value) (xml-attribute name name)
    (not value) ""
    :else (xml-attribute name value)))

(defn render-attr-map [attrs]
  (apply str (sort (map render-attribute attrs))))

(defn merge-attributes [{:keys [id class]} map-attrs]
  (->> map-attrs
       (merge (when id {:id id}))
       (merge-with #(if %1 (str %1 " " %2) %2) (when class {:class class}))))

(defn normalize-element [[tag & content]]
  (let [re-tag    #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?"
        [_ tag id class] (re-matches re-tag (as-str (name tag)))
        tag-attrs {:id    id
                   :class (when class (string/replace class #"\." " "))}
        map-attrs (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn render-element [[tag attrs & content]]
  (str "<" (name tag) (render-attr-map attrs) ">" (as-str (flatten content)) "</" (name tag) ">"))

(defn render-hiccup [hiccup]
  (postwalk
   (fn [node]
     (if (and (not (map-entry? node)) (vector? node))
       (-> node normalize-element render-element)
       node))
   hiccup))

;;; template parser ;;;

(def path-sep (.-sep path))

(defn image? [node]
  (and (vector? node) (= :page/image (first node))))

(defn css? [node]
  (and (vector? node) (= :page/css (first node))))

(defn data-node? [node]
  (and (keyword? node) (= "data" (namespace node))))

(defn eval-forms [template]
  (prewalk
   (fn [node]
     (if (list? node)
       (eval node)
       node))
   template))

(defn slurp [filename & {:keys [encoding]}]
  (.toString
   (if encoding
     (fs/readFileSync filename encoding)
     (fs/readFileSync filename))))

(defn spit [filename data & {:keys [encoding mode flag]
                             :or   {encoding "utf8"
                                    mode     "0o666"
                                    flag     "w"}}]
  (let [data (if (string? data) data (str data))]
    (fs/writeFileSync filename data encoding mode flag)))

(defn inject-css [theme ref]
  [:style
   {:type "text/css"}
   (slurp (str theme path-sep ref))])

(defn image->b64 [file-path {:keys [theme]}]
  (when file-path
    (let [format    (last (string/split file-path #"\."))]
      (str
       "data:image/" format ";base64, "
       (-> (path/resolve (str theme path-sep file-path))
           (fs/readFileSync)
           (.toString "base64"))))))

(defn inject-image [[_ attrs] opts]
  [:img (update attrs :src image->b64 opts)])

(defn parse-path [path]
  (mapv keyword (string/split path #"\.")))

(defn parse-template [{:keys [theme template data] :as opts}]
  (eval-forms
   (postwalk
    (fn [node]
      (cond
        (css? node)
        (map (partial inject-css theme) (rest node))
        (image? node)
        (inject-image node opts)
        (data-node? node)
        (get-in data (parse-path (name node)))
        :else node))
    template)))

;;

(defn gen-html [opts]
  (-> opts parse-template render-hiccup))

(defn parse-edn-file [file]
  (-> file slurp edn/read-string))

(defn write-pdf [{:keys [browser pending target pdf-opts]} html]
  (-> browser
      (.then #(.newPage %))
      (.then
       (fn [page _] 
         (-> (.setContent page html)
             (.then #(.emulateMediaType page "screen"))
             (.then (fn [_ _]
                      (-> (.pdf page (clj->js (merge {:path target} pdf-opts)))
                          (.then
                           (fn [_] (reset! pending false)))
                          (.catch #(js/console.error (.-message %))))))
             (.catch #(js/console.error (.-message %))))))))

(defn write-html [{:keys [target]} html]
  (spit target html))

(defn ensure-build-folder-exists [target]
  (when-not (fs/existsSync target)
    (fs/mkdirSync target)))

(defn compile-document [{:keys [theme format target] :as opts}]
  (let [opts     (merge opts
                        {:theme    theme
                         :template (parse-edn-file (str theme path-sep "template.edn"))
                         :data     (parse-edn-file (str "resources" path-sep "resume.edn"))})
        html     (gen-html opts)]
    (ensure-build-folder-exists target)
    (println "generating" (string/upper-case (name format)) "document:" (str target path-sep "resume." (name format)))
    ((case format :pdf write-pdf :html write-html)
     (assoc opts :target (str target path-sep "resume." (name format)))
     html)))

(defn parse-args []
  (->> (.-argv js/process)
       (drop-while #(not (string/starts-with? % "--")))
       (partition-by #(string/starts-with? % "--"))
       (partition-all 2)
       (reduce
        (fn [args [[k] v]]
          (assoc args
                 (keyword k)
                 (case k
                   "--format" (-> v first keyword)
                   "--theme" (first v))))
        {})))

(defn main [& _]
  (let [args     (parse-args)
        format   (args :--format :pdf)
        theme    (str "resources" path-sep "themes" path-sep (args :--theme "flat")) 
        browser  (when (= :pdf format)
                   (.launch puppeteer #js {:headless true
                                           :args #js ["--no-sandbox" "--disable-setuid-sandbox"]}))
        pending  (when browser
                   (doto (atom true)
                     (add-watch :watcher
                                (fn [_ _ _ pending]
                                  (when-not pending
                                    (.then (js/Promise.resolve browser)
                                           #(.close %)))))))]
    
    (compile-document
     {:theme    theme
      :format   format
      :target   "target"
      :browser  browser
      :pending  pending
      :pdf-opts {:format "A4"
                 :printBackground true
                 :pdfViewport {:width 1920 :height 1280}
                 :margin {:top "0.4in"
                          :bottom "0.4in"
                          :left "0.4in"
                          :right "0.4in"}}})))

 
(main)
