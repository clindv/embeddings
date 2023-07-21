(ns embeddings.glove
  (:gen-class))
(def vec-size 300)
(def amount 40000)
(def ^:private input-file-name (str "resources/glove/glove.6B." vec-size "d.txt"))
(def dictionary (volatile! {}))
(defn init []
  (vreset! dictionary {})
  (with-open [rdr (clojure.java.io/reader input-file-name)]
    (dotimes [n amount]
      (let [[word & vector-string] (clojure.string/split (.readLine rdr) #" ")]
        (vswap! dictionary assoc word (cons n (map (comp #(.floatValue %) read-string) vector-string))))))
  @dictionary)
(def ^:private suffix "txt");"csv"
(defn acrostic []
  (with-open [rdr (clojure.java.io/reader input-file-name)
              wtr (clojure.java.io/writer (str "target/all." suffix))]
    (dotimes [_ amount]
      (try (.write wtr (str (read-string (.readLine rdr)) ","))
           (catch Exception e "")))))
(defn split ([] (split 500))
  ([n]
   (.mkdir (java.io.File. (str "target/" n)))
   (with-open [rdr (clojure.java.io/reader input-file-name)]
     (dotimes [m (quot amount n)]
       (with-open [wtr (clojure.java.io/writer (str "target/" n "/"
                                                    (quot m 100) (quot (rem m 100) 10) (rem m 10) "." suffix))]
         (dotimes [_ n]
           (try (.write wtr (str (read-string (.readLine rdr)) ","))
                (catch Exception e ""))))))))
(defn link [src tar] (map - (rest (@dictionary tar)) (rest (@dictionary src))))
(defn link-vector [src tar] (map - src tar))
(defn plus [src v] (map + (rest (@dictionary src)) v))
(defn norm [v] (apply + (map #(Math/pow % 2) v)))
(def result-amount 20)
(def ^:private near (volatile! (list)))
(def ^:private threshold (* (quot vec-size 50) 20))
(defn search-vec [v]
  (vreset! near (list))
  (doseq [[word [rank & values]] @dictionary]
    (let [norm (norm (link-vector v values))]
      (if (and (> norm 0.01) (< norm (max threshold (or (:norm (first @near)) (* threshold 5)))))
        (vswap! near conj {:word word :norm norm :rank rank}))))
  (take result-amount (sort-by :norm < @near)))
(defn search
  ([ori src tar] (search ori (link src tar)))
  ([ori v] (search-vec (map + (rest (@dictionary ori)) v)))
  ([ori] (search-vec (rest (@dictionary ori)))))
(defn annotate
  [file-name]
  (->> file-name
       slurp
       (#(clojure.string/split % #"[^\d\w-]+|[_]+"))
       distinct
       (map clojure.string/lower-case)
       (map #(list % (first (@dictionary %))))
       (filter (comp not nil? second))
       (sort-by second >)
       (take result-amount)))
(def rankings (volatile! []))
(defn init-ranked []
  (with-open [rdr (clojure.java.io/reader input-file-name)]
    (dotimes [n amount]
      (let [[word & vector-string] (clojure.string/split (.readLine rdr) #" ")]
        (vswap! rankings conj (into [n word] (map (comp #(.floatValue %) read-string) vector-string)))))))
(def ^:private best (volatile! (vec (repeat vec-size (list)))))
(def ^:private best-finished (promise))
(defn- bestify []
  (vreset! best (list))
  (doseq [[rank word & values] @rankings
          [index flag] (->> @best
                            (map first)
                            (map :value)
                            (map #(if (nil? %) 0 %))
                            (map > values)
                            (map-indexed #(list %1 %2)))
          :when flag]
    (vswap! best update index conj {:word word :rank rank :value (nth values index)}))
  (deliver best-finished true))
(def ^:private worst (volatile! (vec (repeat vec-size (list)))))
(def ^:private worst-finished (promise))
(defn- worstify []
  (vreset! worst (list))
  (doseq [[rank word & values] @rankings
          [index flag] (->> @worst
                            (map first)
                            (map :value)
                            (map #(if (nil? %) 0 %))
                            (map < values)
                            (map-indexed #(list %1 %2)))
          :when flag]
    (vswap! worst update index conj {:word word :rank rank :value (nth values index)}))
  (deliver worst-finished true))
(defn envelope []
  (init-ranked)
  (var-set best-finished (promise))
  (var-set worst-finished (promise))
  (doto (Thread. bestify) .start)
  (doto (Thread. worstify) .start)
  @best-finished
  @worst-finished
  (with-open [rdr (clojure.java.io/writer "target/envelope.txt")]
    (binding [*out* rdr]
      (clojure.pprint/pprint (map concat @worst (map reverse @best))))))
