# TODO:
# 4. Choice with default (e.g.) ['Bacon' 'Egg'] default 'Bacon'
  # user must type in freetext
# 5. Choice with numbers ['Bacon' 'Egg'] default 2
(defn always-true [&opt &] (true? true))
(defn myself [x] x)

(defn enumerate
  [xs &opt i]
  (default i 0)
  (seq [x :range [i (+ (length xs) i)]] @[x (xs (- x i))]))

(defn question
  [q da h]
  (default da "")
  (if h
    (string/format "\n%s (%s) [%s] " q h da)
    (string/format "\n%s [%s] " q da)))

(defn ask
  [q &opt &keys {:default da :help h :pred valid-a? :parsefunc pf}]
  (default valid-a? always-true)
  (default pf myself)
  (if-let [resp (string/trim (getline (question q da h)))
           a (if (= resp "") da resp)
           _ (valid-a? a)]
    (pf a)
    (ask q :default da :help h :pred valid-a? :parsefunc pf)))

(defn yn?
  [s]
  (any? (map (partial = (string/ascii-lower s)) ["yes" "y" "no" "n"])))

(defn yes?
  [s]
  (let [s-lower (string/ascii-lower s)]
    (or (= s-lower "yes") (= s-lower "y"))))

(defn yn
  [q &opt &keys {:default da :help h}]
  (default h "yes or no")
  (ask q :default da :help h :pred yn? :parsefunc yes?))

(defn num
  [q &opt &keys {:default da :help h}]
  (default h "enter a number")
  (ask q :default da :help h :pred scan-number :parsefunc scan-number))

(defn ssmmhh
  [s]
  (->> s
       (string/split ":")
       (map scan-number)
       reverse))

(defn ssmmhh?
  [timestamp]
  (let [preds [|(and (<= 0 $) (< $ 60)) |(and (<= 0 $) (< $ 60)) |(<= 0 $)]]
    (when (<= (length timestamp) 3)
      (all true? (map |($0 $1) preds timestamp)))))

(defn time?
  "Checks if string time is a pos num or in HH:MM:SS or MM:SS format."
  [s]
  (if-let [n (scan-number s)]
    (pos? n)
    (ssmmhh? (ssmmhh s))))

(defn n-pow-60-i
  [i n]
  (* n (math/pow 60 i)))

(defn ss
  "Calculates the time (s) from SS, SS:MM, or SS:MM:HH."
  [s]
  (let [timestamp (ssmmhh s)]
    (sum (map (partial apply n-pow-60-i) (enumerate timestamp)))))

(defn time
  [q &opt &keys {:default da :help h}]
  (default h "HH:MM:SS, MM:SS, or SS")
  (ask q :default da :help h :pred time? :parsefunc ss))
