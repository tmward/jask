# TODO:
# 4. Choice with default (e.g.) ['Bacon' 'Egg'] default 'Bacon'
  # user must type in freetext
# 5. Choice with numbers ['Bacon' 'Egg'] default 2
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
  [q &opt &keys {:default da :help h}]
  (let [a (string/trim (getline (question q da h)))]
    (cond
      (and (= a "") da) da
      (not= a "") a
      (ask q :default da :help h))))

(defn yn
  [q &opt &keys {:default da :help h}]
  (default h "yes or no")
  (let [a (string/ascii-lower (ask q :default da :help h))]
    (cond
      (or (= a "yes") (= a "y")) true
      (or (= a "no") (= a "n")) false
      (yn q :default da :help h))))

(defn num
  [q &opt &keys {:default da :help h}]
  (default h "enter a number")
  (if-let [n (scan-number (ask q :default da :help h))]
    n
    (num q :default da :help h)))

(defn hhmmss
  [s]
  (->> s
       (string/split ":")
       (map scan-number)))

(defn valid-hhmmss?
  [hhmmss]
  (let [preds [|(and (<= 0 $) (< $ 60)) |(and (<= 0 $) (< $ 60)) |(<= 0 $)]]
    (all true? (map (fn [p n] (p n)) preds (reverse hhmmss)))))

# allow either one big number in seconds or HH:MM:SS, MM:SS format
(defn time?
  "Checks if string time is a pos num or in HH:MM:SS or MM:SS format."
  [s]
  (if-let [n (scan-number s)]
    (pos? n)
    (valid-hhmmss? (hhmmss s))))

(defn n-pow-60-i
  [i n]
  (* n (math/pow 60 i)))

(defn ss
  "Calculates the time (s) from HH:MM:SS, MM:SS, or SS."
  [s]
  (let [ssmmhh (reverse (hhmmss s))]
    (sum (map (partial apply n-pow-60-i) (enumerate ssmmhh)))))

(defn time
  [q &opt &keys {:default da :help h}]
  (default h "HH:MM:SS, MM:SS, or SS")
  (let [a (ask q :default da :help h)]
    (if (time? a)
      (ss a)
      (time q :default da :help h))))
