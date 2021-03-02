# helper functions
(defn always-true [&opt &] (true? true))
(defn myself [x] x)
(defn parse-then-sub1 [s] (- (scan-number s) 1))
(defn all-truthy?
  "Tests if x, or if array/tuple, all items in x, are truthy."
  [x] 
  (case (type x)
    :array (all truthy? x)
    :tuple (all truthy? x)
    (truthy? x)))
(defn get-items
  "Returns the items in xs at positions in is"
  [xs is]
  (map (partial get xs) is))

(defn enumerate
  [xs &opt i]
  (default i 0)
  (seq [x :range [i (+ (length xs) i)]] @[x (xs (- x i))]))

#ask functions

(defn print-choices
  [xs]
  (print "Available choices are:")
  (each i+c (enumerate xs 1) (printf "(%d) %s" ;i+c)))

(defn question
  [q da h]
  (default da "")
  (if h
    (string/format "\n%s (%s) [%s] " q h da)
    (string/format "\n%s [%s] " q da)))

(defn ask
  [q &opt &keys {:default da :help h :pred valid-a? :parsefunc pf :choices cs}]
  (default valid-a? always-true)
  (default pf myself)
  (when cs (print-choices cs))
  (if-let [resp (string/trim (getline (question q da h)))
           a (if (= resp "") da resp)
           _ (valid-a? a)]
    (pf a)
    (ask q :default da :help h :pred valid-a? :parsefunc pf :choices cs)))

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

(defn str-is-n-between-lr?
  [l r]
  (fn [s]
    (if-let [n (scan-number s)
             _ (int? n)]
      (<= l n r))))

(defn which
  [q xs &opt &keys {:default da :help h}]
  (default h "pick a number")
  (in xs (ask q :default da :help h :choices xs
                    :pred (str-is-n-between-lr? 1 (length xs))
                    :parsefunc parse-then-sub1)))

(def sep-num-items
  ~{:sep (+ (* :s* "," :s*) :s+)
    :item (cmt (<- :d+) ,scan-number)
    :subseqitems (any (* :sep :item))
    :end (* :s* -1)
    :main (* :item :subseqitems :end)})

(defn order-preference
  "Returns items from xs specified by idxs in comma/space separated string. nil if fails"
  [xs s]
  (when-let [idxs (peg/match sep-num-items s)
             zero-based-idxs (map |(- $ 1) idxs)]
    (get-items xs zero-based-idxs)))

(defn which-items
  "Asks user to choose items from an array/tuple."
  [q xs &opt &keys {:default da :help h}]
  (default h "separate items with space or comma, repeats/omissions are ok")
  (let [op (partial order-preference xs)]
    (ask q :default da :help h :choices xs
         :pred (comp all-truthy? op)
         :parsefunc op)))
