# TODO:
# 3. Time
# 4. Choice with default (e.g.) ['Bacon' 'Egg'] default 'Bacon'
  # user must type in freetext
# 5. Choice with numbers ['Bacon' 'Egg'] default 2

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
      (ask q da h))))

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
