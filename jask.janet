# TODO:
# 1. [ X ] Free-text
# 2. Number
# 3. Time
# 4. Choice with default (e.g.) ['Bacon' 'Egg'] default 'Bacon'
  # user must type in freetext
# 5. Choice with numbers ['Bacon' 'Egg'] default 2
# 6. Yes or no (accepting 'yes', 'y', 'no', 'n', case-insensitive)
# 7. True/false (just wrapper around the above one)

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
