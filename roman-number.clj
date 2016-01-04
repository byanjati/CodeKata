(def divider (list 1 5 10 50 100 500 1000))

(defn find-first-num [n]
  (read-string (.toString (first (.toString n)))))

(defn split-num [n]
  (clojure.string/split (.toString n) #""))

(defn rules-num [n]
  (let [splitted-num (split-num n)
        lst-multiplier (reverse (take (count splitted-num) (iterate #(* % 10) 1)))]
    (map #(* (read-string %1) %2) splitted-num lst-multiplier)))

(defn negative-rules [n lst]
  (let [neg-divider (search-divider-neg n lst)]
    (list (- neg-divider n) neg-divider)))

(defn roman-neg [n lst]
  (clojure.string/join "" 
                       (map #(first (recursive-translate % "")) 
                            (negative-rules n lst))))

(def romawi {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"})

(defn search-divider [n lst]
  (let [best-divider (reduce min (filter #(<= 1 %) (map #(/ n %) lst)))
        idx-divider (.indexOf (map #(/ n %) divider) best-divider)]
    (nth divider idx-divider)))

(defn search-divider-neg [n lst]
  (nth lst (.indexOf (map #(int (Math/floor (/ n %))) lst) 0)))

(defn generate-roman [denom times]
  (clojure.string/join "" (map #(romawi %) (repeat times denom))))

(defn recursive-translate [n res]
  (let [denom (search-divider n divider)
        times (int (Math/floor (/ n denom)))]
    (if (> (mod n denom) 0)
      (recursive-translate (mod n denom) (concat res (generate-roman denom times)))
      (concat res (generate-roman denom times)))))

(defn romania[n lst]
  (let [first-num (find-first-num n)]
    (if (= (mod first-num 5) 4)
      (roman-neg n lst)
      (clojure.string/join "" (recursive-translate n "")))))

(defn roman-translate [n lst]
  (clojure.string/join "" (map #(romania % lst) (rules-num n))))

(roman-translate 988 divider)
















