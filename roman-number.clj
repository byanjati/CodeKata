(def divider 
  "list of divider on romanian number"
  (list 1 5 10 50 100 500 1000))

(defn find-first-num [n]
  "find first number"
  (read-string (.toString (first (.toString n)))))

(defn split-num [n]
  "split number into string"
  (clojure.string/split (.toString n) #""))

(defn rules-num [n]
  "split number into an equation structure"
  (let [splitted-num (split-num n)
        lst-multiplier (reverse (take (count splitted-num) (iterate #(* % 10) 1)))]
    (map #(* (read-string %1) %2) splitted-num lst-multiplier)))

(defn negative-rules [n lst]
  "find the equation when in negative condition"
  (let [neg-divider (search-divider-neg n lst)]
    (list (- neg-divider n) neg-divider)))

(defn roman-neg [n lst]
  "roman-translate negative condition"
  (clojure.string/join "" 
                       (map #(first (recursive-translate % "")) 
                            (negative-rules n lst))))

(def romawi 
  "map of romawi translate"
  {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"})

(defn search-divider [n lst]
  "searching for the proper divider of n"
  (let [best-divider (reduce min (filter #(<= 1 %) (map #(/ n %) lst)))
        idx-divider (.indexOf (map #(/ n %) divider) best-divider)]
    (nth divider idx-divider)))

(defn search-divider-neg [n lst]
  "searching for the proper divider n in negative condition"
  (nth lst (.indexOf (map #(int (Math/floor (/ n %))) lst) 0)))

(defn generate-roman [number times]
  "generate roman number in n times"
  (clojure.string/join "" (map #(romawi %) (repeat times number))))

(defn recursive-translate [n res]
  "recursively translate the number"
  (let [denom (search-divider n divider)
        times (int (Math/floor (/ n denom)))]
    (if (> (mod n denom) 0)
      (recursive-translate (mod n denom) (concat res (generate-roman denom times)))
      (concat res (generate-roman denom times)))))

(defn romania[n lst]
  "checking the condition, negative when the number mod 5 is 4"
  (let [first-num (find-first-num n)]
    (if (= (mod first-num 5) 4)
      (roman-neg n lst)
      (clojure.string/join "" (recursive-translate n "")))))

(defn roman-translate [n lst]
  "our main course"
  (clojure.string/join "" (map #(romania % lst) (rules-num n))))

(roman-translate 988 divider)
