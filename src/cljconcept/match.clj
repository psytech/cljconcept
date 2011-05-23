(ns ^{:doc "Macro to match arguments by a given pattern"
      :author "psytech"
      :email "psytech@psycho-technique.net"}
      cljconcept.match)

(defn- filter-arguments [input]
  "Returns all symbols except _ and & from the flattened input"
  (let [table [(symbol "_") (symbol "&")]]
    (loop [x (flatten input) out (list)]
      (if (empty? x)
        out
        (recur (rest x) 
            (concat out
              (when (not-any? #(= % (first x)) table)
                      (list (first x)))))))))
                    
(defmacro match [pattern]
  "Creates a function which returns the arguments specified
   by the pattern as a flat vector."
  (let [args (filter-arguments pattern)
        f (list (symbol "fn") pattern (apply vector args))]
    `~f))

