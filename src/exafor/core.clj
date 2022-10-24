(ns exafor.core
  (:gen-class))

; Escribir una función que calcule la suma
; de los números enteros desde n hasta m (m>n).


(def suma (atom 0))

(defn resetear []      ;Funcion para restablecer las variables 
  (reset! suma 0)
)

;-----------------for_loop_macro--------------------
(defmacro for-loop [[sym init check change :as params] & steps]
  `(loop [~sym ~init value# nil]
     (if  ~check
       (let [new-value# (do ~@steps)]
         (recur ~change new-value#))
       value#)))

;---------------------LOOP y Recur---------------------------
(defn conLoop [numeroN numeroM]  
  (loop [x numeroN]
    (when (<= x numeroM)
      (print x " ")
      (swap! suma + x)
      (recur (+ x 1))
    )
  )
  (println "")
  (println "La suma es: " @suma)
)

;---------------------While---------------------------
(defn conWhile [numeroN numeroM]
  (resetear)
  (def x (atom numeroN))
  (while (<= @x numeroM)
    (do
      (print @x " ")
      (swap! suma + @x)
      (swap! x + 1)
    )
  )
  (println "")
  (println "La suma es: " @suma)
)

;---------------------DoTimes---------------------------

(defn conDotimes [numeroN numeroM]
  (resetear)
  (def con (atom numeroN))
  (def replicas (+ 1 (- numeroM numeroN)))
  (swap! suma + numeroN)

  (dotimes [i replicas]
    (print @con " ")
    (swap! con + 1)
    (if(<= @con numeroM) (swap! suma + @con))
  )

  (println "")
  (println "La suma es: " @suma)
)

;---------------------DoSeq---------------------------
(defn conDoSeq [numeroN numeroM]
  (resetear)
  (def con (atom numeroN))
  (def replicas (+ 1 (- numeroM numeroN)))
  (swap! suma + numeroN)

  (doseq [i (vec (repeat replicas 0))]
    (print @con " ")
    (swap! con + 1)
    (if(<= @con numeroM) (swap! suma + @con))
  )

  (println "")
  (println "La suma es: " @suma)
)

;------------------------------------------------
;             [ con ciclo FOR ]
;------------------------------------------------
(defn conCicloFor [numeroN numeroM]
  (resetear)
  (def con (atom numeroN))
  (def replicas (+ 1 (- numeroM numeroN)))
  (swap! suma + numeroN)

  (for [i (vec (repeat replicas 0))]
    (do
      (print @con " ")
      (swap! con + 1)
      (if(<= @con numeroM) (swap! suma + @con))
    )
  )

  (println "")
  (println "La suma es: " @suma)
)

;------------------------------------------------
;             [ con ciclo ConRecur ]
;------------------------------------------------
(def conRecur 
  (fn ([n x] (conRecur n 1 0 x[]))
    ([n impares suma num x]
      (print n " ")
      (if (zero? n)
        (do
          (prn x "Impares")
          (prn num "^2 es: " suma)
        )
        
        (recur (dec n) (+ impares 2) (+ impares suma) (inc num)
          (conj x impares)
        )
      )
    )
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println "Con Loop")
  (conLoop 5 10)

  (println "Con While")
  (conWhile 5 10)

  (println "Con conDotimes")
  (conDotimes 5 10)

  (println "Con conDoSeq")
  (conDoSeq 5 10)

  (println "Con conCicloFor")
  (conCicloFor 5 10)

  (println "Con conCicloFor")
  (conRecur 5 100)
  
)
