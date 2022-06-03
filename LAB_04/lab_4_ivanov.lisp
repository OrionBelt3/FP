(defun char-count (rune str)
    (setq rune-count 0)
    (loop for word in str
        do (progn (loop for cur-rune across word
            do (
                if (equalp cur-rune rune) (setq rune-count (+ 1 rune-count))
            ))))
    rune-count
)

(print (char-count #\н '("Встаёт рассвет во мгле холодной."
                  "На нивах шум работ умолк.")))

