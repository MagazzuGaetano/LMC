;;;; -*- Mode: Lisp -*-
;;;; lmc.lisp


;;; carica il programma letto da file in memoria

(defun lmc-load (filename)    
  (let ((x (without-empty-lines 
            (without-blank-spaces
             (without-comments
              (to-lower
               (read-from-file filename)))))))
    (initialize-memory
     (label-resolver x
                     (label-loader 0 x)))))


;;; prendendo in input un programma ed una lista di input
;;; esegue il programma dopo che viene caricato in memoria

(defun lmc-run (filename inp)                                                
  (if (and (every #'numberp inp) 
           (every (lambda (x) (between x 0 999)) inp))
      (execution-loop (make-state 'state
                                  :acc 0
                                  :pc 0
                                  :mem (_fill (lmc-load filename))
                                  :in inp
                                  :out nil
                                  :flag 'noflag))
    (progn
      (format *error-output* "ERROR: INVALID INPUT ! ~S" inp)
      nil)))


(defun between (val min max)
  (and (>= val min) (<= val max)))


;;; aggiunge tanti zeri ad una lista m fino ad avere una lista con 100 elementi

(defun _fill (m)
  (append m 
          (make-list (- 100 (length m)) :initial-element 0)))


;;; legge il file dato in input riga per riga

(defun read-from-file (filename)
  (with-open-file (in filename
                      :direction :input
                      :if-does-not-exist :error)
    (read-lines in)))


;;; ritorna una lista composta dalle righe del file letto da stream

(defun read-lines (stream)
  ((lambda (x)
     (when x
       (cons x (read-lines stream))))
   (read-line stream nil)))


(defun to-lower (list)
  (mapcar #'string-downcase list))

(defun without-empty-lines (list)
  (remove "" list :test 'equal))


;;; data una linea ritorna la stessa linea senza commenti

(defun _without-comments (line)
  ((lambda (x)
     (if (null x)
         line
       (subseq line 0 x)))
   (search "//" line)))


;;; toglie i commenti a tutte le linee del file
 
(defun without-comments (list)
  (mapcar #'_without-comments list))


;;; toglie tutti gli spazi prima e dopo ogni elemento della lista

(defun without-blank-spaces (list)
  (mapcar 
   (lambda (x) (string-trim " " x))
   list))


;;; data una stringa ed un token ritorna una lista
;;; delle sotto stringhe separate dal token
 
(defun split (str token)
  ((lambda (x)
     (if x
         (append (list (subseq str 0 x))
                 (split 
                  (subseq str (+ x 1))
                  token))
       (list str)))
   (position token str)))


;;; esegue la split con token lo spazio

(defun split-s (str)
  (remove ""
          (split str #\Space) :test 'equal))


;;; dato un token, che è il nome simbolico di una label
;;; e una lista labels contenente
;;; cons del tipo (indirizzo, nome simbolico)
;;; ritorna l'indirizzo della label dato il suo nome simbolico

(defun search-cons (token labels)
  (first 
   (remove nil
           (mapcar
            (lambda (x)
              (when (equal (second x) token)
                (first x)))
            labels))))


;;; controlla se esistono label duplicate

(defun exist-duplicates (labels)
  (not (equal (length 
               (delete-duplicates (mapcar 'second labels) :test 'equal))
                  (length labels))))


(defun _parse-integer (str)
  (ignore-errors (parse-integer str)))


(defun make-state (name &key acc pc mem in out flag)
  (list name 
        ':acc 
        (if (null acc) 0 acc)
        ':pc
        (if (null pc) 0 pc)
        ':mem
        (if (null mem) nil mem)
        ':in
        (if (null in) nil in)
        ':out
        (if (null out) nil out)
        ':flag
        (if (null flag) 'noflag flag)))


(defun state-name (state) (nth 0 state))

(defun state-acc (state) (nth 2 state))

(defun state-pc (state) (nth 4 state))

(defun state-mem (state) (nth 6 state))

(defun state-in (state) (nth 8 state))

(defun state-out (state) (nth 10 state))

(defun state-flag (state) (nth 12 state))


;(defun fetch (state)
;  (cons (state-code state) (state-addr state)))


(defun current-instruction (state)
  (nth (state-pc state) 
       (state-mem state)))


(defun state-code (state)
  (floor (current-instruction state) 100))


(defun state-addr (state)
  (mod (current-instruction state) 100))


(defun check-overflow (val)
  (if (>= val 1000) 'flag 'noflag))


(defun check-underflow (val)
  (if (< val 0) 'flag 'noflag))


;;; data una lista 
;;; l'indice del valore da modificare
;;; il valore da modificare
;;; ricreo la lista con il valore modificato

(defun recreate-list (mem index val)
  (append 
   (append (subseq mem 0 index) (cons val nil)) 
   (subseq mem (+ 1 index) )))


;;; dato uno stato, il suo nome e delle chiavi
;;; ricreo lo stato con i nuovi dati

(defun recreate-state (state name &key acc pc mem in out flag)
  (make-state name
              :acc (if (null acc) (state-acc state) acc)
              :pc (if (null pc) (state-pc state) pc)
              :mem (if (null mem) (state-mem state) mem)
              :in (if (null in) (state-in state) in)
              :out (if (null out) (state-out state) out)
              :flag (if (null flag) (state-flag state) flag)))


;;; eseguo un'istruzione di halt

(defun execute-hlt (state)
  (make-state 'halted-state
              :acc (state-acc state)
              :pc (state-pc state)
              :mem (state-mem state)
              :in (state-in state)
              :out (state-out state)
              :flag (state-flag state)))


;;; eseguo un'istruzione di add

(defun execute-add (state)
  (let ((x (+ (state-acc state)
              (nth (state-addr state) (state-mem state)))))
    (recreate-state state
                       'state 
                       :acc (mod x 1000)
                       :pc (mod (+ 1 (state-pc state)) 100)
                       :flag (check-overflow x))))


;;; eseguo un'istruzione di sub

(defun execute-sub (state)
  (let ((x (- (state-acc state)
              (nth (state-addr state) (state-mem state)))))
    (recreate-state state
                    'state 
                    :acc (mod x 1000)
                    :pc (mod (+ 1 (state-pc state)) 100)
                    :flag (check-underflow x))))


;;; eseguo un'istruzione di sta

(defun execute-sta (state)
  (let ((x (recreate-list (state-mem state)
                          (state-addr state)
                          (state-acc state))))
    (recreate-state state 
                    'state 
                    :pc (mod (+ 1 (state-pc state)) 100)
                    :mem x)))


;;; eseguo un'istruzione di lda

(defun execute-lda (state)
  (recreate-state state 
                  'state 
                  :acc (nth (state-addr state) (state-mem state))
                  :pc (mod (+ 1 (state-pc state)) 100)))


;;; eseguo un'istruzione di bra

(defun execute-bra (state)
  (recreate-state state
                  'state 
                  :pc (state-addr state)))


;;; eseguo un'istruzione di brz

(defun execute-brz (state)
  (recreate-state state
                  'state 
                  :pc (if (and (= (state-acc state) 0)
                               (equal (state-flag state) 'noflag))
                          (state-addr state)
                        (mod (+ 1 (state-pc state)) 100))))


;;; eseguo un'istruzione di brp

(defun execute-brp (state)
  (recreate-state state
                  'state 
                  :pc (if (equal (state-flag state) 'noflag)
                          (state-addr state)
                        (mod (+ 1 (state-pc state)) 100))))


;;; eseguo un'istruzione di inp

(defun execute-inp (state)
  (if (> (length (state-in state)) 0)
      (make-state 'state
                  :acc (first (state-in state))
                  :pc (mod (+ 1 (state-pc state)) 100)
                  :mem (state-mem state)
                  :in (rest (state-in state))
                  :out (state-out state)
                  :flag (state-flag state))
    (progn
      (print "ERROR: INVALID INPUT!" *error-output*)
      nil)))


;;; eseguo un'istruzione di out

(defun execute-out (state)
  (recreate-state state
                  'state 
                  :pc (mod (+ 1 (state-pc state)) 100)
                  :out (append (state-out state)
                               (list (state-acc state)))))


;;; eseguo un'istruzione in base al tipo d'istruzione

(defun one-instruction (state)
  (cond ((= 0 (state-code state)) (execute-hlt state))
        ((= 1 (state-code state)) (execute-add state))
        ((= 2 (state-code state)) (execute-sub state))
        ((= 3 (state-code state)) (execute-sta state))
        ((= 5 (state-code state)) (execute-lda state))
        ((= 6 (state-code state)) (execute-bra state))
        ((= 7 (state-code state)) (execute-brz state))
        ((= 8 (state-code state)) (execute-brp state))
        ((= 901 (current-instruction state)) (execute-inp state))
        ((= 902 (current-instruction state)) (execute-out state))))


;;; esguo tutte le istruzioni del programma in memoria

(defun execution-loop (state)
  (if (or (equal (state-name state) 'halted-state)
          (null state))
      (state-out state)
      (execution-loop (one-instruction state))))


;;; inizializzo la memoria

(defun initialize-memory (instructions)
  (if (not (null (find 'err instructions :test 'equal)))
      nil
    (unless (null (first instructions))
      (cond ((and (= (length (split-s (first instructions))) 1)
                  (is-instruction (first instructions))
                  (or (equal (first instructions) "hlt")
                      (equal (first instructions) "inp")
                      (equal (first instructions) "out")))
             (append
              (list (get-code (first instructions)))
              (initialize-memory (rest instructions))))
     
            ((and (= (length (split-s (first instructions))) 1)
                  (is-instruction (first instructions))
                  (equal (first instructions) "dat")
                  (append
                   (list 0)
                   (initialize-memory (rest instructions)))))
            ((and (= (length (split-s (first instructions))) 2)
                  (is-instruction (first (split-s (first instructions))))
                  (is-addr (second (split-s (first instructions))))
                  (not (equal (first (split-s (first instructions))) "dat"))
                  (not (equal (first (split-s (first instructions))) "hlt"))
                  (not (equal (first (split-s (first instructions))) "inp"))
                  (not (equal (first (split-s (first instructions))) "out")))
             (append 
              (list
               (calc-mem-val 
                (get-code (first (split-s (first instructions))))
                (_parse-integer (second (split-s (first instructions))))))
              (initialize-memory (rest instructions))))
            ((and (= (length (split-s (first instructions))) 2)
                  (is-instruction (first (split-s (first instructions))))
                  (is-addr (second (split-s (first instructions))))
                  (equal (first (split-s (first instructions))) "dat"))
             (if (between (_parse-integer 
                           (second (split-s (first instructions)))) 0 999)
                 (append 
                  (list (_parse-integer
                         (second (split-s (first instructions)))))
                  (initialize-memory (rest instructions)))
               (progn
                 (format *error-output* 
                         "ERROR: INVALID VALUE! IS NOT BETWEEN (0 999) ~S" 
                         (first instructions))
                 nil)))))))


(defun get-code (instruction)
  (cond 
   ((equal instruction "hlt") 0)
   ((equal instruction "add") 1)
   ((equal instruction "sub") 2)
   ((equal instruction "sta") 3)
   ((equal instruction "lda") 5)
   ((equal instruction "bra") 6)
   ((equal instruction "brz") 7)
   ((equal instruction "brp") 8)
   ((equal instruction "inp") 901)
   ((equal instruction"out") 902)))


;;; risolvo un istruzione assembly in codice macchina

(defun calc-mem-val (val1 val2)
  (if (between val2 0 999)
      (+ (* val1 100) val2)
    (progn
      (format *error-output* 
              "ERROR: INVALID ADDRESS! IS NOT BETWEEN (0 999) ~S ~%" val2)
      nil)))


(defun is-instruction (str)
  (or (equal str "hlt")
      (equal str "dat")
      (equal str "add")
      (equal str "sub")
      (equal str "sta")
      (equal str "lda")
      (equal str "bra")
      (equal str "brz")
      (equal str "brp")
      (equal str "inp")
      (equal str "out")))


(defun is-label (str)
  (and (not (is-instruction str))
       (not (_parse-integer str))))


(defun is-addr (str)
  (when (_parse-integer str) T))


;;; creo una lista di label
;;; associando il loro indirizzo con il nome simbolico
;;; (indirizzo, nome simbolico)

(defun label-loader (index list)
  (if (null (car list))
      nil
      (if (and (or (= (length (split-s (car list))) 2)
                   (= (length (split-s (car list))) 3))
               (is-label (first (split-s (car list))))
               (is-instruction (second (split-s (car list)))))
          (cons (list index
                      (first (split-s (car list))))
                (label-loader (+ index 1)
                              (rest list)))
          (label-loader (+ index 1)
                        (rest list)))))


;;; risolvo le etichette nelle istruzioni
;;; cambiando il loro nome simbolico con l'idirizzo

(defun label-resolver (instructions labels)
  (if (exist-duplicates labels)
      (progn
        (print "LABELS DUPLICATES! ~%")
        nil)
    (unless (null (first instructions))
      (cond ((and (= (length (split-s (first instructions))) 1)
                  (is-instruction (first instructions))
                  (or (equal (first instructions) "hlt")
                      (equal (first instructions) "inp")
                      (equal (first instructions) "out")
                      (equal (first instructions) "dat")))
             (append
              (list (first instructions))
              (label-resolver (rest instructions) labels)))
            ((and (= (length (split-s (first instructions))) 2)
                  (is-instruction (first (split-s (first instructions))))
                  (is-label (second (split-s (first instructions))))
                  (not (equal (first (split-s (first instructions))) "dat"))
                  (not (equal (first (split-s (first instructions))) "hlt"))
                  (not (equal (first (split-s (first instructions))) "inp"))
                  (not (equal (first (split-s (first instructions))) "out")))
             (append
              (list
               (concatenate 'string 
                            (first (split-s (first instructions)))
                            " "
                            (write-to-string 
                             (search-cons 
                              (second 
                               (split-s
                                (first instructions)))
                              labels))))
              (label-resolver (rest instructions) labels)))
            ((and (= (length (split-s (first instructions))) 2)
                  (is-instruction (first (split-s (first instructions))))
                  (is-addr (second (split-s (first instructions))))
                  (not (equal (first (split-s (first instructions))) "hlt"))
                  (not (equal (first (split-s (first instructions))) "inp"))
                  (not (equal (first (split-s (first instructions))) "out")))
             (append (list (first instructions)) 
                     (label-resolver (rest instructions) labels)))
            ((and (= (length (split-s (first instructions))) 2)
                  (is-label (first (split-s (first instructions))))
                  (is-instruction (second (split-s (first instructions))))
                  (or (equal (second (split-s (first instructions))) "hlt")
                      (equal (second (split-s (first instructions))) "inp")
                      (equal (second (split-s (first instructions))) "out")
                      (equal (second (split-s (first instructions))) "dat")))
             (append
              (list (second (split-s (first instructions))))
              (label-resolver (rest instructions) labels)))
            ((and (= (length (split-s (first instructions))) 2)
                  (is-label (first (split-s (first instructions))))
                  (is-addr (second (split-s (first instructions)))))
             (append
              (list (second (split-s (first instructions))))
              (label-resolver (rest instructions) labels)))
            ((and (= (length (split-s (first instructions))) 3)
                  (is-label (first (split-s (first instructions))))
                  (is-instruction (second (split-s (first instructions))))
                  (is-label (third (split-s (first instructions))))
                  (not (equal (second (split-s (first instructions))) "dat"))
                  (not (equal (second (split-s (first instructions))) "hlt"))
                  (not (equal (second (split-s (first instructions))) "inp"))
                  (not (equal (second (split-s (first instructions))) "out")))
             (append
              (list
               (concatenate 'string
                            (second (split-s (first instructions)))
                            " "
                            (write-to-string 
                             (search-cons
                              (third 
                               (split-s 
                                (first instructions)))
                              labels))))
              (label-resolver (rest instructions) labels)))
            ((and (= (length (split-s (first instructions))) 3)
                  (is-label (first (split-s (first instructions))))
                  (is-instruction (second (split-s (first instructions))))
                  (is-addr (third (split-s (first instructions)))))
             (append 
              (list
               (concatenate 'string
                            (second (split-s (first instructions)))
                            " "
                            (third (split-s (first instructions)))))
              (label-resolver (rest instructions) labels)))
            (t
             (progn
               (format *error-output* 
                       "ERROR: INVALID INSTRUCTION! ~S ~%" 
                       (first instructions))
               '(err)))))))


;;;; end of lmc.lisp