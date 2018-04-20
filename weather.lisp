;;;;;;;;;;;;;;;;;;;;;;;;
;; Alec Shashaty      ;;
;; Vassar College     ;;
;; 2018               ;; 
;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :evidentiality-game)

;;;;;;;;;;;;;;;;;;;;;;;;
;; weather definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*weather-vector*))

(defstruct weather
  (name nil)
  (likelihood 0))

(defvar probability-vector (vector 60.0 20.0 10.0 5.0 5.0))

(defun sum-sequence (seq)
     (loop for x across seq summing x))

;returns a vector with a normalized distribution
(defun normalize-distribution (vec)
  (map 'vector (lambda (x) (/ x (sum-sequence vec))) vec))


(defun change-distribution (vec change-amt) 
  (let* 
    ((random-index (random 5))
    (random-elt (svref vec random-index)))

   (setf (weather-likelihood random-elt) (+ (weather-likelihood random-elt) change-amt)) ;; arbitrary
   (normalize-likelihoods vec)
   ))

(defvar sun (make-weather :name "sun" :likelihood 6))
(defvar rain (make-weather :name "rain" :likelihood 1))
(defvar snow (make-weather :name "snow" :likelihood 1))
(defvar hurricane (make-weather :name "hurricane" :likelihood 1))
(defvar tornado (make-weather :name "tornado" :likelihood 1))


(defvar *weather-vector* (vector sun rain snow hurricane tornado))

(defun update-likelihoods (wvec lvec)
  (loop for x from 0 to (- (length wvec) 1)
    do (setf (weather-likelihood (svref wvec x)) (svref lvec x))))

;; (update-likelihoods *weather-vector* probability-vector)


(defun normalize-likelihoods (wvec)
  (let ((sum-l 0.0))
    (loop for x from 0 to (- (length wvec) 1)
      do
        (incf sum-l (weather-likelihood (svref wvec x)))
      )
    (loop for x from 0 to (- (length wvec) 1)
      do
        (setf (weather-likelihood (svref wvec x)) (/ (weather-likelihood (svref wvec x)) sum-l))
      )
    )
  )

(defun print-likelihoods (wvec)
  (loop for x from 0 to (- (length wvec) 1)
    do (write (weather-likelihood (svref wvec x))) (terpri)))


(defun check-weather (wvec with-time? change-amt)
  (when with-time? ;; using a boolean argument, switch on variation
    (change-distribution wvec change-amt))
  (let ((acc 0)
    (rand (random 1.0))
    (current nil))
    (setf acc (+ acc (weather-likelihood (svref wvec 0))))
    (if (> acc rand) (setf current (svref wvec 0)))
    (loop for x from 1 to (- (length wvec) 1)
    do 
      (setf acc (+ acc (weather-likelihood (svref wvec x))))
    (if (and (> acc rand) (not current))
      (setf current (svref wvec x))
      
   ))
    current))

(normalize-likelihoods *weather-vector*)



