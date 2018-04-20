;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alec Shashaty         ;;
;; Vassar College        ;;
;; 2018                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-split & join       ;;
;; c 2012 Sourav Datta   ;;
;; soura.jagat@gmail.com ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :evidentiality-game)

(export '(my-split join) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
    (my-split chars (subseq str 1) (cons accm lst) "")
    (my-split chars (subseq str 1) 
                        lst 
                        (concatenate 'string
           accm
         (string c))))
            ))))


(defun join (str lst &optional (jstr ""))
  (cond
    ((null lst) jstr)
    (t (let ((news (concatenate 'string
    jstr
    (first lst)
    (if (null (rest lst))
        ""
        str))))
  (join str (rest lst) news)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a number of miscellaneous functions moved into a separate file for ease of reading the main file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun re-randomize ()
  ;; wrapper function for resetting the random state
    (setf *random-state* (make-random-state t))
)

(defun print-elts-of-list (lst)
  (loop for l in lst
    do
    (print l)
    )
  )

(defun list-agent-evi-forms (agent)
  (let ((end-list ()))
    (when (evi-lexicon agent)
      (loop for l in (evi-lexicon agent)
        do
        (push (form l) end-list)
        )
      )
    end-list
    )
  )

(defun list-agent-evi-lex (agent evi-meaning)
  (let ((end-list ()))
    (when (evi-lexicon agent)
      (loop for l in (evi-lexicon agent)
        do
        (when (eq (evi-lex-meaning l) evi-meaning)
          (push l end-list))
        )
      )
    end-list
    )
  )

(defun update-evi-lexicon-w-applied (agent)
  (loop for l in (evi-lexicon agent)
    do
    (when (eq (form l) (form (applied-evi-lex agent)))
      (setf (evi-lexicon agent) (delete l (evi-lexicon agent)))
      (print-lex l nil)
      )
    )
  (push (applied-evi-lex agent) (evi-lexicon agent))
  )

(defun sum-priors (agent)
  (loop for x in (obj-lexicon agent) summing (prior-est x))
  )

(defun sum-number-observations (agent)
    (loop for x in (obj-lexicon agent) summing (number-observations x))
)


(defun sum-kl-divergences (agent)
  (loop for x in (obj-lexicon agent) summing (kl-divergence-lex x))
  )

(defun adjust-prior (agent)
  (let ((sum-obv (sum-number-observations agent)))
    (loop for x in (obj-lexicon agent)
      do
      (setf (prior-est x) (/ (number-observations x) (float sum-obv)))
      )   
    )
  )

(defun ind-surprise (p)
  (- (log p 2)))

(defun h-sum-posterior (agent)
  (let ((acc 0.0))
    (loop for obj in (obj-lexicon agent)
      do
      (incf acc (* (posterior-est obj) (ind-surprise (posterior-est obj))))
      )
    acc
    ))

(defun h-sum-prior (agent)
  (let ((acc 0.0))
    (loop for obj in (obj-lexicon agent)
      do
      (incf acc (* (prior-est obj) (ind-surprise (prior-est obj))))
      )
    acc
    ))

(defun average-evi-age (agent)
  (if (evi-age-list agent)
    (let ((sum-age 0.0))
      (loop for num in (evi-age-list agent)
        do
        (incf sum-age num)
        )
      (/ sum-age (float (length (evi-age-list agent))))
    )
    0.0
    )
  )

(defun max-evi-age (agent)
  (if (evi-age-list agent)
    (extremum (evi-age-list agent))
    0.0
    )
  )

(defun average-evi-age-population (agents)
  (let ((sum-age 0.0))
    (loop for agent in agents
      do

      (incf sum-age (average-evi-age agent))
      )
    (/ sum-age (float (length agents)))
    )
  )

(defun average-list (list-access agent)
  (if (funcall list-access agent)
    (let ((sum-l 0.0))
      (loop for num in (funcall list-access agent)
        do
        (incf sum-l num)
        )
      (/ sum-l (float (length (funcall list-access agent))))
    )
    0.0
    )
  )

(defun average-list-for-population (list-access agents)
  (let ((sum-l 0.0))
    (loop for agent in agents
      do
      (incf sum-l (average-list list-access agent))
      )
    (/ sum-l (float (length agents)))
    )
  )

(defun average-max-evi-age-population (agents)
  (let ((sum-age 0.0))
    (loop for agent in agents
      do
      (incf sum-age (max-evi-age agent))
      )
    (/ sum-age (float (length agents)))
    )
  )

(defun average-info-gain (agents)
  (let ((sum-ig 0.0))
    (loop for agent in agents
      do
      (incf sum-ig (info-gain agent))
     )
    (/ sum-ig (float (length agents)))
    )
  )

;; simple wrapper functions
(defun most-likely-weather-est-prior (agent)
  (extremum (obj-lexicon agent) :key 'prior-est)
  )

(defun most-likely-weather-est-posterior (agent)
  (extremum (obj-lexicon agent) :key 'posterior-est)
  )

(defun generate-surprise-prior-observation (agent)
  (if (last-seen agent)
    (progn 
      (let 
          ((surprise 0.0))
          (loop for obj in (obj-lexicon agent)
            do
            (when (eq (weather-name (obj-lex-meaning obj)) (weather-name (last-seen agent)))
              (setf surprise (ind-surprise (prior-est obj)))
              )
            )
          surprise
      
          ))
    ;; if agent hasn't made an observation yet, return 0.0
    0.0
    )
  )

(defun avg-surprise-prior-observation (agents)
  (let 
    ((avg 0.0)
      (agent-num 0.0)
    )
    (loop for agent in agents
      do
      (when (has-access? agent)
        (incf avg (generate-surprise-prior-observation agent))
        (incf agent-num)
        )
      )
    (setf avg (/ avg (float agent-num)))
    avg
    )
  )

(defun generate-surprise-posterior-observation (agent)
  (if (last-seen agent)
    (progn 
      (let 
          ((surprise 0.0))
          (loop for obj in (obj-lexicon agent)
            do
            (when (eq (weather-name (obj-lex-meaning obj)) (weather-name (last-seen agent)))
              (setf surprise (ind-surprise (posterior-est obj)))
              )
            )
          surprise
      
          ))
    ;; if agent hasn't made an observation yet, return 0.0
    0.0
    )
  )

(defun avg-surprise-posterior-observation (agents)
  (let 
    ((avg 0.0)
      (agent-num 0.0)
    )
    (loop for agent in agents
      do
      (when (has-access? agent)
        (incf avg (generate-surprise-posterior-observation agent))
        (incf agent-num)
        )
      )
    (setf avg (/ avg (float agent-num)))
    avg
    )
  )

(defun most-likely-weather (wv)
  (let 
    ((biggest 0.0)
      (most-likely nil)
    )
    (loop for i from 0 to (- (length wv) 1)
      do
      (when (< biggest (weather-likelihood (svref wv i)))
        (setf biggest (weather-likelihood (svref wv i)))
        (setf most-likely (svref wv i)))
      )
    (if most-likely
      most-likely
      (break "most-likely-weather broke with ~a likelihoods" wv))
    )
  )

(defun actual-and-prior-most-likely-est-equal? (w agent)
  (equal (weather-name w) (form (most-likely-weather-est-prior agent)))
  )

(defun actual-and-posterior-most-likely-est-equal? (w agent)
  (equal (weather-name w) (form (most-likely-weather-est-posterior agent)))
  )

(defun percentage-correct-guesses-prior (wv agents)
  (let 
    ((total 0)
      (total-correct 0)
    )
    (loop for agent in agents
      do
      (when (actual-and-prior-most-likely-est-equal? (most-likely-weather wv) agent)
        (incf total-correct)
        )
      (incf total)
      )
    (float (/ total-correct total))
    )
  )

(defun percentage-correct-guesses-posterior (wv agents)
  (let 
    ((total 0)
      (total-correct 0)
    )
    (loop for agent in agents
      do
      (when (actual-and-posterior-most-likely-est-equal? (most-likely-weather wv) agent)
        (incf total-correct)
        )
      (incf total)
      )
    (float (/ total-correct total))
    )
  )

(defun difference-percentage-correct (wv agents)
  (- (percentage-correct-guesses-posterior wv agents) (percentage-correct-guesses-prior wv agents))
  )

(defun prior-kl-divergence (wv agent)
    (let ((sum-kl 0.0)) 
      (loop for obj in (obj-lexicon agent)
          do
          (loop for w from 0 to (- (length wv) 1)
            do
            (when (string-equal (weather-name (svref wv w)) (form obj))
              (incf sum-kl (* (weather-likelihood (svref wv w)) (log (/ (weather-likelihood (svref wv w)) (prior-est obj)) 2)))
              )
            )
          )
      sum-kl
      )
  )

(defun prior-kl-average (wv agents)
  (let ((kl 0.0))
    (loop for agent in agents 
      do
      (incf kl (prior-kl-divergence wv agent))
      )
    (/ kl (float (length agents)))
    )
  )

(defun normalize-posterior (agent)
  (let ((sum-post 0.0))
    (loop for obj in (obj-lexicon agent)
      do
      (incf sum-post (posterior-est obj))
      )
    (loop for obj in (obj-lexicon agent)
      do
      (let ((old-post (posterior-est obj)))
        (setf (posterior-est obj) (/ old-post sum-post))
        )
      )
    )
  )

(defun biased-coin-flip (prob-of-true)
  (let ((rand-num (random 1.0)))
    (if (< rand-num prob-of-true)
      t
      nil)
    )
  )

(defun posterior-kl-divergence (wv agent)
    (let ((sum-kl 0.0)) 
      (loop for obj in (obj-lexicon agent)
          do
          (loop for w from 0 to (- (length wv) 1)
            do
            (when (string-equal (weather-name (svref wv w)) (form obj))
              (incf sum-kl (* (weather-likelihood (svref wv w)) (log (/ (weather-likelihood (svref wv w)) (posterior-est obj)) 2)))
              )
            )
          )
      sum-kl
      )
  )

(defun check-meaning-competitors (new-meaning agent)
  (loop for lex in (evi-lexicon agent)
    do
    (when (eq new-meaning (evi-lex-meaning lex))
      (return t)
      )
    )  
  )

(defun posterior-kl-average (wv agents)
  (let ((kl 0.0))
    (loop for agent in agents 
      do
      (incf kl (posterior-kl-divergence wv agent))
      )
    (/ kl (float (length agents))) 
    )
  )


(defun check-time-interval (interval int-num)
  (if (= (mod int-num interval) 0) t)
  )

(defun print-evidential-interaction (experiment interaction weather)
  ;; Print statement hell: abandon all hope 'ye who enter here...

    ;; interaction number
  (print (concatenate 'string "Interaction " (write-to-string (interaction-number interaction)) ":"))
    ;; speaker and hearer IDs
  (print (concatenate 'string "Interacting agent IDs -- Speaker: " 
    (write-to-string (id (speaker experiment))) " Hearer: " (write-to-string (id (hearer experiment)))))
  (print "----------------")
    ;; current weather pattern
  (print (concatenate 'string "Current weather: " (weather-name weather)))
    ;; what the speaker says in the interaction
  (print (concatenate 'string "Speaker's utterance: " (utterance (speaker experiment))))
    ;; what the speaker intends by its evidential
  (print-lex (applied-evi-lex (speaker experiment)) nil)
  (print "---------------")
    ;; how the hearer parses the evidential:
  (if (parsed? (hearer experiment))
    (progn
      (print "Hearer parsed the evidential suffix!")
      (print "The following is a report of the hearer's update to its posterior probability distribution.")
      (print "*Actual* here represents the real/external probability of the occurence of this weather pattern.")
      (print "*Prior* is the hearer's conception of how likely this event was based purely on observation.")
      (print "*Evi* is derived from the likelihood function associated with the evidential suffix.")
      (print "*Posterior* is the result of multiplying either the Prior (non-feedback condition)")
      (print "or the previous Posterior (feedback condition) by the Evidential likelihood value and normalizing the distribution.")
      (print "-----------------------")
      (loop for lex in (obj-lexicon (hearer experiment))
        do
        ;; prints out the previously mentioned values
        (print-probs lex nil)
        )
      )
    (progn
      (print "Hearer did not recognize this evidential suffix and will try to adopt it into its lexicon.")
      ;; checking if all possible evidential meanings are represented in hearer's lexicon
      (if (applied-evi-lex (hearer experiment))
        (progn
          (print (concatenate 'string "Hearer adopted this meaning based on which meanings it lacked in its lexicon:"))
          (print-lex (applied-evi-lex (hearer experiment)) nil)
          )
        (print "Hearer has an evidential suffix for every possible meaning configuration, and therefore did not adopt this new one.")
        )
      )
    )
  ;; includes a code break so that the interaction print-out is easily located and copy-pasted
  (break "Used special print-evidential-interaction print statement to print out a verbose interaction. Break statement allows for easier copy/pasting.")
  )

;; catch-all function for sending data output/for graphing
(defun push-data-to-recorders (experiment)
  ;; average info gain
  (push (average-info-gain (population experiment)) (average-info-gain-list experiment))

  ;; global vocabulary size
  (push (length (list-all-active-evi experiment)) (vocab-size experiment)) 

  ;; average age of evidentials when they're forgotten
  (push (average-evi-age-population (population experiment)) (average-evi-age-list experiment))

  ;; average time between direct observations
  (push (average-list-for-population 'observation-gap-list (population experiment)) (average-observation-gap-list experiment))

  ;; average time between interactions involving objects
  (push (average-list-for-population 'interaction-gap-list (population experiment)) (average-interaction-gap-list experiment))

  ;; average MAXMIMUM age of evidentials when they're forgotten
  (push (average-max-evi-age-population (population experiment)) (average-max-evi-age-list experiment))

  ;; average level of surprise for observations compared to prior estimate
  (push (avg-surprise-prior-observation (population experiment)) (average-prior-surprise-list experiment))

  ;; average level of surprise for observations compared to posterior estimate
  (push (avg-surprise-posterior-observation (population experiment)) (average-posterior-surprise-list experiment))

  ;; percent of population who guessed most likely weather correctly from prior distribution
  (push (percentage-correct-guesses-prior (weather-vec (world experiment)) (population experiment)) (percent-correct-guesses-prior-list experiment))

  ;; percent of population who guessed most likely weather correctly from posterior distribution
  (push (percentage-correct-guesses-posterior (weather-vec (world experiment)) (population experiment)) (percent-correct-guesses-posterior-list experiment))

  ;; difference between above two measures
  (push (difference-percentage-correct (weather-vec (world experiment)) (population experiment)) (percent-correct-difference-list experiment))

  ;; average of KL divergences from prior distributions to actual distribution
  (push (prior-kl-average (weather-vec (world experiment)) (population experiment)) (average-kl-divergence-from-prior experiment))

  ;; average of KL divergences from posterior distributions to actual distribution
  (push (posterior-kl-average (weather-vec (world experiment)) (population experiment)) (average-kl-divergence-from-posterior experiment))

  )
