;;;;;;;;;;;;;;;;;;;;;;;;
;; Alec Shashaty      ;;
;; Vassar College     ;;
;; 2018               ;; 
;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :evidentiality-game
  (:use :common-lisp
   :test-framework
   :utils
   :monitors
   :web-interface
   :tasks-and-processes
   :meta-layer-learning
   :action-behavior-framework
   :experiment-framework)
  )

(in-package :evidentiality-game)

(load "weather.lisp")
(load "monitors.lisp")
(load "configurations.lisp")
(load "/Users/alecshashaty/Babel2/tutorial/language-games/evidentiality-game/evidential-utils.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class definitions     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; experiment 
;;;;;;;;;;;;;;;

(defclass evi-experiment (experiment) 
  ((last-weather
    :documentation "A slot for holding the weather pattern from the last interaction."
    :type (or null weather)
    :accessor last-weather
    :initform nil
    )
  (vocab-size
    :documentation "A list of vocabulary sizes across interactions."
    :type list
    :accessor vocab-size
    :initform ()
    )
   (average-evi-age-list
    :documentation "A number representing the average age of an evidential before it's forgotten by an agent."
    :type list
    :accessor average-evi-age-list
    :initform ()
    )
   (average-max-evi-age-list
    :documentation "A number representing the average maximum age at which an evidential is forgotten by an agent."
    :type list
    :accessor average-max-evi-age-list
    :initform ()
    )
   (average-info-gain-list
    :documentation "A list of the average information gain across all agents across interactions."
    :type list
    :accessor average-info-gain-list
    :initform ()
    )
   (average-observation-gap-list
    :documentation "The average amount of time between direct observations of an object."
    :type list
    :accessor average-observation-gap-list
    :initform ()
    )
   (average-interaction-gap-list
    :documentation "The average amount of time between interaction observations of an object."
    :type list
    :accessor average-interaction-gap-list
    :initform ()
    )
   (average-prior-surprise-list
      :documentation "The average amount of surprise for each observation made across the population compared to prior."
      :type list 
      :accessor average-prior-surprise-list
      :initform ()
    )
    (average-posterior-surprise-list
      :documentation "The average amount of surprise for each observation made across the population compared to posterior."
      :type list 
      :accessor average-posterior-surprise-list
      :initform ()
    )
   (percent-correct-guesses-prior-list
    :documentation "A percentage representing the portion of the population whose prior estimated most likely weather corresponds to the actual most likely weather."
    :type list
    :accessor percent-correct-guesses-prior-list
    :initform ()
    )
   (percent-correct-guesses-posterior-list
    :documentation "A percentage representing the portion of the population whose posterior estimated most likely weather corresponds to the actual most likely weather."
    :type list
    :accessor percent-correct-guesses-posterior-list
    :initform ()
    )
   (percent-correct-difference-list
    :documentation "The difference between the two previous lists."
    :type list
    :accessor percent-correct-difference-list
    :initform ()
    )
   (average-kl-divergence-from-prior
    :documentation "The added together KL divergences of each agent's prior distribution from the actual distribution."
    :type list
    :accessor average-kl-divergence-from-prior
    :initform ()
    )
   (average-kl-divergence-from-posterior
    :documentation "The added together KL divergences of each agent's posterior distribution from the actual distribution."
    :type list
    :accessor average-kl-divergence-from-posterior
    :initform ()
    )
  ))

;; agent
;;;;;;;;;;;;;

(defclass evi-agent (agent) 
  ((obj-lexicon 
    :documentation "A list of object names (weather) and their probability estimates."
    :type list
    :accessor obj-lexicon
    :initform nil
    )
   (evi-lexicon
    :documentation "A list of evidential suffixes, their hypothesized meanings, and 
    their reinforcement scores."
    :type list
    :accessor evi-lexicon
    :initform nil
    )
   (access
    :documentation "Boolean indicating whether an agent has access to the world."
    :accessor has-access?
    :initform nil
    )
   (parsed?
    :documentation "Boolean indicating whether an agent parsed this interaction."
    :accessor parsed?
    :initform nil
    )
   (applied-obj-lex 
    :documentation "The object to be discussed."
    :type (or null obj-lex) :initform nil :accessor applied-obj-lex
    )
   (applied-evi-lex 
    :documentation "The evidential suffix to be attached."
    :type (or null evi-lex) :initform nil :accessor applied-evi-lex
    )
   (evi-age-list
      :documentation "A list of all the amounts of time this agent's evidentials have stayed in the lexicon before being jettisoned."
      :type list
      :initform ()
      :accessor evi-age-list
      )
    (observation-gap-list
      :documentation "A list of all the time gaps between observations of objects."
      :type list
      :initform ()
      :accessor observation-gap-list
      )
    (interaction-gap-list
      :documentation "A list of all the time gaps between interactions involving objects."
      :type list
      :initform ()
      :accessor interaction-gap-list
      )       
   (last-seen
      :documentation "The weather formation last seen by the agent."
      :type (or null weather)
      :initform nil
      :accessor last-seen
      )
   (first-hand-highest
      :documentation "The highest probability assignment to the first-hand marker for this agent."
      :type (or null number)
      :initform nil
      :initarg :first-hand-highest
      :accessor first-hand-highest
      )
   (second-hand-highest
      :documentation "The highest probability assignment to the second-hand marker for this agent."
      :type (or null number)
      :initform nil
      :initarg :second-hand-highest
      :accessor second-hand-highest
      )
   (kl-threshold
      :documentation "The accumlated KL-divergence score at which the 
      given agent throws out its evidential lexicon."
      :initform nil
      :accessor kl-threshold
      :type (or null number)
      )
   (info-gain
      :documentation "The amount of information gain over time for this agent."
      :type number
      :initform 0.0
      :accessor info-gain
      )
   (last-surprise-prior
    :documentation "A number representing the -logbase2(P) of their last observation compared to their prior."
    :type number
    :initform 0.0
    :accessor last-surprise-prior
    )
    (last-surprise-posterior
    :documentation "A number representing the -logbase2(P) of their last observation compared to their posterior."
    :type number
    :initform 0.0
    :accessor last-surprise-posterior
    )
   (used-prior?
      :documentation "A boolean representing whether this agent has used its prior estimates once."
      :initform nil
      :accessor used-prior?
      )
   (posterior-established?
    :documentation "A boolean representing whether this agent has had an interaction this epoch."
    :initform nil
    :accessor posterior-established?
    )
    )
  )

;; world
;;;;;;;;;;;;;

(defvar weather '*weather-vector*)

(defclass evi-world ()
  ((context
    :documentation "Context is not utilized in this experiment."
    :type list :initform nil :accessor context)
   (objects
    :documentation "A list of objects in the experiment." 
    :initarg :objects 
    :initform nil 
    :accessor objects 
    :type (or null list))
   (weather-vec 
    :documentation "A vector representing the weather possibilities."
    :type vector
    :initform (eval weather)
    :accessor weather-vec)))

;; lexicon definitions
;;;;;;;;;;;;;;;;;;;;;;;

(defclass obj-lex ()
  ((obj-lex-meaning 
    :documentation "What this lexical object refers to."
    :initform nil :initarg :obj-lex-meaning :accessor obj-lex-meaning :type weather)
   (form 
    :documentation "A somewhat arbitrary string form for this lexical object.
    (since there is no ambiguity in referent)"
    :initform "" :initarg :form :accessor form :type string)
    (personal-access
      :documentation "An evidential meaning corresponding to an agent's assessment of
      its own access to the given object: i.e. first-person, second-hand"
      :initform nil :initarg :personal-access :accessor personal-access :type evi-meaning) 
    (how-recently
      :documentation "An integer representing how long ago this object was directly
      perceived."
      :initform 0
      :accessor how-recently
      :type integer
      )
    (how-recently-interaction
      :documentation "An integer representing how long ago this object was heard about in an interaction."
      :initform 0
      :accessor how-recently-interaction
      :type integer
      )
    (number-observations
      :documentation "The number of times this particular object has been observed directly, initialized to 1 to prevent funky estimates."
      :initform 1
      :accessor number-observations
      :type integer
      )  
    (prior-est
      :documentation "The agent's best guess for the probability of this object (weather)
      being the case."
      :initform 0.2 :accessor prior-est :initarg :prior-est :type number) 
    (evi-modified-est
      :documentation "Based on the evidential at hand, what is the base probability this is actually the case?"
      :initform 1.0
      :accessor evi-modified-est
      :type number
      )
    (posterior-est
      :documentation "After factoring in an evidential/observation, what is the new probability?"
      :initform 0.2
      :accessor posterior-est
      :type number
      )
    (used-prior-obj?
      :documentation "A boolean representing whether the prior estimate has been used once to update the posterior."
      :initform nil
      :accessor used-prior-obj?
      )
    (kl-divergence-lex 
      :documentation "The accumulated KL-divergence for the given object in the lexicon."
      :initform 0.0
      :accessor kl-divergence-lex
      :type number)
      ) 
    )

(defclass evi-lex ()
  ((evi-lex-meaning 
    :documentation "Which evidential meaning this suffix is mapped onto by the agent."
    :initform 'no-meaning :initarg :evi-lex-meaning :accessor evi-lex-meaning :type (or symbol evi-meaning))
   (form 
    :documentation "The actual word-form used to represent that meaning."
    :initform nil :initarg :form :accessor form :type string)   
   (reinforcement-score
    :documentation "A number representing an agents' certainty that an evidential
    has been classified correctly."
    :initform (random 1.0) :accessor reinforcement-score :initarg :reinforcement-score :type number)
   (evi-age
    :documentation "How long this particular evidential has been used by this particular agent."
    :initform 0
    :type number 
    :accessor evi-age)
   (parsed-num
    :documentation "The number of times this particular evidential has been parsed in speech from another agent."
    :initform 0
    :type number 
    :accessor parsed-num)
  )
  )

(defclass evi-meaning ()
  ((name
    :documentation "What this evidential is called i.e. first-hand."
    :initform nil
    :initarg :name
    :accessor name
    :type string
    )
   (person
    :documentation "First or second-hand?"
    :initform nil
    :initarg :person
    :accessor person
    :type string 
    )
   (temporality
    :documentation "How recently?"
    :initform nil
    :initarg :temporality
    :accessor temporality
    :type string
    )
   (prob-dist
    :documentation "The type of probability distribution that this evidential applies."
    :initform nil
    :accessor prob-dist
    :initarg :prob-dist
    :type functionp)
   (highest-prob
    :documentation "The highest probability for this evidential."
    :initform nil
    :accessor highest-prob
    :initarg :highest-prob
    :type (or null number)
    )

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; condition classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass evi-exp-test-nofeed-notime (evi-experiment) ())
(defclass evi-exp-test-feed-notime (evi-experiment) ())
(defclass evi-exp-test-nofeed-time (evi-experiment) ())
(defclass evi-exp-test-feed-time (evi-experiment) ())
(defclass evi-exp-notest-nofeed-notime (evi-experiment) ())
(defclass evi-exp-notest-feed-notime (evi-experiment) ())
(defclass evi-exp-notest-nofeed-time (evi-experiment) ())
(defclass evi-exp-notest-feed-time (evi-experiment) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initializing instance (generic function implementation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((experiment evi-experiment) &key)
  (setf (world experiment)
        (make-instance 'evi-world 
                       ))
  (setf (population experiment)
    (loop for i from 1 to (get-configuration experiment
                                                 :population-size)
              for agent = (make-instance 'evi-agent :id i
                                         :experiment experiment
                                         :world (world experiment))
              
              collect agent)))


;; with test lexicon
;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((experiment evi-exp-test-nofeed-notime) &key)
  (set-configuration experiment :test-lexicon-switch t)
  (set-configuration experiment :time-switch nil)
  (set-configuration experiment :feedback-posterior? nil)
  (set-configuration experiment :weather-change-within-epoch? t)
  )

(defmethod initialize-instance :after ((experiment evi-exp-test-feed-notime) &key)
  (set-configuration experiment :test-lexicon-switch t)
  (set-configuration experiment :time-switch nil)
  (set-configuration experiment :feedback-posterior? t)
  (set-configuration experiment :weather-change-within-epoch? t)
  )

(defmethod initialize-instance :after ((experiment evi-exp-test-nofeed-time) &key)
  (set-configuration experiment :test-lexicon-switch t)
  (set-configuration experiment :time-switch t)
  (set-configuration experiment :feedback-posterior? nil)
  (set-configuration experiment :weather-change-within-epoch? t)
  )

(defmethod initialize-instance :after ((experiment evi-exp-test-feed-time) &key)
  (set-configuration experiment :test-lexicon-switch t)
  (set-configuration experiment :time-switch t)
  (set-configuration experiment :feedback-posterior? t)
  (set-configuration experiment :weather-change-within-epoch? t)
  )


;; without test lexicon
;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((experiment evi-exp-notest-nofeed-notime) &key)
  (set-configuration experiment :test-lexicon-switch nil)
  (set-configuration experiment :time-switch nil)
  (set-configuration experiment :feedback-posterior? nil)
  (set-configuration experiment :weather-change-within-epoch? t)
  )

(defmethod initialize-instance :after ((experiment evi-exp-notest-feed-notime) &key)
  (set-configuration experiment :test-lexicon-switch nil)
  (set-configuration experiment :time-switch nil)
  (set-configuration experiment :feedback-posterior? t)
  (set-configuration experiment :weather-change-within-epoch? t)
  )

(defmethod initialize-instance :after ((experiment evi-exp-notest-nofeed-time) &key)
  (set-configuration experiment :test-lexicon-switch nil)
  (set-configuration experiment :time-switch t)
  (set-configuration experiment :feedback-posterior? nil)
  (set-configuration experiment :weather-change-within-epoch? t)
  )

(defmethod initialize-instance :after ((experiment evi-exp-notest-feed-time) &key)
  (set-configuration experiment :test-lexicon-switch nil)
  (set-configuration experiment :time-switch t)
  (set-configuration experiment :feedback-posterior? t)
  (set-configuration experiment :weather-change-within-epoch? t)
  )


;; evidential meanings/likelihood functions & misc defs  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prob-list (list 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)) ;; list of possible configurations for highest likelihood in the likelihood distributions


(defun first-prob-dist (agent temporality experiment)
    (let 
      ((highest 
        (if (first-hand-highest agent) 
          (first-hand-highest agent)
          (random-elt prob-list)
          )
        )) 
      (when (obj-lexicon agent)
        (loop for l in (obj-lexicon agent)
          do
          (if (eq l (applied-obj-lex agent))
            (setf (evi-modified-est l) 
              (if (eq temporality "past")
                (* highest (get-configuration experiment :time-prob-decrement))
                highest))
            (setf (evi-modified-est l) 
              (/ 
                (- 1.0 
                  (if (eq temporality "past")
                    (* highest (get-configuration experiment :time-prob-decrement))
                    highest
                    )
                ) 4.0)
            ) 
              )
            )
          )
      (setf (first-hand-highest agent) highest)
      highest
      )
  )

(defun second-prob-dist (agent temporality experiment)
    (let 
      ((highest 
        (if (second-hand-highest agent) 
          (second-hand-highest agent)
          (random-elt prob-list)
          )
        )) 
      (when (obj-lexicon agent)
        (loop for l in (obj-lexicon agent)
          do
          (if (eq l (applied-obj-lex agent))
            (setf (evi-modified-est l) 
              (if (eq temporality "past")
                (* highest (get-configuration experiment :time-prob-decrement))
                highest))
            (setf (evi-modified-est l) 
              (/ 
                (- 1.0 
                  (if (eq temporality "past")
                    (* highest (get-configuration experiment :time-prob-decrement))
                    highest
                    )
                ) 4.0)
            ) 
              )
            )
          )
      (setf (second-hand-highest agent) highest)
      highest
      )
  )

(defvar first-hand (make-instance 'evi-meaning :name "first-hand" :person "first" :temporality "recently" :prob-dist 'first-prob-dist))
(defvar first-hand-past (make-instance 'evi-meaning :name "first-hand" :person "first" :temporality "past" :prob-dist 'first-prob-dist))

(defvar second-hand (make-instance 'evi-meaning :name "second-hand" :person "second" :temporality "recently" :prob-dist 'second-prob-dist))
(defvar second-hand-past (make-instance 'evi-meaning :name "second-hand" :person "second" :temporality "past" :prob-dist 'second-prob-dist))

(defvar no-meaning (make-instance 'evi-meaning :name "no-meaning"))

(defvar *evidential-meanings* (list first-hand second-hand))
(defvar *evidential-meanings-with-time* (list first-hand second-hand first-hand-past second-hand-past))

(defvar kl-threshold-list '(5 10 15 20 25)) ;; suuper arbitrary, look out

;; defining a test lexicon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar test-first-hand (make-instance 'evi-lex :evi-lex-meaning first-hand :form "testfirst" :reinforcement-score 1.0))
(defvar test-first-hand-past (make-instance 'evi-lex :evi-lex-meaning first-hand-past :form "testfirstpast" :reinforcement-score 1.0))

(defvar test-second-hand (make-instance 'evi-lex :evi-lex-meaning second-hand :form "testsecond" :reinforcement-score 1.0))
(defvar test-second-hand-past (make-instance 'evi-lex :evi-lex-meaning second-hand-past :form "testsecondpast" :reinforcement-score 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; important misc functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-evi-lex (lex agent)
  (push lex (evi-lexicon agent))
  lex)

(defun add-obj-lex (lex agent)
  (push lex (obj-lexicon agent))
  lex)

;; finds the object in the lexicon corresponding to the weather pattern observed
(defun find-obj-lex-for-last-seen (w agent)
  (loop for lex in (obj-lexicon agent)
    do
    (when (string= (form lex) (weather-name w))
      (return lex)))
  )

;; boolean reset at the beginning of every series in a batch––
;; indicates whether object lexicons have been initialized or not
(defvar lex-initialized? nil)

(defmethod initialize-agent ((agent evi-agent) experiment)
  (setf (applied-obj-lex agent) nil)
  (setf (applied-evi-lex agent) nil)
  (setf (communicated-successfully agent) t)
  (setf (has-access? agent) nil)
  (setf (kl-threshold agent) (random-elt kl-threshold-list))
  (unless (get-configuration experiment :test-lexicon-switch) 
    (forget-bad-evidentials agent))
  (when (evi-lexicon agent)
    (loop for lex in (evi-lexicon agent)
      do
      (incf (evi-age lex))
      )
    )
  (when (obj-lexicon agent)
    (when (> (length (obj-lexicon agent)) (get-configuration experiment :total-nr-of-objects))
      (print (length (obj-lexicon agent)))
      (break)
      )
    (loop for lex in (obj-lexicon agent)
      do
      (incf (how-recently lex)) ;; at the beginning of every interaction, increment the number of interactions that have gone by
      (incf (how-recently-interaction lex)) ;; without perceiving an object directly or through interaction
      )                         
    )
  (unless lex-initialized?
    (loop for i from 0 to (- (get-configuration experiment :total-nr-of-objects) 1)
      do
      (add-obj-lex (make-instance 'obj-lex :obj-lex-meaning (svref (eval weather) i) 
        :form (weather-name (svref (eval weather) i))) agent))

    ;; when testing, add the test lexicon at the beginning
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (when (get-configuration experiment :test-lexicon-switch)
      (add-evi-lex test-first-hand agent)
      (add-evi-lex test-first-hand-past agent)
      (add-evi-lex test-second-hand agent)
      (add-evi-lex test-second-hand-past agent)
      (setf (first-hand-highest agent) (get-configuration experiment :test-lexicon-first-highest))
      (setf (second-hand-highest agent) (get-configuration experiment :test-lexicon-second-highest)))
  )
  )

;; neat print out for evidential suffixes
(defmethod print-lex ((lex evi-lex) stream)
  (pprint-logical-block (stream nil)
    (format stream "~%Evidential Form: ~a -> Person: ~a -> Temporality: ~a -> Reinforcement Score: ~a" 
              (form lex)
              (name (evi-lex-meaning lex))
              (temporality (evi-lex-meaning lex))
              (reinforcement-score lex)
            )
    )
  )

;; neat print out for actual, prior, evidential, and posterior distributions
(defmethod print-probs ((lex obj-lex) stream)
  (pprint-logical-block (stream nil)
    (format stream "~%~a: Actual ~a --> Prior ~a --> Evi ~a --> Post ~a~@
      "
      (form lex) 
      (weather-likelihood (obj-lex-meaning lex))
      (prior-est lex)
      (evi-modified-est lex)
      (posterior-est lex)
      )))

(define-event lex-added (lex evi-lex))

(defvar test-utterance 'test-utterance)
(defvar test-meaning 'test-meaning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conceptualization & production ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun choose-topic (agent spread-rumor?)
  (if spread-rumor?
    (setf (applied-obj-lex agent)
        (random-elt (obj-lexicon agent)))
    (setf (applied-obj-lex agent)
      (find-obj-lex-for-last-seen (last-seen agent) agent))
    )
  )

(defun find-appropriate-evi-lex (w agent experiment)
  (if (list-agent-evi-lex agent (assign-meaning w agent experiment))
    (random-elt (list-agent-evi-lex agent (assign-meaning w agent experiment)))
    (random-elt (evi-lexicon agent))
    )
  )

(defun produce (w agent experiment)
  ;; if agent's evidential lexicon is non-empty
  (if (evi-lexicon agent)
    ;; find an appropriate evidential to express deictic relation to weather input
    (setf (applied-evi-lex agent)
      (find-appropriate-evi-lex w agent experiment))
    ;; otherwise, make one up, then do the previous thing
    (progn 
      (invent-evi-name w agent experiment)
      (find-appropriate-evi-lex w agent experiment))
    )
    ;; build an utterance string, then output it  
  (setf (utterance agent) (when (and (applied-obj-lex agent) (applied-evi-lex agent))
    (concatenate 'string (form (applied-obj-lex agent)) "-" (form (applied-evi-lex agent)))))
  (utterance agent)
  )

(defun invent-evi-name (w agent experiment)
  (add-evi-lex (make-instance 'evi-lex 
    :form (make-new-word) 
    :evi-lex-meaning (assign-meaning w agent experiment)
    :reinforcement-score (random 1.0))
           agent))

(defun invent-obj-name (agent x name)
  (add-obj-lex (make-instance 'obj-lex :form name :obj-lex-meaning 'test-utterance :personal-access test-meaning
                          :prior-est x )
           agent))

(defun assign-meaning (w agent experiment)
  (if (get-configuration experiment :time-switch)

    ;; with time applicable:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (<= (how-recently (applied-obj-lex agent)) (how-recently-interaction (applied-obj-lex agent)))
      (if (<= (how-recently (applied-obj-lex agent)) (get-configuration experiment :recently-past-threshold))
        first-hand ;; if object has been directly observed less than 5 interactions ago, use first hand
        first-hand-past ;; otherwise use past tense
        )
      (if (<= (how-recently-interaction (applied-obj-lex agent)) (get-configuration experiment :recently-past-threshold)) 
        second-hand ;; if object has been directly observed less than 5 interactions ago, use second hand
        second-hand-past ;; otherwise use past tense
        )
      )

    ;; without time applicable
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if 
      (and 
        (has-access? agent) 
        (eq (find-obj-lex-for-last-seen w agent) (applied-obj-lex agent)))
    first-hand
    second-hand
    )
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing & interpretation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse ((agent evi-agent))
  ;; take the first half of the parsed utterance and find the object that corresponds to it
  (setf (applied-obj-lex agent) (find (nth 0 (my-split (list (char "-" 0)) (utterance agent))) 
    (obj-lexicon agent) :key 'form :test #'equal))
  ;; take the second half and find an evidential which corresponds to it
  (setf (applied-evi-lex agent) 
    (find (nth 1 (my-split (list (char "-" 0)) (utterance agent))) 
      (evi-lexicon agent) :key 'form :test #'equal))
  ;; when an evidential was found, 
  (when (applied-evi-lex agent)
    ;; add to the count of parses
    (incf (parsed-num (applied-evi-lex agent)))
    ;; measures for gaps betwen interactions
    (push (how-recently-interaction (applied-obj-lex agent)) (interaction-gap-list agent))
    (setf (how-recently-interaction (find (nth 0 (my-split (list (char "-" 0)) (utterance agent))) 
      (obj-lexicon agent) :key 'form :test #'equal)) 0)
    ;; important boolean indicating successful parsing
    (setf (parsed? agent) t)
    ;; output the meaning of the parsed evidential
    (evi-lex-meaning (applied-evi-lex agent))
    )
  )

(defun make-observation (agent)
  (when (has-access? agent)
    (incf (number-observations (find-obj-lex-for-last-seen (last-seen agent) agent))) ;; increment number of observations of this object
    (push (how-recently (find-obj-lex-for-last-seen (last-seen agent) agent)) (observation-gap-list agent))
    (setf (how-recently (find-obj-lex-for-last-seen (last-seen agent) agent)) 0) ;; reset how recently this object was directly observed
    (setf (last-surprise-prior agent) (generate-surprise-prior-observation agent))
    (setf (last-surprise-posterior agent) (generate-surprise-posterior-observation agent))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; invention & adoption ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-unused-meaning (agent experiment)
  (if (get-configuration experiment :time-switch)
    ;; with time
    ;;;;;;;;;;;;;;;;
    (loop for meaning in *evidential-meanings-with-time*
      do
      (unless (check-meaning-competitors meaning agent)
        (return meaning))
      )
    ;; without time
    ;;;;;;;;;;;;;;;;;;;
    (loop for meaning in *evidential-meanings*
      do
      (unless (check-meaning-competitors meaning agent)
        (return meaning))
      )
    )
  )

(defun adopt-name (agent experiment)
  (let 
    ((lex (make-instance 'evi-lex 
    :form (nth 1 (my-split (list (char "-" 0)) (utterance agent))) 
    :evi-lex-meaning (find-unused-meaning agent experiment)
    :reinforcement-score (random 1.0))
    ))
    ;; only when a meaning hasn't been used up already does an agent add a word to its lexicon
    (when (find-unused-meaning agent experiment)
      (add-evi-lex lex agent)
      (setf (applied-evi-lex agent) lex)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;
;;    interact!    ;;
;;;;;;;;;;;;;;;;;;;;;

(defmethod interact (experiment interaction &key)

  (let ((speaker (speaker experiment))
        (hearer (hearer experiment))
        (accessing-agents (random-subset (copy-list (agents experiment)) :include-empty-set? nil))
        (utterance nil)
        ;; setting the weather pattern for the experiment depends on multiple factors:
        ;; firstly, check if there was a weather pattern defined in a previous interaction, 
        ;; whether the configuration value of variable weather patterns within epochs is false,
        ;; and whether the current interaction is not at an epoch boundary.
        ;; if these are true, then just set this interaction's weather pattern to that of the
        ;; last interaction. otherwise, check again whether the interaction number is at an
        ;; epoch boundary. if so, and a variable distribution across time is enabled,
        ;; grab a weather pattern out of the hat while also shifting the distribution by 
        ;; a preset amount. if within the epoch, just grab a weather pattern out of the hat
        ;; with a fixed distribution.
        (current_weather 
          (if 
            (and (last-weather experiment) 
            (not (get-configuration experiment :weather-change-within-epoch?))
              (not 
                (check-time-interval 
                  (get-configuration experiment :time-interval)
                  (interaction-number interaction)))
              )
            ;; return last weather pattern     
            (last-weather experiment)
            ;; else if
            (if (check-time-interval 
                        (get-configuration experiment :time-interval) 
                        (interaction-number interaction)
                        )
                      (check-weather (weather-vec (world experiment)) 
                              (get-configuration experiment :time-switch)
                              (get-configuration experiment :time-distribution-change-amt)
                              )
                      (check-weather (weather-vec (world experiment)) nil 0)
                      ))
          )
        )

  ;; print out the current weather for debugging
  (print (weather-name current_weather))
  ;; initialize all agents in the population
  (loop for a in (population experiment)
    do 
    (initialize-agent a experiment)
    )
  (setf lex-initialized? t)  ;; prevent readding objects to lexicon


  ;; setting the access of the randomly selected agents
  (loop for a in accessing-agents
    do
     (setf (has-access? a) t)
     (setf (last-seen a) current_weather)
     (make-observation a) ;; increment this weather's number of observations
     (adjust-prior a) ;; produce the normalized prior distribution with this new observation taken into account

     ;; how surprised was the agent in seeing this weather? were they expecting this?
     (incf (kl-divergence-lex (find-obj-lex-for-last-seen current_weather a)) (- (appraise-surprise current_weather a) 1))
    )
   (print (concatenate 'string "Accessing #: " (write-to-string (list-length accessing-agents))))

    ;; speaker chooses an object to talk about and invents a phrase
    (if (has-access? speaker)
      (choose-topic speaker nil) ;; don't spread rumor if speaker has access, just report

      (if (biased-coin-flip (get-configuration experiment :rumor-accuracy-rate)) ;; spread rumor otherwise
        (progn
          (setf (has-access? speaker) t)
          (setf (last-seen speaker) current_weather) ;; if the rumor accuracy rate randomly chooses this rumor to be accurate,
          (choose-topic speaker nil) ;; give speaker access to the weather and choose that as the topic
          ) 
        (choose-topic speaker t) ;; otherwise just spread an actual rumor
        ) 
      )

    ;; invent an evidential when either the speaker's evidential lexicon is empty, or if it's randomly selected to do so by the invention rate.
    ;; when testing a preset evidential framework, don't invent any new evidentials.
    (if (evi-lexicon speaker)
      (when 
        (and 
          (not (get-configuration experiment :test-lexicon-switch)) 
          (biased-coin-flip (get-configuration experiment :invention-rate)))
        (invent-evi-name current_weather speaker experiment)
        )
      (invent-evi-name current_weather speaker experiment)
      )
    
    ;; set the utterance, then have a very brief and joyless conversation

    (setf utterance (produce current_weather speaker experiment))
    (setf (utterance speaker) utterance)
    (setf (utterance hearer) utterance)
    (when (utterance hearer)
      (print (utterance hearer))
      (terpri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining how the probability distributions interact

      (if (parse hearer) ;; if the hearer successfully parsed,
        (progn 
          (funcall (prob-dist (parse hearer)) hearer (temporality (parse hearer)) experiment) ;; set the probability distribution to use
              (loop for l in (obj-lexicon hearer)
                do
                ;; if it's the case that the hearer has already used its prior distribution to generate a
                ;; posterior and that the configuration allows for the posterior distribution
                ;; to feedback on itself, set posterior to be the evidential prob distribution multiplied
                ;; by the current posterior distribution
                (if (and (used-prior-obj? l) (get-configuration experiment :feedback-posterior?))
                  (progn
                    (setf (posterior-est l) (* (evi-modified-est l) (posterior-est l)))
                    )
                  ;; otherwise, use the prior rather than the posterior
                  (progn
                    (setf (posterior-est l) (* (evi-modified-est l) (prior-est l)))
                    (setf (used-prior-obj? l) t)
                    )
                  )
                )
              (normalize-posterior hearer)
              ;; print out the probabilities for each item in lexicon (after normalization)
              (loop for l in (obj-lexicon hearer)
                do
                (print-probs l nil)
                )
              (setf (info-gain hearer) (- (h-sum-prior hearer) (h-sum-posterior hearer))))
        ;; if the hearer didn't parse, adopt the name 
          (progn
            (adopt-name hearer experiment)
            (when (get-configuration experiment :test-lexicon-switch)
              (break "Agent adopted a name it didn't recognize during a test lexicon!")))
        ) ;; end if (parse hearer)    
      ) ;; end when (utterance hearer)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; reinforcement/punishment for good evidential use
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (loop for a in (population experiment)
      do
      ;; check if at a period in between epochs/intervals
      (when 
        (and 
          (get-configuration experiment :time-switch) 
          (check-time-interval (get-configuration experiment :time-interval) (interaction-number interaction)) 
          (get-configuration experiment :refresh-posterior-with-prior?))
        (loop for lex in (obj-lexicon a)
          do
            (setf (posterior-est lex) (prior-est lex)) 
            (setf (kl-divergence-lex lex) 0.0) ;; reset the accumulated surprise score for this object
          )
       (normalize-posterior a)
        )
      (reinforce-and-decrement a experiment)

      (unless (get-configuration experiment :test-lexicon-switch) 
        (forget-bad-evidentials a)
        )
      (normalize-posterior a)

      ) ;; end loop 

    ;; for the next interaction, store the current weather pattern
    (setf (last-weather experiment) current_weather)

      
  ) ;; end let

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pushing data to recorders for graphing!
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (push-data-to-recorders experiment)
  (terpri) ;; newline
  (finish-interaction experiment) ;; signals all data monitors that the interaction is finished
 (re-randomize) ;; reset the random seed for the next interaction 

 
) ;; end interact


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alignment/reinforcement ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lists all actively used evidentials in the global vocabulary for data collection
(defun list-all-active-evi (experiment)
  (let ((end-list ()))
    (loop for a in (population experiment)
      do
      (when (evi-lexicon a)
        (loop for l in (list-agent-evi-forms a)
          do
          (unless (member l end-list :test #'equal)
            (push l end-list)
            )
          )
        )
      )
    end-list
    )
  )

(defun reinforce-and-decrement (agent experiment)
  (when 
    (and 
      (not (get-configuration experiment :test-lexicon-switch)) 
      (> (sum-kl-divergences agent) (kl-threshold agent))) ;; when kl-divergences get above an assigned threshold...
    (loop for e in (evi-lexicon agent)
      do
      (decf (reinforcement-score e) (/ (random (get-configuration experiment :evi-score-dec-amt)) (log (length (population experiment)) 2))) ;; decrease score of all evidentials...
      (setf (first-hand-highest agent) nil) ;; and forget the assignment made on probability distributions for both first and second hand
      (setf (second-hand-highest agent) nil)
      ) 
    )
  (loop for e in (evi-lexicon agent)
    do
    (incf (reinforcement-score e) (get-configuration experiment :evi-score-inc-amt))
    )
  )

(defun forget-bad-evidentials (agent)
  (when (evi-lexicon agent)
    (loop for l in (evi-lexicon agent)
      do
      (when (< (reinforcement-score l) 0.0)
        (push (evi-age l) (evi-age-list agent)) ;; record how old this evidential was when it was jettisoned
        (print (concatenate 'string "deleted! " (form l)))
        (setf (evi-lexicon agent) (delete l (evi-lexicon agent)))
        )
      )
    )
  )

(defun appraise-surprise (w agent)
  (when (has-access? agent)
    (let ((p-est (posterior-est (find-obj-lex-for-last-seen w agent))))
      ;; assuming the particular weather hasn't been perceived in use with an evidential,
      ;; assign surprise to 0, since the agent is just seeing this pattern for the first time
     (if (= p-est 0.0)
      0.0
      (ind-surprise p-est)
      )
     )
    )
  )
