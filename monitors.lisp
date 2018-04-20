;;;;;;;;;;;;;;;;;;;;;;;;
;; Alec Shashaty      ;;
;; Vassar College     ;;
;; 2018               ;; 
;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :evidentiality-game)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of series monitor
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor end-series-monitor
	:documentation "Records when a series ends."
	)

(define-event-handler (end-series-monitor series-finished)
	(setf lex-initialized? nil)
	) ;; ensures that object lexicon is not unneccessarily reinitialized

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global vocabulary size 
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor vocab-size-monitor
	:class 'data-recorder
	:documentation "Records size of vocab"
	)

(define-monitor vocab-size-printer
	:class 'data-printer
	:data-sources '(vocab-size-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (vocab-size-monitor interaction-finished)
	(record-value monitor
		(car (vocab-size experiment))))


(define-monitor plot-vocab-size
	:class 'gnuplot-graphic-generator
	:documentation "Plots vocabulary size"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "vocab-size"
		:type "pdf")
	:data-sources '(vocab-size-monitor)
	:caption '("vocabulary size")
	:x-label "number of interactions"
	:y1-label "vocabulary size"
	:error-bars t
	:y1-max 30
	:y1-min 0
	:draw-y1-grid t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average age of evidentials when they're forgotten (by interaction number)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-evi-age-monitor
	:class 'data-recorder
	:documentation "Records average age of evidentials when they're forgotten"
	)

(define-monitor avg-evi-age-printer
	:class 'data-printer
	:data-sources '(avg-evi-age-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (avg-evi-age-monitor interaction-finished)
	(record-value monitor
		(car (average-evi-age-list experiment))))


(define-monitor plot-avg-evi-age
	:class 'gnuplot-graphic-generator
	:documentation "Plots average age of evidentials when they're forgotten"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-evi-age"
		:type "pdf")
	:data-sources '(avg-evi-age-monitor)
	:caption '("average evidential age")
	:x-label "Number of interactions"
	:y1-label "Average evidential age"
	:error-bars t
	:y1-max 15
	:y1-min 0
	:draw-y1-grid t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average MAXIMUM age of evidentials when they're forgotten (by interaction number)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-max-evi-age-monitor
	:class 'data-recorder
	:documentation "Records average age of evidentials when they're forgotten"
	)

(define-monitor avg-max-evi-age-printer
	:class 'data-printer
	:data-sources '(avg-max-evi-age-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (avg-max-evi-age-monitor interaction-finished)
	(record-value monitor
		(car (average-max-evi-age-list experiment))))


(define-monitor plot-max-avg-evi-age
	:class 'gnuplot-graphic-generator
	:documentation "Plots maximum average age of evidentials when they're forgotten"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-max-evi-age"
		:type "pdf")
	:data-sources '(avg-max-evi-age-monitor)
	:caption '("average maximum evidential age")
	:x-label "Number of interactions"
	:y1-label "Average maximum evidential age"
	:error-bars t
	:y1-max 30
	:y1-min 0
	:draw-y1-grid t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average number of interactions between observations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-observation-gap-monitor
	:class 'data-recorder
	:documentation "Records average gap in between observations"
	)

(define-monitor avg-observation-gap-printer
	:class 'data-printer
	:data-sources '(avg-observation-gap-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (avg-observation-gap-monitor interaction-finished)
	(record-value monitor
		(car (average-observation-gap-list experiment))))


(define-monitor plot-avg-observation-gap
	:class 'gnuplot-graphic-generator
	:documentation "Plots average number of interactions between observations"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-obs-gap"
		:type "pdf")
	:data-sources '(avg-observation-gap-monitor)
	:caption '("average observation gap")
	:x-label "Number of interactions"
	:y1-label "Average observation gap"
	:error-bars t
	:y1-max 15
	:y1-min 0
	:draw-y1-grid t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average number of interactions between interactions with the same object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-interaction-gap-monitor
	:class 'data-recorder
	:documentation "Records average gap in between interactions with the same object"
	)

(define-monitor avg-interaction-gap-printer
	:class 'data-printer
	:data-sources '(avg-interaction-gap-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (avg-interaction-gap-monitor interaction-finished)
	(record-value monitor
		(car (average-interaction-gap-list experiment))))


(define-monitor plot-avg-obs-and-int-gap
	:class 'gnuplot-graphic-generator
	:documentation "Plots average gaps between direct observations and conversations about the same object"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-obs-and-int-gap"
		:type "pdf")
	:data-sources '(avg-observation-gap-monitor avg-interaction-gap-monitor)
	:caption '("average observation gap" "average interaction gap")
	:x-label "Number of interactions"
	:y1-label "Average gap"
	:error-bars t
	:y1-max 50
	:y1-min 0
	:draw-y1-grid t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average information gain from prior to posterior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor average-info-gain-monitor
	:class 'data-recorder
	:documentation "Records size of vocab"
	)

(define-monitor average-info-gain-printer
	:class 'data-printer
	:data-sources '(average-info-gain-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (average-info-gain-monitor interaction-finished)
	(record-value monitor
		(car (average-info-gain-list experiment))))


(define-monitor plot-average-info-gain
	:class 'gnuplot-graphic-generator
	:documentation "Plots average info gain"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-info-gain"
		:type "pdf")
	:data-sources '(average-info-gain-monitor)
	:caption '("average information gain")
	:x-label "number of interactions"
	:y1-label "average information gain"
	:error-bars t
	:y1-max 1.0
	:y1-min 0.0
	:draw-y1-grid t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avg surprise between prior/posterior and observation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-surprise-prior-monitor
	:class 'data-recorder
	:documentation "Records average level of surprise compared to prior"
	)

(define-event-handler (avg-surprise-prior-monitor interaction-finished)
	(record-value monitor
		(car (average-prior-surprise-list experiment))
		)
	)

(define-monitor avg-surprise-posterior-monitor
	:class 'data-recorder
	:documentation "Records average level of surprise compared to posterior"
	)

(define-event-handler (avg-surprise-posterior-monitor interaction-finished)
	(record-value monitor
		(car (average-posterior-surprise-list experiment))
		)
	)

(define-monitor plot-average-surprise-observation
	:class 'gnuplot-graphic-generator
	:documentation "Plots average surprise between observation and prior and posterior"
	:graphic-type "pdf"
	:colored t
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-surprise-obs"
		:type "pdf")
	:data-sources '(avg-surprise-prior-monitor avg-surprise-posterior-monitor)
	:caption '("prior" "posterior")
	:x-label "Number of interactions"
	:y1-label "Average surprise"
	:error-bars t
	:y1-max 4.0
	:y1-min 0.0
	:draw-y1-grid t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; percentage of agents whose best guess based on prior and posterior distributions is correct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor percent-correct-guess-prior-monitor
	:class 'data-recorder
	:documentation "Records percentage of agents whose best guess based on prior distribution is correct"
	)

(define-monitor percent-correct-guess-prior-printer
	:class 'data-printer
	:data-sources '(percent-correct-guess-prior-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (percent-correct-guess-prior-monitor interaction-finished)
	(record-value monitor
		(car (percent-correct-guesses-prior-list experiment))))

(define-monitor percent-correct-guess-posterior-monitor
	:class 'data-recorder
	:documentation "Records percentage of agents whose best guess based on posterior distribution is correct"
	)

(define-monitor percent-correct-guess-posterior-printer
	:class 'data-printer
	:data-sources '(percent-correct-guess-posterior-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (percent-correct-guess-posterior-monitor interaction-finished)
	(record-value monitor
		(car (percent-correct-guesses-posterior-list experiment))))


(define-monitor plot-percent-correct-guess
	:class 'gnuplot-graphic-generator
	:documentation "Plots percentage of agents whose best guess based on posterior distribution is correct"
	:graphic-type "pdf"
	:colored t
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "pct-correct-guess"
		:type "pdf")
	:data-sources '(percent-correct-guess-prior-monitor percent-correct-guess-posterior-monitor)
	:caption '("prior" "posterior")
	:x-label "Number of interactions"
	:y1-label "Percentage of correct guesses"
	:error-bars t
	:y1-max 1.0
	:y1-min 0.0
	:draw-y1-grid t)


;; difference between the two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor percent-correct-difference-monitor
	:class 'data-recorder
	:documentation "Records difference between prior and posterior correct guess percentages"
	)

(define-monitor percent-correct-difference-printer
	:class 'data-printer
	:data-sources '(percent-correct-difference-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (percent-correct-difference-monitor interaction-finished)
	(record-value monitor
		(car (percent-correct-difference-list experiment))))



(define-monitor plot-percent-correct-difference
	:class 'gnuplot-graphic-generator
	:documentation "Plots difference between prior and posterior in guessing correctly"
	:graphic-type "pdf"
	:colored t
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "pct-correct-difference"
		:type "pdf")
	:data-sources '(percent-correct-difference-monitor)
	:caption '("Posterior minus prior")
	:x-label "Number of interactions"
	:y1-label "Difference between correct guess percentages of prior and posterior"
	:error-bars t
	:y1-max 1.0
	:y1-min (- 1.0)
	:draw-y1-grid t)

(define-monitor export-percent-correct-difference
	:class 'lisp-data-file-writer
	:documentation "Exports percent correct difference."
	:data-sources '(percent-correct-difference-monitor)
	:file-name (babel-pathname :name "percent-correct-difference" :type "lisp"
							   :directory '("tutorial" "language-games" "evidentiality-game" "raw-data"))
	:add-time-and-experiment-to-file-name t
	)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average across agents of KL divergence from prior to actual distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-prior-kl-monitor
	:class 'data-recorder
	:documentation "Records average across agents of KL divergence from prior to actual distribution"
	)

(define-monitor avg-prior-kl-printer
	:class 'data-printer
	:data-sources '(avg-prior-kl-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (avg-prior-kl-monitor interaction-finished)
	(record-value monitor
		(car (average-kl-divergence-from-prior experiment))))


(define-monitor plot-avg-prior-kl
	:class 'gnuplot-graphic-generator
	:documentation "Plots average info gain"
	:graphic-type "pdf"
	:colored nil
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-prior-kl"
		:type "pdf")
	:data-sources '(avg-prior-kl-monitor)
	:caption '("average KL divergence")
	:x-label "number of interactions"
	:y1-label "Average KL Divergence from prior to actual"
	:error-bars t
	:y1-max 1.0
	:y1-min 0.0
	:draw-y1-grid t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; average across agents of KL divergence from posterior to actual distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor avg-posterior-kl-monitor
	:class 'data-recorder
	:documentation "Records average across agents of KL divergence from posterior to actual distribution"
	)

(define-monitor avg-posterior-kl-printer
	:class 'data-printer
	:data-sources '(avg-posterior-kl-monitor)
	:format-string "~%~d: ~a"
	:interval 1)

(define-event-handler (avg-posterior-kl-monitor interaction-finished)
	(record-value monitor
		(car (average-kl-divergence-from-posterior experiment))))

(define-monitor plot-avg-posterior-kl
	:class 'gnuplot-graphic-generator
	:documentation "Plots average info gain"
	:graphic-type "pdf"
	:colored t
	:add-time-and-experiment-to-file-name t
	:file-name (babel-pathname 
		:directory '("tutorial" "language-games" "evidentiality-game" "graphs")
		:name "avg-posterior-kl"
		:type "pdf")
	:data-sources '(avg-posterior-kl-monitor avg-prior-kl-monitor)
	:caption '("posterior" "prior")
	:x-label "Number of interactions"
	:y1-label "Average KL Divergence"
	:error-bars t
	:y1-max 2.0
	:y1-min 0.0
	:draw-y1-grid t)
