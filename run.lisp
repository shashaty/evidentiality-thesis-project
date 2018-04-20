;;;;;;;;;;;;;;;;;;;;;;;;
;; Alec Shashaty      ;;
;; Vassar College     ;;
;; 2018               ;; 
;;;;;;;;;;;;;;;;;;;;;;;;

(load "/Users/alecshashaty/Babel2/libraries/asdf.lisp")
(load "/Users/alecshashaty/Babel2/init-babel.lisp")
(asdf:operate 'asdf:load-op 'evidentiality-game)

(in-package :evidentiality-game)

(setf *random-state* (make-random-state t)) ;; makes random act .. well.. randomly

(activate-monitor trace-interaction-in-repl)
(activate-monitor end-series-monitor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; monitor activation for graphing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; several monitors are commented out but functional! try them out!

	;; vocab size
 (activate-monitor plot-vocab-size)

	;; average evidential age
; (activate-monitor plot-avg-evi-age)

	;; average MAXMIUM evidential age
; (activate-monitor plot-max-avg-evi-age)

	;; average observation and interaction gaps
; (activate-monitor plot-avg-obs-and-int-gap)

	;; info gain
; (activate-monitor plot-average-info-gain)

	;; average surprise
(activate-monitor plot-average-surprise-observation)

	;; percentage of correct guesses
;;(activate-monitor plot-percent-correct-guess)

	;; percentage difference
(activate-monitor plot-percent-correct-difference)
;;(activate-monitor export-percent-correct-difference)

	;; prior kl divergence
; (activate-monitor plot-avg-prior-kl)

	;; posterior kl divergence
; (activate-monitor plot-avg-posterior-kl)



(run-batch 'evi-exp-test-nofeed-notime 2000 8)
(run-batch 'evi-exp-test-feed-notime 2000 8)
(run-batch 'evi-exp-test-nofeed-time 2000 8)
(run-batch 'evi-exp-test-feed-time 2000 8)

(run-batch 'evi-exp-notest-nofeed-notime 2000 8)
(run-batch 'evi-exp-notest-feed-notime 2000 8)
(run-batch 'evi-exp-notest-nofeed-time 2000 8)
(run-batch 'evi-exp-notest-feed-time 2000 8)

(deactivate-monitor plot-vocab-size)
; (deactivate-monitor plot-avg-evi-age)
; (deactivate-monitor plot-max-avg-evi-age)
; (deactivate-monitor plot-avg-obs-and-int-gap)

; (deactivate-monitor plot-average-info-gain)

(deactivate-monitor plot-average-surprise-observation)

; (deactivate-monitor plot-percent-correct-guess)

(deactivate-monitor plot-percent-correct-difference)
; (deactivate-monitor export-percent-correct-difference)

; (deactivate-monitor plot-avg-prior-kl)
; (deactivate-monitor plot-avg-posterior-kl)

(deactivate-monitor end-series-monitor)
