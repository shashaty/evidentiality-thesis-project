;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alec Shashaty         ;;
;; Vassar College        ;;
;; 2018                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :evidentiality-game)

;; setting configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-configuration-default-value :total-nr-of-objects 5)

(define-configuration-default-value :context-size 1)

  ;; population size
(define-configuration-default-value :population-size 20)

  ;; evidential reinforcement configs
(define-configuration-default-value :evi-score-dec-amt 0.5)
(define-configuration-default-value :evi-score-inc-amt 0.03)

  ;; rates of invention/rumor
(define-configuration-default-value :invention-rate 0.2)
(define-configuration-default-value :rumor-accuracy-rate 0.0)

  ;; time configs
(define-configuration-default-value :time-switch t)
(define-configuration-default-value :weather-change-within-epoch? t)
(define-configuration-default-value :time-prob-decrement 0.2)
(define-configuration-default-value :time-distribution-change-amt 0.5)
(define-configuration-default-value :time-interval 100)
(define-configuration-default-value :refresh-posterior-with-prior? t)
(define-configuration-default-value :recently-past-threshold 50)
(define-configuration-default-value :feedback-posterior? t)

  ;; testing configs
(define-configuration-default-value :test-lexicon-switch t)
(define-configuration-default-value :test-lexicon-first-highest 0.4)
(define-configuration-default-value :test-lexicon-second-highest 0.25)


