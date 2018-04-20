
;;;; This file is covered by the Apache License 2.0 under copyright of VUB AI-Lab Vrije Universiteit Brussel - Sony Computer Science Laboratory Paris - Sony Computer Science Laboratories Inc. - ICREA and Institut de Biologia Evolutiva (UPF-CSIC)
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;; Version: Babel2 10/03/2017
 

(in-package #:asdf)


(defsystem :evidentiality-game
  :depends-on (:test-framework
   :utils
   :monitors
   :web-interface
   :tasks-and-processes
   :meta-layer-learning
   :action-behavior-framework
   :experiment-framework)
  :serial t
  :components 
  ((:file "evidentiality-game")
    (:file "monitors")
    (:file "weather")
    (:file "evidential-utils")
    (:file "configurations")))