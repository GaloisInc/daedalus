#lang racket

;; A simple automaton viewer GUI
;;
;; To use: invoke with a filename that points at a JSON automaton.
;;
;; A JSON automaton has the following format: it consists of an object with 3 keys:
;;  - "name": The name of the automaton (e.g. based on its DDL)
;;  - "states": A JSON array of states (see later)
;;  - "transitions": A JSON array of transitions
;;
;; A JSON state is an object with the following keys:
;;  - "name": The name of the state (must be unique)
;;  - "start": A Boolean - whether the state is a start state
;;  - "accepting": A Boolean - whether the state is an accepting state
;;  - "details" (optional): Extra information to show about the state when clicked on
;;
;; A JSON transition is an object with the following keys:
;;  - "source": the name of the source state
;;  - "target": the name of the target state
;;  - "summary": the label to show in the viewer
;;  - "details" (optional): Extra info to show when the transition's source state is clicked on
;;
;; Example:
;;   {"name": "My automaton", "states": [{"name": "S0", "start": true, "accepting": true, "details": "blah blah blah"}], "transitions": [{"source": "S0", "target": S0, "summary": "ϵ", "details": "Go!"}]} 

(require racket/gui)
(require mrlib/graph)
(require framework)
(require json)

(define current-base-state-font-size (make-parameter 12))

(define get-state-font
  (let ((fonts (make-hash)))
    (lambda (zoom-level)
      (or (hash-ref fonts zoom-level #f)
          (let ([the-font (make-font #:size (* (current-base-state-font-size) zoom-level))])
            (hash-set! fonts zoom-level the-font)
            the-font)))))
                               

(define state-snip%
  (class (graph-snip-mixin snip%)
    (init-field name zoom-level [details #f] [accepting? #f])
      
    (super-new)
    
      
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (call-with-values
       (lambda ()
         (send dc get-text-extent name (get-state-font (unbox zoom-level))))
       (lambda (name-width name-height _1 _2)
         (maybe-set-box! w (+ 10.0 name-width))
         (maybe-set-box! h (+ 10.0 name-height))
         (maybe-set-box! descent 1.0)
         (maybe-set-box! space 1.0)
         (maybe-set-box! lspace 1.0)
         (maybe-set-box! rspace 1.0))))
      
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-font (send dc get-font))
      (send dc set-font (get-state-font (unbox zoom-level)))
      (call-with-values (lambda () (send dc get-text-extent name))
                        (lambda (w h _1 _2)
                          (send dc draw-ellipse
                                (+ x 1.0) (+ y 1.0)
                                (+ w 10.0)
                                (+ h 10.0))
                          (when accepting?
                            (send dc draw-ellipse (+ x 3.0) (+ y 3.0)
                                  (+ w 6.0)
                                  (+ h 6.0)))
                                
                          (send dc draw-text name (+ x 5.0) (+ y 5.0) #t)))
      (send dc set-font old-font))
      
    (define/override (copy)
      (new state-snip% [name name] [zoom-level zoom-level]))

    (define/public (get-details)
      (or details name))))

(struct state (name start? accepting? details) #:transparent)
(struct transition (source target summary details) #:transparent)
(struct graph (name states transitions) #:transparent)

(define (graph-well-formed? g)
  (let ([all-names (set->list (list->set (map state-name (graph-states g))))])
    (= (length all-names) (length (graph-states g)))))

(define (jsexpr->graph jsexpr)
  (define states (make-hash))
  (define (non-null val)
    (if (eqv? val (json-null)) #f val))
  (graph (hash-ref jsexpr 'name)
         (for/list ([state-source (hash-ref jsexpr 'states)])
           (let ([name (hash-ref state-source 'name)]
                 [start? (hash-ref state-source 'start #f)]
                 [accepting? (hash-ref state-source 'accepting #f)]
                 [details (non-null (hash-ref state-source 'details #f))])
             (let ([s (state name start? accepting? details)])
               (hash-set! states name s)
               s)))
         (for/list ([transition-source (hash-ref jsexpr 'transitions)])
           (let ([src (hash-ref transition-source 'source)]
                 [tgt (hash-ref transition-source 'target)]
                 [summary (hash-ref transition-source 'summary)]
                 [details (non-null (hash-ref transition-source 'details #f))])
             (transition (hash-ref states src) (hash-ref states tgt) summary details)))))

(define (graph->jsexpr g)
  (hash 'name (graph-name g)
        'states (for/list ([s (graph-states g)])
                  (hash 'name (state-name s)
                        'start (state-start? s)
                        'accepting (state-accepting? s)
                        'details (state-details s)))
        'transitions (for/list ([t (graph-transitions g)])
                       (hash 'source (state-name (transition-source t))
                             'target (state-name (transition-target t))
                             'summary (or (transition-summary t) (json-null))
                             'details (or (transition-details t) (json-null))))))

(define (read-graph)
  (jsexpr->graph (read-json)))

(define (serialize-graph g)
  (write-json (graph->jsexpr g)))

(define (go g)
  (define state-snips (make-hash))
  (define snip-states (make-hash))
  (define transition-info (make-hash))
  (define zoom-level (box 1.0))

  (define selected-snips (mutable-set))
  
  (define frame (new frame% [label (graph-name g)] [height 700] [width 700]))
  (define outer (new vertical-panel% [parent frame]))
  (define controls (new horizontal-panel% [parent outer] [stretchable-height #f]))
  (define panel (new panel:vertical-dragable% [parent outer]))

  (define pasteboard
    (new (class (graph-pasteboard-mixin pasteboard%)
           (super-new)
           (define (display-selection-info)
             (define to-show (sort (set->list selected-snips) string<?
                                   #:key (lambda (snip)
                                           (state-name (hash-ref snip-states snip)))))
             (define deets
               (string-join
                (for/list ([snip to-show])
                  (define node-deets (send snip get-details))
                  (define out-edge-deets
                    (string-join (for/list ([t (hash-ref transition-info (hash-ref snip-states snip) '())])
                                   (string-append "   To " (state-name (car t)) ":\n"
                                                  (if (cdr t)
                                                      (string-append "      " (cdr t))
                                                      "")))
                                 "\n"))
                  (string-join (list node-deets out-edge-deets) "\n"))
                "\n\n"))
             (send details set-value deets))
           (define/augment (after-select snip on?)
             (if on?
                 (set-add! selected-snips snip)
                 (set-remove! selected-snips snip))
             (display-selection-info)))))
  
  (define canvas (new editor-canvas% [parent panel] [editor pasteboard]))
  (define details (new text-field% [parent panel] [label #f] [style '(multiple)] [font (make-font #:family 'modern)]))

  (define (zoom-button label op)
    (new button%
         [parent controls]
         [label label]
         [callback (lambda (but evt)
                     (define old-zoom (unbox zoom-level))
                     (set-box! zoom-level (max 0.001 (op old-zoom 0.1)))
                     (for ([s (in-hash-values state-snips)])
                       (send (send s get-admin) resized s #t)
                       (define x (box #f))
                       (define y (box #f))
                       (send pasteboard get-snip-location s x y)
                       (define orig-x (/ (unbox x) old-zoom))
                       (define orig-y (/ (unbox y) old-zoom))
                       (send pasteboard move-to s (* orig-x (unbox zoom-level)) (* orig-y (unbox zoom-level)))))]))
  
  (define zoom-in (zoom-button "+" +))
  (define zoom-out (zoom-button "-" -))

  (for ([s (graph-states g)])
    (define snip (new state-snip%
                      [name (state-name s)]
                      [details (state-details s)]
                      [accepting? (state-accepting? s)]
                      [zoom-level zoom-level]))
    (hash-set! state-snips s snip)
    (hash-set! snip-states snip s)
    (send pasteboard insert snip))
  (for ([e (graph-transitions g)])
    (hash-update! transition-info (transition-source e)
                  (lambda (x)
                    (cons (cons (transition-target e) (transition-details e))
                          x))
                  '())
    (add-links (hash-ref state-snips (transition-source e))
               (hash-ref state-snips (transition-target e))
               #f #f #f #f
               (transition-summary e)))

  (dot-positioning pasteboard dot-label #t)

  (send frame show #t))

(define g2
  (let ([states (for/list ([i (in-range 0 50)])
                  (state (format "S~a" i) #f #f (format "More info for ~a" i)))])
    (let ([transitions (for/list ([i (in-range 0 70)])
                         (transition (list-ref states (random 0 50))
                                     (list-ref states (random 0 50))
                                     (format " t~a" i) #f))])
      (graph " test" states transitions))))

#;
(go (let ([s0 (state "Initial state" #t #f #f)]
          [s1 (state "Final state" #f #t "This is the final state")])
      (graph "My pretty automaton.ddl"
             (list s0 s1)
             (list (transition s0 s1 "'a'" "Push a thing, then pop a thing")
                   (transition s1 s1 "ϵ" #f)))))

(module+ main
  (command-line #:args (filename)
                (with-input-from-file filename
                  (lambda ()
                    (define g (read-graph))
                    (go g))
                  #:mode 'text)))
                