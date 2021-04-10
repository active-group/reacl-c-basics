(ns reacl-c-basics.program
  "Programs are an abstraction over items, that have a dedicated result, and form an async monad."
  (:require #?(:cljs [reacl-c.core :as c :include-macros true])
            #?(:clj [reacl-c.core :as c])
            #?(:cljs [reacl-c.dom :as dom :include-macros true])
            #?(:cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [active.clojure.record :as r])
            #?(:cljs [reacl-c-basics.core :as core])
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

;; Note: items are usually nouns, but programs are usually best named with verbs.

(r/define-record-type ^{:rtd-record? true :private true} Done
  (done result) done?
  [result done-result])

(r/define-record-type ^{:rtd-record? true :private true} Program
  (make-program running not-running) really-program?
  [running program-running
   not-running program-not-running])

(letfn [(eager-f [running-item not-running-item running?]
          (if running? running-item not-running-item))]
  (defn- eager
    "A program represented by `running-item` in the running state, and `not-running-item` in the non-running state."
    [running-item not-running-item]
    (make-program running-item not-running-item)))

(defn program?
  "A predicate for programs."
  [v]
  (really-program? v))

(defn- running
  "An item representing the program `p` in the running state."
  [p]
  (program-running p))

(defn not-running
  "An item representing the program `p` in the not-running state."
  [p]
  (program-not-running p))

(defn return
  "A program that immediately returns the given value once when run."
  [v]
  (eager
   (c/init (c/return :action (done v)))
   c/empty))

(defn show "A program that just shows the given item, but never completes."
  [item]
  (eager item item))

(let [k (fn [f st a]
          (if (done? a)
            (f st (done-result a))
            (c/return :action a)))]
  (defn- handle-result [item f]
    (-> item
        (c/handle-action (f/partial k f)))))

(r/define-record-type ^{:rtd-record? true :private true} Jump
  (jump program) jump?
  [program jump-program])

(let [make-run (fn [[st [c run?]]]
                 (if run?
                   (c/return)
                   (c/return :state [st [c true]])))
      f (fn [[_ [current run?]]]
          ;; Note: we always have to render the continuation in not-runing state first, because if
          ;; the previous and next programs are 'identical', then local-states have to be cleared first.
          (-> (c/fragment (c/focus lens/first (if run? (running current) (not-running current)))
                          (when-not run? (c/once make-run)))
              (c/handle-action
               (fn [state a]
                 (if (jump? a)
                   (c/return :state [(first state) [(jump-program a) false]])
                   (c/return :action a))))))]
  (defn- run-on-trampoline [init]
    (c/local-state [init true]
                   (c/dynamic f))))

(defn jump-once [program]
  (c/init (c/return :action (jump program))))

(letfn [(bind-p1-done [st result]
          (c/return :state [(first st) (done result)]))
        (bind-run [[_ p1-result] program cont]
          (if-not (done? p1-result)
            (-> (running program)
                (handle-result bind-p1-done))
            (running (cont (done-result p1-result)))))]
  (defn- simple-bind
    [program cont]
    (eager
     (c/local-state nil
                    (c/dynamic bind-run program cont))
     (not-running program))))

(letfn [(bind-k [cont _ result] (c/return :action (jump (cont result))))]
  (defn- tailrec-bind
    [program cont]
    (eager
     ;; the first program runs on it's own trampoine, the continuation
     ;; is 'thrown' to the trampoline running this bind.
     (-> (run-on-trampoline program)
         (handle-result (f/partial bind-k cont)))
     (not-running program))))

(def ^:private use-trampoline? true) ;; makes binds resp. sequ tail-recursive.

(defn bind ;; TODO: rename 'then' ?
  "A program that runs `program` first, and then the program `(cont
  result)`, where `result` is the result of the first program."
  [program cont]
  (assert (program? program) program)
  (if use-trampoline?
    (tailrec-bind program cont)
    (simple-bind program cont)))

(c/defn-item runner
  "An item that runs the given program once, offering an event handler
  for handling the result of the program."
  [program & [handle-result-f]]
  (-> (if use-trampoline? (run-on-trampoline program) (running program))
      (handle-result (fn [state r]
                       (if (some? handle-result-f)
                         (handle-result-f state r)
                         (c/return))))))

;; TODO ?
#_(c/defn-item mutator [f]
  ;; run program (f state), setting state on done, then restart immediately
  )

(defn wrap*
  "A program wrapped in some additional markup, via `(f item running?)`,
  where `item` is the item representing the program, and `running?` a
  boolean specifying if the program is in the running state or not."
  [f program]
  (eager (f (running program) true)
         (f (not-running program) false)))

(letfn [(wrap-f [f item _]
          (f item))]
  (defn wrap
    "A program wrapped in some additional markup, via `(f item)`, where
  `item` is the item representing the program in either the running or
  the non-running state."
    [f program]
    (wrap* (f/partial wrap-f f) program)))

(defn fmap
  "A program that applies f to the result of the given program."
  [f program]
  (bind program (f/comp return f)))

(let [check-state (fn [done-state? st]
                    (if (done-state? st)
                      (c/return :action (done st))
                      (c/return)))
      check-state-change (fn [done-state? prev new]
                           (c/merge-returned (check-state done-state? new)
                                             (c/return :state new)))]
  (defn await-state
    "A program that shows the given item to start it, until its state is a
  non-nil value, or the given predicate holds, returning that value as
  the program's result. Does not show the item when the program is not
  running."
    [item & [done-state?]]
    (eager
     (let [done-state? (or done-state? some?)]
       (-> (c/fragment
            item
            (c/once
             ;; check initial state; could be done already
             (f/partial check-state done-state?)))
           (c/handle-state-change
            (f/partial check-state-change done-state?))))
     c/empty)))


(let [f (fn [pred a]
          (if (pred a)
            (done a)
            a))]
  (defn await-action
    "A program that shows the given item to start it, until it emits an
  action where the given predicate holds, which is then returned as
  the programs result. Does not show the item when the program is not
  running."
    [item pred]
    (eager
     (-> item
         (c/map-actions (f/partial f pred)))
     c/empty)))

(let [k (fn [sequ-f programs _]
          (apply sequ-f programs))]
  (defn sequ
    "A program consisting of several programs run one after the other,
  returning the result of the last one."
    [program & programs]
    (if (empty? programs)
      program
      (bind program (f/partial k sequ programs)))))

#?(:cljs
   (defn sleep
     "A program that returns `nil` after the given number of milliseconds
  have elapsed."
     [ms]
     (eager (c/handle-action (core/timeout ms)
                             (f/constantly (c/return :action (done nil))))
            c/empty)))

(defn- par-base [programs run-p]
  (eager
   (apply c/fragment
          (map-indexed (partial c/dynamic run-p programs)
                       programs))
   (apply c/fragment
          (map not-running programs))))

(letfn [(par-wrap [item running?]
          (if running?
            (c/local-state {}
                           item)
            item))
        (par-run-p [_ programs idx p]
          (runner (wrap (f/partial c/focus lens/first) p)
            (fn on-result [state result]
              (let [new-parts (assoc (second state) idx result)]
                (cond-> (c/return :state [(first state) new-parts])
                  (= (count new-parts) (count programs))
                  (c/merge-returned (c/return :action (done (mapv new-parts (range (count programs)))))))))))]
  (defn par [& programs]
    "A program running several programs in parallel, returning a sequence
  of all their results after all are finished. The visual items of the
  programs are composed using [[reacl-c.core/fragment]]."
    (wrap* par-wrap
           (par-base programs par-run-p))))

(letfn [(race-wrap [item running?]
          (if running?
            (c/local-state nil ;; -> [idx result] when first is done
                           item)
            item))
        (race-run-p [state _ idx p]
          (if-let [[winner-idx _] (second state)]
            (if (= idx winner-idx)
              (running p) ;; keep the winner in 'running' state, until the whole 'race' is rendered non-running.
              (not-running p))
            (runner (wrap (f/partial c/focus lens/first) p)
              (fn on-result [state result]
                (if (some? (second state))
                  ;; two 'simoultanous' winners? can that happen? Maybe ignore second then...?
                  (do (assert false (str "Race had more than one winner? Previously: " (pr-str (second state)) ", now: " (pr-str [idx result])))
                      (c/return))
                  (let [winner [idx result]]
                    (c/return :state [(first state) winner]
                              :action (done result))))))))]
  (defn race
    "A program running several programms in parallel, until the first one
  is finished, returning the result of that 'winner'."
    [program & programs]
    (wrap* race-wrap
           (par-base programs race-run-p))))

(defmacro do-program
  "A monadic do notation for programs.
  
  For example:

  ```
  (do-program
    [r1 (return 21)]
    (return (* r1 2)))
  ```
  "
  [form & forms]
  (cond
    (empty? forms)
    (cond
      (vector? form) (second form)
      :else form)

    :else
    (cond
      (vector? form) `(bind ~(second form) (fn [~(first form)] (do-program ~@forms)))
      :else `(sequ ~form (do-program ~@forms)))))

#?(:cljs
   ;; TODO: dom/defn-dom (optional attrs)
   (c/defn-item run-button
     "A dom button that runs a program each time it is clicked, but is
     disabled while it is running. The program will be rendered after
     the button in a fragment."
     [attrs & content]
     (-> (c/with-state-as [_ run? :local false]
           (assert (not (contains? attrs :onclick)))
           (let [prog (:program attrs)
                 onresult (:onresult attrs)]
             (c/fragment
              (-> (c/focus lens/first
                           (apply dom/button (-> attrs
                                                 (dissoc :program :onresult)
                                                 (assoc :onclick (fn [st _]
                                                                   (c/return :action ::start)))
                                                 (assoc :disabled (or (:disabled attrs) run?)))
                                  content))
                  (c/handle-action
                   (fn [st a]
                     (cond
                       (= ::start a)
                       (assoc st 1 true)
                       :else
                       (c/return :action a)))))
              (when run?
                (runner prog
                  (fn [st result]
                    (c/return :state (assoc st 1 false)
                              :action [::result result])))))))
         (c/handle-action
          (fn [st a]
            (cond
              (and (vector? a) (= ::result (first a)))
              (if-let [f (:onresult attrs)]
                (f st (second a))
                (c/return))
              :else
              (c/return :action a)))))))

