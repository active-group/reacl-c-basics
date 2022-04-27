(ns reacl-c-basics.programs.core
  "Programs are an abstraction over items, that have a dedicated result, and form an async monad."
  (:require #?(:cljs [reacl-c.core :as c :include-macros true])
            #?(:clj [reacl-c.core :as c])
            #?(:cljs [active.clojure.cljs.record :as r :include-macros true])
            #?(:clj [active.clojure.record :as r])
            #?(:cljs [reacl-c-basics.core :as core])
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens])
  (:refer-clojure :exclude [trampoline]))

;; Note: items are usually nouns, but programs are usually best named with verbs.

(r/define-record-type ^{:rtd-record? true :private true} Done
  (done result) done?
  [result done-result])

(r/define-record-type ^{:rtd-record? true :private true} Program
  (make-program r nr) really-program?
  [r running
   nr not-running])

(defn- eager
  "A program represented by `running-item` in the running state, and `not-running-item` in the non-running state."
  [running-item & [not-running-item]]
  (assert (c/item? running-item))
  (assert (or (nil? not-running-item) (c/item? not-running-item)))
  (make-program running-item not-running-item))

(defn- with [f]
  (make-program (f true) (f false)))

(defn program?
  "A predicate for programs."
  [v]
  (really-program? v))

(defn return
  "A program that immediately returns the given value once when run."
  [v]
  (eager (c/init (c/return :action (done v)))))

(defn show "A program that just shows the given item, but never completes."
  [item]
  (eager item item))

(r/define-record-type ^{:rtd-record? true :private true} Jump
  (make-jump program) jump?
  [program jump-program])

(defn- jump-internal [program]
  (assert (program? program))
  (make-jump program))

(let [h (fn [state a]
          (if (jump? a)
            (c/return :state [(first state)
                              {:pc (inc (:pc (second state)))
                               :current (jump-program a)}])
            (c/return :action a)))
      f (fn [running? [_ st]]
          (cond-> (c/focus lens/first ((if running? running not-running) (:current st)))
            ;; use a key to ensure that a second program does not see local states of the previous one.
            true (c/keyed (:pc st))
            running? (c/handle-action h)))]
  (defn- trampoline-internal [running? program]
    (assert (program? program))
    ;; returns an item running program initially, and another program when 'jump' is emitted.
    (c/local-state {:pc 0 :current program}
                   (c/dynamic (f/partial f running?)))))

(let [k (fn [f st a]
          (if (done? a)
            (f st (done-result a))
            (c/return :action a)))]
  (defn- handle-result [item f]
    (-> item
        (c/handle-action (f/partial k f)))))

(let [h (fn [[st _] v]
          [st [v]])
      f (fn [running? program cont [_ res]]
          (let [here (comp (partial c/focus lens/first) (if running? running not-running))]
            (if (empty? res)
              (cond-> (c/keyed (here program) "first")
                running? (handle-result h))
              (c/keyed (here (cont (first res))) "second"))))]
  (defn then
    "A program that runs `program` first, and then the program `(cont
  result)`, where `result` is the result of the first program."
    [program cont]
    (with (fn [running?]
            (c/local-state []
                           (c/dynamic (f/partial f running? program cont)))))))

(let [jmp (fn [cont _]
            (c/return :action (jump-internal (cont))))]
  (defn- then-jump
    [program cont]
    (with (fn [running?]
            (if running?
              (-> (running program)
                  (handle-result (f/partial jmp cont)))
              (not-running program))))))

(let [jumping-program (fn [program]
                        (with (fn [running?]
                                (-> ((if running? running not-running) program)
                                    (handle-result (fn [st res]
                                                     (if (program? res)
                                                       (c/return :action (jump-internal program))
                                                       (c/return :action (done res)))))))))]
  (defn trampoline
    "A trampoline can be used to make programs that use mutual recursion
  run without consuming any stack or heap, thus can potentially run
  forever. This returns a program that runs the given program until it
  returns. If it returns another program, then the execution is
  continues with that program. If it returns something else, then the
  program completes."
    [program]
    (with (fn [running?]
            (trampoline-internal running? (jumping-program program))))))

(let [h (fn [state a]
          (if (done? a)
            (c/return :state [(first state) false]
                      :action a)
            (c/return :action a)))
      f (fn [program [_ running?]]
          (if running?
            (c/fragment
             (-> (c/focus lens/first (running program))
                 (c/handle-action h)))
            (c/fragment (c/focus lens/first (not-running program))
                        (when (nil? running) (c/focus lens/second (c/init true))))))
      ignore-res (fn [st _]
                   st)]
  (c/defn-item runner
    "An item that runs the given program once, offering an event handler
  for handling the result of the program."
    [program & [handle-result-f]]
    (assert (program? program) program)
    (-> (c/local-state true #_nil ;; delay start until mount? why?
                       (c/dynamic (f/partial f program)))
        (handle-result (or handle-result-f ignore-res)))))

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

#_(letfn [(wrap-f [f item _]
          (f item))]
  (defn wrap-running
    "A program wrapped in some additional markup, via `(f item)`, where
  `item` is the item representing the program in the running
  state. The non-running item is not modified."
    [f program]
    (eager (f (running program)) (not-running program))))

(defn fmap
  "A program that applies f to the result of the given program."
  [f program]
  (then program (f/comp return f)))

(let [check-state (fn [done-state? prev new]
                    (if (and (not (done-state? prev))
                             (done-state? new))
                      ;; we must not emit multiple done actions.
                      (c/return :action (done new))
                      (c/return)))
      check-state-change (fn [done-state? prev new]
                           (c/merge-returned (check-state done-state? prev new)
                                             (c/return :state new)))]
  (defn await-state
    "A program that shows the given item to start it, until its state is a
  non-nil value, or the given predicate holds, returning that value as
  the program's result. Shows `not-running` when the program is not
  running, which defaults to [[reacl-c.core/empty]]."
    [item & [done-state? not-running]]
    ;; TODO: help with an item that should not be running on the whole program state? isolate-state, local-state not easy... await-local-state ?
    (eager
     (let [done-state? (or done-state? some?)]
       (-> (c/fragment
            item
            (c/once
             ;; check initial state; could be done already
             (f/partial check-state done-state?)))
           (c/handle-state-change
            (f/partial check-state-change done-state?))))
     not-running)))

#_(defn await-state* [f & [done-state?]]
  (await-state (f true) done-state? (f false)))

(let [f (fn [pred a]
          (if (pred a)
            (done a)
            a))]
  (defn await-action
    "A program that shows the given item to start it, until it emits an
  action where the given predicate holds, which is then returned as
  the programs result. Shows `not-running` when the program is not
  running, which defaults to [[reacl-c.core/empty]]."
    [item pred & [not-running]]
    (eager
     (-> item
         (c/map-actions (f/partial f pred)))
     not-running)))

#_(defn await-action* [f pred]
  (await-action (f true) pred (f false)))

(let [k (fn k [program programs]
          (if (empty? programs)
            program
            (then-jump program (f/partial k (first programs) (rest programs)))))]
  (defn sequ
    "A program consisting of several programs run one after the other,
  returning the result of the last one."
    [program & programs]
    (assert (every? program? (cons program programs)))
    (with (fn [running?]
            (trampoline-internal running? (k program programs))))))

#?(:cljs
   (defn sleep
     "A program that returns `nil` after the given number of milliseconds
  have elapsed."
     [ms]
     (eager (c/handle-action (core/timeout ms)
                             (f/constantly (c/return :action (done nil)))))))

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
        (par-on-result [programs idx state result]
          (let [new-parts (assoc (second state) idx result)]
            (cond-> (c/return :state [(first state) new-parts])
              (= (count new-parts) (count programs))
              (c/merge-returned (c/return :action (done (mapv new-parts (range (count programs)))))))))
        (par-run-p [_ programs idx p]
          (runner (wrap (f/partial c/focus lens/first) p)
                  (f/partial par-on-result programs idx)))]
  (defn par [& programs]
    "A program running several programs in parallel, returning a sequence
  of all their results after all are finished. The visual items of the
  programs are composed using [[reacl-c.core/fragment]]."
    (wrap* par-wrap
           (par-base programs par-run-p))))

(defn race
  "A program running several programms in parallel, until the first one
  is finished, returning the result of that 'winner'."
  [program & programs]
  (eager (apply c/fragment (map running (cons program programs)))
         nil))

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
    ;; TODO: local let bindings.
    (empty? forms)
    (cond
      (vector? form) (second form)
      :else form)

    :else
    (cond
      (vector? form) `(then ~(second form) (fn [~(first form)] (do-program ~@forms)))
      :else `(sequ ~form (do-program ~@forms)))))

(defmacro defn-program
  "A macro just like `defn`, but where the body is wrapped in [[do-program]]."
  [name params & body]
  ;; TODO: docstring, schema annotations, preconditions
  `(defn ~name ~params
     (do-program ~@body)))
