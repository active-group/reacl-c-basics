(ns reacl-c-basics.dialogs
  "Utilities for easy usage of User-Agent controlled <dialog> items.

  See https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog
  and https://caniuse.com/?search=dialog

  Note: a major difference between this and the raw dialog is that the
  dialogs here are shown/open by default."
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [clojure.string :as string]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]))

(def ^{:doc "The raw <dialog> dom item."}
  dialog (partial dom/h "dialog"))

(c/defn-effect ^:private set-show-modal! [ref value]
  (let [^js/Dialog d (c/deref ref)]
    (if value
      (.showModal d)
      (.close d))))

(defn- with-ref-attrs [attrs f]
  (if (:ref attrs)
    (f attrs)
    (c/with-ref
      (fn [ref]
        (f (assoc attrs :ref ref))))))

(defn- modal-cancel [user-cancel state ev]
  (.preventDefault ev)
  (if user-cancel
    (user-cancel state ev)
    (c/return)))

(dom/defn-dom modal
  "Shows the content in an open modal dialog.

   The :onCancel event is trigger when the user presses ESC, which
   does nothing per default.

   Render a hidden dialog via the :open attribute set to false, which
   can also be used to show/hide the dialog later."
  [attrs & content]
  (let [user-cancel (or (:onCancel attrs) (:oncancel attrs))]
    (with-ref-attrs attrs
      (fn [attrs]
        (c/fragment
         (apply dialog
                (dom/merge-attributes (dissoc attrs :open :onCancel :oncancel)
                                      {:onCancel (f/partial modal-cancel user-cancel)})
                content)
         (c/execute-effect (set-show-modal! (:ref attrs) (get attrs :open true))))))))

(dom/defn-dom parallel
  "Shows the content in an open non-modal dialog.

   Render a hidden dialog via the :open attribute set to false, which
   can also be used to show/hide the dialog later."
  [attrs & content]
  (apply dialog (dom/merge-attributes {:open true} attrs)
         content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- alternative-evs [name]
  [name (keyword (string/lower-case (str name)))])

(defn- dissoc-ev [attrs & names]
  (apply dissoc attrs (mapcat alternative-evs names)))

(defn- get-ev [attrs name]
  (some (partial get attrs) (alternative-evs name)))

(defrecord ^:private A [id value])

(defn- with-action-modal [attrs id-event-map default f]
  (-> (f (dom/merge-attributes {:onCancel (f/constantly (c/return :action default))}
                               (apply dissoc-ev (dissoc attrs :onCancel :oncancel)
                                      (map second id-event-map))))
      (c/handle-action (fn [state a]
                         (if (and (instance? A a)
                                  (contains? id-event-map (:id a)))
                           (if-let [h (get-ev attrs (get id-event-map (:id a)))]
                             (h state (:value a))
                             (c/return))
                           (c/return :action a))))))

(defn- action-modal [attrs id-event-map default & content]
  (with-action-modal attrs id-event-map default
    (fn [attrs]
      (apply modal attrs content))))

(defn ack
  "Returns an action that can be emitted from the content of
  an [[acknowledge]] dialog."
  ([] (A. :ack nil)))

(dom/defn-dom acknowledge
  "Shows the content in an open modal dialog.

   The :onAck event is triggered when the user presses ESC, or the
   content emits the [[ack]] action."
  [attrs & content]
  (apply action-modal attrs
         {:ack :onAck}
         (ack)
         content))

(defn ok
  "Returns an action that can be emitted from the content of
  an [[acknowledge]] dialog."
  ([] (ok nil))
  ([value] (A. :ok value)))

(defn cancel
  "Returns an action that can be emitted from the content of
  an [[acknowledge]] dialog."
  ([] (A. :cancel nil)))

(dom/defn-dom prompt
  "Shows the content in an open modal dialog.

   The :onCancel event is triggered when the user presses ESC, or the
   content emits the [[cancel]] action.

   The :onOk event is triggered when the content emits the [[ok]] action."
  [attrs & content]
  (apply action-modal attrs
         {:ok :onOk
          :cancel :onCancel}
         (cancel)
         content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gen-id [_]
  (random-uuid))

(defrecord ^:private Show [id value])

(defn- ->show
  ([id]
   (->show id nil))
  ([id value]
   (->Show id value)))

(defrecord ^:private Hide [id])

(c/defn-item mediator
  "A mediator simplifies showing modal dialogs in response to actions.

   That can be used to let the user confirm certain actions, with a
   \"Are you sure?\" dialog, or even open an edit dialog in response
   to a button click. It could also be used to let the user acknowedge
   errors of Ajax calls.

   For example, if you have a button that deletes an entity:

   ```
   (dom/button {:onClick (constantly (c/return :action (delete entity-id)))})
   ```

   To get an acknowledgement from the user before doing that, you can
   rewrite that button with:

   ```
   (mediator (fn [hide entity-id]
               (prompt {:onOk (constantly (c/return :action hide
                                                    :action (delete entity-id)))
                        :onCancel (constantly (c/return :action hide))}
                       \"Are you sure?\"
                       (dom/button {:onClick (constantly (c/return :action (ok)))} \"Ok\")))
             (fn [show]
               (dom/button {:onClick (constantly (c/return :action (show entity-id)))})))
   ```

   Note: Emitting a second `(show ...)` before the previous dialog has
   emitted `hide` will replace the previous dialog.
"
  [ask-f item-f]
  (c/with-state-as [_ [id show-v] :local [nil ::none]]
    (if (nil? id)
      (c/focus (lens/>> lens/second lens/first) (c/once (f/partial gen-id)))
      (c/fragment
       (-> (c/focus lens/first (item-f (f/partial ->show id)))
           (c/handle-action (fn [state a]
                              (cond
                                (instance? Show a)
                                (c/return :state (assoc-in state [1 1] (:value a)))

                                :else
                                (c/return :action a)))))
       (c/focus lens/second
                (when (not= show-v ::none)
                  (-> (ask-f (Hide. id)
                             show-v)
                      (c/handle-action (fn [state a]
                                         (cond
                                           (instance? Hide a)
                                           (c/return :state (assoc-in state [1] ::none))

                                           :else
                                           (c/return :action a)))))))))))
