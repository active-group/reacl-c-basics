(ns reacl-c-basics.workspaces
  "A Nubank workspaces card type for reacl-c."
  (:require [nubank.workspaces.model :as wsm] ;; needed for keyword prefix alias ::wsm/
            [nubank.workspaces.card-types.util :as ct.util]
            [reacl-c.main :as reacl-c]
            [nubank.workspaces.ui.core :as uc]
            [fulcro.client.localized-dom :as rdom]))

(defn- reacl-c-toolbar [options get-state]
  (when (:show-state? options)
    (fn []
      (rdom/div {:style {:flex          "0 0 100%"
                         :display       "flex"
                         :flexDirection "row"
                         :width         "100%"}}
                ;; Idea: if the state is simple/a short string, we could show it 'live' (would need a timer?)
                (rdom/div {:style {:flex         "1"
                                   :display      "flex"
                                   :marginRight  "auto"
                                   :maxWidth     "calc(100% - 130px)"
                                   :overflow     "hidden"
                                   :textOverflow "ellipsis"
                                   :fontFamily   "SourceCodePro-Light"
                                   :fontWeight   "light"}}
                          (rdom/div {:style {:alignSelf "center"
                                             :position  "relative"
                                             :top       "-1px"}}
                                    (let [state-str  (pr-str (get-state))
                                          state-str' (if (>= 90 (count state-str))
                                                       state-str
                                                       (str (subs state-str 0 90) "…"))]
                             (str "State: " state-str'))))
                (uc/button {:style   {:justifySelf "end"}
                            :onClick (fn [_ev] (js/console.log (get-state)))}
                           "Log current state")))))

(defn- run-rec! [node item state-atom]
  (reacl-c/run-controlled node item {:state      @state-atom
                                     :set-state! (fn [st cb]
                                                   (reset! state-atom st)
                                                   (run-rec! node item state-atom)
                                                   (when cb (cb)))}))

;; simple version:
(defn- reacl-c-simple-card-init [{::wsm/keys [card-id]
                                  :as        card} item-f options]
  (let [current (atom (:state options))
        start!  (fn [node]
                  (run-rec! node (reacl-c/execute-effects (item-f)) current))
        stop!   (fn [node]
                  (js/ReactDOM.unmountComponentAtNode node))]
    (ct.util/positioned-card
     card
     {::wsm/dispose
      (fn [node]
        (stop! node))

      ::wsm/refresh
      (fn [node]
        (stop! node)
        (start! node))

      ::wsm/render
      (fn [node]
        (start! node))

      ::wsm/render-toolbar
      (reacl-c-toolbar options (fn [] @current))
      })))

(defn reacl-c-card*
  "A card rendering the item returned by the given thunk `item-f`.
  
  Options may be:
  - `:state`  specify an initial state for item. Defaults to `nil`.
  - `:show-state?` adds UI elements to inspect the current state.

  See [[reacl-c-card]] for a macro version of this.
  "
  [item-f & options]
  {::wsm/init #(reacl-c-simple-card-init % item-f (apply hash-map options))})
