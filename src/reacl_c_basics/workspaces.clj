(ns reacl-c-basics.workspaces)

(defmacro reacl-c-card
  "A card rendering the given item.
  
  Options may be:
  - `:state`  specify an initial state for item. Defaults to `nil`.
  - `:show-state?` adds UI elements to inspect the current state.
  "
  [item & options]
  `(reacl-c-card* (fn [] ~item) ~@options))
