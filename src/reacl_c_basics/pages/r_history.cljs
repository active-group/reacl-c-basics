(ns ^:no-doc reacl-c-basics.pages.r-history)

(defprotocol History
  (start! [this nav-path! path-exists?] "Start listening to history events, calling `(nav-path! path)` when the top of the stack changes and `(path-exists? path)` returns truthy.")
  (get-current [this] "Return the path currently on the top of the history stack.")
  (push! [this path] "Push a new path on the history stack, NOT triggering the `nav-path!` callback.")
  (stop! [this] "Stop listening to history events."))
