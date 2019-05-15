(ns ^:figwheel-no-load battleship.dev
  (:require
    [battleship.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
