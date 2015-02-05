(ns demo.handler
  (:use cooljure.core)
  (:require 
    [clojure.string         :as str] )
  (:require 
    [compojure.core :refer [defroutes routes]]
    [ring.middleware.resource :refer [wrap-resource]]
    [ring.middleware.file-info :refer [wrap-file-info]]
    [hiccup.middleware :refer [wrap-base-url]]
    [compojure.handler :as handler]
    [compojure.route :as route]
    [demo.routes.home :refer [home-routes]]
  ))

(defn init []
  (println "demo is starting"))

(defn destroy []
  (println "demo is shutting down"))

(defroutes app-routes
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (-> (routes home-routes app-routes)
      (handler/site)
      (wrap-base-url)))
