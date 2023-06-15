(ns scheduling-tbd.web.middleware.core
  (:require
   [ring.middleware.defaults :as defaults]
   [ring.middleware.cors :refer [wrap-cors]] ; experimental
   [ring.middleware.session.cookie :as cookie]))

;;; (:middleware env/defaults) is just: (defn wrap-dev [handler opts] (-> handler ))
;;; Compare to ~/Documents/git/clojure/kit-example/guestbook/src/clj/kit/guestbook/web/middleware/core.clj
;;; (If you don't have such a thing, you can built it; it is just the 'getting started' demo for kit-clj.)
(defn wrap-base
  [{:keys [site-defaults-config cookie-secret]}]
  (let [s ^String cookie-secret
        cookie-store (cookie/cookie-store {:key (.getBytes s)})]
    (fn [handler]
      (-> (defaults/wrap-defaults handler
                                  (assoc-in site-defaults-config [:session :store] cookie-store))
          (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                     :access-control-allow-methods [:get :put :post :delete])))))
