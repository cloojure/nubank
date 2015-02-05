(ns tst.demo.handler
  (:use demo.handler)
  (:use clojure.test
        cooljure.core
        ring.mock.request))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= (:status response) 200))
      (is (.contains (:body response) "Hello World"))))

  (testing "add-edge good"
    (let [response (app (request :get "/add-edge/3/4"))]
      (is (= (:status response) 200))
      (is (re-find #"Added Edge"    (:body response)))
      (is (re-find #"n1=3"          (:body response)))
      (is (re-find #"n2=4"          (:body response)))
    ))

  (testing "add-edge bad"
    (let [uri-path  "/add-edge/-1/4"
          response  (app (request :get uri-path)) ]
      ; (is (= (:status response) 404))  #todo
        (is (re-find #"Error"  (:body response))))
    (let [uri-path  "/add-edge/1/999"
          response  (app (request :get uri-path)) ]
      ; (is (= (:status response) 404))  #todo
        (is (re-find #"Error"  (:body response))))
  )

  (testing "not-found route"
    (let [response (app (request :get "/invalid"))]
      (is (= (:status response) 404)))))
