(ns app.repl.core
  (:require
   [clojure.string :as string]
   [reagent.core :as r]
   [app.sci :as sci]
   [app.utils :as utils :refer [in?]]
   [app.env :refer [debug DEBUG]]
   [app.session :as session]
   [app.tutorial :refer [tutorial]]
   [app.repl.view :refer [repl-view]]))

;; Collection of map with the REPL command history.
(defonce repl-history
  (r/atom [{:type :special
            :value "여기에 클로저 표현식을 입력해보세요"}]))

;; Store the REPL input while typed in the input el
(defonce repl-input (r/atom nil))

;; Store a multiline command as a single string
(defonce repl-multiline (r/atom nil))

;; External to the component because needs to be used by
;; the focus function
(defonce input-el (r/atom nil))

;; Set a command input placeholder in case of EOF error
(defonce input-placeholder (r/atom nil))

(defn- write-repl!
  "Append `s` to the REPL history.
  Optional keyword `k` to use as a type."
  ([s]
   (write-repl! s :output))
  ([s k]
   (swap! repl-history conj {:type k :value s})))

(defn tutorial-active? []
  (true? (session/get :tutorial)))

;; -------------------------
;; SCI utils
;; -------------------------

(defn inc-step!
  "Increment the current tutorial steps."
  []
  (let [tut-len (count tutorial)
        curr-step (session/get :step)]
    (when (< curr-step (dec tut-len))
      (session/inc! :step))
    nil))

(defn dec-step!
  "Increment the current tutorial steps."
  []
  (session/dec! :step)
  nil)

(defn print-help
  "Print helper to screen."
  []
  (let [help-str ["헬퍼 함수들:"
                  ""
                  "시작      - 튜토리얼을 시작합니다"
                  "재시작    - 튜토리얼을 다시 시작합니다"
                  "지우기    - REPL 전체를 지웁니다"
                  "다음-단계 - 다음 단계로 넘어갑니다"
                  "이전-단계 - 이전 단계로 돌아갑니다"
                  ""
                  "(doc 이름)을 이용해서 주어진 이름의 변수, 함수의 문서를 확인하세요."
                  ""
                  "REPL입력을 지우려면 Ctrl-C를 누르세요."]]
    (doseq [s help-str]
      (write-repl! s)))
  nil)

(defn start-tutorial
  "Start the tutorial setting the initial step into the session."
  []
  (session/set! :tutorial true) ; activate the tutorial
  (session/set! :step 0)
  "튜토리얼 시작!")

(defn clear-repl
  "Delete all the repl history."
  []
  (reset! repl-history [])
  nil)

(defn restart-tutorial
  "Reset the tutorial session."
  []
  (session/reset-session!))

(defn set-name
  "Save user name into the session."
  [s]
  (when (string? s)
    (session/set! :내-이름 s)
    {:내-이름 s}))

(defn set-step
  "Navigate the tutorial to a specific step."
  [step]
  (session/set! :step step))

(defn set-prompt
  "Change the prompt style."
  [& {:keys [색 모양]}]
  (when (string? 모양)
    (session/set! :prompt-text 모양))
  (when (string? 색)
    (let [;; Required by tailwind to import classes into styles
          valid-colors ["text-amber-400"
                        "text-yellow-400"
                        "text-red-400"
                        "text-green-400"
                        "text-orange-400"
                        "text-slate-400"
                        "text-gray-400"
                        "text-teal-400"
                        "text-lime-400"
                        "text-blue-400"
                        "text-violet-400"
                        "text-purple-400"
                        "text-pink-400"
                        "text-rose-400"
                        "text-emerald-400"]
          full-color (str "text-" (string/lower-case 색) "-400")
          colors ["amber" "yellow" "red" "green" "orange" "slate" "gray" "teal"
                  "lime" "blue" "violet" "purple" "pink" "rose" "emerald"]]
      (if (in? valid-colors full-color)
        (session/set! :prompt-color full-color)
        (str "Invalid color: "
             색
             "! Valid colors are: "
             (string/join " " colors)
             ".")))))

;; Initialize an internal print funcion
(sci/set-print-fn (fn [s] (write-repl! s)))

;; Append the start-tutorial function as 'start
(sci/extend-ctx {:namespaces {'user {'시작 start-tutorial
                                     '지우기 clear-repl
                                     '재시작 restart-tutorial
                                     '내-이름 set-name
                                     '다음-단계 inc-step!
                                     '이전-단계 dec-step!
                                     '단계-설정 (when DEBUG set-step)
                                     '프롬프트-설정 set-prompt
                                     '더보기 (fn [] true)
                                     '도움 print-help}}})

;; Add REPL functions like `doc`
(sci/eval-string "(require '[clojure.repl :refer :all])")

;; -------------------------
;; REPL element
;; -------------------------

(defn- check-tutorial-test
  "Check if the sci output pass the test function.
  If it does, increase the tutorial step."
  [out]
  (when (tutorial-active?)
    (let [step-idx (session/get :step)
          step (nth tutorial step-idx)
          test-fn (:test step)]
      (try (when (test-fn out)
             (debug "TEST PASSED")
             (inc-step!))
           (catch :default _)))))

(defn- input-command
  "Return the entire command typed into the REPL.
  Update the repl-multiline in case."
  [in]
  (if (empty? @repl-multiline) in @repl-multiline))

(defn- update-multiline!
  "If `repl-multiline` is not empty, append `repl-input` value
  to it."
  [in]
  (when (not-empty @repl-multiline)
    (->> (str @repl-multiline in)
         (reset! repl-multiline))))

(defn- write-input!
  "Append the current input to the respective atom."
  [in]
  (if (empty? @repl-multiline)
    (write-repl! in :input)
    (write-repl! in :input-multi)))

(defn- reset-input!
  "Reset both the atom and the input value
  to avoid on-change event wrong behaviour."
  []
  (reset! repl-input nil)
  (set! (.-value @input-el) ""))

(defn- handle-keydown
  "Onkeydown event for the REPL input; Evaluate the string
  using SCI and add the output/error to the REPL. Manage
  the last command using arrow-up and a basic multiline
  in case of EOF error."
  [e]
  (let [in @repl-input]
    ;; Enter
    (when (and (= (.-key e) "Enter") (not-empty in))
      (let [in (str in \newline)]
        (debug "input: " (pr-str in))
        (update-multiline! in)
        (write-input! in)
        (let [cmd (input-command in)]
          (try (let [out (sci/eval-string cmd)
                     out-str (binding [*print-length* 20]
                               (pr-str out))]
                 (check-tutorial-test out)
                 (debug "output: " out-str)
                 (reset! repl-multiline nil)
                 (reset! input-placeholder nil)
                 ;; Append to history
                 (write-repl! out-str))
               (catch :default e
                 (debug "error" (ex-data e))
                 (cond (string/includes? (.-message e) "EOF while reading")
                       (let [err-data (ex-data e)
                             delimiter (:edamame/expected-delimiter err-data)
                             col (:column err-data)]
                         (reset! repl-multiline cmd)
                         (reset! input-placeholder (str "Expected delimiter `"
                                                        delimiter
                                                        "` column: "
                                                        col)))
                       :else
                       (do (reset! repl-multiline nil)
                           (reset! input-placeholder nil)
                           ;; Append error to history
                           (write-repl! (.-message e) :error))))))
        (reset-input!)))
    ;; Arrow Up
    (when (= (.-key e) "ArrowUp")
      (let [inputs (filter #(= (:type %) :input) @repl-history)
            last-in (last inputs)]
        (reset! repl-input (:value last-in))))))

(defn focus-input
  "Focus on REPL input element."
  []
  (.focus @input-el))

(defn view []
  [:<>
   [repl-view
    {:input-el input-el
     :input-placeholder input-placeholder
     :on-keydown handle-keydown
     :repl-input repl-input
     :repl-history repl-history
     :repl-multiline repl-multiline}]])
