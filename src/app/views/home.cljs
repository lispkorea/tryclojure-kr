(ns app.views.home
  (:require
   [reagent.core :as r]
   [clojure.string :as string]
   [app.repl.core :as repl]
   [markdown.core :refer [md->html]]
   [app.session :as session]
   [app.tutorial :refer [tutorial]]))

(def intro-title
  "��� �ð� ����������?")

(def intro-content
  "> �ͼ����� �߱��Ѵٸ�, ���� ���ο� ���� ��� �� ���� ���̴�. - ��ġ ��Ű

��, �ѹ� ��ƺ��ô�!

<span id=\"location-of-editor\">�����ʿ�</span>
 �������� ���� �� ������ *�а�(Read) ���ϰ�(Eval) ����ϱ�(Print) �ݺ�(Loop)*�ϴ� **REPL**�� �ֽ��ϴ�.
 
`(+ 1 2)`�� Ÿ�����ϰų�, �׳� �ڵ带 Ŭ���ϸ� �ڵ尡 �ڵ����� �Էµ˴ϴ�.
 �� ���� ��ɾ ���Ƿ��� `(����)`�� �Է��ϼ���.
   
�غ� �Ǽ����� `(����)`�� �Է��ϼ���!")

(defn compute-step
  "Returns a list of `title` and `content`
  based on the current step stored into the session."
  []
  (if (repl/tutorial-active?)
    (let [step-idx (session/get :step)
          step (nth tutorial step-idx)]
      [(:title step) (:content step)])
    [intro-title intro-content]))

(defn- link-target
  "Add target=_blank to link in markdown."
  [text state]
  [(string/replace text #"<a " "<a target=\"_blank\" ")
   state])

;; Regex to find [[var]] pattern in strings
(def re-doublebrackets #"(\[\[(.+)\]\])")

(defn- session-vars
  "Replace `[[var]]` in markdown text using session
  variables."
  [text state]
  [(let [res (re-find re-doublebrackets text)]
     (if res
       (let [k (keyword (last res))
             v (if (session/has? k)
                 (session/get k)
                 "unset")]
         (string/replace text re-doublebrackets v))
       text))
   state])

(defn- parse-md [s]
  (md->html s :custom-transformers [link-target session-vars]))

;; -------------------------
;; Views
;; -------------------------

(defn- handle-tutorial-click
  "When user click of `code` DOM node, fill the REPL input
  with the code content and focus on it."
  [e]
  (let [target (.-target e)
        node-name (.-nodeName target)]
    (when (= node-name "CODE")
      (->> (.-textContent target)
           (reset! repl/repl-input))
      (repl/focus-input))))

(defn tutorial-view [[title content]]
  [:div {:class ["bg-gray-200"
                 "text-black"
                 "dark:text-white"
                 "dark:bg-gray-800"
                 "shadow-lg"
                 "sm:rounded-l-md"
                 "xs:rounded-t-md"
                 "w-full"
                 "md:p-8"
                 "p-6"
                 "min-h-[200px]"
                 "opacity-95"]
         :on-click handle-tutorial-click}
   [:h1 {:class ["text-3xl" "mb-4" "font-bold" "tracking-tight"]}
    title]
   [:div {:class ["leading-relaxed" "last:pb-0"]
          :dangerouslySetInnerHTML #js{:__html (parse-md content)}}]])

(defn view []
  (r/create-class
   {:display-name "home-view"

    :component-did-mount
    (fn []
      ;; Focus on input after first rendered
      (repl/focus-input))

    :reagent-render
    (fn []
      [:div {:class ["flex"
                     "sm:flex-row"
                     "flex-col"
                     "items-center"
                     "justify-center"
                     "xl:-mt-32"
                     "lg:-mt-8"
                     "mt-0"]}
       [:div {:class ["flex-1" "z-0"]}
        [tutorial-view (compute-step)]]
       [:div {:class ["flex-1"
                      "z-10"
                      "sm:w-auto"
                      "w-full"
                      "sm:mt-0"
                      "mt-7"
                      "sm:mb-0"
                      "mb-14"]}
        [repl/view]]])}))

(defn- update-location-of-editor []
  (let [window-width (. js/window -innerWidth)
        location-of-editor-dom (.getElementById js/document "location-of-editor")]
    (when location-of-editor-dom
      (set! (. location-of-editor-dom -innerHTML)
            (if (< window-width 640) "�ٷ� �Ʒ�" "�ٷ� ������")))))

(.addEventListener js/window "load" update-location-of-editor)
(.addEventListener js/window "resize" update-location-of-editor)
