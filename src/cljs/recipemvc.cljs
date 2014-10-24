(ns recipemvc
  (:require [reagent.core :as reagent :refer [atom]]))

(def foo bar)
(def recipes (atom (sorted-map)))

(def counter (atom 0))

(defn add-recipe [title url]
  (let [id (swap! counter inc)]
    (swap! recipes assoc id {:id id :title title :url url :done false})))

(defn toggle [id] (swap! recipes update-in [id :done] not))
(defn save [id title] (swap! recipes assoc-in [id :title] title ))
(defn add-url [id url] (swap! recipes assoc-in [id :url] url ))
(defn delete [id] (swap! recipes dissoc id))

(defn mmap [m f a] (->> m (f a) (into (empty m))))
(defn complete-all [v] (swap! recipes mmap map #(assoc-in % [1 :done] v)))
(defn clear-done [] (swap! recipes mmap remove #(get-in % [1 :done])))

(add-recipe "Mochi", 
            "http://www.japanesecooking101.com/sweet-mochi-recipe/ ")
(add-recipe "Black Beans and Rice", 
            "http://allrecipes.com/recipe/cuban-black-beans-i/")
(add-recipe "Chicken a la King", 
            "http://www.bettycrocker.com/recipes/chicken-a-la-king/2fd637ba-2c30-4db5-95d0-e57b4547251e")
(add-recipe "Mexican Chocolate Tofu Pudding", 
            "http://www.nytimes.com/2009/05/20/dining/201mrex.html")
(complete-all false)

(defn recipe-input [{:keys [title url on-save on-stop]}]
  (let [val-str (if (nil? title) url title)
        val (atom val-str)
        stop #(do (reset! val "")  
                  (if on-stop (on-stop)))
        save #(let [v (-> @val str clojure.string/trim)]
                (if-not (empty? v) (on-save v))
                (stop))]
    (fn [props]
      [:input (merge props
                     {:type "text" :value @val :on-blur save
                      :on-change #(reset! val (-> % .-target .-value))
                      :on-key-up #(case (.-which %)
                                    13 (save)
                                    27 (stop)
                                    nil)})])))

(def recipe-edit (with-meta recipe-input
                 {:component-did-mount #(.focus (reagent/dom-node %))}))

(defn recipe-stats [{:keys [filt active done]}]
  (let [props-for (fn [name]
                    {:class (if (= name @filt) "selected")
                     :on-click #(reset! filt name)})]
    [:div
     [:span#recipe-count
      [:strong active] " " (case active 1 "item" "items")]
     ;[:ul#filters
     ; [:li [:a (props-for :all) "All"]]
     ; [:li [:a (props-for :active) "Active"]]
     ; [:li [:a (props-for :done) "Completed"]]]
     (when (pos? done)
       [:button#clear-completed {:on-click clear-done}
        "Clear completed " done])]))

(defn recipe-item []
  (let [editing-title (atom false)
        editing-url (atom false)]
    (fn [{:keys [id done title url]}]
      [:li {:class (str (if done "completed ")
                        (if @editing-title "editing")
                        (if @editing-url "editing"))}
       [:div.view
        [:label [:a {:href url 
                    :on-double-click #(reset! editing-title true)} title]]
        [:input.toggle {:type "checkbox" :checked done
                        :on-change #(reset! editing-url true)}]
        [:button.destroy {:on-click #(delete id)}]]
       (when @editing-title
         [recipe-edit {:class "edit" :title title 
                     :on-save #(save id %)
                     :on-stop #(reset! editing-title false)}])
       (when @editing-url
         [recipe-edit {:class "edit" :url url 
                       :on-save #(add-url id %)
                       :on-stop #(reset! editing-url false) }])])))

(defn recipe-app [props]
  (let [filt (atom :all)]   
    (fn []
      (let [items (vals @recipes)
            active (count items)
            title (atom "")] 
        [:div
         [:section#recipeapp
          [:header#header
           [:h1 "recipes"]
           [recipe-input {:id "new-recipe"
                        :placeholder "What's your new recipe?"
                        :on-save #(reset! title %)}]
           [recipe-input {:id "new-recipe"
                        :placeholder "URL?"
                        :value @title
                        :on-save #(add-recipe @title %) }]]
          (when (-> items count pos?)
            [:div
             [:section#main
              ;[:input#toggle-all {:type "checkbox" :checked (zero? active)
              ;                    :on-change #(complete-all (pos? active))}]
              [:label {:for "toggle-all"} "Mark all as complete"]
              [:ul#recipe-list
               (for [recipe (filter (case @filt
                                    :active (complement :done)
                                    :done :done
                                    :all identity) items)]
                 ^{:key (:id recipe)} [recipe-item recipe])]]
             [:footer#footer
              [recipe-stats {:active active}]]])]
         [:footer#info
          [:p "©Team Awesome"]]]))))
    

(defn ^:export run []
  (reagent/render-component [recipe-app] (.-body js/document)))
