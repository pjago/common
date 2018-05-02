(ns common.repl
  (:use arcadia.core arcadia.linear)
  (:import ArcadiaBehaviour ArcadiaState
           [CtWheels TagsAndLayers]
           [System.Reflection BindingFlags]
           [UnityEngine GameObject Resources Camera Vector3]
           [UnityEngine.SceneManagement SceneManager]
           [UnityEditor AssetDatabase PrefabUtility EditorUtility])
  (:require [arcadia.core :as ac]
            [arcadia.sugar :as as]
            [clojure.string :as str]))

;; UTIL

(defn resolve-but-dont-break-please [sym]
  (if-not (re-find #".*(?=\.)" (str sym))
    (try (->> sym str (str "UnityEngine.") symbol resolve)
         (catch Exception fever (->> sym str symbol resolve)))
    (try (->> sym str symbol resolve)
         (catch Exception fever nil))))

(defn prefab? [obj]
  (if (obj-nil obj)
    (#{1 2} (int (PrefabUtility/GetPrefabType obj)))))

;; DOPE FNS

(defonce ^{:private true} folders
  (let [f (new |System.String[]| 1)]
    (aset f 0 "Assets/Resources") f))

(defn -path [id]
  (or (cond
        (or (keyword? id) (instance? System.Type id))
        (str/replace (re-find #"[^:].*" (str id)) \. \/)
        (string? id)
        (or (re-find #"(?<=Resources/)[^\.]*" id)
            (->> (AssetDatabase/FindAssets id folders)
                 (mapv #(AssetDatabase/GUIDToAssetPath %))
                 (filter #(re-find #"(?<=Resources/).*\..*" %))))
        (instance? UnityEngine.GameObject id)
        (or (re-find #"(?<=Resources/)[^\.]*"
              (AssetDatabase/GetAssetPath
                (if (prefab? id) id
                  (PrefabUtility/GetPrefabParent id))))
            (let [t (.tag id)]
              (if (= (first t) \:)
                (str/replace (subs t 1) \. \/))))
        (instance? UnityEngine.Component id)
        (-path (.-gameObject id))
        (instance? UnityEngine.Object id)
        (re-find #"(?<=Resources/)[^\.]*"
          (AssetDatabase/GetAssetPath id))
        (instance? clojure.lang.Namespace id)
        (str (str/replace (str id) \. \/) "/")
        ; (instance? clojure.lang.Var id)
        (instance? clojure.lang.IMeta id)
        (if (:ns (meta id))
          (str (-path (:ns (meta id)))
               (:name (meta id)))
          (str (:name (meta id))))
        :else nil)
      ""))

(def ^{:doc "memoized path inside Assets/Resources, for an id"}
  path (memoize -path))

(defn aka "short path of id" [id]
  (let [p (path id)]
    (if-let [jao (and (string? p) (peek (str/split p #"/")))]
      (if-not (= jao "")
        jao))))

(defn where "find-ns" [id]
  (let [p (path id)]
    (if-let [p (and (string? p) (re-find #".*(?=/)" p))]
      (if (not= p "")
        (if-let [q (str/replace p \/ \.)]
          (find-ns (symbol q)))))))

(defn keypath "path, but in keyword form" [id]
  (let [p (path id)]
    (if-let [p (and (string? p) (re-find #".*(?=/)" p))]
      (keyword (str/replace p \/ \.) (aka id))
      (keyword (aka id)))))

(defn- ensure-type [t]
  (cond
    (isa? t UnityEngine.Object) t
    (instance? UnityEngine.Object t) (type t)
    :else UnityEngine.Object))

;; IDENTITY

(declare resource resources)

(defn o= "true only if x and y share some resources"
  [x y]
  (cond
    (coll? (path x))
    (contains? (set (resources x)) (resource y))
    (coll? (path y))
    (contains? (set (resources y)) (resource x))
    (instance? clojure.lang.Namespace x)
    (str/starts-with? (str (where y)) (str x))
    (instance? clojure.lang.Namespace y)
    (str/starts-with? (str (where x)) (str y))
    :else
    (= (or (seq (resources x)) x) 
       (or (seq (resources y)) y))))

;; SOURCE

(defn resource "returns the first resource with id, of type -utype.\n  tested with assets kept under the Assets/Resources folder"
  ([id]
   (cond
     (nil? id) nil
     (instance? UnityEngine.Component id)
     (resource (.-gameObject id))
     (and (instance? UnityEngine.Object id) 
          (not (zero? (int (PrefabUtility/GetPrefabType id)))))
     (if (prefab? id) id (PrefabUtility/GetPrefabParent id))
     (coll? (path id))
     (first (resources id (ensure-type id)))
     (instance? clojure.lang.Namespace id) id
     :else
     (Resources/Load (path id) (ensure-type id))))
  ([id -utype] ;cache src
   (let [src (resource id) utype (ensure-type -utype)]
    (cond
      (nil? id) nil
      (satisfies? ISceneGraph -utype)
      (first (resources id -utype))
      (instance? UnityEngine.Object -utype)
      (resource -utype (type -utype))
      (isa? (type src) -utype) src
      (isa? (type src) utype)
      (if (some? -utype) (state (gobj src) -utype) src)
      (isa? utype UnityEngine.Component)
      (cond
        (and (isa? (type id) utype)
             (not (zero? (int (PrefabUtility/GetPrefabType id)))))
        (if (prefab? id) id (PrefabUtility/GetPrefabParent id))
        (satisfies? IEntityComponent src)
        (cmpt src utype)
        :else ;cache missed
        (if-let [gob (resource id GameObject)]
          (cmpt gob utype)))
      (coll? (path id))
      (first (resources id utype))
      :else
      (Resources/Load (path id) utype)))))

(defn resources "equivalent to resource, but returns all hits"
  ([id] (resources id nil))
  ([id -utype]
   (cond 
     (nil? id) nil
     (satisfies? ISceneGraph -utype)
     (distinct (sequence (comp (map #(resource % GameObject)) (filter #(o= id %))) (gobj-seq -utype)))
     (instance? UnityEngine.Object -utype)
     (list (resource -utype (type -utype)))
     (and (satisfies? ISceneGraph id) (not (prefab? id)))
     (list (resource id -utype))
     (or (isa? -utype UnityEngine.Object) (nil? -utype))
     (let [utype (ensure-type -utype)]
       (if-not (coll? (path id))
         (cond 
           (isa? utype UnityEngine.Component)
           (mapcat #(cmpts % utype) (resources id GameObject))
           (and (instance? GameObject id) (not= id (PrefabUtility/FindPrefabRoot id)))
           (list (resource id utype))
           (instance? clojure.lang.Namespace id)
           (filter #(o= id %) (resources "" utype))
           :else
           (filter #(= (path id) (path %)) (resources "" utype)))
         (let [utypes (keep (comp resolve-but-dont-break-please symbol) (re-seq #"(?<=t:)[^\s]*" id))
               ctypes (filter #(isa? % UnityEngine.Component) utypes)
               new-id (reduce #(str/replace %1 (re-pattern (str "t:(" (aka %2) "|" %2 ")")) "") id ctypes)]
           (cond
             (seq ctypes)
             (distinct (sequence (comp (mapcat #(resources new-id %)) (keep #(resource % utype))) ctypes))
             (seq (str/trim new-id))
             (distinct
               (for [guid (AssetDatabase/FindAssets new-id folders)
                     :let [file (AssetDatabase/GUIDToAssetPath guid)
                           type (AssetDatabase/GetMainAssetTypeAtPath file)]
                     src (resources (Resources/Load (path file) type) utype)]
                 src))
             (isa? utype UnityEngine.Component)
             (mapcat #(cmpts % utype) (Resources/LoadAll "" GameObject))
             :else
             (Resources/LoadAll "" utype)))))
     :else
     (sequence (comp (filter #(o= id %)) (keep #(state (gobj %) -utype)))
               (resources "" ArcadiaState)))))

;; SEARCH

(defn objects-root "finds all the root gobs on the active scene, with id" 
  [id]
  (let [all (new |System.Collections.Generic.List`1[UnityEngine.GameObject]| (.rootCount (SceneManager/GetActiveScene)))]
    (.GetRootGameObjects (SceneManager/GetActiveScene) all)
    (filter #(o= id %) all)))

(defn objects-search "like resources, but for instances" ;bug: (objects-search "t:ArcadiaState") doesn't work
  ([id]
   (objects-search id GameObject))
  ([id utype]
   (as-> #(o= id %) pred
     (if-let [id (resource id GameObject)]
       (if (not (prefab? id))
         #(and (satisfies? ISceneGraph %) (= (gobj id) (gobj %)))
         pred)
       pred)
     (cond
       (isa? utype UnityEngine.Object)
       (filter pred (objects-typed utype))
       (instance? GameObject utype)
       (filter pred (gobj-seq utype))
       (nil? utype)
       (objects-search id GameObject)
       :else
       (sequence (comp (filter pred) (keep #(state (gobj %) utype)))
                 (objects-typed ArcadiaState))))))

(def ^{:doc "like resource, but for a instance"} 
  object-search
  (comp first objects-search))

;; CLONE

(defmacro ^{:doc "performs body while bindings inactive, then restores their activeSelf"}
  with-inactive [bindings & body]
  (let [syms (take-nth 2 bindings)
        prev (repeatedly (count syms) #(gensym "self"))]
    `(let [~@bindings
           ~@(mapcat (fn [p s] [p (list '.activeSelf s)]) 
                     prev syms)]
       ~@(map #(list '.SetActive % false) syms)
       (let [return# (do ~@body)]
         ~@(map #(list '.SetActive %1 %2) syms prev)
         return#))))

(defn clone! "instatiates trying to keep prefab connection.\n  if passed two arguments, will clone second into first."
  ([id]
   (let [src (resource id)]
     (cond
       (instance? GameObject id)
       (with-inactive [id id]
         (if (and (prefab? id) (= id (PrefabUtility/FindPrefabRoot id)))
           (PrefabUtility/InstantiatePrefab id)
           (PrefabUtility/InstantiateAttachedAsset id)))
       (instance? UnityEngine.GameObject src)
       (with-inactive [src src] (PrefabUtility/InstantiatePrefab src))
       (instance? UnityEngine.Object src)
       (instantiate src)
       (instance? UnityEngine.Object id)
       (instantiate id)
       :else nil)))
  ([id utype]
   (cond
     (or (isa? (type (resource id)) utype) (nil? utype))
     (clone! id)
     (isa? utype UnityEngine.Object)
     (clone! (resource id utype))
     (instance? UnityEngine.Component utype)
     (let [id (if (satisfies? IEntityComponent id) id (clone! id GameObject))
           typ (type utype)
           src (resource utype typ)
           old (first (filter #(= src (resource % typ)) (cmpts id typ)))
           cmp (or old (ensure-cmpt id typ))]
       (EditorUtility/CopySerializedIfDifferent utype cmp)
       (if (instance? ArcadiaState utype)
         (let [gobj (.-gameObject utype)]
           (reduce #(state+ %1 %2 (state gobj %2)) id (.. utype -state -Keys))))
       (if (instance? ArcadiaBehaviour utype)
         (set! (.-ifnInfos cmp) (.-ifnInfos utype)))
       cmp)
     (instance? UnityEngine.Object utype)
     (let [id (if (instance? (type utype) id) id (clone! id (type utype)))]
       (EditorUtility/CopySerializedIfDifferent utype id)
       (if (satisfies? IEntityComponent utype)
         (reduce clone! id (cmpts utype UnityEngine.Component)))
       id)
     :else
     (let [id (if (satisfies? IEntityComponent id) id (resource id GameObject))]
       (when-let [x (state id utype)]
         (if (satisfies? ISnapshotable x)
           (mutable (snapshot x))
           x))))))

;; CLEAR

(defn purge! "removes arcadia components, even if they're assets."
  [^GameObject gob]
  (when (satisfies? IEntityComponent gob)
    (run! #(UnityEngine.Object/DestroyImmediate % true) 
          (cmpts gob ArcadiaBehaviour))
    (when-let [s (cmpt gob ArcadiaState)]
      (if (= s HookStateSystem/arcadiaState)
        (set! HookStateSystem/hasState false))
      (UnityEngine.Object/DestroyImmediate s true))))

(declare clc)

(defn clear-all! "removes all root gobs from the active scene\n  & clears the log."
  [& {:keys [except]}]
  (if (not (some #{:log} except)) (clc))
  (let [all (new |System.Collections.Generic.List`1[UnityEngine.GameObject]| (.rootCount (SceneManager/GetActiveScene)))
        fns (filter fn? except)]
    (.GetRootGameObjects (SceneManager/GetActiveScene) all)
    (if (some some? fns)
      (run! destroy-immediate (remove (apply some-fn fns) all))
      (run! destroy-immediate all))))

(defn clear! "removes root gobs, with id, in active scene.\n  id defaults to *ns*"
  ([] (run! destroy-immediate (objects-root *ns*)))
  ([id] (run! destroy-immediate (objects-root id))))

;; KEYBINDINGS

(def ^{:doc "short for object-search"} s> object-search)
(def ^{:doc "short for resource"} s< resource)
(def ^{:doc "short for objects-search"} ss> objects-search)
(def ^{:doc "short for resources"} ss< resources)

;; SUGAR

(defmacro -destructuring-forms [syms]
 `(do
   ~@(for [sym syms]
      `(defmethod as/destructuring-form ~sym
        [bvec# [~'_ ~'& ids#] utype#]
        (reduce
          #(conj %1 (symbol (name %2)) (list ~sym %2 utype#))
           bvec# ids#)))))

(-destructuring-forms 
  [`s> `s< `ss> `ss< 
   `resource `resources 
   `object-search `objects-search 
   `clone!])

;; TEMP? until a better way of having editor only code

(require '[common.processing :as x])

(alter-var-root #'x/keypath (constantly keypath))
(alter-var-root #'x/resource (constantly resource))
(alter-var-root #'x/clone! (constantly clone!))

(defn tag-all! "creates tags for every GameObject resource found with id.\n  You can pass an empty string to really tag all."
  [id]
  (run! #(TagsAndLayers/AddTag %)
        (into #{}
              (comp (keep keypath)
                    (map str))
              (resources id GameObject))))

;; REPL UX possibly todo: move to separate file, same ns

(import '[UnityEditor EditorApplication EditorGUIUtility Selection SceneView GameView])

(defn clc "clears log" []
  (-> (Type/GetType "UnityEditor.LogEntries, UnityEditor.dll")
      (.GetMethod "Clear" (enum-or BindingFlags/Static BindingFlags/Public))
      (.Invoke nil nil)))

(defn play "hit editor play" ;bug: steals focus
  ([] 
   (play (not EditorApplication/isPlaying)))
  ([toggle] 
   (do (set! EditorApplication/isPlaying toggle)
       (.Focus (EditorWindow/GetWindow GameView))
       nil)))

(defn pause "hit editor pause" ;bug: steals focus, won't stay on scene view
  ([] 
   (pause (not EditorApplication/isPaused)))
  ([toggle] 
   (do (set! EditorApplication/isPaused toggle)
       (.Focus SceneView/lastActiveSceneView)
       nil)))

(defn select "selects gobj in the hierarchy" [id]
  (->> (object-search id GameObject)
       (set! Selection/activeGameObject)))

(defn zoom "scene view zoom" [id] ;todo: zoom distance
  (if-let [sv SceneView/lastActiveSceneView] 
    (if-let [gob (object-search id GameObject)]
      (let [rot (.-rotation sv)]
        (.AlignViewToObject sv (.-transform gob))
        (set! (.-rotation sv) rot)
        nil))))

(defn focus "scene view focus. 0-arity will focus game view" ;bug: steals focus
  ([] (.Focus (EditorWindow/GetWindow GameView)))
  ([id]
   (if (instance? UnityEditor.EditorWindow id)
     (.Focus id)
     (do (select id)
         (.Focus SceneView/lastActiveSceneView)
         (EditorApplication/ExecuteMenuItem "Edit/Frame Selected")
         (EditorApplication/ExecuteMenuItem "Edit/Lock View to Selected")
         nil))))

(defn ping "pings in the hierarchy" [id]
  (->> (object-search id GameObject)
       (EditorGUIUtility/PingObject)))

(defn toggle "toggles gobj or cmpt tree view"
  [id bool] ;bug: steals focus the first time
  (if-let [obj (object-search id GameObject)]
    (if (instance? UnityEngine.Component id)
      (do (select obj) ;bug: needs to wait one frame
          (let [tracker UnityEditor.ActiveEditorTracker/sharedTracker
                ts (.activeEditors tracker)]
            (dotimes [i (count ts)]
              (if (= (.target (nth ts i)) id)
                (.SetVisible tracker i bool)))))
      (do (if (not (instance? UnityEditor.SceneHierarchyWindow UnityEditor.EditorWindow/focusedWindow))
            (UnityEditor.EditorApplication/ExecuteMenuItem "Window/Hierarchy"))
          (.. (do UnityEditor.SceneHierarchyWindow) ;wut
              (GetMethod "SetExpandedRecursive")
              (Invoke
                UnityEditor.EditorWindow/focusedWindow
                (doto (new |System.Object[]| 2)
                      (aset 0 (.GetInstanceID obj))
                      (aset 1 bool))))))))