(ns common.processing
  (:use [arcadia.core :exclude [if-cmpt log children]] arcadia.linear)
  (:import [UnityEngine Application GameObject MeshRenderer Rigidbody RigidbodyConstraints]
           [CtWheels TagsAndLayers]
           [clojure.lang IMeta IDeref]
           ArcadiaState ArcadiaBehaviour |ArcadiaBehaviour+IFnInfo[]|)
  (:refer-clojure :exclude [map replace tree-seq doto memoize])
  (:require [clojure.core :as c]
            [arcadia.core :as ac]
            [clojure.walk :as w]))

(declare tag)

(defn keypath [x]
  (cond 
    (keyword? x) x
    (instance? IMeta x)
    (as-> (meta x) m
          (keyword (str (:ns m))
                   (str (:name m))))
    (instance? GameObject x)
    (tag x)))

(defn resource [x ^System.Type t]
  (UnityEngine.Resources/Load
    (if-let [kx (keypath x)]
      (->> (subs (str kx) 1)
           (c/replace {\. \/})
           (apply str)))
    t))

(defn clone! [x]
  (c/doto (instantiate x)
    (-> (.name) 
        (set! (.name x)))))

;;TEMP? arcadia-unity #273

(def gentagged @#'ac/gentagged)
(def meta-tag @#'ac/meta-tag)

(defmacro if-cmpt
  "Execute body of code if `gob` has a component of type `cmpt-type`"
  [gob [cmpt-name cmpt-type] then & else]
  (let [gobsym (gentagged "gob__" 'UnityEngine.GameObject)]
    `(let [obj# ~gob]
       (if (obj-nil obj#)
         (with-gobj [~gobsym obj#]
           (if-let [~(meta-tag cmpt-name cmpt-type) (cmpt ~gobsym ~cmpt-type)]
             ~then
             ~@else))
         ~@else))))

;; COMP

(defn branch? [x]
  (or (fn? x) (var? x)))

(defn wrap [x]
  (if (branch? x) x
    (let [init (cond
                 (instance? IDeref x) #(deref x)
                 (sequential? x) (fn [] x)
                 :else (fn [] [x]))]
      (fn
        ([] (init))
        ([acc in] acc)
        ([acc] acc)))))

(def nop (wrap nil))

(defn init [f1 f]
  (fn
    ([] (f1 (f)))
    ([acc in] (f acc in))
    ([acc] (f acc))))
    
(defn step [f2 f]
  (fn
    ([] (f))
    ([acc in] (f2 acc in) (f acc in))
    ([acc] (f acc))))

(defn complete [f1 f]
  (fn
    ([] (f))
    ([acc in] (f acc in))
    ([acc] (f1 acc) (f acc))))

(defn has-arity? [f n]
  (if (var? f)
    (.HasArity @f n)
    (.HasArity f n)))

(defn doto [f & fs]
  (let [f0 (rseq (filterv #(has-arity? % 0) fs))
        f1 (cons f (filter #(has-arity? % 1) fs))
        f2 (cons f (filter #(has-arity? % 2) fs))]
    (fn
      ([] (doseq [f f0] (f)) (f))
      ([acc in] (doseq [f f2] (f acc in)) acc)
      ([acc] (doseq [f f1] (f acc)) acc))))

;; TRAVERSAL

(defn map "maps xf on init" [xf]
  (->> #(if (branch? %) (xf %) %)
       (partial c/map)
       (partial init)))

(defn tree-seq "tree-seqs xf on init" [xf]
  (fn continue [rfn]
    (if-not (branch? rfn) rfn
      (init (partial c/map continue)
            (xf rfn)))))

(defn prewalk "prewalks f on init" [f]
  (fn continue [rfn]
    (if-not (branch? rfn) rfn
      (init (partial w/prewalk (comp continue f))
            rfn))))

(defn postwalk "postwalks f on init" [f]
  (fn continue [rfn]
    (if-not (branch? rfn) rfn
      (init (partial w/postwalk (comp f continue))
            rfn))))

;; UTIL

(defn tag [^GameObject gob]
  (let [t (.tag gob)]
    (if (= (nth t 0) \:)
      (keyword (subs t 1))
      (keyword "unity" t))))

;tagged assimilates objects-tagged to objects-search
;it also facilitates the usage of isa? with tags
(defn tagged [rf]
  (let [t (volatile! "Untagged")]
    (fn
      ([gob child] (rf gob child))
      ([]
       (c/doto (rf) (->> first keypath str (vreset! t))))
      ([gob]
       (if (and (instance? GameObject gob) 
                (TagsAndLayers/AddTag @t))
         (set! (.tag gob) @t))
       (rf gob)))))

(defn freeze [pos & [rot]]
  (letfn [(fpos [x]
            (case x
              :x RigidbodyConstraints/FreezePositionX
              :y RigidbodyConstraints/FreezePositionY
              :z RigidbodyConstraints/FreezePositionZ))
          (frot [x]
            (case x
              :x RigidbodyConstraints/FreezeRotationX
              :y RigidbodyConstraints/FreezeRotationY
              :z RigidbodyConstraints/FreezeRotationZ))]
    (let [c (transduce (c/map frot) enum-or
                (transduce (c/map fpos) enum-or 
                  RigidbodyConstraints/None
                  pos)
                rot)]
      (partial complete
        #(with-cmpt % [rb Rigidbody]
           (set! (.constraints rb) c))))))

(defn paint [material]
  (partial complete
    (fn [gob]
      (if-cmpt gob [mr MeshRenderer]
        (set! (.-sharedMaterial mr) material)))))

(defn memoize [rf]
  (let [done (volatile! nil)]
    (fn 
      ([] (if-let [x @done] x (vreset! done (rf))))
      ([gob child] (rf gob child))
      ([gob] (rf gob)))))

(defn log [f]
  #(let [% (memoize %)]
     (doto %
       (fn
         ([] (ac/log "init" (f (first (%)))))
         ([gob child] (ac/log "step" (f gob)))
         ([gob] (ac/log "complete" (f gob)))))))

;fresh addresses the same problem as defmutable-once,
;but through walking a roundtrip of snapshot and mutable
(defn fresh [rf]
  (let [mm @#'ac/maybe-mutable
        ms @#'ac/maybe-snapshot
        m (c/memoize #(w/walk (comp (partial %1 %1) ms) mm %2))]
    (doto rf
      #(if-cmpt % [as ArcadiaState]
         (reduce-kv state+ %
           (m m (state %)))))))

(defn set-active [self]
  #(doto %
     (fn [gob]
       (if (instance? GameObject gob)
         (.SetActive gob self)))))

(defn children [transform-to]
  (partial step
    (fn [gob child]
      (if (satisfies? ISceneGraph gob)
        (child+ gob child transform-to)))))

(defn replace [lookup]
  (partial init
    #(cons (lookup (first %)) 
           (rest %))))

(defn with-db [lookup]
  (partial init
    (partial w/postwalk
      #(if (branch? %) % (lookup %)))))

(def with-state
  (partial init
    (fn [[h :as b]]
      (if (satisfies? IEntityComponent h)
        (ensure-cmpt h ArcadiaState))
      b)))

(def initialize-state
  (partial init
    (fn [[h :as b]]
      (if-cmpt h [as ArcadiaState]
        (do (set! (.fullyInitialized as) false)
            (.Initialize as)))
      b)))

;hooks are normaly executed in the order they're added.
;with execution-order you can specify the order by last
(defn execution-order [message-kw-groups]
  (letfn [(compare-by [ks]
            (comp (if (vector? ks) 
                    (zipmap ks (range (count ks)))
                    ks)
                  #(.key %)))]
    #(doto %
       (fn [gob]
         (doseq [[mk group] message-kw-groups
                 :let [k (compare-by group)]
                 h (cmpts gob (hook-types mk))
                 :let [ifns (aclone (.ifnInfos h))]]
           (.RemoveAllFunctions h)
           (.InvalidateIndexes h)
           (doseq [f (sort-by k ifns)]
             (.AddFunction h (.fn f) (.key f))))))))

;; RENDER

(def ^{:doc "flattens init to be [node & branches]"}
  flatten-wrap
  (partial init
    #(let [[h & t] (flatten %)]
       (cons h (c/map wrap t)))))

(defn unwrap "lazy-seq of branches interleaved with their init"
  [root]
  (lazy-seq
    (when (branch? root)
      (let [branch (root)]
        (->> (mapcat unwrap branch)
             (cons branch)
             (cons root))))))

(defn fab "clones from id's resource, if found.\n  otherwise returns id"
  [id]
  (if-let [src (resource id GameObject)]
    (let [old (.activeSelf src)]
      (.SetActive src false)
      (let [gob (clone! src)]
        (.SetActive src old)
        gob))
    id))

(def ^{:doc "default value of *renderer*"}
  renderer
  (tree-seq
    (comp
      flatten-wrap
      (children false)
      (set-active true)
      fresh
      initialize-state
      with-state
      (replace fab)
      tagged)))

(def ^{:dynamic true :doc "xf applied by render.\n  can be restored with (restore-renderer)"}
  *renderer* renderer)

(defn restore-renderer "restores *renderer* var-root" []
  (alter-var-root #'*renderer* (fn [_] @#'renderer)))

(defn renders "xf interface of render, useful for mocking an init"
  ([x node tail] (init (fn [_] (cons node tail)) (wrap x)))
  ([x tail] (init #(concat % tail) (wrap x)))
  ([x] (wrap x)))

(def ^{:doc "transduces from root recursively"}
  render
  (comp
    (fn [root]
      (let [v (volatile! []) 
            f (*renderer* root)]
        (letfn [(-render [f [node & tail]]
                  (vswap! v conj node) ;; init
                  (doseq [c (c/map #(-render % (%)) tail)]
                    (f node c)) ;; step
                  (f node) ;;complete
                  node)]
          (-render f (f))
          @v)))
    renders))

(def ^{:doc "transduces from root in depth-first order"}
  batch-render
  (comp
    (fn [root]
      ;; init
      (let [job (->> (*renderer* root)
                     (unwrap)
                     (apply array-map))]
        ;; step
        (doseq [[rf [node & tail]] job
                child (keep #(first (job %)) tail)]
          (rf node child))
        ;; complete
        (mapv (fn [[rf [node _]]] (rf node)) job)))
    renders))

(defn gen-factory []
  (let [stack (volatile! [])]
    (fn [[rf [node & tail] :as in]]
      (let [[rf [top _ & tail] :as in] (peek @stack)]
        (when (some? in)
          (rf top node)
          (vswap! stack pop)
          (if (seq tail)
            (vswap! stack conj [rf (cons top tail)])
            (rf top))))
      (if (seq tail)
        (vswap! stack conj in)
        (rf node))
      node)))

(def ^{:doc "lazy-seq of rendering nodes"}
  lazy-render ;bug: couldn't use sequence (it was eager)
  (comp
    (fn [root]
      (->> (*renderer* root)
           (unwrap)
           (partition 2)
           (c/map (gen-factory))))
    renders))

(def ^{:doc "xf that groups a render by transform root"}
  root ;bug: (sequence root (lazy-render ...)) is eager
  (comp (drop-while nil?)
        (partition-by #(.. % transform root))
        (map first)))

;; DEBUG

(defn unwrap-all "like unwrap, but eagerly applies f to all branches"
  [f root]
  (->> ((tree-seq memoize) root)
       (unwrap)
       (w/postwalk #(if (branch? %) (f %) %))))