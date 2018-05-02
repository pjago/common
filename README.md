# common
library for arcadia-unity, running clojure-clr on Unity3D.

The main goal of this library is to mix clojure's namespace convention with the Resources folder of Unity.<br/>
The idea is that during development you place your assets all in a single Assets/Resources folder, that could be easily shared.

Here is how I do it.<br/>
At common.repl there is a function named **path**, that does it's best to return a string which can be used with UnityEngine.Resources/Load.

Examples:

```clojure
(in-ns 'pjago.stuff)
(path :pjago.stuff/barrel) ;=> "pjago/stuff/barrel"
(path :pjago.thing/barrel) ;=> "pjago/thing/barrel"
(path ::barrel) ;=> "pjago/stuff/barrel"
(path *ns*) ;=> "pjago/stuff/"
(path nil) ;=> System.InvalidCastException.. jk it returns ""
```

The **path** function is used by three other functions: **resource**, **clone!** and **object-search**.<br/>
So if you would like to clone the barrel, you can call (clone! ::barrel) from it's namespace.<br/>
Now let's say you're looking for anything named barrel:

```clojure
(path "barrel") ; => 
'("Assets/Resources/pjago/stuff/barrel.prefab"
  "Assets/Resources/pjago/stuff/barrel.mat"
  "Assets/Resources/pjago/thing/barrel.mat")
```

So if you input a string, **path** will return a sequence of all hits.

Wait.. so a barrel is a prefab and is also a material? Doesn't this creates name collisions?<br/>
That's right, you generally need to pass a second argument to ensure there is no collisions.

```clojure
(resource ::barrel)                        ;=> #unity/Material 10412
(resource ::barrel UnityEngine.Material)   ;=> #unity/Material 10412
(resource ::barrel UnityEngine.GameObject) ;=> #unity/GameObject 10562
```

In short, the names you name can be shared with anyone, and we all can access our assets easily.<br/>
Testing on keywords is pretty standard clojure, and it's open to extensions if done with **isa?**.<br/>
For me it's convenient for procedural generation, and dealing with hierarchy dependencies.

The common.repl is meant to be Editor only, but you can use common.processing to achieve a simillar effect in production.<br/>
It basically instruments 'reducing' functions in the format of barrel:

```clojure
(declare update)

(defn barrel
  ([] [::barrel (repeat (rand-int 10) ::rupees)]) ;init
  ([gob child] (state+ child :value (first (shuffle [1 5])))) ;step
  ([gob] (hook+ gob :update :revenge #'update))) ;complete
  
; Then you (hopefully) can do something as:

(def barrels
  (->> (cons nil (repeat barrel))  ;same structure as init: first is parent, rest is children
       (lazy-render)               ;transducible context, returns gobj-seq
       (sequence root)             ;groups the seq by transform root, returns roots
       (atom)))

(defn update [^GameObject bar k]
  (when (zelda-broke bar)
    (destroy bar)
    (swap! barrels next)))

(swap! barrels #(drop 50 %))
```

DISCLAIMER:<br/>
None of the code in this README was tested.<br/>
So please try it out, and see if you like it.
