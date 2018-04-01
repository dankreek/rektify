(ns rektify.object
  "
  # Object creation and manipulation for Rektify.

  All public functions in this namespace have either `obj-desc` or `prop-map`
  as the first argument. These are maps which describe how Rektify should
  create, manipulate and destroy objects.

  ## `obj-desc`

  The `obj-desc` map contains functions which perform instantiation, tree
  manipulation, and destruction of the object being described. The keys in the
  map are described as follows:

  * `:constructor` A reference to the JavaScript constructor function which will
    create the object being described.

  * `:destructor` _(optional)_ a function which is called on an object when it
    is being destroyed. The function will have the signature `[obj-desc obj&]`
    where `obj-desc` is the object description map for the object, and `obj&`
    which is a reference to the object being destroyed. The `:destructor`
    function should destroy all of its children as well as remove itself from
    its parent, if a parent exists.

  * `:constructor-list` _(optional)_ the list of constructor signatures which
    can be used to instantiate the object being described.

  * `:post-constructor` _(optional)_ a function which will be called after an
    object has been instantiated and all of its initial properties have been
    set. The signature for the `:post-constructor` function should be `[obj-desc
    obj& init-props]` where:
      * `obj-desc` is the object description map which was used to instantiate
        the object.
      * `obj&` is a reference to the object which was created
      * `init-props` a map of the object's initial properties that were set
        during and after instantiation.

  * `:default-props` a map of `property keys` -> `values` which contain default
    values for all properties which have defaults. These are used when a
    property is set on an object and is then later no long set, usually while
    calling `update-props!`. The properties which are no longer set are all
    assigned the default value set in `:default-props`.

  * `:get-parent` function which returns a reference to the object's parent,
    or `nil` if the object has no parent. The function takes a single argument,
    which is a reference to the object.

  * `:add-child` function that adds a child to the end of the node's list of
    children. The function takes two arguments, a reference to the parent object
    and a reference to the new child object. This function should return a
    reference to the new child.

  * `:get-children` function that returns an array or vector of references to
    the object's children. This function takes a single argument, which is a
    reference to the object.

  * `:replace-child` function that replaces a child of an object. This function
    takes three arguments. The first is a reference to the parent object, the
    second is a reference to the object ot be replaced and the third is a
    reference to the new child which is will take the old child's place. This
    function should return a reference to the old child.

  * `:remove-child` function that removed a child from an object. This function
    takes two arguments. The first is a reference to the object and the second
    is a reference to the child to be removed and should return a reference to
    the removed child.

  * `:child-index` function that returns the index of a child, `nil` if the
    child object is not a child of the object.

  * `:prop-map` the map of properties which Rektify will used to manipulate an
    object's JavaScript projects. It is described below.

  ## `prop-map`

  The property mapping is a map of keywords to property description maps. The
  keywords are the keys of the properties themselves and the descriptions
  explain to Rektify how it should manipulate properties on the objects. Each
  property description contains the following keys:

  * `:property` a string which is the name of the property on the JavaScript
    object being described.

  * `:getter` a function which will return a value for the property. The
    function takes 2 parameters. The first being a reference to the object
    itself, the second is the name of the property to be returned.

  * `:setter` a function which will set a value for a property on an object. The
    function takes 3 parameters. The first being a reference to the object
    itself, the second is the name of the property to be set, and the third is
    the value to set the property to.
  "
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(def ^:private required-obj-desc-keys
  #{:get-parent
    :add-child
    :child-index
    :replace-child-at
    :remove-child-at
    :get-children
    :constructor
    :prop-map})


(def ^:private optional-obj-desc-keys
  #{:default-props
    :constructor-list
    :post-constructor
    :destructor})


(def ^:no-doc valid-obj-desc-keys
  (set/union required-obj-desc-keys optional-obj-desc-keys))


(defn ^:private constructor-sig
  "Find the first constructor signature that contains all the property keys
  specified in `init-prop-keys`. If the constructor list is `nil` or empty then
  the default constructor, `[]`, is returned."
  [constructor-list init-prop-keys]
  (let [key-set (set init-prop-keys)
        constructor-list (or constructor-list [[]])
        found-sig (loop [sigs constructor-list]
                    (when-let [sig (first sigs)]
                      (if (every? key-set sig)
                        sig
                        (recur (rest sigs)))))]
    (assert (vector? found-sig)
            (str "No valid constructor was found for the initial props "
                 key-set " and the constructor list "
                 constructor-list))
    found-sig))


(defn ^:private instantiate
  "Create a new object with the provided constructor and the given list of
  constructor arguments"
  ([constructor-fn]
    (new constructor-fn))
  ([constructor-fn a]
    (new constructor-fn a))
  ([constructor-fn a b]
    (new constructor-fn a b))
  ([constructor-fn a b c]
    (new constructor-fn a b c))
  ([constructor-fn a b c d]
    (new constructor-fn a b c d))
  ([constructor-fn a b c d e]
    (new constructor-fn a b c d e))
  ([constructor-fn a b c d e f]
    (new constructor-fn a b c d e f))
  ([constructor-fn a b c d e f g]
    (new constructor-fn a b c d e f g))
  ([constructor-fn a b c d e f g h]
    (new constructor-fn a b c d e f g h)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public


(defn parent
  "Return a reference to the object's parent using the :get-parent function
  from the obj-desc."
  [obj-desc obj&]
  (let [get-parent-fn (:get-parent obj-desc)]
    (assert (fn? get-parent-fn)
            ":get-parent must be a function defined in the object description")
    (get-parent-fn obj&)))


(defn children
  "Return a list of the object's children using the `:get-children` function
  from the `obj-desc`."
  [obj-desc obj&]
  (let [get-children-fn (:get-children obj-desc)]
    (assert (fn? get-children-fn)
            ":get-children must be a function defined in the object description")
    (get-children-fn obj&)))


(defn add-child!
  "Add a child to the object using the `:add-child` function from the `obj-desc`.
  The child will be added to the end of the child list.

  Returns a reference to the `new-child`."
  [obj-desc obj& new-child&]
  (let [add-child-fn (:add-child obj-desc)]
    (assert (fn? add-child-fn)
            ":add-child must be a function defined in the object description")
    (add-child-fn obj& new-child&)))


(defn replace-child!
  "Replace the object's `old-child` with the `new-child` using the `:replace-child`
  function from the `obj-desc`.

  Returns a reference to the `old-child`."
  [obj-desc obj& old-child& new-child&]
  (let [replace-child-fn (:replace-child obj-desc)]
    (assert (fn? replace-child-fn)
            ":replace-child must be a function defined in the object description")
    (replace-child-fn obj& old-child& new-child&)))


(defn remove-child!
  "Remove the child from the object's children list using the `:remove-child`
  function from the `obj-desc`.

  Returns a reference to the `child`."
  [obj-desc obj& child&]
  (let [remove-child-fn (:remove-child obj-desc)]
    (assert (fn? remove-child-fn)
            ":remove-child must be a function defined in the object description")
    (remove-child-fn obj& child&)))


(defn child-index
  "Return the index of the child object"
  [obj-desc &obj &child]
  (let [child-index-fn (:child-index obj-desc)]
    (assert (fn? child-index-fn)
            ":child-index must be a function defined in the object description")
    (child-index-fn &obj &child)))


(defn destroy!
  "If a `:destructor` exists in the `obj-desc` call it on the object, otherwise
  do nothing. The object's `:destructor` should destroy all child objects and
  remove itself from its parent, if a parent exists."
  [obj-desc obj& ]
  (when-let [destructor-fn (:destructor obj-desc)]
    (destructor-fn obj-desc obj&)))


(defn prop
  "Return the value of a property from an object using the getter define in
  `obj-desc`."
  [obj-desc obj& key]
  (assert (contains? (:prop-map obj-desc) key)
          (str "There is no definition for the property " key))
  (let [getter (get-in obj-desc [:prop-map key :getter])
        obj-prop (get-in obj-desc [:prop-map key :property])]
    (assert (fn? getter)
            (str "The getter for the property " key " must be a function."))
    (assert (string? obj-prop)
            (str "The property " key " must have the name of the "
                 "object's property defined."))
    (getter obj& obj-prop)))


(defn props
  "Return a map of properties from an object using the property getters in the
  `obj-desc`."
  [obj-desc obj&]
  (let [prop-map (get obj-desc :prop-map)]
    (loop [props prop-map
           got-props (transient {})]
      (if-let [[prop desc] (first props)]
        (recur (rest props)
               (if-let [getter (get desc :getter)]
                 (assoc! got-props prop (getter obj& (get desc :property)))
                 got-props))
        (persistent! got-props)))))


(defn set-prop!
  "Given an object and its object description, set the value of the property.
  And optional previous value can be supplied which will allow the setter to
  determine if it should actually go through the work of setting the property if
  the value didn't change."
  [prop-map obj& prop val]
  (let [setter! (get-in prop-map [prop :setter])
        obj-prop (get-in prop-map [prop :property])]
    (if setter!
      (setter! obj& obj-prop val)
      (throw (js/Error. (str "No :setter found for property " prop))))))


(defn set-props!
  "Using the setters in the `obj-desc`, set all the properties in `props` on the
  object."
  [obj-desc obj& props]
  (let [prop-map (get obj-desc :prop-map)]
    (loop [set-props props]
      (if-let [[prop val] (first set-props)]
        (do (set-prop! prop-map obj& prop val)
            (recur (rest set-props)))
        obj&))))


(defn set-default-prop!
  "Set the default value of the property on the object given a map of default
   values"
  [obj-desc obj& prop]
  (let [default-val (get-in obj-desc [:default-props prop])]
    (if (some? default-val)
      (set-prop! (:prop-map obj-desc) obj& prop default-val)
      (throw
        (js/Error.
          (str "The property " prop " does not have a default value set and must be "
               "explicitly specified"))))))


(defn update-props!
  "Update properties in the given objects to reflect `props`. The `prev-props`
  will be used to determine which new properties need to be set. If a property
  existed in `prev-props` but no longer exists, then the default values
  described in `obj-desc` will be set on the object."
  [obj-desc obj& props prev-props]
  (let [prop-map (get obj-desc :prop-map)]
    ;; Set all new props
    (loop [set-props props]
      (if-let [[key val] (first set-props)]
        (let [prev-val (get prev-props key)]
          ;; Don't call the setter if the value is the same as previously set
          (when (not= val prev-val)
            (set-prop! prop-map obj& key val))
          (recur (rest set-props)))))
    ;; Set defaults for any props that are no longer set
    (loop [prev-keys (keys prev-props)]
      (if-let [key (first prev-keys)]
        (let [prev-val (get prev-props key)]
          (when (not (contains? props key))
            (set-default-prop! obj-desc obj& key))
          (recur (rest prev-keys)))))
    obj&))


(defn construct-obj!
  "Construct a new object as described by the `obj-desc` with the provided
  properties. After the object is constructed the post-constructor function
  will be called if it is defined in the `obj-desc`. The new object will
  be returned."
  ([obj-desc]
    (construct-obj! obj-desc {}))
  ([obj-desc init-props]
   (let [constructor-list (:constructor-list obj-desc)]
     (assert (or (nil? constructor-list) (vector? constructor-list))
             (str "'" constructor-list "'" "is not a valid :constructor-list.")))
   (let [effective-props (merge (:default-props obj-desc) init-props)
         sig (constructor-sig (:constructor-list obj-desc)
                              (keys effective-props))
         args (mapv #(get effective-props %) sig)
         new-obj& (apply instantiate (:constructor obj-desc) args)
         non-constructor-props (apply dissoc init-props sig)]
     ;; Set all props that weren't included in the constructor on the new object
     (set-props! obj-desc new-obj& non-constructor-props)
     (when-let [post-constructor-fn (:post-constructor obj-desc)]
       (post-constructor-fn obj-desc new-obj& init-props))
     new-obj&)))

