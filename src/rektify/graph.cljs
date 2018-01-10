(ns rektify.graph)

(defprotocol ^:no-doc IGraphNode
  "Methods for types which are members of a graph."

  (-get-children
    [this]
    "Get the list of children of this object.")

  (-get-parent
    [this]
    "Get the parent of this object.")

  (-add-child!
    [this child]
    "Add a child to the end of the object's child list")

  (-child-index
    [this child]
    "Get the index in the child list of the provided child object.")

  (-replace-child-at!
    [this new-child index]
    "Replace the object at the given index with the provided child.")

  (-remove-child-at!
    [this index]
    "Remove the child of this object at the given index. Return the child object
    that was removed."))

