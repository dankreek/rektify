goog.provide('test.classes');

/*******************************************************************************
 * Fish
 */

/** @constructor */
test.classes.Fish = function Fish() {
  this._children = [];
  this._destroyed = false;
  this._parent = null;
};

test.classes.Fish.prototype.addChild = function(child) {
  child._parent = this;
  this._children.push(child);
};

test.classes.Fish.prototype.getChildIndex = function(child) {
  var childI = this._children.indexOf(child);
  if (childI >= 0) {
    return childI;
  } else {
    throw Error('Child not found');
  }
};

test.classes.Fish.prototype.removeChild = function(child) {
  var childIndex = this.getChildIndex(child);

  if ((childIndex !== 0) && !childIndex) {
    throw Error('Child not found');
  }

  this.removeChildAt(childIndex);
};

test.classes.Fish.prototype.getChildAt = function(index) {
  return this._children[index];
};

test.classes.Fish.prototype.replaceChildAt = function(newChild, index) {
  if ((index !== 0) && !index) {
    throw Error('no index provided');
  }

  if ((index < 0) || (index > this._children.length)) {
    throw Error('invalid index for replaceChildAt');
  }

  var oldChild = this._children[index];
  oldChild._parent = null;

  this._children[index] = newChild;
};

test.classes.Fish.prototype.removeChildAt = function(index) {
  if ((index !== 0) && !index) {
    throw Error('index is missing or invalid');
  }

  if ((index < 0) || (index >= this._children.length)) {
    throw Error('Index ' + index + ' is out of bounds.');
  }

  var child = this._children[index];

  child._parent = null;
  this._children.splice(index, 1);
  return child;
};

test.classes.Fish.prototype.getChildren = function() {
  return this._children;
};

test.classes.Fish.prototype.getParent = function() {
  return this._parent;
};

test.classes.Fish.prototype.destroy = function() {
  if (this._parent) {
    this._parent.removeChild(this);
  }

  this._children = null;
  this._parent = null;
  this._destroyed = true;
};

test.classes.Fish.prototype.isDestroyed = function() {
  return this._destroyed;
};

/*******************************************************************************
 * OneFish
 */

/**
 * @constructor
 * @extends {test.classes.Fish}
 */
test.classes.OneFish = function OneFish() {
  test.classes.Fish.call(this);
  this.someProp = false;
  this.wasConstructorCalled = true;
};

test.classes.OneFish.prototype = Object.create(test.classes.Fish.prototype);
test.classes.OneFish.prototype.constructor = test.classes.OneFish;

/*******************************************************************************
 * TwoFish
 */

/**
 * @param firstArg
 * @param secondArg
 * @constructor
 * @extends {test.classes.Fish}
 */
test.classes.TwoFish = function TwoFish(firstArg, secondArg) {
  test.classes.Fish.call(this);
  if (!firstArg || !secondArg) {
    throw Error('TwoFish constructor needs two args');
  }

  this.first = firstArg;
  this.second = secondArg;
  this.postConstructorCalled = false;
}

test.classes.TwoFish.prototype = Object.create(test.classes.Fish.prototype);
test.classes.TwoFish.prototype.constructor = test.classes.TwoFish;

/*******************************************************************************
 * RedFish
 */

/**
 * @constructor
 * @extends {test.classes.Fish}
 */
test.classes.RedFish = function RedFish() {
  test.classes.Fish.call(this);
  this.x = 0;
  this.y = 1;
  this.z = -1;
};

test.classes.RedFish.prototype = Object.create(test.classes.Fish.prototype);
test.classes.RedFish.prototype.constructor = test.classes.RedFish;

/**
 * @param x
 * @param y
 * @param z
 * @public
 */
test.classes.RedFish.prototype.setSomething = function(x, y, z) {
  this.x = x;
  this.y = y;
  this.z = z;
};

/*******************************************************************************
 * BlueFish
 */

/**
 * @constructor
 * @extends {test.classes.Fish}
 */
test.classes.BlueFish = function BlueFish() {
  test.classes.Fish.call(this);
};

test.classes.BlueFish.prototype = Object.create(test.classes.Fish.prototype);
test.classes.BlueFish.prototype.constructor = test.classes.BlueFish;

