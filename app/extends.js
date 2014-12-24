function __extends(child, parent) {
  "use strict";

  var key;
  for (key in parent) {
    if (parent.hasOwnProperty(key)) {
      child[key] = parent[key];
    }
  }
  function Ctor() {
    this.constructor = child;
  }
  Ctor.prototype = parent.prototype;
  child.prototype = new Ctor();
  child.__super__ = parent.prototype;
  return child;
}
