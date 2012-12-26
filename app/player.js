var Player = (function() {
  function MethodNotImplementedError(message) {
    this.name = arguments.callee.name
    this.message = message;
  };
  MethodNotImplementedError.prototype = Error.prototype;

  function Player(name) {
    this.name = name;
    this.cards = [];
  }

  var methodNames = ['bid'];
  for ( var i = 0; i < methodNames.length; i += 1 ) {
    var methodName = methodNames[i];
    Player.prototype[methodName] = function() {
      throw new MethodNotImplementedError(this.constructor.name + " does not implement '" + methodName + "'");
    }
  }

  return Player;
}());
