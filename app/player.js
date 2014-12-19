var Player = (function() {
  function MethodNotImplementedError(message) {
    this.name = arguments.callee.name
    this.message = message;
  };
  MethodNotImplementedError.prototype = Error.prototype;

  function Player(name) {
    this.name = name;
    this.cards = [];
    this._score = 0;
  }

  // TODO: Inject these in a strategy object, instead of using inheritance. i.e:
  //   var randomPlayer = new Player("Name", RandomStrategy);
  //   var humanPlayer = new Player("Name", UserStrategy);
  // This will allow us to protect cards and score from potential cheats!
  var methodNames = ['bid', 'declareTrump', 'nextCardToPlay'];
  for ( var i = 0; i < methodNames.length; i += 1 ) {
    var methodName = methodNames[i];
    Player.prototype[methodName] = function() {
      throw new MethodNotImplementedError(this.constructor.name + " does not implement '" + methodName + "'");
    }
  }

  Player.prototype.setScore = function(newScore) {
    this.score = newScore;
  }

  Player.prototype.addCard = function(card) {
    this.cards.push(card);
  }

  return Player;
}());
