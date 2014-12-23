function InsufficientDiscard(message) {
  this.name = arguments.callee.name
  this.message = message;
};
InsufficientDiscard.prototype = new Error();
InsufficientDiscard.prototype.constructor = InsufficientDiscard;

var Player = (function() {
  function MethodNotImplementedError(message) {
    this.name = arguments.callee.name
    this.message = message;
  };
  MethodNotImplementedError.prototype = new Error();
  MethodNotImplementedError.prototype.constructor = MethodNotImplementedError;

  function Player(name) {
    this.name = name;
    this.cards = [];
    this._score = 0;
  }

  // TODO: Inject these in a strategy object, instead of using inheritance. i.e:
  //   var randomPlayer = new Player("Name", RandomStrategy);
  //   var humanPlayer = new Player("Name", UserStrategy);
  // This will allow us to protect cards and score from potential cheats!
  var methodNames = ['bid', 'declareTrump', 'nextCardToPlay', 'cardsToDiscard'];
  for ( var i = 0; i < methodNames.length; i += 1 ) {
    var methodName = methodNames[i];
    Player.prototype[methodName] = function() {
      throw new MethodNotImplementedError(this.constructor.name + " does not implement '" + methodName + "'");
    }
  }

  Player.prototype.score = function(newScore) {
    if (arguments.length > 0) {
      this._score = newScore;
    }

    return this._score;
  }

  Player.prototype.addCard = function(card) {
    this.cards.push(card);
  }

  Player.prototype.hasCard = function(card) {
    return this.cards.indexOf(card) >= 0;
  }

  Player.prototype.playCard = function() {
    var nextCard = this.nextCardToPlay();
    this.cards = _.reject(this.cards, function(card) { return card.equals(nextCard); });
    return nextCard;
  }

  Player.prototype.numCards = function() {
    return this.cards.length;
  }

  Player.prototype.discard = function() {
    var discardedCards = this.cardsToDiscard();
    if (this.numCards() - discardedCards.length > 6) {
      throw new InsufficientDiscard(["Player '", this.name, "' tried to discard too few cards."].join(''));
    }
    this.cards = _.reject(this.cards, function(card) {
      return discardedCards.indexOf(card) >= 0;
    });
  }

  return Player;
}());
