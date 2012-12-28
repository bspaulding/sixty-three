var Round = (function() {
  function Round(game) {
    this.game = game;
    this.kiddy = [];
  }

  Round.prototype.play = function() {}

  Round.prototype.bidForTrump = function() {
    this.trumpSuit = Suit.spades;
    for ( var i = 0; i < this.players().length; i += 1 ) {
      this.players()[i].bid();
    }
    this.controllingPlayer = this.players()[0];
  }

    for ( var i = 0; i < 3; i += 1 ) {
      this.kiddy.push(this.deck().pop());
    }
  }

  Round.prototype.players = function() {
    return this.game.players;
  }

  Round.prototype.deck = function() {
    return this.game.deck;
  }

  return Round;
}());
