var Round = (function() {
  function Round(game) {
    this.game = game;
    this.kiddy = [];
  }

  Round.prototype.play = function() {
    this.bidForTrump();
    _.each(this.players(), function(player) {
      player.playCard();
    });
  }

  Round.prototype.bidForTrump = function() {
    var highBidder;
    var bid = 0;
    for ( var i = 0; i < this.players().length; i += 1 ) {
      var player = this.players()[i];
      var playerBid = player.bid();
      if (playerBid > bid) {
        highBidder = player;
        bid = playerBid;
      }
    }
    this.controllingPlayer = highBidder;
  }

  Round.prototype.setTrump = function() {
    this.trumpSuit = Suit.spades;
  }

  Round.prototype.deal = function() {
    for ( var k = 0; k < 3; k += 1 ) {
      for ( var i = 0; i < this.players().length; i += 1 ) {
        var player = this.players()[i];
        for ( var j = 0; j < 3; j += 1 ) {
          player.addCard(this.deck().pop());
        }
      }
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
