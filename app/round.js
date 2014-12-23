var Round = (function() {
  function Round(game) {
    this.game = game;
    this.kiddy = [];
  }

  Round.prototype.play = function() {
    this.deal();
    this.bidForTrump();
    this.redeal();
    for (var i = 0; i < 6; i += 1) {
      _.each(this.players(), function(player) {
        player.playCard();
      });
    }
    this.players()[0].score(63);
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

  Round.prototype.redeal = function() {
    var self = this;
    _.each(this.players(), function(player) {
      player.discard();
      while (player.numCards() < 6) {
        player.addCard(self.draw());
      }
    });
  }

  Round.prototype.players = function() {
    return this.game.players;
  }

  Round.prototype.deck = function() {
    return this.game.deck;
  }

  Round.prototype.draw = function() {
    return this.deck().draw();
  }

  Round.prototype.totalScore = function() {
    return _.reduce(this.players(), function(sum, player) {
      return sum + player.score();
    }, 0);
  }

  return Round;
}());
