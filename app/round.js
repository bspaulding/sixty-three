/*global _: false, Suit: false */
var Round = (function () {
  "use strict";

  function Round(game) {
    this.game = game;
    this.kiddy = [];
  }

  Round.prototype.play = function () {
    var i;
    function callPlayCard(player) {
      player.playCard();
    }

    this.deal();
    this.bidForTrump();
    this.redeal();
    for (i = 0; i < 6; i += 1) {
      _.each(this.players(), callPlayCard);
    }
    this.players()[0].score(63);
  };

  Round.prototype.bidForTrump = function () {
    var highBidder, bid = 0, i, player, playerBid;

    for (i = 0; i < this.players().length; i += 1) {
      player = this.players()[i];
      playerBid = player.bid();
      if (playerBid > bid) {
        highBidder = player;
        bid = playerBid;
      }
    }
    this.controllingPlayer = highBidder;
  };

  Round.prototype.setTrump = function () {
    this.trumpSuit = Suit.spades;
  };

  Round.prototype.deal = function () {
    var k, i, j, player;

    for (k = 0; k < 3; k += 1) {
      for (i = 0; i < this.players().length; i += 1) {
        player = this.players()[i];
        for (j = 0; j < 3; j += 1) {
          player.addCard(this.deck().pop());
        }
      }
    }

    for (i = 0; i < 3; i += 1) {
      this.kiddy.push(this.deck().pop());
    }
  };

  Round.prototype.redeal = function () {
    var self = this;
    _.each(this.players(), function (player) {
      player.discard();
      while (player.numCards() < 6) {
        player.addCard(self.draw());
      }
    });
  };

  Round.prototype.players = function () {
    return this.game.players;
  };

  Round.prototype.deck = function () {
    return this.game.deck;
  };

  Round.prototype.draw = function () {
    return this.deck().draw();
  };

  Round.prototype.totalScore = function () {
    return _.reduce(this.players(), function (sum, player) {
      return sum + player.score();
    }, 0);
  };

  return Round;
}());
