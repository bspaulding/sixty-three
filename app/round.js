var Round = (function() {
  function Round(game) {
    this.game = game;
    this.kiddy = [];
  }

  Round.prototype.play = function() {}

  Round.prototype.deal = function() {
    for ( var i = 0; i < this.players().length; i += 1 ) {
      var player = this.players()[i];
      for ( var j = 0; j < 5; j += 1 ) {
        player.addCard(this.deck().pop());
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
