var SixtyThree = (function() {
  function SixtyThree() {
    this.deck = new Deck();
    this.ruleSet = new RuleSet();
  }

  SixtyThree.prototype.start = function() {
    this.setupPlayers();

    while ( !this.gameIsOver() ) {
      var round = new Round(this);
      round.play();
    }
  }

  SixtyThree.prototype.setupPlayers = function() {
    this.players = [
      new RandomPlayer("Player 1"),
      new RandomPlayer("Player 2"),
      new RandomPlayer("Player 3"),
      new RandomPlayer("Player 4")
    ];
  }

  SixtyThree.prototype.gameIsOver = function() {
    return this.topScore() >= 200;
  }

  SixtyThree.prototype.topScore = function() {
    var playersSortedByScoreAsc = _.sortBy(this.players, function(player) {
      return player.score;
    })

    return playersSortedByScoreAsc[playersSortedByScoreAsc.length-1].score;
  }

  return SixtyThree;
}());
