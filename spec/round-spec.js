describe("Round", function() {
  var round;

  var eachPlayer = function(f) {
    for ( var i = 0; i < round.players().length; i += 1 ) {
      f(round.players()[i]);
    }
  };

  var spyOnPlayers = function(method) {
    eachPlayer(function(player) {
      spyOn(player, method).and.callThrough();
    });
  };

  var expectPlayerSpies = function(method, expectation) {
    eachPlayer(function(player) {
      expect(player[method])[expectation]();
    });
  };

  beforeEach(function() {
    var game = new SixtyThree();
    game.setupPlayers();
    round = new Round(game);
  });

  describe("play", function() {
    it("deals, bids, deals, plays all hands, totals score", function() {
      spyOn(round, 'deal').and.callThrough();
      spyOn(round, 'redeal').and.callThrough();
      expect(round.totalScore()).toEqual(0);
      spyOnPlayers('bid');
      spyOnPlayers('playCard');

      round.play();

      expect(round.deal).toHaveBeenCalled();
      expectPlayerSpies('bid', 'toHaveBeenCalled');
      expect(round.redeal).toHaveBeenCalled();
      expectPlayerSpies('playCard', 'toHaveBeenCalled');
      eachPlayer(function(player) {
        expect(player['playCard'].calls.count()).toEqual(6);
        expect(player.numCards()).toEqual(0);
      });
      round.players()
      expect(round.totalScore()).toEqual(63);
    });
  });

  describe("redeal", function() {
    it("should call discard, re-deal to 6 cards each", function() {
      spyOnPlayers('discard');
      round.setTrump();
      round.redeal();
      expectPlayerSpies('discard', 'toHaveBeenCalled');
      _.each(round.players(), function(player) {
        expect(player.numCards()).toEqual(6);
      });
    });
  });

  describe("totalScore", function() {
    it("equals the sum of players' scores", function() {
      expect(round.totalScore()).toEqual(0);

      round.players()[0].score(10);
      round.players()[1].score(11);
      round.players()[2].score(12);
      round.players()[3].score(13);

      expect(round.totalScore()).toEqual(46);
    });
  });

  describe("setTrump", function() {
    it("should set trump to the controllingPlayer's #declareTrump", function() {
      round.controllingPlayer = round.players()[0];
      round.controllingPlayer.declareTrump = function() {
        return Suit.spades;
      };
      round.setTrump();

      expect(round.trumpSuit).toBeInstanceOf(Suit);
      expect(round.trumpSuit).toEqual(round.controllingPlayer.declareTrump());
    });
  });

  describe("bidForTrump", function() {
    it("should ask each player for their bid", function() {
      spyOnPlayers('bid');
      round.bidForTrump();
      expectPlayerSpies('bid', 'toHaveBeenCalled');
    });

    it("should set controllingPlayer to a Player in the round", function() {
      round.bidForTrump();

      expect(round.controllingPlayer).toBeInstanceOf(Player);
      expect(round.players()).toContain(round.controllingPlayer);
    });

    it("should set controllingPlayer to the Player who bids the highest", function() {
      for ( var i = 0; i < round.players().length; i += 1 ) {
        if ( i != 2 ) {
          var player = round.players()[i];
          spyOn(player, 'bid').and.returnValue(0);
        }
      }

      var winningPlayer = round.players()[2];
      spyOn(winningPlayer, 'bid').and.returnValue(63);

      round.bidForTrump();

      expect(round.controllingPlayer).toEqual(winningPlayer);
    });
  });

  describe("deal", function() {
    it("should deal 9 cards to each player", function() {
      round.deal();

      for ( var i = 0; i < round.players().length; i += 1 ) {
        var player = round.players()[i];
        expect(player.cards.length).toEqual(9);
      }
    });

    it("should deal 3 cards to the kiddy", function() {
      round.deal();

      expect(round.kiddy.length).toEqual(3);
    });

    it("should deal 3 sets of 3 to players, then 3 to kiddy", function() {
      var numCards = round.deck().cards.length;
      var playersCards = [[], [], [], []];

      for ( var i = 0; i < 3; i += 1 ) {
        for ( var j = 0; j < round.players().length; j += 1 ) {
          for ( var k = 0; k < 3; k += 1 ) {
            numCards -= 1;
            var card = round.deck().cards[numCards];
            playersCards[j].push(card);
          }
        }
      }

      var kiddyCards = [];
      for ( var i = 0; i < 3; i += 1 ) {
        numCards -= 1;
        kiddyCards.push(round.deck().cards[numCards]);
      }

      round.deal();

      for ( var i = 0; i < playersCards.length; i += 1 ) {
        expect(round.players()[i].cards).toEqual(playersCards[i]);
      }

      expect(round.kiddy).toEqual(kiddyCards);
    });
  });
});
