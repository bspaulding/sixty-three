describe("Round", function() {
  var round;

  beforeEach(function() {
    var game = new SixtyThree();
    game.setupPlayers();
    round = new Round(game);
  });

  describe("bidForTrump", function() {
    it("should set trump to a Suit", function() {
      round.bidForTrump();

      expect(round.trumpSuit).toBeInstanceOf(Suit);
    });

    it("should ask each player for their bid", function() {
      for ( var i = 0; i < round.players().length; i += 1 ) {
        spyOn(round.players()[i], 'bid').andCallThrough();
      }

      round.bidForTrump();

      for ( var i = 0; i < round.players().length; i += 1 ) {
        expect(round.players()[i].bid).toHaveBeenCalled();
      }
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
          spyOn(player, 'bid').andReturn(0);
        }
      }

      var winningPlayer = round.players()[2];
      spyOn(winningPlayer, 'bid').andReturn(63);

      round.bidForTrump();

      expect(round.controllingPlayer).toEqual(winningPlayer);
    });
  });

  describe("deal", function() {
    it("should deal 5 cards to each player", function() {
      round.deal();

      for ( var i = 0; i < round.players().length; i += 1 ) {
        var player = round.players()[i];
        expect(player.cards.length).toEqual(5);
      }
    });

    it("should deal 3 cards to the kiddy", function() {
      round.deal();

      expect(round.kiddy.length).toEqual(3);
    });

    xit("should deal 3 to players, then 3 to kiddy, then 2 to players", function() {
    });
  });
});
