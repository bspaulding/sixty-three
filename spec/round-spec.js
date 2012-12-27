describe("Round", function() {
  var round;

  beforeEach(function() {
    var game = new SixtyThree();
    game.setupPlayers();
    round = new Round(game);
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
