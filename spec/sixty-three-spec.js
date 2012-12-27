describe("SixtyThree", function() {
  var sixtyThree;

  beforeEach(function() {
    sixtyThree = new SixtyThree();
  });

  describe("gameIsOver", function() {
    it("should return true when topScore is >= 200", function() {
      spyOn(sixtyThree, 'topScore').andReturn(200);
      expect(sixtyThree.gameIsOver()).toEqual(true);
    });

    it("should return false when topScore is < 200", function() {
      spyOn(sixtyThree, 'topScore').andReturn(199);
      expect(sixtyThree.gameIsOver()).toEqual(false);
    });
  });

  describe("topScore", function() {
    it("should return the highest player's score", function() {
      sixtyThree.setupPlayers();
      for ( var i = 0; i < sixtyThree.players.length; i += 1 ) {
        sixtyThree.players[i].setScore(10 * i);
      }

      expect(sixtyThree.topScore()).toEqual(10 * (sixtyThree.players.length - 1));
    });
  });
});
