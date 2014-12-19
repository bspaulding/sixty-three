describe("Player", function() {
  var player;
  var sevenOfHearts = new Card(7, Suit.hearts);

  beforeEach(function() {
    player = new Player();
  });

  describe("score", function() {
    it("should set score", function() {
      player.score(199);
      expect(player.score()).toEqual(199);
    });
  });

  describe("addCard", function() {
    it("adds the card to the players hand", function() {
      expect(player.hasCard(sevenOfHearts)).toEqual(false);

      player.addCard(sevenOfHearts);

      expect(player.hasCard(sevenOfHearts)).toEqual(true);
    });
  });

  describe("playCard", function() {
    it("removes and returns the #nextCardToPlay", function() {
      player.addCard(sevenOfHearts);
      player.nextCardToPlay = function() { return sevenOfHearts; };

      var playedCard = player.playCard();

      expect(playedCard).toEqual(sevenOfHearts);
      expect(player.hasCard(playedCard)).toEqual(false);
    });
  });

  describe("numCards", function() {
    it("returns the number of cards in the players hand", function() {
      expect(player.numCards()).toEqual(0);
      player.addCard(sevenOfHearts);
      expect(player.numCards()).toEqual(1);
    });
  });
});
