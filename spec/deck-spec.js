describe("Deck", function() {
  var deck;

  beforeEach(function() {
    deck = new Deck();
  });

  describe("pop", function() {
    it("should return the last card", function() {
      var lastCard = deck.cards[deck.cards.length-1];

      expect(deck.pop()).toEqual(lastCard);
    });

    it("should remove the returned card from the deck", function() {
      var originalLength = deck.length();
      var poppedCard = deck.pop();

      expect(deck.cards.indexOf(poppedCard)).toEqual(-1);
      expect(deck.length()).toEqual(originalLength - 1);
    });
  });

  describe("length", function() {
    it("should return the length of cards", function() {
      deck.cards = [1,2,3];

      expect(deck.length()).toEqual(3);
    });
  });
});
