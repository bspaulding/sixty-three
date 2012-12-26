describe("Suit", function() {
  describe("oppositeSuit", function() {
    it("should be spades for clubs", function() {
      expect(Suit.clubs.oppositeSuit()).toEqual(Suit.spades);
    });

    it("should be clubs for spades", function() {
      expect(Suit.spades.oppositeSuit()).toEqual(Suit.clubs);
    });

    it("should be hearts for diamonds", function() {
      expect(Suit.diamonds.oppositeSuit()).toEqual(Suit.hearts);
    });

    it("should be diamonds for hearts", function() {
      expect(Suit.hearts.oppositeSuit()).toEqual(Suit.diamonds);
    });
  });
});
