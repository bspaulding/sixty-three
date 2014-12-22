var matchers = {};

matchers.toBeInstanceOf = function(expected) {
  return {
    compare: function(actual, expected) {
      return {
        pass: (actual instanceof expected),
        message: "Expected " + actual + " to be an instance of " + expected.name
      };
    }
  };
}

beforeEach(function() {
  jasmine.addMatchers(matchers);
});
