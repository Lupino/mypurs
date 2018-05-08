exports.add = function(a) {
  return function(b) {
    return a + b;
  }
}

exports._add1 = function(a, b) {
  return a + b;
}

exports.randInt = function(a) {
  return function(b) {
    return function() {
      return Math.floor(Math.random() * (b - a) + a);
    }
  }
}

exports._randInt1 = function(a, b) {
  return function() {
    return Math.floor(Math.random() * (b - a) + a);
  }
}

exports._maybeBigZero = function(a) {
  return function(nothing) {
    return function(just) {
      if (a > 0) {
        return just(a);
      } else {
        return nothing;
      }
    }
  }
}

exports._eitherBigZero = function(a) {
  return function(left) {
    return function(right) {
      if (a < 0) {
        return left("less then zero");
      } else {
        return right(a)
      }
    }
  }
}
