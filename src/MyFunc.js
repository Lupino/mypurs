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

var fs = require('fs');
exports._readText = function(fn) {
  return function(left) {
    return function(right) {
      return function(cb) {
        return function() {
          fs.readFile(fn, (function(err, text) {
            if (err) {
              return cb(left(err))();
            } else {
              return cb(right(text.toString()))();
            }
          }));
          return {}
        }
      }
    }
  }
}

exports._readTextAff1 = function(fn) {
  return function(onError, onSuccess) {
    fs.readFile(fn, function(err, text) {
      if (err) {
        onError(err);
      } else {
        onSuccess(text.toString());
      }
    });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  }
}

exports._readTextAff2 = function(fn) {
  return function() {
    return new Promise(function(resolve, reject) {
      fs.readFile(fn, function(err, text) {
        if (err) {
          return reject(err);
        } else {
          return resolve(text.toString());
        }
      })
    });
  }
}
