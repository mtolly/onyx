"use strict";

// module Draw

exports.getWindowDims = function(){
  return {w: window.innerWidth, h: window.innerHeight};
}

exports.numMod = function(x) {
  return function(y) {
    return x % y;
  };
};
