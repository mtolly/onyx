"use strict";

// module Draw

exports.getWindowDims = function(){
  return {width: window.innerWidth, height: window.innerHeight};
}

exports.numMod = function(x) {
  return function(y) {
    return x % y;
  };
};
