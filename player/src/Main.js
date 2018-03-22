"use strict";

// module Main

exports.onyxSong = window.onyxSong;

exports.onPoint = function(onclick) {
  return function() {
    window.addEventListener("touchstart", function(e) {
      var touch = e.touches[0];
      onclick({x: touch.clientX, y: touch.clientY})();
      e.stopPropagation();
      e.preventDefault();
    });
    window.addEventListener("click", function(e) {
      onclick({x: e.clientX, y: e.clientY})();
    });
  };
};

exports.displayError = function(err) {
  return function() {
    document.getElementById('the-error-message').innerHTML = err;
    document.getElementById('the-canvas').style.display = 'none';
  };
};

exports.setTitle = function(str) {
  return function() {
    document.title = str;
  };
};
