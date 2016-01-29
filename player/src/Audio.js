"use strict";

// module Audio

exports.getTheAudio = function(){
  return document.getElementById('the-audio');
};

exports.onLoad = function(audio){
  return function(cb){
    return function(){
      if (audio.networkState == HTMLMediaElement.NETWORK_LOADING) {
        var listener = function(){
          audio.removeEventListener('canplaythrough', listener);
          cb();
        }
        audio.addEventListener('canplaythrough', listener);
      } else if (audio.networkState == HTMLMediaElement.NETWORK_IDLE) {
        setTimeout(cb, 0);
      }
    };
  };
};

exports.play = function(audio){
  return function(){
    audio.play();
  };
};

exports.pause = function(audio){
  return function(){
    audio.pause();
  };
};

exports.playFrom = function(t){
  return function(audio){
    return function(){
      var audioLen = audio.seekable.end(0);
      audio.pause();
      if (t < audioLen) {
        audio.currentTime = t;
        audio.play();
      }
    }
  }
}
