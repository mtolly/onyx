"use strict";

// module Audio

function AudioHandle(audio){
  var self = this;
  self.audio = audio;

  self.playFrom = function(t){
    if (t < self.audio.duration()) {
      var sound_id = self.audio.play();
      self.audio.seek(t);
      self.stop = function(){
        self.audio.stop(sound_id);
      };
    } else {
      self.stop = function(){};
    }
  };
}

exports.loadAudio = function(cb){
  return function(){
    var sound = new Howl({
      src: ['preview-audio.ogg', 'preview-audio.mp3'],
      autoplay: false,
      loop: false,
      preload: true,
      onload: function() {
        cb(new AudioHandle(sound))();
      }
    });
  };
};

exports.playFrom = function(audio){
  return function(t){
    return function(){
      audio.playFrom(t);
    }
  };
};

exports.stop = function(audio){
  return function(){
    audio.stop();
  };
};
