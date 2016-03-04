"use strict";

// module Audio

function AudioHandle(audio){
  var self = this;
  self.audio = audio;

  self.playFrom = function(t, cb){
    if (t < self.audio.duration()) {
      self.audio.on('play', function(){
        self.audio.off('play');
        cb();
      });
      var sound_id = self.audio.play();
      self.audio.seek(t);
      self.stop = function(){
        self.audio.stop(sound_id);
      };
    } else {
      self.stop = function(){};
      cb();
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
    return function(cb){
      return function(){
        audio.playFrom(t, cb);
      };
    };
  };
};

exports.stop = function(audio){
  return function(){
    audio.stop();
  };
};
