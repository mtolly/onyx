"use strict";

// module Main

exports.onyxSong = window.onyxSong;

exports.onPoint = function(onclick) {
  return function() {
    var touched = false;
    window.addEventListener("touchstart", function(e) {
      touched = true;
      var touch = e.touches[0];
      onclick({x: touch.clientX, y: touch.clientY})();
      e.stopPropagation();
      e.preventDefault();
    });
    window.addEventListener("click", function(e) {
      if (touched) return;
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

exports.openMenu = function() {
  document.getElementById('the-side-menu').classList.add('menu-open');
};

exports.closeMenu = function() {
  document.getElementById('the-side-menu').classList.remove('menu-open');
};

var menuImmutable = null;
var menuMutable = null;

exports.fillMenu = function(song) {
  return function(settings) {
    return function() {
      menuMutable = settings;
      menuImmutable = JSON.parse(JSON.stringify(menuMutable));
      function tag(p,t,f){
        var element = document.createElement(t);
        if (f) f(element);
        p.appendChild(element);
      }
      var menu = document.getElementById('the-side-menu');
      tag(menu, 'h1', function(h1){
        h1.innerHTML = song.title;
      });
      tag(menu, 'h2', function(h2){
        h2.innerHTML = song.artist;
      });
      if (song.author.match(/\S/)) {
        tag(menu, 'h4', function(h4){
          h4.innerHTML = 'Author: ' + song.author;
        });
      }
      tag(menu, 'div', function(divSwitches){
        divSwitches.className = 'switches';
        function settingsToggle(key, name) {
          tag(divSwitches, 'p', function(p){
            tag(p, 'label', function(label){
              tag(label, 'input', function(checkbox){
                checkbox.type = 'checkbox';
                checkbox.checked = menuMutable[key];
                checkbox.addEventListener('change', function(e){
                  menuMutable[key] = e.target.checked;
                  menuImmutable = JSON.parse(JSON.stringify(menuMutable));
                });
              });
              label.insertAdjacentHTML('beforeend', ' ' + name);
            });
          });
        }
        settingsToggle('autoplay', 'Enable Bots');
        settingsToggle('leftyFlip', 'Lefty Flip');
        settingsToggle('staticVert', 'Static Instruments');
      });
      function setIcon(part, fpart, img) {
        function title(t) { img.alt = t; img.title = t; }
        switch (fpart.partType) {
          case 'five':
            title('5-Fret');
            if (part.partName === 'bass') {
              img.src = 'images-dom/icon-bass.png';
            } else if (part.partName === 'keys') {
              img.src = 'images-dom/icon-keys.png';
            } else {
              img.src = 'images-dom/icon-guitar.png';
            }
            break;
          case 'six':
            img.src = 'images-dom/icon-ghl.png';
            title('6-Fret (GHL)');
            break;
          case 'drums':
            img.src = 'images-dom/icon-drums.png';
            title('(Pro) Drums');
            break;
          case 'prokeys':
            img.src = 'images-dom/icon-pro-keys.png';
            title('Pro Keys');
            break;
          case 'protar':
            if (part.partName === 'bass') {
              img.src = 'images-dom/icon-pro-bass.png';
              title('Pro Bass');
            } else {
              img.src = 'images-dom/icon-pro-guitar.png';
              title('Pro Guitar');
            }
            break;
          case 'vocal':
            img.src = 'images-dom/icon-vocal-3.png'; // TODO
            title('Vocals');
            break;
        }
      }
      menuMutable.parts.forEach(function(part){
        tag(menu, 'div', function(div){
          tag(div, 'h3', function(h3){
            h3.innerHTML = (part.partName === 'vocal' ? 'vocals' : part.partName);
          });
          tag(div, 'form', function(form){
            part.flexParts.forEach(function(fpart){
              tag(form, 'p', function(p){
                tag(p, 'img', function(img){
                  img.className = 'instrument-icon';
                  setIcon(part, fpart, img);
                });
                fpart.difficulties.forEach(function(diff){
                  tag(p, 'label', function(label){
                    tag(label, 'input', function(checkbox){
                      checkbox.type = 'checkbox';
                      checkbox.checked = diff.enabled;
                      checkbox.addEventListener('change', function(e){
                        diff.enabled = e.target.checked;
                        menuImmutable = JSON.parse(JSON.stringify(menuMutable));
                      });
                    });
                    label.insertAdjacentHTML('beforeend', ' ' + diff.diffName);
                  });
                });
              });
            });
          });
        });
      });
      tag(menu, 'p', function(p){
        p.className = 'onyx-link';
        p.innerHTML = '<a target="_blank" href="https://github.com/mtolly/onyxite-customs">Onyx Music Game Toolkit</a>';
      });
      menu.addEventListener('click', function(e){ e.stopPropagation(); });
      menu.addEventListener('touchstart', function(e){ e.stopPropagation(); });
    };
  };
};

exports.readMenu = function() {
  return menuImmutable;
};
