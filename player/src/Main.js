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

exports.openMenu = function() {
  document.getElementById('the-side-menu').classList.add('menu-open');
};

exports.closeMenu = function() {
  document.getElementById('the-side-menu').classList.remove('menu-open');
};

var menuImmutable = null;
var menuMutable = null;

exports.fillMenu = function(obj) {
  return function() {
    menuMutable = obj;
    menuImmutable = JSON.parse(JSON.stringify(menuMutable));
    function tag(p,t,f){
      var element = document.createElement(t);
      if (f) f(element);
      p.appendChild(element);
    }
    var menu = document.getElementById('the-side-menu');
    menuMutable.parts.forEach(function(part){
      tag(menu, 'div', function(div){
        tag(div, 'h2', function(h2){
          h2.innerHTML = part.partName;
        });
        tag(div, 'form', function(form){
          part.flexParts.forEach(function(fpart){
            tag(form, 'label', function(label){
              tag(label, 'p', function(p){
                tag(p, 'img', function(img){
                  img.className = 'instrument-icon';
                  switch (fpart.partType) {
                    case 'five':
                      if      (part.partName === 'bass') img.src = 'images-dom/icon-bass.png';
                      else if (part.partName === 'keys') img.src = 'images-dom/icon-keys.png';
                      else                               img.src = 'images-dom/icon-guitar.png';
                      break;
                    case 'six':
                      img.src = 'images-dom/icon-ghl.png';
                      break;
                    case 'drums':
                      img.src = 'images-dom/icon-drums.png';
                      break;
                    case 'prokeys':
                      img.src = 'images-dom/icon-pro-keys.png';
                      break;
                    case 'protar':
                      if      (part.partName === 'bass') img.src = 'images-dom/icon-pro-bass.png';
                      else                               img.src = 'images-dom/icon-pro-guitar.png';
                      break;
                    case 'vocal':
                      img.src = 'images-dom/icon-vocal-3.png'; // TODO
                      break;
                  }
                });
                p.insertAdjacentHTML('beforeend', ' ');
                tag(p, 'input', function(checkbox){
                  checkbox.type = 'checkbox';
                  checkbox.checked = fpart.enabled;
                  checkbox.addEventListener('change', function(e){
                    fpart.enabled = e.target.checked;
                    menuImmutable = JSON.parse(JSON.stringify(menuMutable));
                  });
                });
              });
            });
          });
        });
      });
    });
    menu.addEventListener('click', function(e){ e.stopPropagation(); });
  };
};

exports.readMenu = function() {
  return menuImmutable;
};
