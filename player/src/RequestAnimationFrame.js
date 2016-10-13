"use strict";

// module RequestAnimationFrame

var requestAnimationFrame = null;

// http://www.paulirish.com/2011/requestanimationframe-for-smart-animating/
exports.requestAnimationFrame_ = function(window_) {
    return function(action) {

        if (!requestAnimationFrame) {
            requestAnimationFrame = (function() {
                return window_.requestAnimationFrame ||
                    window_.webkitRequestAnimationFrame ||
                    window_.mozRequestAnimationFrame ||
                    function(callback) {
                        window_.setTimeout(callback, 1000 / 60);
                    };
            })();
        }

        return function() {
            return requestAnimationFrame(action);
        };
    }
};
