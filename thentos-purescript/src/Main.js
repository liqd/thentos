/* global exports */
"use strict";

// module Main
exports.publish = function(moduleName) {
    // (this can most likely be implemented with DOM.HTML.window in purs.)
    return function(valueName) {
        return function(value) {
            return function __do() {
                if (!window.PS) {
                    window.PS = {};
                }
                if (!window.PS[moduleName]) {
                    window.PS[moduleName] = {};
                }
                window.PS[moduleName][valueName] = value;
                console.log("registered: PS['" + moduleName + "]." + valueName);
            }
        }
    }
}
