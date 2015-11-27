/* global exports */
"use strict";

// module Error
exports.throwJS = function(e) { console.log("*** " + e); throw e; };
exports.warnJS = function(e) { console.log(">>> " + e); return function(v) { return v; }; };
exports.stringify = function(o) { return JSON.stringify(o); };
