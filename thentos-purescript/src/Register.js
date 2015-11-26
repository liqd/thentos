/* global exports */
"use strict";

// module Register
exports.eventInputValue = function(ev) {
    return { value: ev.target.value,
             validity: ev.target.validty }

};
