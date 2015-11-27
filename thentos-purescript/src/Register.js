/* global exports */
"use strict";

// module Register
exports.eventInputValue = function(ev) {
    return { value: ev.target.value,
             validity: marshal(ev.target.validity) };

};

// ev.target.validity is an rich object, not just a json dictionary.
// i think.  anyway, this function turns it into something that can be
// passed into purescript via the foreign function binding.
var marshal = function(v) {
    return {
        valueMissing:    v.valueMissing
      , typeMismatch:    v.typeMismatch
      , patternMismatch: v.patternMismatch
      , tooLong:         v.tooLong
      , rangeUnderflow:  v.rangeUnderflow
      , rangeOverflow:   v.rangeOverflow
      , stepMismatch:    v.stepMismatch
      , badInput:        v.badInput
      , customError:     v.customError
      , valid:           v.valid
    };
};
