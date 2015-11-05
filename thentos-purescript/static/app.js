window.state = {};

window.onload = function () {
    console.log('initializing some app...');

    PS['Main'].indicator("body")();

    var counterSync  = PS['Main'].counter("body")(function () {
        console.log('tyck!');
    });
    var counterAsync = PS['Main'].counter_("body")(function () {
        console.log('tack!');
    });

    counterSync();

    counterAsync(
        function (handle) {
            window.state['canceler'] = function() {
                handle.value0(function (x) {})(function (x) {}); return true;
            };

            window.state['driver'] = function (signal) {
                handle.value1(signal)(function (e) { }, function(e) { });
            };
        }
    );

    console.log('initialization of some app complete!');
};

var _tick = function(e) {
    // console.log(e);
    // e.stopPropagation();
    window.state.driver(PS['Main'].tick);
}

var _clear = function(e) {
    window.state.driver(PS['Main'].clear);
}
var _kill = function(e) {
    window.state.canceler();
}
