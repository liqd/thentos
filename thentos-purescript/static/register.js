window.state = {};

window.onload = function () {
    console.log('initializing register widget...');
    var widgetRoot = document.getElementById("thentos-register");
    PS['Register'].mainEl(PS["Data.Maybe"].Nothing)(widgetRoot)();
    console.log('initialization register widget complete!');
};
