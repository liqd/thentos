window.state = {};

window.onload = function () {
    console.log('initializing register widget...');
    PS['Register'].main("#thentos-register")();
    console.log('initialization register widget complete!');
};
