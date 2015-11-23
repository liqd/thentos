window.state = {};

window.onload = function () {
    console.log('initializing register widget...');
    PS['Register'].main("body")();
    console.log('initialization register widget complete!');
};
