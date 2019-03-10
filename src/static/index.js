require('./index.html');

// Inject elm app into div#main
const entrypoint = require('../elm/Main.elm');
const elmApp = entrypoint.Elm.Main.init({
    node: document.getElementById('main'),
});
