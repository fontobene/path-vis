require('./index.html');

function getHashParams() {
    return window.location.hash
        .substring(1)
        .split('&')
        .map((v) => v.split('='))
        .reduce((m, pair) => {
            m[pair[0]] = pair[1];
            return m;
        }, {});
}

// Inject elm app into div#main
const entrypoint = require('../elm/Main.elm');
const elmApp = entrypoint.Elm.Main.init({
    node: document.getElementById('main'),
    flags: getHashParams()['init'] || '',
});
