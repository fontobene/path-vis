'use strict';

require('./index.html');

const Elm = require('./Main.elm');
const elmDiv = document.getElementById('main');
const elmApp = Elm.Main.embed(elmDiv);
