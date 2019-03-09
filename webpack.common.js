// Base
const path = require("path");
const webpack = require('webpack')

// Plugins
const CopyWebpackPlugin = require('copy-webpack-plugin');

// Webpack config
module.exports = {
    entry: {
        app: ['./src/static/index.js']
    },
    output: {
        path: path.resolve(__dirname + '/dist'),
        filename: '[name].js'
    },
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]',
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader?verbose=true',
            }
        ],

        noParse: /\.elm$/,
    },
    plugins: [
        new CopyWebpackPlugin([ { from: 'static', to: 'static' } ])
    ],
    target: 'web',
    devServer: {
        inline: true,
        stats: {
            colors: true,
            hash: false,
            modules: false,
        },
    },
};
