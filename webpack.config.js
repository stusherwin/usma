'use strict';

const path = require('path');

const webpack = require('webpack');

const isWebpackDevServer = process.argv.filter(a => path.basename(a).indexOf('webpack-dev-server') >= 0).length;
const isWatch = process.argv.filter(a => a === '--watch').length

var plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

module.exports = {
    entry: "./client/src/index.tsx",

    output: {
        filename: "./dist/app.js",
        path: __dirname + "/dist"
    },

    devServer: {
      contentBase: './client/static',
      historyApiFallback: true,
      port: 5023,
      stats: 'errors-only',
      proxy: {
        '/api/**': 'http://localhost:8082/',
      }
    },

    devtool: "eval-source-map",

    resolve: {
        modules: [ 'node_modules', 'bower_components' ],
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    module: {
        rules: [
            { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
            { enforce: "pre", test: /\.js$/, loader: "source-map-loader" },
            { test: /\.(png|woff|woff2|eot|ttf|svg)$/, loader: 'url-loader?limit=100000' },
        ]
    },

    externals: {
        "react": "React",
        "react-dom": "ReactDOM"
    },

    plugins: [
      new webpack.LoaderOptionsPlugin({
        debug: true
      }),
      new webpack.DefinePlugin({
        'process.env.NODE_ENV': JSON.stringify('production')
      }),
      new webpack.optimize.UglifyJsPlugin()
    ].concat(plugins)
};