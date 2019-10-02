"use strict";
/* eslint-env node */

var fs = require("fs");
var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var UglifyJSPlugin = require("uglifyjs-webpack-plugin");
var MiniCssExtractPlugin = require("mini-css-extract-plugin");
var AutoDllPlugin = require("autodll-webpack-plugin");


var IS_PROD = process.env.NODE_ENV === "production";
console.log(IS_PROD ? "Making PRODUCTION build" : "Making DEVELOPMENT build");  // eslint-disable-line no-console

var checkDeps = function () {
  var deps = [
    { path: "./bower_components", cmd: "bower i" },
    { path: "./output", cmd: "make build-purs" },
  ];
  var commands = [];
  for (var i = 0; i < deps.length; i++) {
    if (!fs.existsSync(deps[i].path)) {
      commands.push(deps[i].cmd);
    }
  }
  if (commands.length > 0) {
    console.error("Looks like you need to run `" + commands.join(" && ") + " ` first"); // eslint-disable-line no-console
    process.exit(1);
    return;
  }
};

var nodeEnvPlugin = new webpack.DefinePlugin({
  "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV),
});

var minifyJSPlugin = new UglifyJSPlugin({
  sourceMap: false,
  uglifyOptions: {
    parallel: true,
    sourceMap: false,
    compress: {
      warnings: false,
      comparisons: false, // don"t optimize comparisons https://github.com/mapbox/mapbox-gl-js/issues/4359
    },
    ecma: 6,
  }
});

var commonPlugins = [nodeEnvPlugin].concat(IS_PROD ? [ minifyJSPlugin ] : []);

module.exports = function(/*env*/) {
  checkDeps();
  return {
    mode: IS_PROD ? "production" : "development",
    entry: {
      "styles": "./frontend/src/styles.css",
      "index": "./frontend/src/index.js"
    },
    output: {
      path: path.join(__dirname, "frontend/dist"),
      filename: IS_PROD ? "[name].[hash].js": "[name].js",
    },
    devtool: IS_PROD ? false : "eval",
    devServer: {
      contentBase: path.join(__dirname, "frontend/dist"),
      disableHostCheck: true,
      // noInfo: true, // only errors & warns on hot reload
    },
    watch: false,
    module: {
      rules: [
        {
          test: /\.css$/,
          use: [
            IS_PROD ? MiniCssExtractPlugin.loader : "style-loader",
            "css-loader",
            {
              loader: "postcss-loader",
              options: {
                plugins: function () { return IS_PROD ? [require("autoprefixer"), require("cssnano")] : []; }
              }
            }
          ],
        }
      ]
    },
    plugins: commonPlugins.concat([
      new HtmlWebpackPlugin({
        inject: true,
        title: "FOAM Signal",
        chunks: ["index"],
        template: "./frontend/src/index.html",
        filename: "index.html",
      }),
      new webpack.DefinePlugin({
        "process.env.API_BASE_URL": JSON.stringify(process.env.API_BASE_URL),
        "process.env.GA_TRACKING_ID": JSON.stringify(process.env.GA_TRACKING_ID)
      }),
      new MiniCssExtractPlugin({
        filename: IS_PROD ? "[name].[hash].css": "[name].css",
        chunkFilename: IS_PROD ? "[id].[hash].css": "[id].css",
      }),
      new AutoDllPlugin({
        inject: true, // Will inject the DLL bundles into html files
        filename: IS_PROD ? "[name].[hash].js": "[name].js",
        debug: true,
        entry: {
          vendor:
            // It's supposed to be same as `Object.keys(require('./package.json').dependencies)` but instead if that,
            // we type all deps here, so it's more explicit and we don't accidentally include something undesired in bundle.
            [
              "react",
              "react-dom"
            ]
        },
        config: { mode: "none" }, // https://github.com/asfktz/autodll-webpack-plugin/issues/115
        plugins: commonPlugins,
      }),
    ])
  };

};

