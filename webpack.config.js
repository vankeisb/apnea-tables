var path = require("path");

module.exports = {
    entry: {
        app: [
            './src/index.js'
        ]
    },

    output: {
        path: path.resolve(__dirname + "/src"),
        filename: '[name].js',
    },

    module: {
        loaders: [
            // {
            //     test: /\.html$/,
            //     exclude: /node_modules/,
            //     loader: 'file',
            // },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack',
            }
        ],

        noParse: /\.elm$/,
    },

    devServer: {
        inline: true,
        stats: {colors: true},
    },

};
