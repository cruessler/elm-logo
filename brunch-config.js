module.exports = {
  config: {
    paths: {
      watched: ["app"]
    },

    files: {
      javascripts: {
        joinTo: "js/app.js"
      },
      stylesheets: {
        joinTo: "css/app.css"
      }
    },

    // https://elixirforum.com/t/javascript-errors-stop-brunch-watcher-full-restart-required/7584/4
    notifications: false
  }
};
