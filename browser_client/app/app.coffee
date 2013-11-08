GroupListView = require("views/grouplistview")

App =
  init: ->

    # backbonetest - based on 
    # http://adamjspooner.github.io/coffeescript-meet-backbonejs/05/docs/script.html
    Backbone.sync = (method, model, success, error) ->
      success()

    #list_view = new ListView
    group_list_view = new GroupListView

module.exports = App
