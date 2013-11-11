GroupListView = require "views/grouplistview"
GroupList = require "models/grouplist"
CurrentUser = require "models/currentuser"
LoginSidebarView = require "views/loginsidebarview"
LoginFormView = require "views/loginformview"

class Router extends Backbone.Router
  routes: 
    "login": "login"
    "groups": "groups"

  login: -> 
    $('.main_section').hide()
    $('#section_login').show()

  groups: -> 
    $('.main_section').hide()
    $('#section_groups').show()


App =
  init: ->

    # backbonetest - based on 
    # http://adamjspooner.github.io/coffeescript-meet-backbonejs/05/docs/script.html
    Backbone.sync = (method, model, success, error) ->
      success()

    # in-app virtual pages
    router = new Router
    Backbone.history.start()
    window.router = router

    router.navigate("login", {trigger:true})

    window.currentuser = currentuser = new CurrentUser
    
    lsv = new LoginSidebarView {
      el: $('#login_sidebar')
      model: currentuser
    }
    lsv.render()

    new LoginFormView { el: $('#login_form'), model: currentuser }

    #list_view = new ListView
    grouplist = new GroupList
    new GroupListView collection: grouplist

module.exports = App
