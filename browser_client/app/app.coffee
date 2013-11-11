GroupListView = require "views/grouplistview"
GroupList = require "models/grouplist"
CurrentUser = require "models/currentuser"
LoginSidebarView = require "views/loginsidebarview"
LoginFormView = require "views/loginformview"

class Router extends Backbone.Router
  routes: 
    "" : "groups"
    "login": "login"
    "groups": "groups"
    "group/:gid": "group"

  update_breadcrumbs: (bcs) ->
    bc = $ '.breadcrumbs' 
    bc.empty()
    for {title,path} in bcs 
      bc.append "<li><a href='#{path}'>#{title}</a></li>"

  login: -> 
    @update_breadcrumbs []
    $('.main_section').hide()
    window.currentuser.logout()
    $('#section_login').show()

  groups: -> 
    if not window.currentuser.get 'authenticated' 
      @navigate 'login',{trigger:true,replace:true}
    else
      @update_breadcrumbs [{title:'Groups',path:'groups'}]
      $('.main_section').hide()
      $('#section_groups').show()

  group: (gid) ->
    if not window.currentuser.get 'authenticated' 
      @navigate 'login',{trigger:true,replace:true}
    else
      $('.main_section').hide()
      $('#section_group').show()
      #alert "nav to group #{gid}"
      #@update_breadcrumbs [{title:'Groups',path:'groups'}]
    

App =
  init: ->

    # backbonetest - based on 
    # http://adamjspooner.github.io/coffeescript-meet-backbonejs/05/docs/script.html
    Backbone.sync = (method, model, success, error) ->
      success()

    window.currentuser = currentuser = new CurrentUser
    
    # in-app virtual pages
    router = new Router
    Backbone.history.start()
    window.router = router

    router.navigate("login", {trigger:true})

    lsv = new LoginSidebarView {
      el: $('#login_sidebar')
      model: currentuser
    }
    lsv.render()

    new LoginFormView { el: $('#login_form'), model: currentuser }

    #list_view = new ListView
    grouplist = new GroupList
    new GroupListView collection: grouplist

    # anchors
    $(document).on 'click','a', (ev) ->
      #alert "click"
      href = $(@).attr 'href'
      console.log "click #{href}"
      router.navigate(href,{trigger:true})
      ev.preventDefault()

module.exports = App
