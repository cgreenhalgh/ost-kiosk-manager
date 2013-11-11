# view of group list - test for now

Group = require('models/group')
GroupView = require('views/groupview')

module.exports = class GroupListView extends Backbone.View

  el: $ '#groups_container'

  counter: 0

  initialize: ->
    @collection.bind 'add', @appendItem
    
    $('.do_add_group').on 'click',@addItem

  addItem: =>
    console.log "grouplist addItem"
    @counter++
    item = new Group
    gid = "gp#{@counter}"
    item.set gid: gid
    @collection.add item
    $(@el).foundation('section','reflow')

    window.router.navigate "group/#{gid}",{trigger:true}

  appendItem: (item) =>
    item_view = new GroupView model: item
    $(@el).append item_view.render().el

  #events: 'click .do_add_group' : 'addItem'

