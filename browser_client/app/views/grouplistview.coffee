# view of group list - test for now

Group = require('models/group')
GroupView = require('views/groupview')

module.exports = class GroupListView extends Backbone.View

  el: $ '#groups_container'

  initialize: ->
    @collection.bind 'add', @appendItem

    $('.add_group').on 'click',@addItem
    console.log "grouplist registered for add"

  addItem: =>
    console.log "grouplist addItem"
    @counter++
    item = new Group
    item.set gid: "gp#{@counter}" 
    @collection.add item
    $(@el).foundation('section','reflow')

  appendItem: (item) =>
    item_view = new GroupView model: item
    $(@el).append item_view.render().el

  #events: 'click button' : 'addItem'

