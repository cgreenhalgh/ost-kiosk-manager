# view of group list - test for now

Group = require('models/group')
GroupView = require('views/groupview')
GroupList = require('models/grouplist')

module.exports = class GroupListView extends Backbone.View

  el: $ 'body'

  initialize: ->
    _.bindAll @,'render','addItem','appendItem'

    @collection = new GroupList
    @collection.bind 'add', @appendItem

    @counter = 0
    @render()

  render: ->
    $(@el).append '<button>Add Group</button>'
    $(@el).append '<ul></ul>'

  addItem: ->
    @counter++
    item = new Group
    item.set gid: "gp#{@counter}" 
    @collection.add item

  appendItem: (item) ->
    item_view = new GroupView model: item
    $('ul').append item_view.render().el

  events: 'click button' : 'addItem'

