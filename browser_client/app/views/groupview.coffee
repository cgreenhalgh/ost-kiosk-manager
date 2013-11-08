# view of group - test for now

module.exports = class GroupView extends Backbone.View

  tagName: 'li'

  initialize: ->
    _.bindAll @,'render','unrender','remove'
    @model.bind 'change', @render
    @model.bind 'remove', @unrender

  render: ->
    $(@el).html """
      <span>#{@model.get 'gid'} #{@model.get 'gtitle'}!</span>
      <span class="delete">delete</span>
    """
    @

  unrender: =>
    $(@el).remove()

  remove: -> @model.destroy()

  events:
      #'click .swap': 'swap'
      'click .delete': 'remove'
