# view of group in group list

module.exports = class GroupView extends Backbone.View

  tagName: 'div'
  className: 'section'

  initialize: ->
    @model.bind 'change', @render
    @model.bind 'remove', @unrender

  render: =>
    $(@el).html """
      <p class="title" data-section-title>#{@model.get 'gtitle'}</p>
      <div class="content" data-section-content>
        <p>Dummy group #{@model.get 'gid'} content</p>
        <button class="small button delete">delete</button>
      </div>
    """
    @

  unrender: =>
    $(@el).remove()

  remove: => @model.destroy()

  events:
      #'click .swap': 'swap'
      'click .delete': 'remove'
