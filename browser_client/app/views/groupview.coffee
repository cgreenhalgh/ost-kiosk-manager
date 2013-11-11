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
        <p>Group #{@model.get 'gid'}</p>
        <ul class="button-group">
          <li><button class="small button do_view">View</button></li>
          <li><button class="small button do_delete">Delete</button></li>
        </ul>
      </div>
    """
    @

  unrender: =>
    $(@el).remove()

  remove: => @model.destroy()

  view: =>
    window.router.navigate "group/#{@model.get 'gid'}",{trigger:true}

  events:
      'click .do_delete': 'remove'
      'click .do_view': 'view'
