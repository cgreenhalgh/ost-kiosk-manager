# list of groups (=user) 

Group = require('models/group')

module.exports = class GroupList extends Backbone.Collection

  model: Group
