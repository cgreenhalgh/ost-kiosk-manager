# coffee-script placebook2html convertor


eco = require "eco"
fs = require 'fs'

console.log 'reading templates...'
htmlTemplate = fs.readFileSync __dirname + "/templates/html.eco", "utf-8"

if process.argv.length<3
  console.log 'usage: coffee placebook2html.coffee <PLACEBOOK-DATA.JSON-FILE>'
  process.exit -1

placebookfn = process.argv[2]

# read placebook file 
console.log 'read '+placebookfn
sdata = fs.readFileSync placebookfn, 'utf8'
console.log 'read '+sdata.length+' bytes'
try 
  jdata = JSON.parse sdata
catch err
  console.log "Error parsing #{placebookfn} as JSON: #{err.message}"
  process.exit -1 

id = jdata.id
metadata = jdata.metadata
pages = jdata.pages

if not id? or not metadata? or not pages?
  console.log "Error: #{placebookfn} doesn't look like a placebook JSON file (missing id, metadata or pages)"
  process.exit -1

console.log "Processing placebook #{id}: #{metadata.title}"

# reorganise/sort pages/columns and page content
# find maps indexed by page...
maps = {}
columns = [] 

for page,pix in pages
  for item in page.items when item.type=='MapImageItem'
    # e.g. "geom":"POLYGON ((52.95029275324379 -1.18927001953125, 52.95525697845466 -1.18927001953125, 52.95525697845466 -1.18377685546875, 52.95029275324379 -1.18377685546875, 52.95029275324379 -1.18927001953125))"
    polygonPattern = new RegExp '^POLYGON \\(\\([0-9. ,-]*\\)\\)$'
    lat0 = lat1 = 0
    lon0 = lon1 = 0
    if polygonPattern.test item.geom
      # console.log 'map POLYGON' 
      ps = item.geom.substring( 'POLYGON (('.length, item.geom.length-2 ).split(',')
      cs = for p in ps
        ss = p.trim().split ' '
        #console.log "#{p} -> coord #{new Number ss[0]},#{new Number ss[1]}"
        { lat: (new Number ss[0]), lon: (new Number ss[1]) }
      lat0 = lat1 = cs[0].lat
      lon0 = lon1 = cs[0].lon
      for c in cs
        lat0 = Math.min lat0,c.lat
        lat1 = Math.max lat1,c.lat
        lon0 = Math.min lon0,c.lon
        lon1 = Math.max lon1,c.lon
    map = 
      id: item.id
      data: item
      items: []
      lat0: lat0
      lat1: lat1
      lon0: lon0
      lon1: lon1
    maps[pix] = map
    item.map = map
    console.log "Found map #{item.id} on page #{pix} lat #{lat0}:#{lat1} lon #{lon0}:#{lon1}"
for page,pix in pages
  #console.log "found page #{page.id}"
  for item in page.items
    #console.log "found item #{item.id} #{item.type}"
    cix = pix*2+item.parameters.column
    if not columns[cix]
      columns[cix] = []
    order = item.parameters.order
    columns[cix][order] = item
    # map item // parameters:{"marker":49,"markerShow":1,"mapPage":0,...}, "geom":"POINT (52.953654873938675 -1.1884485483169311)"
    if item.geom? && item.parameters.mapPage?
      map = maps[item.parameters.mapPage]
      if map?
        console.log "Found marker #{item.parameters.marker} on map #{map.id}"
        map.items.push item
        # TODO 
        item.top = 0
        item.left = 0
        pointPattern = new RegExp '^POINT \\([0-9. -]*\\)$'
        if pointPattern.test item.geom       
          ss = item.geom.substring('POINT ('.length, item.geom.length-1).split(' ')
          console.log "Found point #{ss}"
          lat = new Number(ss[0])
          lon = new Number(ss[1])
          item.top = (map.lat1-lat)/(map.lat1-map.lat0)
          item.left = (lon-map.lon0)/(map.lon1-map.lon0)

# TODO
manifestfn = 'placebook.manifest'

data = 
  title: metadata.title
  manifestfn: manifestfn
  columns: columns
  maps: maps

try
  html = eco.render htmlTemplate, data
catch err
  console.log "Templating error: #{err}"
  process.exit -2

outfn = 'placebook.html'
console.log "writing #{outfn}"
fs.writeFileSync outfn, html

console.log "Writing #{manifestfn}"
manifest = 'CACHE MANIFEST\n'+
  outfn+'\n'

fs.writeFileSync manifestfn, manifest

process.exit 0

