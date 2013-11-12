# coffee-script version of cache builder
https = require 'https'
fs = require 'fs'
xml2js = require 'xml2js'

if process.argv.length<3 
  console.log 'usage: coffee cb.coffee <KIOSK-ATOM-FILE>'
  process.exit -1

atomfn = process.argv[2]


parser = new xml2js.Parser()

# is file hidden
is_hidden = (entry) ->
  #console.dir entry
  for cs in entry.category ? [] when cs.$.scheme == 'visibility' and cs.$.term == 'hidden'
    return true
  return false

# read main atom file 
console.log 'read '+atomfn
data = fs.readFileSync atomfn, 'utf8'
console.log 'read '+data.length+' bytes'

# try reading shorturls.js
shorturls = []
shorturlsfn = 'shorturls.js'
try 
  shorturlsdata = fs.readFileSync shorturlsfn,'utf8'
  shorturls = JSON.parse shorturlsdata
catch e
  console.log 'could not read '+shorturlsfn+': '+e

# try reading cache.js
cache = {}
cachefn = 'cache.js'
try 
  cachedata = fs.readFileSync cachefn,'utf8'
  cache = JSON.parse cachedata
catch e
  console.log 'could not read '+cachefn+': '+e

# make one shorturl
add_shorturl = (shorturls,url) ->
  #console.log 'shorturl for '+url
  sus = for su in shorturls when su.url == url
    su.shorturl ? ''

  if sus.length == 0
    # work out shorturl later
    shorturls.push { url: url } 

get_baseurl = (feed) ->
  baseurl = for link in feed.link when link.$.rel == 'self'
    link.$.href
  if baseurl.length < 1
    console.log 'No self link found - cannot work out shorturls'
    process.exit -1
  baseurl = baseurl[0]
  ix = baseurl.lastIndexOf '/'
  baseurl = baseurl.slice 0,ix+1
  console.log 'Base URL = '+baseurl
  return baseurl

# shorturl for each 
make_shorturls = (feed,shorturls) ->
  console.log 'make_shorturls...'
  # TODO

  # atom url -> get url
  baseurl = get_baseurl feed
  geturl = baseurl+'get.html'

  # work out URLs to be shortened
  # get.html?u=URL&t=TITLE&a=HELPERURL...
  # each entry...
  for entry in feed.entry when not is_hidden entry
    title = entry.title[0]
    # each enclosure    
    for link in entry.link when link.$.rel == 'enclosure'
      fileurl = link.$.href
      mime = link.$.type

      url = geturl+'?u='+encodeURIComponent(fileurl)+'&t='+encodeURIComponent(title) 

      add_shorturl shorturls,url

      # each helper app
      for appentry in feed.entry 
        supports = for cat in appentry.category ? [] when cat.$.scheme == 'supports-mime-type' and cat.$.term == mime
          cat.$.label ? cat.$.term 
        if supports?.length > 0
          appurls = for link in appentry.link ? [] when link.$.rel == 'enclosure'
            link.$.href
          if appurls?.length > 0
            add_shorturl shorturls,url+'&a='+encodeURIComponent(appurls[0])


# parse file(s) and call worker(s)
parser.parseString data,(err,result) ->
  if err 
    console.log 'Error parsing '+atomfn+': '+err
    process.exit -1

  feed = result.feed
  console.log 'Feed '+feed.title+' ('+feed.id+')'

  make_shorturls feed,shorturls

  fix_shorturls = (shorturls,i) ->
    if i >= shorturls.length
      # done
      console.log 'write shorturls.js'
      fs.writeFileSync shorturlsfn,JSON.stringify shorturls
    else
      su = shorturls[i]
      if su.shorturl == undefined
        # see https://developers.google.com/url-shortener/v1/getting_started
        # POST application/json {"longUrl":"XXX"} ->
        #   https://www.googleapis.com/urlshortener/v1/url
        # { "kind": "urlshortener#url", "id": "http://goo.gl/XXXX",
        #  "longUrl": "XXX" }
        console.log 'shorten '+su.url
        req = {longUrl: su.url}
        reqs = JSON.stringify req
        options =
          hostname: 'www.googleapis.com'
          path: '/urlshortener/v1/url'
          method: 'POST'
          headers: { 'content-type': 'application/json' }

        hreq = https.request options, (res) ->
          if res.statusCode != 200
            console.log 'got shortener response '+res.statusCode
            process.exit -1

          res.setEncoding 'utf8'
          res.on 'data',(chunk) ->
            console.log 'shortener response: '+chunk
            jres = JSON.parse chunk
            su.shorturl = jres.id
            # recurse
            fix_shorturls shorturls,i+1

        hreq.on 'error',(e) ->
          console.log 'Error shortening url: '+e
          process.exit -1

        hreq.end reqs  
        console.log 'sent '+reqs
  
  fix_shorturls shorturls,0

  # download icons and visible enclosures, populating cache.json
  baseurl = get_baseurl feed
  cache.baseurl = baseurl

  icons = for entry in feed.entry 
    for link in entry.link when link.$.rel == 'alternate'
      link.$
  icons = [].concat icons...
  #console.dir icons
  # TODO

  console.log 'write cache.js'
  fs.writeFileSync cachefn,JSON.stringify cache


# TODO download icons and non-hidden files

#req = http.get "http://www.google.com/index.html", (res) ->
#  console.log "Got response: " + res.statusCode
#req.on 'error', (e) ->
#  console.log "Got error: " + e.message

