# mirage_server design/implementation notes

For version 1 the goal is to output:

- atom feed file containing selected items
- shorturls Json file with short urls for item helper pages
- cache versions of content files (e.g. SHA-1 hashes of content as name)

So a user has 

- an email address
- a password (or other means of authentication)
- a public name (title)
- some content sets...
- some website deployments...
- some device deployments...

`/user:EMAIL`

A content group (=> feed) has

- an internal name (ID)
- a title
- a version / subtitle
- a last modified date/time
- an URI
- some content items...

`/user/group:EMAIL,GROUPNAME`

A content item (=> entry) has

- a file url 
- a title
- a summary
- an icon URL
- a file mime type
- a browser visibility, e.g. hidden or not
- a (possibly empty) list of required device (types)
- a (possibly empty) list of mime types supported
- a (general) visibility, e.g. hidden or public
- a last modified date/time

`/user/group/item:USERNAME,GROUPID,ITEMID`
`/user/group/nextitemid:USERNAME,GROUPID`

A website deployment has

- a base url
- a set of groups (IDs)
- a set of short url mappings

A device deployment has

- a device id?
- a set of groups (IDs)

