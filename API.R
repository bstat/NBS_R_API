################################################
## Next Big Sound API Module (R)
## API Documentation: http://api3.nextbigsound.com/
##
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##
## Built and maintained by team Anomaly www.weareanomaly.com
## Author: Byron Walker
## Created: 2014-01-09
## 
## Built as mirror of Buck Heroux's python module: https://github.com/buckheroux/NBS-API-Python
## Using S4 class structure.
##
################################################
library(RCurl)
library(httr)

################################################
## Resource
setClass("Resource"
         ,slots = list("key" = "character"
                      ,"secret" = "character"
                      , "ext" = "character"
                      , "base" = "character")
         ,prototype = list("key" = ""
                          ,"secret" = ""
                          ,"ext" = ""
                          ,"base" = ".api3.nextbigsound.com/")
         )

# HTTP GET
setGeneric("get", def = function(x, ...) {base::get(x,...)})

setMethod("get"
          ,"Resource"
          ,function(x, url = "", params = "")
            { 
              if(params == ""){
                q <- ""
              } else {
                q <- "?"
              }
              url <- paste(url, x@ext, q, URLencode(params), sep = "")
              #return(url)
              return(GET(url))
            }
          )

# HTTP POST
setGeneric("post", function(x, ...) standardGeneric("post"));

setMethod("post"
          ,"Resource"
          ,definition = function(x, url="", dat="", ...)
            {
              url <- paste(url, x@ext, sep = "")
              return(POST(url, body = dat, encode = 'form'))              
            }
          )
 
# Generates URL based on method/class called.
setGeneric("genUrl", function(x, ...){standardGeneric("genUrl")})

setMethod("genUrl"
          ,"Resource"
          ,definition = function(x)
            {
              # method <- "METHOD"
            
              # Evaluating the call stack to aquire method name; similar to the python methodology.
              # This implementation seems clunky in R atm; worth revisiting.
              # print(sys.calls())
              method <- deparse(sys.call(-9)[[1]])
              method <- gsub("(.*)\\(.*", replacement = "\\1" ,x = method) ## strip out method name
              
              return(tolower(paste("http://", x@key, x@base, class(x),"/", method, sep = "")))
            }
          )


################################################
## Artist
setClass("Artists",
         ,slots = list()
         ,prototype = list()
         ,contains = "Resource"
         )

##
setGeneric("view", function(x, ...){standardGeneric("view")})
setMethod("view"
          ,"Artists"
          ,function(x, id = "")
            {
              return(get(x, paste(genUrl(x), "/", id, sep = "")))
            }
          )

##
search <- function(x, ...) UseMethod("search")
search.default <- function(x,...){base::search()}
setGeneric("search")
setMethod("search"
          ,"Artists"
          ,function(x, query = "")
          {
            return(get(x, genUrl(x), sprintf("q=%s", query)))
          }
)

##
setGeneric("rank", function(x, ...){standardGeneric("rank")})
setMethod(f = "rank"
          ,"Artists"
          ,definition = function(x, type, ids)
          {
            uids <- paste(ids, sep = "-")
            return(get(paste(genUrl(x), "/", type, "/", uids , sep = "")))            
          }
)

##
setGeneric("add", function(x, ...){standardGeneric("add")})
setMethod(f = "add"
          ,"Artists"
          ,definition = function(x, name, profiles)
          {
            if(x@secret == ""){
              stop("A private key is needed.\n")
            } else {
              dat <-  list( 'data[name]' = name
                           ,'data[profiles]' = paste(profiles, sep = "&&")
                           ,'data[key]' = x@secret)
              return(post(x, genUrl(x), dat = dat))
            }            
          }
)

################################################
## Factory
setClass("ResourceFactory",
         ,slots = list("key" = "character"
                       ,"secret" = "character"
                       , "ext" = "character")
         ,prototype = list("key" = ""
                           ,"secret" = ""
                           ,"ext" = "")
)

##
setGeneric("setKey", function(x, ...){standardGeneric("setKey")})
setMethod(f = "setKey"
          ,"ResourceFactory"
          ,definition = function(x, key){ x@key <- key})

##
setGeneric("getKey", function(x, ...){standardGeneric("getKey")})
setMethod(f = "getKey"
          ,"ResourceFactory"
          ,definition = function(x){return(x@key)})
##
setGeneric("setSecret", function(x, ...){standardGeneric("setSecret")})
setMethod(f = "setSecret"
          ,"ResourceFactory"
          ,definition = function(x, secret){ x@secret <- secret })

##
setGeneric("getSecret", function(x, ...){standardGeneric("getSecret")})
setMethod(f = "getSecret"
          ,"ResourceFactory"
          ,definition = function(x){return(x@secret)})
##
setGeneric("setExt", function(x, ...){standardGeneric("setExt")})
setMethod(f = "setExt"
          ,"ResourceFactory"
          ,definition = function(x, ext){ x@ext <- ext })

##
setGeneric("getExt", function(x, ...){standardGeneric("getExt")})
setMethod(f = "getExt"
          ,"ResourceFactory"
          ,definition = function(x){return(x@ext)})

##
setGeneric("getArtist", function(x, ...){standardGeneric("getArtist")})
setMethod(f = "getArtist"
          ,"ResourceFactory"
          ,definition = function(x){
            return(new("Artist", key = x@key, secret = x@secret, ext = x@ext))
            }
          )

##
setGeneric("getGenres", function(x, ...){standardGeneric("getGenres")})
setMethod(f = "getGenres"
          ,"ResourceFactory"
          ,definition = function(x){
            return(new("Genres", key = x@key, secret = x@secret, ext = x@ext))
          }
)

##
setGeneric("getMetrics", function(x, ...){standardGeneric("getMetrics")})
setMethod(f = "getMetrics"
          ,"ResourceFactory"
          ,definition = function(x){
            return(new("Metrics", key = x@key, secret = x@secret, ext = x@ext))
          }
)

##
setGeneric("getProfiles", function(x, ...){standardGeneric("getProfiles")})
setMethod(f = "getProfiles"
          ,"ResourceFactory"
          ,definition = function(x){
            return(new("Profiles", key = x@key, secret = x@secret, ext = x@ext))
          }
)

##
setGeneric("getServices", function(x, ...){standardGeneric("getServices")})
setMethod(f = "getServices"
          ,"ResourceFactory"
          ,definition = function(x){
            return(new("Services", key = x@key, secret = x@secret, ext = x@ext))
          }
)

################################################
## Genres
setClass("Genres",
         ,slots = list()
         ,prototype = list()
         ,contains = "Resource"
)

setGeneric("artist", function(x, ...){standardGeneric("artist")})
setMethod(f = "artist"
          ,"Genres"
          ,definition = function(x, id = "")
          {
            return(get(paste(genUrl(x), "/", id, sep = "")))
          }
)

################################################
## Metrics
setClass("Metrics",
         ,slots = list()
         ,prototype = list()
         ,contains = "Resource"
)

##
setGeneric("profile", function(x, ...){standardGeneric("profile")})
setMethod("profile"
          ,"Metrics"
          ,definition = function(x, id = "", opt = "")
          {
            return(post(x, url = paste(genUrl(x),"/", id, sep = "") , dat = opt))
          }
)

##
setMethod("artist"
          ,"Metrics"
          ,definition = function(x, id = "", opt = "")
          {
            return(post(x, url = paste(genUrl(x),"/", id, sep = "") , dat = opt))
          }                      
)

################################################
## Profiles
setClass("Profiles",
         ,slots = list()
         ,prototype = list()
         ,contains = "Resource"
)

##
setMethod("artist"
          ,"Profiles"
          ,definition = function(x, id = "")
          {
            return(get(paste(genUrl(x), "/", id, sep = "")))
          }
)

##
setMethod("search"
          ,"Profiles"
          ,function(x, url = "")
          {
            return(get(x, genUrl(x), sprintf("u=%s", url)))
          }
)

##
setMethod(f = "add"
          ,"Profiles"
          ,definition = function(x, id, profiles)
          {
            if(x@secret == ""){
              stop("A private key is needed.\n")
            } else {
              dat <-  list(  'data[profiles]' = paste(profiles, sep = "&&")
                            ,'data[key]' = x@secret)
              return(post(x, paste(genUrl(x), "/", id, sep = ""), dat = dat))
            }
          }
)

################################################
## Services
setClass("Services",
         ,slots = list()
         ,prototype = list()
         ,contains = "Resource"
)

##
list <- function(x, ...) UseMethod("list")
list.default <- function(x,...){base::list()}
setGeneric("list")

setMethod("list"
          ,"Services"
          ,function(x)
          {
            url <- genUrl(x)
            return(get(x, substr(url, 1, nchar(url)-5)))
          }
)

