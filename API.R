################################################
## Next Big Sound API Module (R)
## API Documentation: http://api3.nextbigsound.com/
##
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##
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
              #return(list(url=url, dat=dat))
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
          ,signature("Artists")
          ,function(x, query = "")
          {
            return(get(x, genUrl(x), sprintf("q=%s", query)))
            #return(genUrl(x))
          }
)
##
setGeneric("rank", function(x, ...){standardGeneric("rank")})

setMethod(f = "rank"
          ,signature(x = "Artists")
          ,definition = function(x, type, ids)
          {
            print("Not implemented.\n")
          }
)

##
setGeneric("add", function(x, ...){standardGeneric("add")})

setMethod(f = "add"
          ,signature(x = "Artists")
          ,definition = function(x)
          {
            print("Not implemented.\n")
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
          ,signature(x = "Genres")
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


