## These functions pull models from the ddmore repository
.baseUrl = "http://repository.ddmore.eu"

#' Set the base URL to use as a repository
#' By default, we use the ddmore public repository
#' 
#' @param url The base url, e.g. http://repository.ddmore.eu
#' @export
setBaseUrl <- function(url) {
    .baseUrl <<- url
}

#' Fetch a list of models.
#' 
#' @param query Specify a search query. Can be omitted
#' @param offset Return results with an offset
#' @param all Should we make multiple queries to retrieve all results?
#' 
#' @details
#' This method can only return 10 results at a time.
#' This maxResults configured by the user preferences, rather than a query parameter.
#' See https://bitbucket.org/jummp/jummp/src/master/jummp-plugins/jummp-plugin-web-application/grails-app/controllers/net/biomodels/jummp/webapp/SearchController.groovy for more information.
#' 
#' @export
getSearchResult <- function(query, offset=0, all=FALSE) {
    url <- httr::parse_url(.baseUrl)
    url$query$format <- "json"
    url$query$offset <- offset
    if(missing(query)) {
        url$path <- paste0(url$path, "/models")
    } else {
        url$path <- paste0(url$path, "/search")
        url$query$query <- query
    }
    url <- httr::build_url(url)
    jsonString <- readLines(url, warn = F)
    searchResult <- rjson::fromJSON(jsonString)
    searchResult$url <- url
    class(searchResult) <- "ddmoreSearch"
    
    if(all) searchResult <- downloadAll(searchResult)
    return(searchResult)
}

downloadAll <- function(first) {
    models <- first$models
    url <- httr::parse_url(first$url)
    
    max <- first$modelsAvailable
    if(endsWith(url$path, "search"))
        max <- first$matches
    M <- first$queryParameters$maxResults
    
    pb <- dplyr::progress_estimated(n=floor( max / M), min_time=3)
    for(i in seq(M, max, by=M)) {
        url$query$offset <- i
        jsonString <- readLines(httr::build_url(url), warn=F)
        result <- rjson::fromJSON(jsonString)
        models <- c(models, result$models)
        pb$tick()$print()
    }
    
    first$models <- models
    first
}

#' Retrieve a model description based on an identifier
#' 
#' @param id model identifier
#' 
#' @export
getModel <- function(id) {
    url <- httr::parse_url(.baseUrl)
    url$path <- paste0(url$path, "/model/", id)
    url$query$format <- "json"
    url <- httr::build_url(url)
    
    jsonString <- readLines(url, warn = F)
    model <- rjson::fromJSON(jsonString)
    class(model) <- "ddmoreModel"
    model$url <- url
    return(model)
}

#' Print a ddmore search result in a nice format
#' @inheritParams base::print
#' @export
print.ddmoreSearch <- function(x, ...) {
    cat("URL: ", x$url, "\n")
    print( as.data.frame(x) )
}

#' Represent a search result as a data.frame
#' @inheritParams base::as.data.frame
#' @export
as.data.frame.ddmoreSearch <- function(x, row.names=NULL, optional=FALSE, ...) {
    dplyr::bind_rows(x$models)
}

#' Save the associated files of a model to disk
#' @param model either the model identifier, or the downloaded model using `getModel`
#' @param destination destination directory
#' 
#' @export
saveFiles <- function(model, destination) {
    if(missing(destination)) stop("Please specify a destination directory")
    dir.create(destination)
    
    if(is.character(model)) model <- getModel(model)
    
    stopifnot(inherits(model, "ddmoreModel"))
    url <- httr::parse_url(model$url)
    
    files <- c(model$files$main, model$files$additional)
    pb <- dplyr::progress_estimated(n=length(files))
    for(i in files) {
        file <- i$name
        url$path <- paste0("/model/download/", model$identifier)
        url$query <- base::list(filename=file)
        #Example URL: http://repository.ddmore.foundation/model/download/DDMODEL00000273.8?filename=Command.txt
        httr::GET(httr::build_url(url), httr::write_disk(file.path(destination, file), overwrite=TRUE))
        pb$tick()$print()
    }
}
