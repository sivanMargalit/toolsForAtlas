#' getStructuredData
#'
#' getting text data, in JSON format from given URL
#' (using httr:GET())
#'
#'
#' @param URL the url of the page to retrieve
#' @param ApiToken Token (or key). default in NULL, means without any token
#' @param timeout query time out. default is 120
#'
#' @return parsed data
#'
#' @import httr
#' @export
#'
getStructuredData<-function(URL,
                            ApiToken=NULL,
                            timeOut=120){
  if (is.null(ApiToken)){
    resp <- GET(URL,
                timeout(timeOut))
  }else{
    resp <- GET(URL,
                add_headers(Authorization = sprintf("ApiToken %s",ims.token)),
                timeout(timeOut))
  }
  if (http_type(resp) != "application/json") {
    stop("getStructuredData: API did not return json", call. = FALSE)
  }
  # view response status
  http_status(resp)

  parsed <- jsonlite::fromJSON(content(resp, "text"),
                               simplifyVector = FALSE)
  if (http_error(resp)){
    stop(sprintf("getStructuredData: %s", parsed$Message), call. = FALSE)
  }
  return(parsed)
}

