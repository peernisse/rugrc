#' get_lat_long
#' Return lattitude and longitude from an address and zipcode
#' Queries the Utah UGRC API to obtain lat/long
#' @description Handles authentication and connecting to UGRC API
#' to return lat/long from an address
#' @returns Named list of latitude and longitude in NAD83
#' @param address The street address to geocode
#' @param zipcode The zipcode associated with the `address`
#' @param apikey UGRC authenticaion key. System key used as default.
#'
#' @examples
#' get_lat_long(address = '3286 E Vera Cir', zipcode = 84121)
#'
#'
#' @export
get_lat_long <- function(address, zipcode, apikey = NULL) {
    stopifnot(inherits(address, 'character'))
    if(!inherits(zipcode, 'numeric')) zipcode <- as.numeric(zipcode)
    if(is.null(apikey)) apikey <- "UGRC-36B5E20E189882"


    address <- URLencode(address)
    wkid <- "26912" # spatial reference code for Utah (NAD83/UTM zone 12N)
    score <- "0" #Set to 0 for completeness but functional minimum of 60 by API
    suggest <- "0"
    locator <- "all" # options: "all","addressPoints","roadCenterlines"
    pobox <- "true" # true =
    referer <- "https://api-client.ugrc.utah.gov/"

    url <- paste("https://api.mapserv.utah.gov/api/v1/geocode/",address,"/",
        zipcode,"/","?spatialReference=",wkid,"&acceptScore=",score,
        "&suggest=",suggest,"&locators=",locator,"&pobox=",pobox,"&apiKey=",
        apikey, sep="",collapse=NULL)

    lat <- httr::content(httr::GET(url, httr::add_headers(referer = referer)))[['result']][['location']][['y']]
    long <- httr::content(httr::GET(url, httr::add_headers(referer = referer)))[['result']][['location']][['x']]

    return(c('lat' = lat, 'long' = long))

}




