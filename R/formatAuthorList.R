# extract and separate authors and trim white-space then remove spaces in names
# remove leading comma from Jr

formatAuthorList <- function(authorVector){

  fix_spaces <- function(x) {
    gsub(" ", "", x, fixed = TRUE)
  }
  
  fix_periods <- function(x) {
    gsub(".", "", x, fixed = TRUE)
  }
  
  fix_commas <- function(x) {
    gsub(",", "", x, fixed = TRUE)
  }

  auths = gsub(", Jr", " Jr", authorVector, fixed = TRUE)
  auths = strsplit(auths, ".,", fixed=T)
  auths = lapply(auths, str_trim)
  
  auths = lapply(auths, fix_spaces)
  auths = lapply(auths, fix_periods)
  auths = lapply(auths, fix_commas)

  return(auths)
}
