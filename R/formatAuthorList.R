# extract and separate authors and trim white-space then remove spaces in names
formatAuthorList <- function(authorVector){
auths = strsplit(authorVector, ".,", fixed=T)
auths = lapply(auths, str_trim)

fix_spaces <- function(x) {
  gsub(" ", "", x, fixed = TRUE)
}

fix_periods <- function(x) {
  gsub(".", "", x, fixed = TRUE)
}

fix_commas <- function(x) {
  gsub(",", "", x, fixed = TRUE)
}

auths = lapply(auths, fix_spaces)
auths = lapply(auths, fix_periods)
auths = lapply(auths, fix_commas)

return(auths)
}


