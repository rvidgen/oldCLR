createEdgeList <- function(authors){
edgesDF <- data.frame( "auth1" = character(), "auth2" = character(),"weight" = integer(), stringsAsFactors=FALSE)
numarticles = length(authors)

for (article in 1:numarticles) {
  numauthors = length(authors[[article]])
  numcoauths = numauthors - 1
  if (numcoauths > 0) { 
    for (author in 1:numcoauths) {  
      numdl = numauthors - author
      for (dl in 1:numdl) {  
        dlco = author + dl
        auth1 = authors[[article]][author]
        auth2 = authors[[article]][dlco]
        weight = 1
        edgesDF[nrow(edgesDF) + 1, ] <- c( auth1, auth2, weight)
      }
    }
  }
}

edgesDF$weight <- as.numeric(as.character(edgesDF$weight))
return(edgesDF)
}
