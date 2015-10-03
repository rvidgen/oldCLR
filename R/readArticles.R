#' Reads in and processes data for clr
#'
#' @param articleFiles - vector of one or more csv files containing article data
#' @param dataSource - source of files, currently "Scopus" is supported
#' @return returns a data frame of concatenated files with columns renamed as necessary
#' @author Richard Vidgen
#' @export
#' 

readArticles <- function(articleFiles, dataSource){
  # merge the input files
  numFiles = length(articleFiles)
  for (file in 1:numFiles) {  
    fileDF = read.csv(articleFiles[file], stringsAsFactors = FALSE)
    if (file == 1) {
      readArticles = fileDF
    } else {
      readArticles = rbind(readArticles, fileDF)
    }
  }

  # check for duplicate articles and remove
  readArticles = unique(readArticles)

  # add ID column to dataframe to have unique identifier for each article
  readArticles$ID <- seq.int(nrow(readArticles))
  
  # rename columns 
  readArticles = rename(readArticles, c("Source.title"="sourcetitle",
                      "Cited.by"="cites",
                      "Art..No."="artNo",
                      "Page.start"="pagestart",
                      "Page.end"="pageend",
                      "Page.count"="pagecount",
                      "Authors.with.affiliations"="authaffil",
                      "Author.Keywords"="keywords",
                      "Document.Type"="doctype"
                      ))
  
  return(readArticles)
}

