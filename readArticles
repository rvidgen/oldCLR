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
  infile1 <- read.csv(articleFiles[1], stringsAsFactors = FALSE)
  infile2 <- read.csv(articleFiles[2], stringsAsFactors = FALSE)
  articleFiles = rbind(infile1, infile2)

  # check for duplicate articles and remove
  articleFiles = unique(articleFiles)

  # add ID column to dataframe to have unique identifier for each article
  articleFiles$ID <- seq.int(nrow(articleFiles))
  
  # rename columns 
  articleFiles = rename(articleFiles, c("Source.title"="sourcetitle",
                      "Cited.by"="cites",
                      "Art..No."="artNo",
                      "Page.start"="pagestart",
                      "Page.end"="pageend",
                      "Page.count"="pagecount",
                      "Authors.with.affiliations"="authaffil",
                      "Author.Keywords"="keywords",
                      "Document.Type"="doctype"
                      ))
  
  return(articleFiles)
}

