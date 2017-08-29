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
  # readArticles = unique(readArticles)

  # add ID column to dataframe to have unique identifier for each article
  readArticles$ID <- seq.int(nrow(readArticles))
  
  # rename columns 
  colnames(readArticles)[colnames(readArticles)=="Source.title"] <- "sourcetitle"
  colnames(readArticles)[colnames(readArticles)=="Cited.by"] <- "cites"
  colnames(readArticles)[colnames(readArticles)=="Art..No."] <- "artNo"
  colnames(readArticles)[colnames(readArticles)=="Page.start"] <- "pagestart"
  colnames(readArticles)[colnames(readArticles)=="Page.end"] <- "pageend"
  colnames(readArticles)[colnames(readArticles)=="Page.count"] <- "pagecount"
  colnames(readArticles)[colnames(readArticles)=="Authors.with.affiliations"] <- "authaffil"
  colnames(readArticles)[colnames(readArticles)=="Author.Keywords"] <- "authorKeywords"
  colnames(readArticles)[colnames(readArticles)=="Index.Keywords"] <- "indexKeywords"
  colnames(readArticles)[colnames(readArticles)=="Document.Type"] <- "doctype"

  return(readArticles)
}
