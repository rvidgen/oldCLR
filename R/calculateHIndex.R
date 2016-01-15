# calculation of h-index
calculateHIndex <- function (authorArtDF){
	numOfArticles = nrow(authorArtDF)
	hIndex <- 0
	for (articleCounter in 1: numOfArticles){
		currentArticle <- authorArtDF[articleCounter,]
		currentCites <- currentArticle$cites
		if (!is.na(currentCites) && (articleCounter <= currentCites)){
			hIndex <- articleCounter	
		} 
	}
	return(hIndex)
}
