# this R script uses the clr pckage to analyze Scopus downloads in three aspects:
#   IMPACT - citation analysis of authors, articles, journals
#   STRUCTURE - network analysis of co-authorships
#   CONTENT - topic modelling of abstracts
# follow the instruction below to run and see clr.pdf for full details
#

# if this is the first time running clr then download code from github
# install devtools package if not already done and get the clr package
#library(devtools)
#install_github("rvidgen/clr")
library(clr)

ls("package:clr")
lsf.str("package:clr")

# set the working directory to where your Scopus csv download files are stored
mainDir = "/Users/......./data"
# all the ouput analysis will go in this sub folder in the data directory - delete the output folder and contents before running
outputDir = "output"
# abstracts are written as txt file in this sub folder of output
absDir = "abstracts"

setwd(mainDir)
list.files()

# set the value of k here for the topic model
# recommended range 20 to 30 (try smaller and larger values to explore data)
k = 30

# specify words to remove from topic model here
# include the words of the search term used in Scopus to get clearer topic wordclouds
remove_words = c("abstract", "study", "research", "results", "paper", "available",
#                 "technology", "acceptance", "model", "tam", "user", "use",
#                 "information", "system", "systems",
                 "one", "two", "three", "four",
                 "findings", "analysis", "elsevier", "limited", "emerald",
                 "ieee", "ltd", "taylor", "francis", "igi", "springer", "verlag")

# get all the csv files, read in and concatenate, remove dupoicate records
files <- list.files(pattern = "\\.(csv|CSV)$") 
articleDF = readArticles(articleFiles = files, dataSource = "Scopus")

nrow(articleDF)
colnames(articleDF)
head(articleDF)

# create an output directory if it does not exist
if (file.exists(outputDir)){
  setwd(file.path(mainDir, outputDir))
} else {
  dir.create(file.path(mainDir, outputDir))
  setwd(file.path(mainDir, outputDir))
}

getwd()
list.files()

# for source title: remove spaces, punctuation, convert to lower case
articleDF$sourcetitleClean = tolower(articleDF$sourcetitle)
articleDF$sourcetitleClean = gsub("[[:punct:]]", "", articleDF$sourcetitleClean)
articleDF$sourcetitleClean = gsub(" ", "", articleDF$sourcetitleClean)
articleDF$sourcetitleShort = str_sub(articleDF$sourcetitleClean, start=1, end=15)

colnames(articleDF)
head(articleDF)
unique(articleDF$sourcetitle)
length(unique(articleDF$sourcetitle))
unique(articleDF$sourcetitleShort)

#
# IMPACT ANALYSIS
#
# graph number of papers by year and plot
cdata <- ddply(articleDF, c("Year"), summarise,
               countYear = length(Year))
cdata
pdf("pubsperyear.pdf")
plot(cdata$Year, cdata$countYear, type='l', xlab= 'year', ylab= 'no of papers', col='blue', lwd=2)
dev.off()


# look at most highly cited papers
articlecites = articleDF[order(articleDF$cites,decreasing=TRUE),]
head(articlecites)
head(articlecites$cites)
write.csv(articlecites, file='articleanalysis.csv', row.names=F)

# look at most common sources
sourcefreq = count(articleDF, "sourcetitle")
str(sourcefreq)
sourcefreq = sourcefreq[order(sourcefreq$freq,decreasing=TRUE),]
head(sourcefreq)

# calculate citations for each source
group_string <- "select articleDF.sourcetitle, sum(articleDF.cites) as SrcCites
from articleDF
group by articleDF.sourcetitle
order by SrcCites DESC"
sourcecites = sqldf(group_string)
head(sourcecites)

# join citations to number of papers
join_string <- "select sourcefreq.sourcetitle, sourcefreq.freq, sourcecites.SrcCites
from sourcefreq, sourcecites
where sourcefreq.sourcetitle = sourcecites.sourcetitle"
sourceDF = sqldf(join_string)
sourceDF$impact = sourceDF$SrcCites / sourceDF$freq
sourceDF = sourceDF[order(sourceDF$impact,decreasing=TRUE),]
head(sourceDF)
write.csv(sourceDF, file='sourceanalysis.csv', row.names=F)


# extract and separate authors and trim white-space then remove spaces in names
authors = formatAuthorList(authorVector = articleDF$Author)
head(authors)

# calculate citations for each author
# create a separate table of authors that can be joined to the article table
numarts = nrow(articleDF)
numarts

authdf <- data.frame(ID=integer(), auth=character(),
                     stringsAsFactors=FALSE)
str(authdf)

for (art in 1:numarts) {
  numauths = length(authors[[art]])
  for (auth in 1:numauths) {  
    newrow <- data.frame(ID=art, auth=authors[[art]][auth], stringsAsFactors=FALSE)
    authdf = rbind(authdf, newrow)
  }
}

head(authdf)

# create list of unique authors and paper count
group_string <- "select authdf.auth, count(authdf.auth) as authpapercount
from authdf
group by authdf.auth
order by authpapercount DESC"
authunique = sqldf(group_string)
head(authunique)
nrow(authunique)

# join articles and authors
join_string <- "select articleDF.*, authdf.auth
from articleDF
left join authdf
on articleDF.ID = authdf.ID
order by articleDF.ID"
artauthDF = sqldf(join_string)
head(artauthDF)
artauthDF[1:20,]
colnames(artauthDF)

group_string <- "select authunique.auth, sum(artauthDF.cites) as authcites
from authunique
left join artauthDF
on authunique.auth = artauthDF.auth
group by authunique.auth
order by authcites DESC"
authcit = sqldf(group_string)

# set NA for citation count to 0
authcit[is.na(authcit)] <- 0

head(authcit)
tail(authcit)
authcit[1:50,]
nrow(authcit)

# join author freq and cites
join_string <- "select authunique.auth, authunique.authpapercount, authcit.authcites 
from authunique, authcit
where authunique.auth = authcit.auth"
authorDF = sqldf(join_string)
head(authorDF)
authorDF$impact = authorDF$authcites / authorDF$authpapercount
authorDF = authorDF[order(authorDF$authcites,decreasing=TRUE),]
head(authorDF)

# calculate h index
numAuthors = nrow(authorDF)
hIndexVector <- c()
for (author in 1: numAuthors){
	authorID <- authorDF[author,1]
	authorArtDF <- subset(artauthDF, auth == authorID)
	authorArtDF <- authorArtDF[order(authorArtDF $cites,decreasing=TRUE),]
	hIndex <- calculateHIndex(authorArtDF)
	hIndexVector[author] <- hIndex
	}
authorDF$hIndex = hIndexVector
head(authorDF)

authorDF = authorDF[order(authorDF$hIndex,authorDF$authcites, decreasing=TRUE),]
write.csv(authorDF, file='authoranalysis.csv', row.names=F)

#
# STRUCTURE ANALYSIS
#
# create a co-author edgelist file
edgesDF <- createEdgeList(authors = authors)
str(edgesDF)
head(edgesDF)
tail(edgesDF)

# read edgelist file (vertices are already in authorDF)
g=graph.data.frame(edgesDF,directed=FALSE, vertices=authorDF)
g = simplify(g, remove.multiple = TRUE)
g

# remove isolates
g <- delete.vertices(g, V(g)[ degree(g)==0 ])
g

list.vertex.attributes(g)
list.edge.attributes(g)
E(g)
V(g)
E(g)$weight
V(g)$authpapercount

E(g)$label = E(g)$weight

# inspect and extract components
is.connected(g, mode="weak")
clusters(g, mode="weak")
no.clusters(g, mode="weak")
no.clusters(g, mode="strong")

cluster.distribution(g, cumulative = FALSE, mul.size = FALSE)

dg <- decompose.graph(g, max.comp=50)
str(dg)

comps = t(sapply(seq_along(dg), function(x) c(Subgraph = x, NodeCount = vcount(dg[[x]]), EdgeCount = ecount(dg[[x]]))))
comps


# create indexed table of components in descending order of size
cv = as.data.frame(sapply(dg, vcount))
cv$comp = rownames(cv)
cv = rename(cv, c("sapply(dg, vcount)" = "vcountcomp"))
cv = cv[order(cv$vcountcomp,decreasing=TRUE),] 
nrow(cv)
cv$ID <- seq.int(nrow(cv))
cv
main = as.numeric(cv$comp[1])
main

# write out degree for main component
sapply(dg, vertices)
sapply(dg, vcount)
deg = as.data.frame(degree(dg[[main]]))
names(deg)[names(deg) == 'degree(dg[[main]])'] <- 'degree'
str(deg)
head(deg)
write.csv(deg, file='degreeMain.csv', row.names=T)

# plot component size and save as pdf
pdf("compsize.pdf")
plot(cv$ID, cv$vcountcomp, xlab = "component", ylab = "size")
title(main="component size")
lines(cv$vcountcomp, type="o", pch=22, lty=1, col="blue")
dev.off()

# fit Loess curve if wanted
#lo <- loess(y~x)
#lines(predict(lo), col='red', lwd=2)


# find closeness scores for each component and write to file with auth citation count
for (compclose in 1:10) {
  compcl = closeness(dg[[as.numeric(cv$comp[cv$ID==compclose])]], normalized=T)
  compcl = as.data.frame(compcl)
  auths = rownames(compcl)
  compcl$author = auths
  compcl = compcl[order(compcl$compcl,decreasing=TRUE),] 
  # join closeness to citation count
  join_string <- "select compcl.*, authorDF.authcites
  from compcl
  left join authorDF
  on compcl.author = authorDF.auth
  order by compcl.compcl DESC"
  compcite = sqldf(join_string)
  compcite = compcite[c("author", "authcites", "compcl")]
  filename = paste("componentclose", compclose, ".csv", sep = "")
  write.csv(compcite, filename, row.names=F)
}

#create a graph in R viewer of complete network and main component

par(mfrow=c(1,1))
pdf("all_auths.pdf")
plot(g, layout=layout.auto, vertex.label=NA, edge.label=NA,
     main='Complete network', vertex.size=3,
     vertex.color="yellow", edge.color='lightblue')
dev.off()

pdf("main.pdf")
l = layout.kamada.kawai(dg[[main]])
plot(dg[[main]], layout=l, vertex.label=NA, edge.label=NA,
     main='Main component', vertex.size=3,
     vertex.color="yellow", edge.color='lightblue')
dev.off()


# create graphs in R viewer of top 10 components
par(mfrow=c(1,1))
pdf("auth_comps.pdf")
for (compout in 1:10) {
  plot(dg[[as.numeric(cv$comp[cv$ID==compout])]], layout=layout.kamada.kawai, vertex.label.color='darkblue',
       vertex.label.font=1, main=paste("Co-authorship component ", compout), edge.label.color='darkred',
       vertex.label.dist=.3, vertex.color="yellow", edge.color='lightblue')
}
dev.off()

# write a gml file for export to Gephi
write.graph(graph = g, file = 'coauth.gml', format = 'gml')

# write a DL file for input to UCINET
writeUCINET(outfile = 'dloutput.txt', authors = authors, nodecount = 3222)

#
# CONTENT ANALYSIS
#

# remove the non-text characters from abstracts
articleDF$Abstract <- gsub("[^[:alnum:]///' ]", " ", articleDF$Abstract)
head(articleDF$Abstract)

# create an abstract directory for the txt files if it does not exist
if (file.exists(absDir)){
  setwd(file.path(mainDir, outputDir, absDir))
} else {
  dir.create(file.path(mainDir, outputDir, absDir))
  setwd(file.path(mainDir, outputDir, absDir))
}

getwd()

abstracts = nrow(articleDF)
abstracts

# write each abstract as a text file
# loop to write the txt files
for (abstract in 1:abstracts) {
  absText = articleDF[abstract, 16]
  filename = paste("abstract", abstract, ".txt", sep = "")
  writeLines(absText, con = filename)
}

# back to output dir
setwd(file.path(mainDir, outputDir))


# read in text files and create corpus
corpus <-Corpus(DirSource(absDir))
corpus
corpus[[1]]

# clean up the corpus
myStopwords <- c(stopwords("english"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, myStopwords) 
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, stemDocument)

corpus <- tm_map(corpus, removeWords, remove_words)

corpus[[1]]

# create DTM
dtm <- DocumentTermMatrix(corpus)
inspect(dtm[1:5, 1:10])
findFreqTerms(dtm, 200)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm <- dtm[rowTotals> 0, ]           #remove all docs without words

burnin = 1000
iter = 1000
keep = 50

model <- LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep))
model

# this will not work if docs removed from dtm due to zero words
json = topicmodels_json_ldavis (model, corpus, dtm)
serVis(json, out.dir = 'ldavis', open.browser = FALSE) 

# get 10 most important words for each topic
terms(model, 10)


#par(oma=c(2.5,2.5,3.5,1.5))
#layout(matrix(1:6,2,3))
#lapply(1:k, function(x) { plot_wordcloud(model, dtm, x, 30); title(paste("Topic ",x), cex=1.5)})
#title("Word Clouds for 30 Most Frequent Terms using LDA_GIB", outer=T, cex = 1.5)


par(mfrow=c(1,1))

pdf("topic_cloud.pdf")
for (i in 1:k){
  plotWordCloud(model, dtm, i, 30)
  title(paste("Topic ", i))
}
dev.off()


# create a dataframe of posteriors
topicdf = posterior(model)
topicdf = topicdf$topics
topicdf = data.frame(topicdf)


# add the top topic for each abstract
topicdf$topTopic = topics(model,1)

# add an index, extract numeric part, sort the dataframe to same order as abstracts
topicdf$index <- row.names(topicdf)
pat <- "(\\d)+"
topicdf$ID = as.numeric(str_extract(topicdf$index, pat))
topicdf = topicdf[order(topicdf$ID), ]
head(topicdf)
tail(topicdf)


# add the top topic to each abstract
# sort the abstracts by topic and then by citation and write out file
topiccite <- merge(articleDF, topicdf,by="ID")
topiccite$cites[is.na(topiccite$cites)] <- 0
topiccite = topiccite[order(topiccite$topTopic, topiccite$cites, decreasing=T), ]
write.csv(topiccite, file='topiccites.csv', row.names=F)

# count the number of papers per topic
topic_string <- "select topiccite.topTopic as topic, count(topiccite.topTopic) as noPapers
from topiccite
group by topiccite.topTopic
order by topic"
topicpapers = sqldf(topic_string)
topicpapers
write.csv(topicpapers, file='topicpapers.csv', row.names=F)
