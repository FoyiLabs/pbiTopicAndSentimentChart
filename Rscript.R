# The following code to create a dataframe and remove duplicated rows is always executed and acts as a preamble for your script: 

# dataset <- data.frame(Review)
# dataset <- unique(dataset)

# Paste or type your script code here:

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Build functions
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#This function takes a text document as input and returns sentiment value as output. 
#Since there could be null or non-ascii characters that break the function, 
#the error handler will return a zero(0) instead.
getSentiment <- function(x)
{
  return(tryCatch(vader::get_vader(x, incl_nt = T, neu_set = T)[["compound"]], error=function(e) 0))
}

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Data pre-processing 
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
dataset$doc_id <- 1:nrow(dataset)
names(dataset) <- c("text", "doc_id")

#build corpus
corpus <- tm::Corpus(tm::DataframeSource(dataset))

#clean corpus
corpus <- tm::tm_map(corpus, tm::removePunctuation)
corpus <- tm::tm_map(corpus, tolower)
corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("english"))
corpus <- tm::tm_map(corpus, tm::stemDocument)

#build document term matrix
dtm <- tm::DocumentTermMatrix(corpus, control = list(stopwords = TRUE))

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ] 

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Topic modelling using Latent dirichlet analysis(LDA)
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#Number of topics
k <- 3

#Run LDA using Gibbs sampling
ldaOut <-topicmodels::LDA(dtm, k, method="Gibbs")
topicData <- data.frame(doc_id = ldaOut@documents, Topic =topicmodels::topics(ldaOut))
terms <- as.data.frame(topicmodels::terms(ldaOut,5))
termsList <- apply(terms, MARGIN = 2, FUN = function(x) paste0(x, sep = "+", collapse = ""))
termsDf <- data.frame(Topic = 1:length(termsList), Terms = termsList)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Sentiment analysis using VADER
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
dataset$sentimentScore <- lapply(X = dataset$text, function(x) getSentiment(x))
dataset$sentiment <- ifelse(dataset$sentimentScore <= -0.2, "Negative", ifelse(dataset$sentimentScore >= 0.2, "Positive", "Neutral"))
dataset <- merge(x = dataset, y = topicData, by = c("doc_id"), all.x = TRUE)

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Plots
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
sentimentAgg <- aggregate(dataset$doc_id, by = list(dataset$Topic,dataset$sentiment), FUN = length)
names(sentimentAgg) <- c("Topic", "Sentiment", "Reviews")
sentimentAgg <- sentimentAgg[order(-sentimentAgg$Reviews),]
sentimentAgg2 <- merge(x = sentimentAgg, y = termsDf, by = c("Topic"), all.x = TRUE)

plotOutput <- ggplot2::ggplot(data = sentimentAgg2, 
                              ggplot2::aes(x = Terms, y = Reviews, fill = Sentiment)) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::scale_fill_manual(values=c("#E69F00", "grey", "steelblue")) +
  ggplot2::coord_flip() +
  ggplot2::geom_text(ggplot2::aes(label=Reviews),color="white", face = "bold",position = ggplot2::position_stack(vjust = 0.5)) +
  ggplot2::labs(x = "Topic terms", y = "Number of reviews") +
  ggplot2::theme(axis.text = ggplot2::element_text(size=18),
                 axis.title = ggplot2::element_text(size=12,face="bold"))

#render plot onto PBI canvas
plotOutput
