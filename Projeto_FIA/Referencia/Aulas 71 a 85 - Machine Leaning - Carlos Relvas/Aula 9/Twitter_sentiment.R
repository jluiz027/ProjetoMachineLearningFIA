install.packages('twitteR', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('plyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('stringr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('RTextTools', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('e1071', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('party', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(RTextTools)
library(party)


consumerKey <- "user--consumer--key"
consumerSecret <- "user--consumer--Secret"
accessToken <- "access--token--key"
accessTokenSecret <- "acess--token--Secret"

setup_twitter_oauth(consumerKey, consumerSecret, 
                    access_token = accessToken, 
                    access_secret = accessTokenSecret)

list1 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-04', until ='2016-10-05')
list2 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-05', until ='2016-10-06')
list3 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-06', until ='2016-10-07')
list4 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-07', until ='2016-10-08')
list5 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-08', until ='2016-10-09')
list6 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-09', until ='2016-10-10')
list7 <- searchTwitter('Samsung',n=400, lang="en", 
                       since='2016-10-10', until ='2016-10-11')
list8 <- searchTwitter('Samsung',n=400, lang="en")

list <- c(list1, list2, list3, list4, list5, list6, list7, list8)

df <- twListToDF(list)
df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')

write.csv(df, "/Users/carlos/Desktop/twitter_samsung.csv", row.names = F)

df <- read.csv("twitter_samsung.csv", header=T, stringsAsFactors = F)

score.sentiment <- function(sentences, pos.words, neg.words){
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- str_replace_all(sentence, "[^[:alnum:]]", " ")
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('positive_words.txt', what='character', comment.char=';') 
neg <- scan('negative_words.txt', what='character', comment.char=';')
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
Dataset <- df
Dataset$text <- as.factor(Dataset$text)
scores <- score.sentiment(Dataset$text, pos.words, neg.words)

stat <- scores
stat$created <- df$created
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', 
                                  ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())

ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  ggtitle('Samsung')


id_notneutral <- stat[,4] %in% c('positive', 'negative')
data_model <- stat[id_notneutral,]

sentence <- data_model[,2]
sentence <- gsub('[[:punct:]]', "", sentence)
sentence <- gsub('[[:cntrl:]]', "", sentence)
sentence <- gsub('\\d+', "", sentence)
sentence <- str_replace_all(sentence, "[^[:alnum:]]", " ")
sentence <- tolower(sentence)

set.seed(42)
id_train <- sample(1:nrow(data_model), 0.8*nrow(data_model), replace = F)

mat= create_matrix(sentence, language="english", 
                   removeStopwords=TRUE, removeNumbers=TRUE, 
                   stemWords=FALSE)

help(create_matrix)

mat = as.matrix(mat)

classifier = naiveBayes(mat[id_train,], as.factor(data_model[id_train,4]))
predicted = predict(classifier, mat)

table(data_model[id_train,4], predicted[id_train])
table(data_model[-id_train,4], predicted[-id_train])



dados <- data.frame(cbind(data_model[,4], as.matrix(mat)))
names(dados)[1] <- c('Sentimento')

fit3 <- ctree(as.factor(Sentimento) ~ ., data=dados[id_train,], 
             controls = ctree_control(maxdepth = 3))
fit5 <- ctree(as.factor(Sentimento) ~ ., data=dados[id_train,], 
             controls = ctree_control(maxdepth = 5))
fit10 <- ctree(as.factor(Sentimento) ~ ., data=dados[id_train,], 
             controls = ctree_control(maxdepth = 10))
fit <- ctree(as.factor(Sentimento) ~ ., data=dados[id_train,])

predicted3 = predict(fit3, newdata=dados)
predicted5 = predict(fit5, newdata=dados)
predicted10 = predict(fit10, newdata=dados)
predicted = predict(fit, newdata=dados)

tab3 <- table(dados[-id_train,1], predicted3[-id_train])
(tab3[1,1]+tab3[2,2])/sum(tab3)
tab5 <- table(dados[-id_train,1], predicted5[-id_train])
(tab5[1,1]+tab5[2,2])/sum(tab5)
tab10 <- table(dados[-id_train,1], predicted10[-id_train])
(tab10[1,1]+tab10[2,2])/sum(tab10)
tab <- table(dados[-id_train,1], predicted[-id_train])
(tab[1,1]+tab[2,2])/sum(tab)


