############ first we need to load some packages that will be used

library(rtweet)
library(dplyr)


## let's to get the twitter data
tweet_df <- search_tweets("Pantanal", n = 2000,
                          include_rts = FALSE,lang="en")



#### let's to do a clean in the tweets text


tweet_df $text <- gsub("#([a-z|A-Z|0-9|_])*","", tweet_df$text) # remove hashtags
tweet_df $text <- gsub('@([a-z|A-Z|0-9|_])*', '', tweet_df$text) # remove palavras com @ (menções)
tweet_df $text <- gsub('https://','', tweet_df$text) # removes https://
tweet_df$text <- gsub('http://','', tweet_df$text) # removes http://
tweet_df$text <- gsub('[^[:graph:]]', ' ', tweet_df$text) # removes graphic characters like emoticons 
tweet_df$text <- gsub('[[:punct:]]', '', tweet_df$text) # removes punctuation 
tweet_df$text <- gsub('[[:cntrl:]]', '', tweet_df$text) # removes control characters
tweet_df$text <- gsub("\\w*[0-9]+\\w*\\s*", "", tweet_df$text) # removes numbers
tweet_df$text <- gsub('_', ' ', tweet_df$text) # replace underline by space
tweet_df$text = gsub('[ \t]{2,}', ' ', tweet_df$text) ## remove unnecessarie spaces
tweet_df$text = gsub('^\\s+|\\s+$', ' ', tweet_df$text)   ## remove unnecessarie spaces
tweet_df$text <- tolower(tweet_df$text) # minor case
tweet_df$text  = gsub('\\n', ' ', tweet_df$text ) ### remove rows breaks
tweet_df <- tweet_df[!duplicated(tweet_df$text),]  ### remove duplicated tweets


##### Let's to add a id to identify tweets
tweet_df$tweet_id<-seq(from=1,length(tweet_df$user_id))

### one option is to join alll data in a unique text
full_text<-tibble(line=1:length(tweet_df$text),text=tweet_df$text)
full_text

# but in this case i will not use it

### now I will combine all rows in just one great text
library(tidyr)

### tokenization (break text into rows with just one word)
library(tidytext)

text_df<-tweet_df[,c(91,5)] %>%  unnest_tokens(word, text)

### remove words not useful
data(stop_words)  ### a df with classification of words

#### antijoin to remove this words
tidy_text <- text_df %>%
  anti_join(stop_words)


library(ggplot2)
### to find the most common words in the tweets
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_bw()+
  scale_y_continuous(name="Number of citations")+
  theme(axis.text.y= element_text(colour="black",size = 20))+
  theme(axis.title.y= element_text(colour="black",size = 20))+
  theme(axis.title.x = element_text(colour="black",size = 20))+
  theme(axis.text.x = element_text(colour="black",size = 20))+
  theme(plot.title = element_text(colour="black",size = 20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

###### Sentiment analysis

## for sentinment analysis we have 3 opotions of lexicons
#The three general-purpose lexicons are:
#  • AFINN from Finn Årup Nielsen ("afinn")
#• Bing from Bing Liu and collaborators ("bing")
#• NRC from Saif Mohammad and Peter Turney ("nrc")

#All three lexicons are based on unigrams, i.e., single words. These lexicons contain
#many English words and the words are assigned scores for positive/negative sentiment,
#and also possibly emotions like joy, anger, sadness, and so forth. Each lexicon has their own criteria

library("textdata")
text_sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing"))

### let's to see the main words in each category
bing_word_counts <- text_sentiment %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


### to change the writing of levels
bing_word_counts$sentiment<-as.factor(bing_word_counts$sentiment)
levels(bing_word_counts$sentiment)<-c("Negative","Positive")

### let's to make a nice plot showing the main words in each category
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()  +
  theme_bw()+
  scale_fill_manual(values=c("#56b4e9", "#cc79a7"))+
  theme(axis.text.x=element_text(colour="black",size=20))+
  theme(axis.text.y= element_text(colour="black",size = 20))+
  theme(axis.title.y= element_blank())+
  theme(axis.title.x = element_text(colour="black",size = 20))+
  theme(plot.title = element_text(colour="black",size = 20))+
  theme(strip.text.x = element_text(color="black",size=20))+
  theme(strip.background =element_rect(fill="#CCCCCC",colour="black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



######## let's to analyse the sentiment in each tweet

### identify each tweet using thetweet if as key

##### here i will build a df with the % of each category in each tweet
df_sent<-text_sentiment %>%
  group_by((tweet_id))%>%
  summarise(total_words=n_distinct(word),
            n_positive=n_distinct(word[sentiment=="positive"]),
           n_negative=n_distinct(word[sentiment=="negative"]),
           positive=(n_positive/total_words)*100,
           negative=(n_negative/total_words)*100)

#### here a do a thin form of this data with only the collumns I want
data_sent<-df_sent[,c(1,5:6)] %>%
  gather(., key=type,value=perc,2:3)

## let  s evaluate whether there are differences between positive and negative
m1<-lm(perc~type,data=data_sent)
hist(resid(m1))
summary(m1)

### let's to obtain the mean values of each category
gdata<-data_sent %>%
  group_by(type) %>%
  summarise(mean=mean(perc),
            sd=sd(perc))



#### a plot for the means
gdata$type<-as.factor(gdata$type)
levels(gdata$type)<-c("Negative","Positive")


ggplot(gdata) +
  aes(x = type, y = mean) +
  geom_bar(stat="identity",width = 0.5,
           position=position_dodge(width=0.8),colour="black") +
  labs(title = "Pantanal twwets in Twitter") +
  theme_bw()+
  scale_y_continuous(name="Proportion in tweets (%)",limits=c(0,100))+
  scale_x_discrete(name=NULL)+
  scale_fill_manual(values=c("#56b4e9", "#cc79a7"))+
  theme(axis.text.x=element_text(colour="black",size=30))+
  theme(axis.text.y= element_text(colour="black",size = 30))+
  theme(axis.title.y= element_blank())+
  theme(axis.title.x = element_text(colour="black",size = 30))+
  theme(plot.title = element_text(colour="black",size = 30))+
  theme(legend.text = element_text(color="black", size=30))+
  theme(legend.title=element_blank())+
  geom_text(aes(label=round(mean,2)),size=9,position=position_dodge(width=0.8),vjust=-2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


######### wordclouds
library(wordcloud)

freq<- tidy_text %>%
  count(word)
freq<-freq[freq$word!="pantanal",]
tiff("word.cloud.tiff",width = 1000,height = 1000,res=300)
wordcloud(freq$word,freq$n,max.words = 100,colors=c("blue","gray"))
dev.off()

########## association rules analysis





############# Let's to do a example in portuguese
### for the Lexicon in portugues I will use the package lexiconPT

library(lexiconPT)


## let's to get the twitter data

# I will get all tweets from the last days that are related to the Environment Minister Ricardo Salles
tweet_rs <- search_tweets("rsallesmma", n = 2000,
                          include_rts = FALSE,lang="pt")



#### let's to do a clean in the tweets text


tweet_rs $text<- gsub("#([a-z|A-Z|0-9|_])*","", tweet_rs$text) # remove hashtags
tweet_rs $text<- gsub('@([a-z|A-Z|0-9|_])*', '', tweet_rs$text) # remove palavras com @ (menções)
tweet_rs$text<- gsub('https://','', tweet_rs$text) # removes https://
tweet_rs$text<- gsub('http://','', tweet_rs$text) # removes http://
tweet_rs$text<- gsub('[^[:graph:]]', ' ', tweet_rs$text) # removes graphic characters like emoticons 
tweet_rs$text<- gsub('[[:punct:]]', '', tweet_rs$text) # removes punctuation 
tweet_rs$text<- gsub('[[:cntrl:]]', '', tweet_rs$text) # removes control characters
tweet_rs$text<- gsub("\\w*[0-9]+\\w*\\s*", "", tweet_rs$text) # removes numbers
tweet_rs$text<- gsub('_', ' ', tweet_rs$text) # replace underline by space
tweet_rs$text<- gsub('[ \t]{2,}', ' ', tweet_rs$text) ## remove unnecessarie spaces
tweet_rs$text<- gsub('^\\s+|\\s+$', ' ', tweet_rs$text)   ## remove unnecessarie spaces
tweet_rs$text<- tolower(tweet_rs$text) # minor case
tweet_rs$text<-gsub('\\n', ' ', tweet_rs$text ) ### remove rows breaks
tweet_rs<-tweet_rs[!duplicated(tweet_rs$text),]  ### remove duplicated tweets


##### Let's to add a id to identify tweets
tweet_rs$tweet_id<-seq(from=1,length(tweet_rs$user_id))


# load datasets from the lexiconPt packge
## Here we have two datas, the firts is the open lexicon in which the word receives 1, -1 or 0, indicating its tendency
# the second is the sentilex, in which the word receives 1, -1 or 0, indicating its tendency

data("oplexicon_v3.0")
data("sentiLex_lem_PT02")


op30 <- oplexicon_v3.0
lex <- sentiLex_lem_PT02


### tokenization (break text into rows with just one word)
library(tidytext)

text_rs<-tweet_rs[,c(91,5)] %>%  unnest_tokens(term, text)


### Let's to link this words to the two lexicons 
text_rs<-text_rs %>% 
  left_join(op30[,c(1,3)], by = "term",op_polarity=polarity) %>%
    left_join(lex [,c(1,3)], by="term",lex_polarity = polarity)
                
colnames(text_rs)<-c("tweet_id","term","polarity_op","polarity_lex")



######### let's to select only words has classification in one of two data base

text_rs<-text_rs[!is.na(text_rs$polarity_op) | !is.na(text_rs$polarity_lex), ]

### let s to remove the neutral words
text_rs<-text_rs[!text_rs$polarity_op==0 | !text_rs$polarity_lex==0, ]

### let's to classify  each word accordind to each database
text_rs$class_op<-ifelse(text_rs$polarity_op==-1,"negative",ifelse(text_rs$polarity_op==1,"positive",NA))
text_rs$class_lex<-ifelse(text_rs$polarity_lex==-1,"negative",ifelse(text_rs$polarity_lex==1,"positive",NA))


### let's to do a plot only for op database
## for this i will thus remove all non-classified words according to this database

text_rs[!is.na(text_rs$class_op),] %>%
  count(term, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_bw()+
  scale_y_continuous(name="Number of citations")+
  theme(axis.text.y= element_text(colour="black",size = 20))+
  theme(axis.title.y= element_text(colour="black",size = 20))+
  theme(axis.title.x = element_text(colour="black",size = 20))+
  theme(axis.text.x = element_text(colour="black",size = 20))+
  theme(plot.title = element_text(colour="black",size = 20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##lets to do a nice plot identifying the main words in each category
### let's to see the main words in each category
op_word_counts <- text_rs[!is.na(text_rs$class_op),] %>%
  count(term, class_op, sort = TRUE) %>%
  ungroup()


### to change the writing of levels
op_word_counts$class_op<-as.factor(op_word_counts$class_op)
levels(op_word_counts$class_op)<-c("Negative","Positive")




op_word_counts  %>%
  group_by(class_op) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(term= reorder(term, n)) %>%
  ggplot(aes(term, n, fill = class_op)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~class_op, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()  +
  theme_bw()+
  scale_fill_manual(values=c("#56b4e9", "#cc79a7"))+
  theme(axis.text.x=element_text(colour="black",size=20))+
  theme(axis.text.y= element_text(colour="black",size = 20))+
  theme(axis.title.y= element_blank())+
  theme(axis.title.x = element_text(colour="black",size = 20))+
  theme(plot.title = element_text(colour="black",size = 20))+
  theme(strip.text.x = element_text(color="black",size=20))+
  theme(strip.background =element_rect(fill="#CCCCCC",colour="black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
