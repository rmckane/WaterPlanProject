library("tidyverse") #includes dplyr, ggplot2, tidyr, purr, readr, and tibble
library("broom") #has the tidy fn to convert non-tidy objects into tidy objects
library("SnowballC")
library("stringr") #has str_extract fn to avoid some of the strange utf-8 text encodings
library("tm") #has a lot of text cleaning fns, like remove numbers and punctuation; incl. NLP and slam
library("tidytext") #has unnest_tokens fn; includes SnowballC; #has tidy fn to convert topics into tidy format
library("topicmodels")
library("wordcloud")
library("quanteda")
library("pdftools")


read <- readPDF(control = list(text = "-layout")) #define engine
files <- list.files(path = "/Users/rachelmckane/Desktop/Plans2/", pattern = "pdf$")
files_path <- paste("/Users/rachelmckane/Desktop/Plans2/",files,sep="")
WaterPlans_text <- VCorpus(URISource(files_path), 
                            readerControl = list(reader = read))

#Having trouble with penn

#potentialy try VCorpus instead of Corpus


class(WaterPlans_text) 


stpwrds <- tm::stopwords("SMART") #list of stopwords in the tm pkg for the SMART option
addwords <- c( "state", "water","plan", "colorado") #Take out drought
states <- read.csv("/Users/rachelmckane/Desktop/State_List.csv", stringsAsFactors = FALSE)
all_stopwords <- c(stpwrds,addwords,states[['State']],states[['State_Abbreviation']]) #merge the two lists
all_stopwords <- tolower(all_stopwords) #make all state names/abbrevs lowercase
rm(stpwrds,addwords,states)

#Define function to get rid of weird issues
(f_weirdos <- content_transformer(function(x, pattern) gsub(pattern, "", x)))

#now do some special corpus cleaning...order of functions matters!
WaterPlans_text_clean <- WaterPlans_text %>%
  tm_map(f_weirdos,"[^[:alnum:] ]") %>% #gets rid of form feed (/f) and euro symbol, but not a accents
  tm_map(f_weirdos,"â") %>% #remove accents
  tm_map(content_transformer(tolower)) %>% #Switch to lower case
  tm_map(removeWords, all_stopwords) %>% #remove stopwords; want to remove stopwords 1st because they contain punctuation
  tm_map(removePunctuation) %>% #Remove punctuation marks 
  tm_map(removeNumbers) %>% #Remove numbers
  tm_map(stripWhitespace) #Remove white space 




### Constructing the Document Term Matrix

WaterPlans.dtm <- DocumentTermMatrix(WaterPlans_text_clean, 
                                     control = list(stopwords = TRUE,
                                                    stemming = TRUE,
                                                    bounds = list(global = c(3, Inf))))

#Terms that appear in less documents than the lower bound bounds$global[1] or in more documents than the upper bound bounds$global[2] are discarded. Defaults to list(global = c(1, Inf)) (i.e., every term will be used).
WaterPlans.dtm #sparsity is 72% and max term length is 18, with 3610 terms in at least 3 docs

inspect(WaterPlans.dtm[1:10,]) 


##TOTAL WORDS


total_terms <- rowSums(as.matrix(WaterPlans.dtm)) # each row is a document; they all have words-- WOO
total_terms <- tidy(total_terms) %>% rename(State = "names", Total_Words = "x")
total_terms

##TOTAL WORDS WITHOUT STOPWORDS
mycorpus <- corpus(WaterPlans_text_clean)
total_terms_raw <- summary(mycorpus)
total_terms_raw_words <- total_terms_raw$Tokens



#Word search using the quanteda package
mycorpus <- corpus(WaterPlans_text_clean)
sust<- kwic(mycorpus, "sustainability") 

sust<- table(sust$docname) %>% tidy()
sust<- sust %>% dplyr::rename(State = "Var1", Sustainability = "Freq")

#Climate Change
cc<- kwic(mycorpus, c(phrase("climate change")))
cc<- table(cc$docname) %>% tidy()
cc<- cc %>% dplyr::rename(State = "Var1", Climate_Change = "Freq")

#Scarcity 
scar<- kwic(mycorpus, "scarcity") 

scar<- table(scar$docname) %>% tidy()
scar<- scar %>% dplyr::rename(State = "Var1", Scarcity = "Freq")


#Climate Varaibility 
cv<- kwic(mycorpus, c(phrase("climate variability")))
cv<- table(cv$docname) %>% tidy()
cv<- cv %>% dplyr::rename(State = "Var1", Climate_Variability = "Freq")


#Extreme Events

ee<- kwic(mycorpus, c(phrase("extreme events")))
ee<- table(ee$docname) %>% tidy()
ee<- ee %>% dplyr::rename(State = "Var1", Extreme_Events = "Freq")

#Quality

qq<- kwic(mycorpus, c(phrase("quality")))
qq<- table(qq$docname) %>% tidy()
qq<- qq %>% dplyr::rename(State = "Var1", Quality = "Freq")

word_data <- total_terms %>% left_join(cc) %>% left_join(sust) %>% 
  left_join(scar) %>% left_join(cv) %>% left_join(ee) %>% left_join(qq)

word_data <- word_data %>%
  mutate(Climate_Change_Percent = Climate_Change/Total_Words*100,
         Sustainability_Percent = Sustainability/Total_Words*100,
         Climate_Variabiliyt_Percent = Climate_Variability/Total_Words*100,
         Extreme_Events_Percent = Extreme_Events/Total_Words*100,
         Quality_Percent = Quality/Total_Words*100)
         

write.csv(word_data,"/Users/rachelmckane/Desktop/word_data.csv")

# Plotting the words that occur at least 50 times
freq <- sort(colSums(as.matrix(WaterPlans.dtm)), decreasing=TRUE) 

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

p <- ggplot(subset(wf, freq>2000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Words that occur at least 50 times")

p




##Word Topic Probabilities
plan_lda <- LDA(WaterPlans.dtm, k = 3, control = list(seed = 1234))
plan_topics <- tidy(plan_lda , matrix = "beta") 

plan_topics 

plan_top_terms <- plan_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

plan_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#This gives us the top 10 words that are most common with each topic
#
# Let's look at the spread-- the terms that had the greatest difference in beta between the two topics

beta_spread <- plan_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(log_ratio)

beta_spread 



#Sort by document
waterplan_documents <- tidy(plan_lda, matrix = "gamma")
waterplan_documents


write.csv(waterplan_documents,"/Users/rachelmckane/Desktop/Topics.csv")


#Below is T's code for top 5 wrds 

Topics_Exploration <- function(No_Topics) {
  drought_lda <- LDA(WaterPlans.dtm, k = No_Topics, control = list(seed = 1234))
  

  #convert into a tidy object
  drought_topics <- tidy(drought_lda, matrix = "beta")
  
  plot1 <- drought_topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  print(plot1)
  
  return(drought_lda)  
}



DroughtDocs_2LDA <- Topics_Exploration(No_Topics=2)
DroughtDocs_2LDA <- Topics_Exploration(No_Topics=3)
DroughtDocs_2LDA <- Topics_Exploration(No_Topics=4)
DroughtDocs_2LDA <- Topics_Exploration(No_Topics=5)



#### Map 
devtools::install_github("UrbanInstitute/urbnmapr")
library(tidyverse)
library(urbnmapr)

states <- read.csv("/Users/rachelmckane/Desktop/State_List.csv", stringsAsFactors = FALSE)
states$State <- tolower(states$State)

word_data$State <- str_sub(word_data$State, 1, str_length(word_data$State)-4)
word_data$State <- tolower(word_data$State)

statewords_allstates <- left_join(states,word_data)


devtools::install_github("UrbanInstitute/urbnmapr")
library(tidyverse)
library(urbnmapr)

map_data <- left_join(urbnmapr::states, statewords_allstates, by= c("state_abbv" = "State_Abbreviation"))


map_data %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Sustainability_Percent)) +
  geom_polygon(color = NA) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  ggtitle("Sustainability Word Use") +
  labs(fill = "Sustainability") +
  theme(legend.position="bottom") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(size=14))


map_data %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Total_Words)) +
  geom_polygon(color = NA) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  ggtitle("Total Words") +
  labs(fill = "Total Words") +
  theme(legend.position="bottom") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(size=14))
  

map_data %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Quality_Percent)) +
  geom_polygon(color = NA) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  ggtitle("Quality Word Use") +
  labs(fill = "Quality") +
  theme(legend.position="bottom") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(size=14))


map_data %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Climate_Change_Percent)) +
  geom_polygon(color = NA) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  ggtitle("Climate Change Word Use") +
  labs(fill = "Climate Change") +
  theme(legend.position="bottom") +
  theme(panel.background = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(size=14))

