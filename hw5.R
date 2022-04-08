# Load libraries
library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)

#Read the data
speech = read_csv("presidential_speeches_sample.csv")

#Tokenize speech data
tokens = unnest_tokens(speech, word, content)

head(tokens)

#Remove stopwords
stop_words
tokens = anti_join(tokens, stop_words, by="word")

#Lemmatize words
tokens = mutate(tokens, lemma=lemmatize_words(word))
View(head(tokens))

#Create a table with word counts by document
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())

#Turn into a matrix
word_mtx = cast_dtm(wcounts, document, lemma, count)

#Build the topic model
model = LDA(word_mtx, 10, control=list(seed=42))

#beta matrix
beta = tidy(model, matrix="beta")


#filter the top 10 words for each topic
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10

#ggplot with the top 10 data frame sorted by term and then by beta
ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()

#Filter out words that are common in presidental speeches
unique(filter(tokens, lemma=="question")$word)
custom_stop_words = bind_rows(
  stop_words,  
  tibble(word=c("laughter", "president", "people", "american", "nation", "america", "nations", "country", "countries", "question"))
)

#Repeat steps to remove additional stop words (words that are common in presidential speeches)
tokens = anti_join(tokens, custom_stop_words, by="word")
tokens = mutate(tokens, lemma=lemmatize_words(word))
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)
model = LDA(word_mtx, 10, control=list(seed=42))
beta = tidy(model, matrix="beta")
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)

ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()

# gamma matrix to see what documents are in what topics
gamma = tidy(model, matrix="gamma")

#Topic 1 seems to focus on education
filter(gamma, topic==1) %>% arrange(-gamma)

#Topic 3 seems to focus on honoring sports teams
filter(gamma, topic==3) %>% arrange(-gamma)

#Topic 6 seems focus on the economy and our budget
filter(gamma, topic==6) %>% arrange(-gamma)

#Topic 7 seems focus on terroism and Iraq
filter(gamma, topic==7) %>% arrange(-gamma)

#Create year column and extract the year
speech[,c("year")] = str_match(speech$document, "[[:digit:]]{4}")
speech
view(speech)
speech$year
as.numeric(str_extract(speech$year, "[[:digit:]]{4}"))
speech = mutate(speech, year=as.character(str_extract(speech$year, "[[:digit:]]{4}")))
tokens = unnest_tokens(speech, word, content)
head(tokens)

#Repeat previous steps again
tokens = anti_join(tokens, custom_stop_words, by="word")
tokens = mutate(tokens, lemma=lemmatize_words(word))
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)
model = LDA(word_mtx, 10, control=list(seed=42))
beta = tidy(model, matrix="beta")
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10

ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()

#Added year and gamma column 
gamm = left_join(speech, gamma, on="documents")

#Avg for topic 6 each year
economy = filter(gamm, topic==6)
economy = economy %>%
  group_by(year) %>%
  summarize(gamma=mean(gamma))

#Avg for topic 7 each year
iraq = filter(gamm, topic==7)
iraq = iraq %>%
  group_by(year) %>%
  summarize(gamma=mean(gamma))

#I couldn't figure out how to plot only the filtered topics so this is a graph of all the topics
ggplot(gamm, aes(y=gamma, x=year)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()


