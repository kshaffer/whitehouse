library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(tm)
library(topicmodels)
library(lubridate)

trump <- read_csv('data/trump-20170301.csv')
obama <- read_csv('data/obama-20170120.csv')

wh_stop_words <- as_tibble(cbind(
  word = c('topicssupreme', 'penninside', 'participatedigitalfollow', 
           'outwe', 'officeyour', 'issuespopular', 'eventsstate', 
           'administrationpeoplepresident', 'videosvideo', 
           'videosfeatured', 'herehomephotos', 'thoughtswe', 'pennhistory',
           'participatejoin', 'officespeeches', 'issuestop',
           'issuesamerica', 'involvedsupport', 'galleryview', 'eventsthe',
           'administrationthe', 'administrationpresident', 'officesoffice',
           'moreseniors', 'moredefense', 'issuescivil', 'initiativeslets',
           'worthycheck', 'photosview', 'performancessee', 'gallerywatch',
           'eventstune', 'materialbudgetary', 'housewest', 'housepresident',
           'var', 'padding', 'barack', 'obama', 'donald', 'trump', 'joe', 
           'jill', 'biden', 'mike', 'karen', 'pence', 'michelle', 'melania')))


# unnest whitehouse.gov corpora by word, remove stop words
tidy_trump <- trump %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(wh_stop_words)

tidy_obama <- obama %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(wh_stop_words)


# most common words
tidy_trump %>%
  count(word, sort = TRUE)

tidy_obama %>%
  count(word, sort = TRUE)


# unnest whitehouse.gov corpora by bigram, remove stop words
tidy_trump_bigrams <- trump %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, wh_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ')

tidy_obama_bigrams <- obama %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, wh_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ')


# most common bigrams
tidy_trump_bigrams %>%
  count(bigram, sort = TRUE)

tidy_obama_bigrams %>%
  count(bigram, sort = TRUE)


# unnest whitehouse.gov corpora by trigram, remove stop words
tidy_trump_trigrams <- trump %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word3 %in% c(stop_words$word, wh_stop_words$word)) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

tidy_obama_trigrams <- obama %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word3 %in% c(stop_words$word, wh_stop_words$word)) %>%
  unite(trigram, word1, word2, word3, sep = ' ')


# most common trigrams
tidy_trump_trigrams %>%
  count(trigram, sort = TRUE) 

tidy_obama_trigrams %>%
  count(trigram, sort = TRUE) 


# combine sites into single data frame

tidy_whitehouse <- bind_rows(tidy_trump %>% 
                               mutate(administration = "trump"),
                             tidy_obama %>% 
                               mutate(administration = "obama"))


tidy_whitehouse_bigrams <- bind_rows(tidy_trump_bigrams %>% 
                                       mutate(administration = "trump"),
                                     tidy_obama_bigrams %>% 
                                       mutate(administration = "obama"))


tidy_whitehouse_trigrams <- bind_rows(tidy_trump_trigrams %>% 
                                        mutate(administration = "trump"),
                                      tidy_obama_trigrams %>% 
                                        mutate(administration = "obama"))


# frequency ratios

word_ratios <- tidy_whitehouse %>%
  filter(str_detect(word, "[a-z]")) %>%
  count(word, administration) %>%
  #filter(sum(n) >= 10) %>%
  spread(administration, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))

bigram_ratios <- tidy_whitehouse_bigrams %>%
  filter(str_detect(bigram, "[a-z]")) %>%
  count(bigram, administration) %>%
  #filter(sum(n) >= 10) %>%
  spread(administration, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))

trigram_ratios <- tidy_whitehouse_trigrams %>%
  filter(str_detect(trigram, "[a-z]")) %>%
  count(trigram, administration) %>%
  #filter(sum(n) >= 10) %>%
  spread(administration, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -trigram) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))


# plot most characteristic words, bigrams, trigrams

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (Trump/Obama)") +
  scale_fill_discrete(name = "", labels = c("Trump", "Obama"))

bigram_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, logratio)) %>%
  ggplot(aes(bigram, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (Trump/Obama)") +
  scale_fill_discrete(name = "", labels = c("Trump", "Obama"))

trigram_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(trigram = reorder(trigram, logratio)) %>%
  ggplot(aes(trigram, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (Trump/Obama)") +
  scale_fill_discrete(name = "", labels = c("Trump", "Obama"))


# compare filtered bigrams
search_string <- 'climate'
# Interesting searches: terror, islam, radical, student, science, climate,
# border, isis/isil, migra, school, college, soviet, russia, 
# reagan, extrem
bigram_ratios %>%
  filter(grepl(search_string, bigram)) %>%
  group_by(logratio < 0) %>%
  top_n(4, abs(logratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, logratio)) %>%
  ggplot(aes(bigram, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Trump/Obama)") +
  ggtitle(paste('Relative frequency of bigrams containing "',
                search_string,
                '"',
                '\non whitehouse.gov (Jan 20 vs. Mar 1)\n',
                sep = '')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "", labels = c("Trump", "Obama"))

tidy_whitehouse %>%
  filter(grepl(search_string, word)) %>%
  count(word, administration) %>%
  spread(administration, n, fill = 0) %>%
  mutate(obama_count = obama,
         trump_count = trump,
         total = obama_count + trump_count) %>%
  select(word, total, obama_count, trump_count) %>%
  left_join(word_ratios) %>%
  arrange(desc(total))

tidy_whitehouse_bigrams %>%
  filter(grepl(search_string, bigram),
         administration == 'trump') %>%
  group_by(bigram, administration) %>%
  summarize(n())

tidy_trump_bigrams %>%
  filter(grepl(search_string, bigram)) %>%
  group_by(bigram) %>%
  summarize(n())


# remove pages containing "This is historical material" (from previous administrations)

tidy_trump_non_hist <- trump %>%
  filter(!grepl('This is historical material', text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(wh_stop_words) 

tidy_obama_non_hist <- obama %>%
  filter(!grepl('This is historical material', text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(wh_stop_words)

tidy_whitehouse_non_hist <- bind_rows(tidy_trump_non_hist %>% 
                                        mutate(administration = "trump"),
                                      tidy_obama_non_hist %>% 
                                        mutate(administration = "obama"))


tidy_trump_bigrams_non_hist <- trump %>%
  filter(!grepl('This is historical material', text)) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, wh_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ') 

tidy_obama_bigrams_non_hist <- obama %>%
  filter(!grepl('This is historical material', text)) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, wh_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, wh_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ') 

tidy_whitehouse_bigrams_non_hist <- bind_rows(tidy_trump_bigrams_non_hist %>% 
                                       mutate(administration = "trump"),
                                     tidy_obama_bigrams_non_hist %>% 
                                       mutate(administration = "obama"))


bigram_ratios_non_hist <- tidy_whitehouse_bigrams_non_hist %>%
  filter(str_detect(bigram, "[a-z]")) %>%
  count(bigram, administration) %>%
  #filter(sum(n) >= 10) %>%
  spread(administration, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))

# compare filtered bigrams
search_string <- 'migra'
# Interesting searches: terror, islam, radical, student, science, climate,
# border, isis/isil, migra, school, college, soviet, russia, 
# reagan, extrem
bigram_ratios_non_hist %>%
  filter(grepl(search_string, bigram)) %>%
  group_by(logratio < 0) %>%
  top_n(8, abs(logratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, logratio)) %>%
  ggplot(aes(bigram, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Trump/Obama)") +
  ggtitle(paste('Relative frequency of bigrams containing "',
                search_string,
                '"',
                '\non whitehouse.gov (Jan 20 vs. Mar 1)\n',
                sep = '')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "", labels = c("Trump", "Obama"))

migra <- tidy_whitehouse_non_hist %>%
  filter(grepl(search_string, word),
         !grepl('aglancebordersecuritystrengtheningenforcementearnedcitizenshipstreamliningimmigratio', word)) %>%
  group_by(word, administration) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

migra_bigram_trump <- tidy_whitehouse_bigrams_non_hist %>%
  filter(grepl(search_string, bigram),
         administration == 'trump') %>%
  group_by(bigram, administration) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

migra_bigram <- tidy_whitehouse_bigrams_non_hist %>%
  filter(grepl(search_string, bigram)) %>%
  group_by(bigram, administration) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
