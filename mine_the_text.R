library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(tidyr)
library(tm)
library(topicmodels)
library(broom)
library(purrr)
library(lubridate)
library(readr)
library(ggplot2)

trump <- read_csv('trump-20170125.csv')

clean_post <- function(text) {
  head1 <- 'the WHITE HOUSEPresident Donald J. Trump Get in Touch  Home Briefing RoomFrom the News RoomLatest NewsRead the latest news from the White House From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop IssuesAmerica First Energy Plan America First Foreign Policy Bringing Back Jobs And Growth Making Our Military Strong Again Standing Up For Our Law Enforcement Community Trade Deals Working For All Americans The AdministrationPeoplePresident Donald J. Trump Vice President Mike Pence First Lady Melania Trump Mrs. Karen Pence The Cabinet Special EventsThe 58th Presidential Inauguration ParticipateJoin UsTours & Events Jobs with the Administration Internships White House Fellows Share Your ThoughtsWe the People Petitions Contact the White House 1600 PennHistory & GroundsPresidents First Ladies The Vice President\'s Residence & Office Eisenhower Executive Office Building Camp David Air Force One Our GovernmentThe Executive Branch The Legislative Branch The Constitution Federal Agencies & Commissions Elections & Voting State & Local Government Search form Search You are hereHome'
  head2 <- 'ParticipateWhite House Fellows Join Us Tours & Events Jobs with the Administration Internships White House FellowsAbout the Fellowship Current Class Selection Process & Calendar Frequently Asked Questions Contact Apply'
  head3 <- '1600 PennFirst Ladies History & Grounds Presidents First Ladies The Vice President\'s Residence & Office Eisenhower Executive Office Building Camp David Air Force One'
  head4 <- "1600 PennPresidents History & Grounds Presidents First Ladies The Vice President's Residence & Office Eisenhower Executive Office Building Camp David Air Force One"
  head5 <- "Briefing RoomPresidential ActionsExecutive Orders Briefing Room Speeches & Remarks Press Briefings Statements & Releases Presidential ActionsExecutive Orders Presidential Memoranda Proclamations Legislation Nominations & Appointments Disclosures"
  head6 <- "Briefing RoomPresidential Actions"
  head7 <- "Presidential Memoranda Briefing Room Speeches & Remarks Press Briefings Statements & Releases Presidential ActionsExecutive Orders Presidential Memoranda Proclamations Legislation Nominations & Appointments Disclosures"
  head8 <- "the WHITE HOUSEPresident Donald J. Trump Get in Touch  Home Briefing RoomFrom the News RoomLatest NewsRead the latest news from the White House From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop Issues"
  head9 <- "The White House Office of the Press Secretary For Immediate Release"
  foot <- 'Follow Us: Twitter Facebook Instagram Youtube Email Twitter Instagram Facebook Contact Us Home Briefing RoomFrom the News RoomLatest News From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop IssuesAmerica First Energy Plan America First Foreign Policy Bringing Back Jobs And Growth Making Our Military Strong Again Standing Up For Our Law Enforcement Community Trade Deals Working For All Americans The AdministrationPeoplePresident Donald J. Trump Vice President Mike Pence First Lady Melania Trump Mrs. Karen Pence The Cabinet Special EventsThe 58th Presidential Inauguration ParticipateJoin UsTours & Events Jobs with the Administration Internships White House Fellows Share Your ThoughtsWe the People Petitions Contact the White House 1600 PennHistory & GroundsPresidents First Ladies The Vice President\'s Residence & Office Eisenhower Executive Office Building Camp David Air Force One Our GovernmentThe Executive Branch The Legislative Branch The Constitution Federal Agencies & Commissions Elections & Voting State & Local Government USA.gov Privacy Policy Copyright Policy'
  text_no_head <- gsub(head1, '', text)
  text_no_head2 <- gsub(head2, '', text_no_head)
  text_no_head3 <- gsub(head3, '', text_no_head2)
  text_no_head4 <- gsub(head4, '', text_no_head3)
  text_no_head5 <- gsub(head5, '', text_no_head4)
  text_no_head6 <- gsub(head6, '', text_no_head5)
  text_no_head7 <- gsub(head7, '', text_no_head6)
  text_no_head8 <- gsub(head8, '', text_no_head7)
  text_no_head9 <- gsub(head9, '', text_no_head8)
  text_no_head_no_foot <- gsub (foot, '', text_no_head9)
  return(text_no_head_no_foot)
}

cleaned_text <- trump$text %>%
  clean_post()
  
trump_clean <- as_tibble(cbind(title = trump$title, text = cleaned_text))

# unnest trump whitehouse.gov corpus by word, remove stop words
tidy_trump <- trump_clean %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# most common words
tidy_trump %>%
  count(word, sort = TRUE) %>%
  filter(n > 220) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words on whitehouse.gov on January 25, 2017') +
  coord_flip()

word_count <- tidy_trump %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

# unnest trump whitehouse.gov corpus by bigram, remove stop words
tidy_trump_bigrams <- trump %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(bigram, word1, word2, sep = ' ')

# most common bigrams
tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams on whitehouse.gov on January 25, 2017') +
  coord_flip()

bigram_count <- tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  mutate(bigram = reorder(bigram, n))

# unnest trump whitehouse.gov corpus by trigram, remove stop words
tidy_trump_trigram <- trump %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word3 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

# most common trigrams
tidy_trump_trigram %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 390) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams on whitehouse.gov on January 25, 2017') +
  coord_flip()

trigram_count <- tidy_trump_trigram %>%
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

# compare to a corpus of trump campaign speeches (in a separate repository)
tidy_wh <- tidy_trump

speeches <- read_file('../trump_speeches/data/full_speech.txt')
speeches <- as_tibble(cbind(source = 'trump_speeches', text = speeches))
tidy_speeches <- speeches %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

trump_talk <- bind_rows(tidy_wh %>% 
                          mutate(source = "whitehouse.gov"),
                        tidy_speeches %>% 
                          mutate(source = "campaign"))

frequency <- trump_talk %>% 
  group_by(source) %>% 
  count(word, sort = TRUE) %>% 
  left_join(trump_talk %>% 
              group_by(source) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(source, word, freq) %>% 
  spread(source, freq) %>%
  arrange(whitehouse.gov, campaign)

ggplot(frequency, aes(whitehouse.gov, campaign, color = abs(campaign - whitehouse.gov))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(x = 'whitehouse.gov', y = 'Campaign speeches')

tidy_speech_bigrams <- speeches %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(bigram, word1, word2, sep = ' ')

tidy_speech_trigrams <- speeches %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word3 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

trump_bigrams <- bind_rows(tidy_trump_bigrams %>% 
                             mutate(source = "whitehouse.gov"),
                           tidy_speech_bigrams %>% 
                             mutate(source = "campaign"))

trump_trigrams <- bind_rows(tidy_trump_trigram %>% 
                             mutate(source = "whitehouse.gov"),
                           tidy_speech_trigrams %>% 
                             mutate(source = "campaign"))

frequency <- trump_bigrams %>% 
  group_by(source) %>% 
  count(bigram, sort = TRUE) %>% 
  left_join(trump_talk %>% 
              group_by(source) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(source, bigram, freq) %>% 
  spread(source, freq) %>%
  arrange(whitehouse.gov, campaign)

ggplot(frequency, aes(whitehouse.gov, campaign, color = abs(campaign - whitehouse.gov))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = bigram), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(x = 'whitehouse.gov', y = 'Campaign speeches')

word_ratios <- trump_talk %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, source) %>%
  filter(sum(n) >= 10) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(campaign / whitehouse.gov)) %>%
  arrange(desc(logratio))

bigram_ratios <- trump_bigrams %>%
  filter(!str_detect(bigram, "^@")) %>%
  count(bigram, source) %>%
  filter(sum(n) >= 10) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>%
  mutate(logratio = log(campaign / whitehouse.gov)) %>%
  arrange(desc(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (campaign/whitehouse.gov)") +
  scale_fill_discrete(name = "", labels = c("campaign", "whitehouse.gov"))

bigram_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, logratio)) %>%
  ggplot(aes(bigram, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (campaign/whitehouse.gov)") +
  scale_fill_discrete(name = "", labels = c("campaign", "whitehouse.gov"))

trigram_ratios <- trump_trigrams %>%
  filter(!str_detect(trigram, "^@")) %>%
  count(trigram, source) %>%
  filter(sum(n) >= 10) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -trigram) %>%
  mutate(logratio = log(campaign / whitehouse.gov)) %>%
  arrange(desc(logratio))

trigram_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(trigram = reorder(trigram, logratio)) %>%
  ggplot(aes(trigram, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("log odds ratio (campaign/whitehouse.gov)") +
  scale_fill_discrete(name = "", labels = c("campaign", "whitehouse.gov"))
