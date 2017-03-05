library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(tm)
library(topicmodels)
library(purrr)
library(lubridate)
library(readr)

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
           'eventstune', 'materialbudgetary', 'housewest', 'housepresident')))

# unnest whitehouse.gov corpora by word, remove stop words
tidy_trump <- trump %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(wh_stop_words)

tidy_obama <- obama %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(wh_stop_words)

# count a single word
tidy_obama %>%
  filter(word == 'women') %>%
  count(word) %>%
  mutate(share = n/length(tidy_obama$word))

# most common words
tidy_trump %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words on whitehouse.gov on March 1, 2017') +
  coord_flip()

tidy_obama %>%
  count(word, sort = TRUE) %>%
  filter(n > 8000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words on whitehouse.gov on January 20, 2017') +
  coord_flip()

trump_word_count <- tidy_trump %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

obama_word_count <- tidy_obama %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

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

# count a single bigram
tidy_obama_bigrams %>%
  count(bigram) %>%
  filter(bigram == 'climate change') %>%
  mutate(share = n/length(tidy_obama_bigrams$bigram))

# most common bigrams
tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 3000) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams on whitehouse.gov on March 1, 2017') +
  coord_flip()

tidy_obama_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 5300) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams on whitehouse.gov on January 20, 2017') +
  coord_flip()

trump_bigram_count <- tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  mutate(bigram = reorder(bigram, n))

obama_bigram_count <- tidy_obama_bigrams %>%
  count(bigram, sort = TRUE) %>%
  mutate(bigram = reorder(bigram, n))

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
  count(trigram, sort = TRUE) %>%
  filter(n > 3000) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams on whitehouse.gov on March 1, 2017') +
  coord_flip()

tidy_obama_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 5200) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams on whitehouse.gov on January 20, 2017') +
  coord_flip()

trump_trigram_count <- tidy_trump_trigrams %>%
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

obama_trigram_count <- tidy_obama_trigrams %>%
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))


# compare corpora

tidy_whitehouse <- bind_rows(tidy_trump %>% 
                               mutate(source = "trump"),
                             tidy_obama %>% 
                               mutate(source = "obama"))

word_frequency <- tidy_whitehouse %>% 
  group_by(source) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_whitehouse %>% 
              group_by(source) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

word_frequency <- word_frequency %>% 
  select(source, word, freq) %>% 
  spread(source, freq) %>%
  arrange(trump, obama)

ggplot(word_frequency, 
       aes(trump, obama, color = abs(obama - trump))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(x = 'Trump\'s whitehouse.gov,  Mar 1, 2017', 
       y = 'Obama\'s whitehouse.gov, Jan 20, 2017')


tidy_whitehouse_bigrams <- bind_rows(tidy_trump_bigrams %>% 
                                       mutate(source = "trump"),
                                     tidy_obama_bigrams %>% 
                                       mutate(source = "obama"))

bigram_frequency <- tidy_whitehouse_bigrams %>% 
  group_by(source) %>% 
  count(bigram, sort = TRUE) %>% 
  left_join(tidy_whitehouse_bigrams %>% 
              group_by(source) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

bigram_frequency <- bigram_frequency %>% 
  select(source, bigram, freq) %>% 
  spread(source, freq) %>%
  arrange(trump, obama)

ggplot(bigram_frequency, 
       aes(trump, obama, color = abs(obama - trump))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = bigram), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(x = 'Trump\'s whitehouse.gov, Mar 1, 2017', 
       y = 'Obama\'s whitehouse.gov, Jan 20, 2017')


tidy_whitehouse_trigrams <- bind_rows(tidy_trump_trigrams %>% 
                                       mutate(source = "trump"),
                                     tidy_obama_trigrams %>% 
                                       mutate(source = "obama"))

trigram_frequency <- tidy_whitehouse_trigrams %>% 
  group_by(source) %>% 
  count(trigram, sort = TRUE) %>% 
  left_join(tidy_whitehouse_trigrams %>% 
              group_by(source) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

trigram_frequency <- trigram_frequency %>% 
  select(source, trigram, freq) %>% 
  spread(source, freq) %>%
  arrange(trump, obama)

ggplot(trigram_frequency, 
       aes(trump, obama, color = abs(obama - trump))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = trigram), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(x = 'Trump\'s whitehouse.gov, Mar 1, 2017', 
       y = 'Obama\'s whitehouse.gov, Jan 20, 2017')


word_ratios <- tidy_whitehouse %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, source) %>%
  filter(sum(n) >= 10) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))

bigram_ratios <- tidy_whitehouse_bigrams %>%
  filter(!str_detect(bigram, "^@")) %>%
  count(bigram, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))

trigram_ratios <- tidy_whitehouse_trigrams %>%
  filter(!str_detect(trigram, "^@")) %>%
  count(trigram, source) %>%
  filter(sum(n) >= 10) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -trigram) %>%
  mutate(logratio = log(trump / obama)) %>%
  arrange(desc(logratio))

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
  top_n(25, abs(logratio)) %>%
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


# compare corpora
tidy_presidents <- tidy_trump %>%
  mutate(president = 'trump') %>%
  full_join(tidy_obama %>%
              mutate(president = 'obama')) %>%
  unique() %>%
  filter(str_detect(word, "[a-z]"))

frequency <- tidy_presidents %>% 
  group_by(president) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_presidents %>% 
              group_by(president) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(president, word, freq) %>% 
  spread(president, freq) 

word_ratios <- tidy_presidents %>%
  filter(str_detect(word, "[a-z]")) %>%
  count(word, president) %>%
  filter(sum(n) >= 10) %>%
  spread(president, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(ratio = log(obama/trump)) %>%
  arrange(desc(ratio))

word_ratios %>%
  filter(!word %in% c('barack', 'obama', 'donald', 'trump', 'joe', 'jill',
                      'biden', 'mike', 'karen', 'pence', 'michelle', 'melania')) %>%
  group_by(ratio < 0) %>%
  top_n(15, abs(ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, ratio)) %>%
  ggplot(aes(word, ratio, fill = ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Obama/Trump)") +
  ggtitle('Words that most uniquely define the text of an administration\'s whitehouse.gov website') +
  scale_fill_discrete(name = "", labels = c("Obama", "Trump"))

tidy_president_bigrams <- tidy_trump_bigrams %>%
  mutate(president = 'trump') %>%
  full_join(tidy_obama_bigrams %>%
              mutate(president = 'obama')) %>%
  unique()

frequency <- tidy_president_bigrams %>% 
  group_by(president) %>% 
  count(bigram, sort = TRUE) %>% 
  left_join(tidy_president_bigrams %>% 
              group_by(president) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(president, bigram, freq) %>% 
  spread(president, freq) 

bigram_ratios <- tidy_president_bigrams %>%
  filter(str_detect(bigram, "[a-z]")) %>%
  count(bigram, president) %>%
  filter(sum(n) >= 10) %>%
  spread(president, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>%
  mutate(obama_trump_ratio = log(obama/trump)) %>%
  arrange(desc(obama_trump_ratio))

bigram_ratios %>%
  group_by(obama_trump_ratio < 0) %>%
  top_n(15, abs(obama_trump_ratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, obama_trump_ratio)) %>%
  ggplot(aes(bigram, obama_trump_ratio, fill = obama_trump_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Obama/Trump)") +
  ggtitle('Bigrams that most uniquely define the text of an administration\'s whitehouse.gov sites') +
  scale_fill_discrete(name = "", labels = c("Obama", "Trump"))


tidy_president_trigrams <- tidy_trump_trigrams %>%
  mutate(president = 'trump') %>%
  full_join(tidy_obama_trigrams %>%
              mutate(president = 'obama')) %>%
  unique()

frequency <- tidy_president_trigrams %>% 
  group_by(president) %>% 
  count(trigram, sort = TRUE) %>% 
  left_join(tidy_president_trigrams %>% 
              group_by(president) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(president, trigram, freq) %>% 
  spread(president, freq) 

trigram_ratios <- tidy_president_trigrams %>%
  filter(str_detect(trigram, "[a-z]")) %>%
  count(trigram, president) %>%
  filter(sum(n) >= 10) %>%
  spread(president, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -trigram) %>%
  mutate(obama_trump_ratio = log(obama/trump)) %>%
  arrange(desc(obama_trump_ratio))

trigram_ratios %>%
  group_by(obama_trump_ratio < 0) %>%
  top_n(15, abs(obama_trump_ratio)) %>%
  ungroup() %>%
  mutate(trigram = reorder(trigram, obama_trump_ratio)) %>%
  ggplot(aes(trigram, obama_trump_ratio, fill = obama_trump_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Obama/Trump)") +
  ggtitle('Trigrams that most uniquely define the text of an administration\'s whitehouse.gov sites') +
  scale_fill_discrete(name = "", labels = c("Obama", "Trump"))


# compare filtered bigrams
search_string <- 'reagan'
# Interesting searches: terror, islam, radical, student, science, wom, climate,
# border, isis/isil, migra, school, college, natural, nuclear, soviet, africa,
# reagan, 
bigram_ratios %>%
  filter(grepl(search_string, bigram)) %>%
  group_by(obama_trump_ratio < 0) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, obama_trump_ratio)) %>%
  ggplot(aes(bigram, obama_trump_ratio, fill = obama_trump_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Obama/Trump)") +
  ggtitle(paste('Relative frequency of bigrams containing "',
                search_string,
                '"',
                '\non whitehouse.gov (Jan 20 vs. Mar 1)\n',
                sep = '')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "", labels = c("Obama", "Trump"))



  # compare lists of page titles

na_to_false <- function(text) {
  if (is.na(text)) {
    return(FALSE)
  } else {
    return(text)
  }
}

page_titles <- obama %>%
  select(title) %>%
  mutate(obama = TRUE) %>%
  full_join(trump %>%
              select(title) %>%
              mutate(trump = TRUE)) %>%
  unique() %>%
  mutate(obama = sapply(obama, na_to_false),
         trump = sapply(trump, na_to_false))

universal_pages <- page_titles %>%
  filter(obama == TRUE,
         trump == TRUE) %>%
  mutate(page_title = title,
         obama_Jan20 = obama,
         trump_Mar01 = trump) %>%
  select(page_title, obama_Jan20, trump_Mar01)

pages_no_trump <- page_titles %>%
  filter(obama == TRUE,
         trump == FALSE) %>%
  mutate(page_title = title,
         obama_Jan20 = obama,
         trump_Mar01 = trump) %>%
  select(page_title, obama_Jan20, trump_Mar01)

# trump_diffs <- page_titles %>%
#   filter(trump25 != trump31) %>%
#   mutate(page_title = title,
#          obama_Jan20 = obama,
#          trump_Jan25 = trump25,
#          trump_Jan31 = trump31) %>%
#   select(page_title, obama_Jan20, trump_Jan25, trump_Jan31)
# 
pages_no_obama <- page_titles %>%
  filter(obama == FALSE,
         trump == FALSE) %>%
  mutate(page_title = title,
         obama_Jan20 = obama,
         trump_Mar01 = trump) %>%
  select(page_title, obama_Jan20, trump_Mar01)

write_csv(pages_no_obama, 'diffs/pages_new_with_trump.csv')
write_csv(universal_pages, 'diffs/pages_always_on_whitehouse_dot_gov.csv')
# write_csv(trump_diffs, 'diffs/pages_new_or_deleted_on_Jan31.csv')
write_csv(pages_no_trump, 'diffs/pages_unique_to_obama.csv')

# export different versions of Judicial Branch pages

write_file(obama %>% 
             filter(title == 'The Judicial Branch | whitehouse.gov') %>%
             select(text) %>%
             as.character(),
           'diffs/judicial_branch_Jan20.txt')

write_file(trump31 %>% 
             filter(title == 'The Judicial Branch | whitehouse.gov') %>%
             select(text) %>%
             as.character(),
           'diffs/judicial_branch_Jan31.txt')


# cleaning headers (posibly)

clean_trump_post <- function(text) {
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
  text_no_head_no_foot <- gsub(foot, '', text_no_head9)
  return(text_no_head_no_foot)
}

clean_obama_post <- function(text) {
  head1 <- "the WHITE HOUSEPresident Barack Obama Contact Us Get Email Updates  Home Briefing RoomFrom the News RoomLatest NewsRead the latest blog posts from 1600 Pennsylvania Ave Share-WorthyCheck out the most popular infographics and videos PhotosView the photo of the day and other galleries Video GalleryWatch behind-the-scenes videos and more Live EventsTune in to White House events and statements as they happen Music & Arts PerformancesSee the lineup of artists and performers at the White House From the Press OfficeYour Weekly Address Speeches & Remarks Press Briefings Statements & Releases White House Schedule Presidential Actions Legislation Nominations & Appointments Disclosures IssuesPopular Topics"
  head7 <- "Developer Tools Tools You Can Use Join UsTours & Events Jobs with the Administration Internships White House Fellows Presidential Innovation Fellows United States Digital Service Leadership Development Program Speak OutWe the People Petitions Contact the White House Citizens Medal Champions of Change 1600 PennInside the White HouseWest Wing Tour Eisenhower Executive Office Building Tour Video Series Décor and Art Holidays See All History & GroundsPresidents First Ladies The Vice President's Residence & Office Eisenhower Executive Office Building Camp David Air Force One Our GovernmentThe Executive Branch The Legislative Branch The Judicial Branch The Constitution Federal Agencies & Commissions Elections & Voting State & Local Government Resources Search form"
  head2 <- "Search  You are hereHomeBlog"
  head3 <- "Search What's HappeningGet the feed Share This: Twitter Facebook Email"
  head4 <- "Search You are hereHome1600 Penn"
  head5 <- "Home • The Administration • 1 is 2 Many Home About Blog Apps Against Abuse Take Action Against Abuse Resources NotAlone Take Action Against Abuse For Teens For Young Adults For Parents For Schools"
  head6 <- "Criminal Justice Reform The Record"
  head8 <- "Cabinet Exit Memos"
  head9 <- "Cuba See All Top IssuesCivil Rights Climate Change Economy Education Foreign Policy Health Care Iran Deal Immigration Action MoreDefense Disabilities Ethics Equal Pay Homeland Security Reducing Gun Violence Rural Service MoreSeniors & Social Security Taxes Technology Trade Urban and Economic Mobility Veterans Women The AdministrationPeoplePresident Barack Obama Vice President Joe Biden First Lady Michelle Obama Dr. Jill Biden The Cabinet Executive Office of the President Senior White House Leadership Other Advisory Boards Executive OfficesOffice of Management and Budget Office of Science and Technology Policy Council of Economic Advisers Council on Environmental Quality National Security Council See All InitiativesLets Move Joining Forces Reach Higher My Brother's Keeper Precision Medicine Special EventsState of the Union Inauguration Medal of Freedom ParticipateDigitalFollow Us on Social Media We the Geeks Hangouts Mobile Apps"
  head10 <- "Supreme Court Nomination Criminal Justice Reform"
  head11 <- "Search  You are hereHome"
  foot <- 'Follow Us: Twitter Facebook Instagram Youtube Email Twitter Instagram Facebook Contact Us Home Briefing RoomFrom the News RoomLatest News From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop IssuesAmerica First Energy Plan America First Foreign Policy Bringing Back Jobs And Growth Making Our Military Strong Again Standing Up For Our Law Enforcement Community Trade Deals Working For All Americans The AdministrationPeoplePresident Donald J. Trump Vice President Mike Pence First Lady Melania Trump Mrs. Karen Pence The Cabinet Special EventsThe 58th Presidential Inauguration ParticipateJoin UsTours & Events Jobs with the Administration Internships White House Fellows Share Your ThoughtsWe the People Petitions Contact the White House 1600 PennHistory & GroundsPresidents First Ladies The Vice President\'s Residence & Office Eisenhower Executive Office Building Camp David Air Force One Our GovernmentThe Executive Branch The Legislative Branch The Constitution Federal Agencies & Commissions Elections & Voting State & Local Government USA.gov Privacy Policy Copyright Policy'
  text_no_head <- gsub(head1, '', text, perl=TRUE)
  text_no_head2 <- gsub(head2, '', text_no_head, perl=TRUE)
  text_no_head3 <- gsub(head3, '', text_no_head2, perl=TRUE)
  text_no_head4 <- gsub(head4, '', text_no_head3, perl=TRUE)
  text_no_head5 <- gsub(head5, '', text_no_head4, perl=TRUE)
  text_no_head6 <- gsub(head6, '', text_no_head5, perl=TRUE)
  text_no_head7 <- gsub(head7, '', text_no_head6, perl=TRUE)
  text_no_head8 <- gsub(head8, '', text_no_head7, perl=TRUE)
  text_no_head9 <- gsub(head9, '', text_no_head8, perl=TRUE)
  text_no_head10 <- gsub(head10, '', text_no_head9, perl=TRUE)
  text_no_head11 <- gsub(head11, '', text_no_head10, perl=TRUE)
  text_no_head_no_foot <- gsub(foot, '', text_no_head11)
  return(text_no_head_no_foot)
}

trump_clean <- as_tibble(cbind(title = trump$title, text = trump$text %>% clean_trump_post()))
obama_clean <- as_tibble(cbind(title = trump$title, text = obama$text %>% clean_obama_post()))

# to use cleaned version instead of original
trump <- trump_clean
obama <- obama_clean