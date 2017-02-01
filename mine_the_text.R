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

trump <- read_csv('trump-20170131.csv')
obama <- read_csv('obama-20170120.csv')

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

# unnest whitehouse.gov corpora by word, remove stop words
tidy_trump <- trump %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_obama <- obama %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# most common words
tidy_trump %>%
  count(word, sort = TRUE) %>%
  filter(n > 650) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words on whitehouse.gov on January 31, 2017') +
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
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(bigram, word1, word2, sep = ' ')

tidy_obama_bigrams <- obama %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(bigram, word1, word2, sep = ' ')

# most common bigrams
tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams on whitehouse.gov on January 31, 2017') +
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
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word3 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

tidy_obama_trigrams <- obama %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word2 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  filter(!word3 %in% c(stop_words$word, 'li', 'ul', 'http', 'htrf')) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

# most common trigrams
tidy_trump_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 75) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams on whitehouse.gov on January 25, 2017') +
  coord_flip()

tidy_obama_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 2500) %>%
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
  labs(x = 'Trump\'s whitehouse.gov, Jan 31, 2017', 
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
  labs(x = 'Trump\'s whitehouse.gov, Jan 31, 2017', 
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
  labs(x = 'Trump\'s whitehouse.gov, Jan 25, 2017', 
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
  filter(sum(n) >= 10) %>%
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

