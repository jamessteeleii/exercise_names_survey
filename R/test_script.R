library(tidyverse)
library(tidytext)

SM_dat <- read_csv("Social Media data.csv")

# Let's do one example - Q34 "Wrist Flexion/Curl"

SM_token_Q34 <- SM_dat %>%
  select(ResponseId, Q33, Q34) %>%
  unnest_tokens(word, Q34)

data("stop_words")

SM_token_Q34 <- SM_token_Q34 %>%
  anti_join(stop_words) %>%
  filter(!is.na(word) & !is.na(Q33))

# spelling errors - https://books.psychstat.org/textmining/data.html
library(hunspell)
words <- unique(SM_token_Q34$word)
bad_words <- hunspell(words)
bad_words <- unique(unlist(bad_words))
suggest_words <- hunspell_suggest(bad_words)
suggest_words <- unlist(lapply(suggest_words, function(x) x[1]))

### Add checking the suggestions

library(stringi)

bad.whole.words <- paste0("\\b", bad_words, "\\b")

SM_token_Q34$word_check <- stri_replace_all_regex(SM_token_Q34$word, bad.whole.words, suggest_words,
                                            vectorize_all = FALSE)

# Plot simple counts
SM_token_Q34 %>%
  count(word, sort = TRUE) %>%
  # filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  theme_classic()

# plot relationships between word use across categorical variable
library(scales)

SM_token_Q34 %>%
  count(Q33, word) %>%
  group_by(Q33) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = Q33, values_from = proportion) %>%
  ggplot(aes(x = NO, y = YES)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = 0.1, size = 2.5) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  theme_bw()

# A word cloud
library(wordcloud)

SM_token_Q34 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

SM_token_Q34 %>%
  count(word, Q33) %>%
  reshape2::acast(word ~ Q33, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# What about using the term frequency?
recognise_words <- SM_token_Q34 %>%
  count(Q33, word, sort = TRUE)

total_words <- recognise_words %>%
  group_by(Q33) %>%
  summarize(total = sum(n))

recognise_words <- left_join(recognise_words, total_words)

ggplot(recognise_words, aes(n/total, fill = Q33)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~Q33, ncol = 2, scales = "free_y") +
  theme_bw()

# Zipfs law

freq_by_rank <- recognise_words %>%
  group_by(Q33) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = Q33)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# exponent of power law
lm <- lm(log10(`term frequency`) ~ log10(rank), data = freq_by_rank)

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = Q33)) +
  geom_abline(intercept = lm$coefficients[1], slope = lm$coefficients[2],
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# inverse document (person) frequency
tf_idf <- recognise_words %>%
  bind_tf_idf(word, Q33, n)

tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

tf_idf %>%
  group_by(Q33) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Q33)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Q33, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_bw()

# Common bigrams
SM_bigram_Q34 <- SM_dat %>%
  select(ResponseId, Q33, Q34) %>%
  separate(Q34, into = c("word1", "word2"), sep = " ")

bigrams_filtered <- SM_bigram_Q34 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n+100), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_graph()

# Things we can do
# Simply look at the most common names by exercise
# Look at relationships/differences between words used by different participant characteristics
# Similarly, whether similar words are used for say different kinds of exercise (by modality, body part, muscle groups, movement etc.)
# Need to create themes/topics from the data

# https://www.kaianalytics.com/post/how-to-use-text-analysis-techniques-bring-qualitative-data-to-life#amp_tf=From%20%251%24s&aoh=16751908604679&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.kaianalytics.com%2Fpost%2Fhow-to-use-text-analysis-techniques-bring-qualitative-data-to-life
# https://content-analysis-with-r.com/index.html

# https://bookdown.org/daniel_dauber_io/r4np_book/mixed-methods-research.html
# https://martinctc.github.io/blog/a-short-r-package-review-rqda/
