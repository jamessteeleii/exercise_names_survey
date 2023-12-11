library(tidyverse)
library(tidytext)
library(lspline)
library(marginaleffects)

# read in and prepare data
key <- read_csv("data/Key.csv")

data_path <- "./data"
files <- dir(data_path, pattern = "Exercise Names Survey*")

data <- tibble(survey = files) |>
  mutate(file_contents = map(survey,
                             ~ read_csv(file.path(data_path, .)))
  )

data_useful <- unnest(data, cols = file_contents) |>
  mutate(survey = case_when(survey == files[1] ~ "NSCA",
                            survey == files[2] ~ "Social Media",
                            survey == files[3] ~ "Virtruvian")
         )


data_long <- pivot_longer(
  data = data_useful,
  cols = c(24:65,67:74),
  names_to = "question",
  values_to = "response"

)

data_long <- left_join(data_long, key, by="question")

data <- data_long |>
  mutate(question_wording = case_when(
    question_wording == "What do you personally call this exercise? If you are unsure or do not recognise the exercise, what is your best guess as to what this exercise is called? Type your answer in the space below."
    ~ "response_name",
    question_wording == "Do you recognise this exercise?"
    ~ "recognise"
  )) |>
  pivot_wider(id_cols = c(ResponseId,book_exercise_name),
              names_from = question_wording,
              values_from = response,
              unused_fn = max) |>
  filter(Q16 == "Green",
         Q5 == "Yes")


# Let's create tokens
data("stop_words")

data_tokens <- data |>
  select(ResponseId, book_exercise_name,
         body_position, body_part, action, equipment, equipment_position, action_direction, misc,
         recognise, response_name) |>
  unnest_tokens(word, response_name) |>
  anti_join(stop_words) |>
  filter(!is.na(word) & !is.na(recognise)) %>%
  mutate(recognise = factor(recognise, levels= c("YES", "NO")))

# spelling errors - https://books.psychstat.org/textmining/data.html
library(hunspell)
words <- unique(data_tokens$word)
bad_words <- hunspell(words)
bad_words <- unique(unlist(bad_words))
suggest_words <- hunspell_suggest(bad_words)
suggest_words <- unlist(lapply(suggest_words, function(x) x[1]))


    # # combine and compare suggestions (manually checked obvious errors)
    #
    bad_suggest_words <- bind_cols(bad_words, suggest_words)

    count_words <- count(data_tokens, word)

    bad_suggest_words <- inner_join(count_words, bad_suggest_words, by = c(word = "...1"))

# Recode the incorrect suggestions manually with more than 2 uses
# (manually editing original if obvious incorrect spelling)
suggest_words <- recode(suggest_words,
                        "flye" = "fly",
                        "flue" = "fly",
                        "flues" = "fly",
                        "flyes" = "fly",
                        "pend lay" = "pendlay",
                        "probated" = "pronated",
                        "DEC" = "deck",
                        "insulated" = "supinated",
                        "hop" = "ohp",
                        "felt" = "delt",
                        "pull downs" = "pull down",
                        "antediluvian" = "vitruvian",
                        "devoid" = "deltoid",
                        "soles" = "soleus",
                        "flex or" = "flexor",
                        "trice" = "tricep",
                        "pendent" = "pendlay",
                        "resistivity" = "resistive",
                        "kinetics" = "isokinetic",
                        "fliers" = "fly",
                        "font" = "dont",
                        "selector" = "selectorized",
                        "cal" = "calf",
                        "dumbbells" = "dumbbell",
                        "flatcar" = "flat bar",
                        "gastronomic" = "gastrocnemius",
                        "overboard" = "hoverboard",
                        "playpen" = "pendlay",
                        "pen delay" = "pendlay",
                        "tr" = "trx",
                        "virtual" = "vitruvian",
                        "id" = "idk",
                        "Maxine" = "machine",
                        "precede" = "pec deck",
                        "raid" = "raise",
                        "pinnate" = "supinate",
                        "trapezes" = "trapezius",
                        "trapeziums" = "trapezius",
                        "barb" = "barbell",
                        "calf raises" = "calf raise",
                        "chestfuls" = "chest fly",
                        "extrasensory" = "extensor",
                        "extensions" = "extension",
                        "floors" = "flexor",
                        "gluten" = "gluteal",
                        "spelldown" = "lat pull down",
                        "ply" = "Olympic",
                        "peck" = "pec deck",
                        "peddle" = "pendlay",
                        "dependably" = "pendlay",
                        "pen lay" = "pendlay",
                        "preach" = "press",
                        "detonated" = "pronated",
                        "teases" = "seated",
                        "selector" = "selectorized",
                        "serrate" = "serratus",
                        "ottoman" = "zottman"
                        )


### Add checking the suggestions

library(stringi)

bad_whole_words <- paste0("\\b", bad_words, "\\b")

data_tokens$word <- stri_replace_all_regex(data_tokens$word, bad_whole_words, suggest_words,
                                            vectorize_all = FALSE)

# for all double barrel terms split unnest again
data_tokens <-  data_tokens |>
  unnest_tokens(word, word)

# Plot simple counts
data_tokens |>
  count(word, sort = TRUE) |>
  filter(n > 600) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  theme_classic()


# tf as proportion of all words based on recognition

recognise_words <- data_tokens |>
  count(recognise, word, sort = TRUE) |>
  ungroup() |>
  mutate(total = sum(n))

ggplot(recognise_words, aes(n/total, fill = recognise)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap("recognise") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(trans = scales::pseudo_log_trans()) +
  theme_bw()

ggsave("term_frequency_recognise.png", width = 10, height = 5, dpi = 300)

# Zipfs law

freq_by_rank <- recognise_words |>
  group_by(recognise) |>
  mutate(rank = row_number(),
         tf = n/total,
         log10_rank = log10(rank),
         log10_tf = log10(tf)) |>
  ungroup()

lm <- lm(log10_tf ~ lspline(log10_rank, 1) * recognise, data = freq_by_rank)

zipf_preds <- predictions(lm,
                          variables = list(recognise = c("NO","YES")))

freq_by_rank |>
  ggplot() +
  geom_line(aes(x=rank, y=tf, color = recognise),
            size = 1.1, alpha = 0.8, show.legend = FALSE) +
  geom_ribbon(data = zipf_preds,
              aes(x=10^log10_rank, ymin=10^conf.low, ymax=10^conf.high,
                  group = recognise),
              alpha = 0.25) +
  geom_line(data = zipf_preds,
            aes(x=10^log10_rank, y=10^estimate, color = recognise),
            linetype = "dashed") +

  labs(x = "Term Frequency (proportion on log10 scale)",
       y = "Term Rank (log10 scale)",
       color = "Recognised\nExercise?") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()



# tf as proportion of all words for exercise (only recognised words)

exercise_words <- data_tokens |>
  filter(recognise == "YES") |>
  count(book_exercise_name, word, sort = TRUE)

total_words <- exercise_words |>
  group_by(book_exercise_name) |>
  summarize(total = sum(n))

exercise_words <- left_join(exercise_words, total_words)

ggplot(exercise_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap("book_exercise_name", scales = "free") +
  scale_y_continuous(trans = scales::pseudo_log_trans()) +
  theme_bw()

ggsave("term_frequency_exercise.png", width = 10, height = 5, dpi = 300)

# Zipfs law

freq_by_rank <- exercise_words |>
  group_by(book_exercise_name) |>
  mutate(rank = row_number(),
         tf = n/total,
         log10_rank = log10(rank),
         log10_tf = log10(tf)) |>
  ungroup()

lm <- lm(log10_tf ~ lspline(log10_rank, 1) * book_exercise_name, data = freq_by_rank)

zipf_preds <- predictions(lm,
                          variables = list(book_exercise_name = unique(freq_by_rank$book_exercise_name)))

freq_by_rank |>
  ggplot() +
  geom_line(aes(x=rank, y=tf),
            size = 1.1, alpha = 0.8, show.legend = FALSE) +
  geom_ribbon(data = zipf_preds,
              aes(x=10^log10_rank, ymin=10^conf.low, ymax=10^conf.high),
              alpha = 0.25) +
  geom_line(data = zipf_preds,
            aes(x=10^log10_rank, y=10^estimate),
            linetype = "dashed") +
  labs(x = "Term Frequency (proportion on log10 scale)",
       y = "Term Rank (log10 scale)") +
  facet_wrap("book_exercise_name") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()



# tf as proportion of all words for exercise (both recognised and unrecognised words)

exercise_recognise_words <- data_tokens |>
  count(book_exercise_name, recognise, word, sort = TRUE)

total_words <- exercise_recognise_words |>
  group_by(book_exercise_name) |>
  summarize(total = sum(n))

exercise_recognise_words <- left_join(exercise_recognise_words, total_words)

ggplot(exercise_recognise_words, aes(n/total, fill = recognise)) +
  geom_histogram(alpha=0.5) +
  facet_wrap("book_exercise_name", scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(trans = scales::pseudo_log_trans()) +
  labs(x = "Proportion of Total Words",
       y = "Count (log10 scale)",
       fill = "Recognised\nExercise?") +
  theme_bw()

ggsave("term_frequency_exercise.png", width = 10, height = 5, dpi = 300)

# Zipfs law

freq_by_rank <- exercise_recognise_words |>
  group_by(book_exercise_name, recognise) |>
  mutate(rank = row_number(),
         tf = n/total,
         log10_rank = log10(rank),
         log10_tf = log10(tf)) |>
  ungroup()

lm <- lm(log10_tf ~ lspline(log10_rank, c(1)) * book_exercise_name * recognise, data = freq_by_rank)

zipf_preds <- predictions(lm,
                          variables = list(book_exercise_name = unique(freq_by_rank$book_exercise_name),
                                           recognise = c("NO","YES")))

freq_by_rank |>
  ggplot() +
  geom_line(aes(x=rank, y=tf, color = recognise),
            size = 1.1, alpha = 0.8, show.legend = FALSE) +
  geom_ribbon(data = zipf_preds,
              aes(x=10^log10_rank, ymin=10^conf.low, ymax=10^conf.high,
                  group = recognise),
              alpha = 0.25) +
  geom_line(data = zipf_preds,
            aes(x=10^log10_rank, y=10^estimate, color = recognise),
            linetype = "dashed") +
  labs(x = "Term Frequency (proportion on log10 scale)",
       y = "Term Rank (log10 scale)",
       color = "Recognised\nExercise?") +
  facet_wrap("book_exercise_name") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()








# inverse exercise (person) frequency
tf_ief <- exercise_words |>
  bind_tf_idf(word, book_exercise_name, n)

tf_ief |>
  select(-total) |>
  arrange(desc(tf_idf))

library(forcats)

tf_ief |>
  group_by(book_exercise_name) |>
  top_n(5) |>
  ggplot(aes(x=tf_idf,
             y=reorder_within(word, tf_idf, book_exercise_name))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term Frequency - Inverse Exercise Frequency (tf-ief)", y = NULL) +
  scale_y_reordered() +
  facet_wrap(~book_exercise_name, scales = "free") +
  theme_bw()













data_tokens <- data |>
  select(ResponseId, book_exercise_name,
         body_position, body_part, action, equipment, equipment_position, action_direction, misc,
         recognise, response_name) |>
  unnest_tokens(word, response_name) |>
  anti_join(stop_words) |>
  filter(!is.na(word) & !is.na(recognise)) %>%
  mutate(recognise = factor(recognise, levels= c("YES", "NO")))




# Common bigrams
data_bigrams <- data |>
  select(ResponseId, book_exercise_name,
         body_position, body_part, action, equipment, equipment_position, action_direction, misc,
         recognise, response_name) |>
  unnest_tokens(bigram, response_name, token = "ngrams", n =2) |>
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% stop_words$word) |>
  filter(!word2 %in% stop_words$word) |>
  filter(!is.na(word1))

# # spelling errors - https://books.psychstat.org/textmining/data.html
# words1 <- unique(data_bigrams$word1)
# bad_words1 <- hunspell(words1)
# bad_words1 <- unique(unlist(bad_words1))
# suggest_words1 <- hunspell_suggest(bad_words1)
# suggest_words1 <- unlist(lapply(suggest_words1, function(x) x[1]))
#
#
# # # combine and compare suggestions (manually checked obvious errors)
# #
# bad_suggest_words1 <- bind_cols(bad_words1, suggest_words1)
#
# count_words1 <- count(data_bigrams, word1)
#
# bad_suggest_words1 <- inner_join(count_words1, bad_suggest_words1, by = c(word1 = "...1"))
#
# # Recode the incorrect suggestions manually with more than 2 uses
# # (manually editing original if obvious incorrect spelling)
# suggest_words1 <- recode(suggest_words1,
#                         "flye" = "fly",
#                         "flue" = "fly",
#                         "flues" = "fly",
#                         "flyes" = "fly",
#                         "pend lay" = "pendlay",
#                         "probated" = "pronated",
#                         "DEC" = "deck",
#                         "insulated" = "supinated",
#                         "hop" = "ohp",
#                         "felt" = "delt",
#                         "pull downs" = "pull down",
#                         "antediluvian" = "vitruvian",
#                         "devoid" = "deltoid",
#                         "soles" = "soleus",
#                         "flex or" = "flexor",
#                         "trice" = "tricep",
#                         "pendent" = "pendlay",
#                         "resistivity" = "resistive",
#                         "kinetics" = "isokinetic",
#                         "fliers" = "fly",
#                         "font" = "dont",
#                         "selector" = "selectorized",
#                         "cal" = "calf",
#                         "dumbbells" = "dumbbell",
#                         "flatcar" = "flat bar",
#                         "gastronomic" = "gastrocnemius",
#                         "overboard" = "hoverboard",
#                         "playpen" = "pendlay",
#                         "pen delay" = "pendlay",
#                         "tr" = "trx",
#                         "virtual" = "vitruvian",
#                         "id" = "idk",
#                         "Maxine" = "machine",
#                         "precede" = "pec deck",
#                         "raid" = "raise",
#                         "pinnate" = "supinate",
#                         "trapezes" = "trapezius",
#                         "trapeziums" = "trapezius",
#                         "barb" = "barbell",
#                         "calf raises" = "calf raise",
#                         "chestfuls" = "chest fly",
#                         "extrasensory" = "extensor",
#                         "extensions" = "extension",
#                         "floors" = "flexor",
#                         "gluten" = "gluteal",
#                         "spelldown" = "lat pull down",
#                         "ply" = "Olympic",
#                         "peck" = "pec deck",
#                         "peddle" = "pendlay",
#                         "dependably" = "pendlay",
#                         "pen lay" = "pendlay",
#                         "preach" = "press",
#                         "detonated" = "pronated",
#                         "teases" = "seated",
#                         "selector" = "selectorized",
#                         "serrate" = "serratus",
#                         "ottoman" = "zottman"
# )
#
#
# ### Add checking the suggestions
#
# bad_whole_words1 <- paste0("\\b", bad_words1, "\\b")
#
# data_bigrams$word1 <- stri_replace_all_regex(data_bigrams$word1, bad_whole_words1, suggest_words1,
#                                            vectorize_all = FALSE)
#
# # for all double barrel terms split unnest again
# data_bigrams_word1 <-  data_bigrams |>
#   unnest_tokens(word1, word1)
#
#
#
# # spelling errors - https://books.psychstat.org/textmining/data.html
# words2 <- unique(data_bigrams$word2)
# bad_words2 <- hunspell(words2)
# bad_words2 <- unique(unlist(bad_words2))
# suggest_words2 <- hunspell_suggest(bad_words2)
# suggest_words2 <- unlist(lapply(suggest_words2, function(x) x[1]))
#
#
# # # combine and compare suggestions (manually checked obvious errors)
# #
# bad_suggest_words2 <- bind_cols(bad_words2, suggest_words2)
#
# count_words2 <- count(data_bigrams, word2)
#
# bad_suggest_words2 <- inner_join(count_words2, bad_suggest_words2, by = c(word2 = "...1"))
#
# # Recode the incorrect suggestions manually with more than 2 uses
# # (manually editing original if obvious incorrect spelling)
# suggest_words2 <- recode(suggest_words2,
#                          "flye" = "fly",
#                          "flue" = "fly",
#                          "flues" = "fly",
#                          "flyes" = "fly",
#                          "pend lay" = "pendlay",
#                          "probated" = "pronated",
#                          "DEC" = "deck",
#                          "insulated" = "supinated",
#                          "hop" = "ohp",
#                          "felt" = "delt",
#                          "pull downs" = "pull down",
#                          "antediluvian" = "vitruvian",
#                          "devoid" = "deltoid",
#                          "soles" = "soleus",
#                          "flex or" = "flexor",
#                          "trice" = "tricep",
#                          "pendent" = "pendlay",
#                          "resistivity" = "resistive",
#                          "kinetics" = "isokinetic",
#                          "fliers" = "fly",
#                          "font" = "dont",
#                          "selector" = "selectorized",
#                          "cal" = "calf",
#                          "dumbbells" = "dumbbell",
#                          "flatcar" = "flat bar",
#                          "gastronomic" = "gastrocnemius",
#                          "overboard" = "hoverboard",
#                          "playpen" = "pendlay",
#                          "pen delay" = "pendlay",
#                          "tr" = "trx",
#                          "virtual" = "vitruvian",
#                          "id" = "idk",
#                          "Maxine" = "machine",
#                          "precede" = "pec deck",
#                          "raid" = "raise",
#                          "pinnate" = "supinate",
#                          "trapezes" = "trapezius",
#                          "trapeziums" = "trapezius",
#                          "barb" = "barbell",
#                          "calf raises" = "calf raise",
#                          "chestfuls" = "chest fly",
#                          "extrasensory" = "extensor",
#                          "extensions" = "extension",
#                          "floors" = "flexor",
#                          "gluten" = "gluteal",
#                          "spelldown" = "lat pull down",
#                          "ply" = "Olympic",
#                          "peck" = "pec deck",
#                          "peddle" = "pendlay",
#                          "dependably" = "pendlay",
#                          "pen lay" = "pendlay",
#                          "preach" = "press",
#                          "detonated" = "pronated",
#                          "teases" = "seated",
#                          "selector" = "selectorized",
#                          "serrate" = "serratus",
#                          "ottoman" = "zottman"
# )
#
#
# ### Add checking the suggestions
#
# bad_whole_words2 <- paste0("\\b", bad_words2, "\\b")
#
# data_bigrams$word2 <- stri_replace_all_regex(data_bigrams$word2, bad_whole_words2, suggest_words2,
#                                              vectorize_all = FALSE)
#
# # for all double barrel terms split unnest again
# data_bigrams_word2 <-  data_bigrams |>
#   unnest_tokens(word2, word2)








library(tidygraph)

highschool


as_tbl_graph(highschool)

# new bigram counts:
bigram_counts <- data_bigrams |>
  group_by(book_exercise_name) |>
  count(word1, word2, sort = TRUE)

library(igraph)

bigram_graph <- bigram_counts |>
  filter(n > 5) |>
  as_tbl_graph()

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
  facet_graph(name) +
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
