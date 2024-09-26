# Read in survey key
read_key <- function(key_file) {
  key <- read_csv(key_file)
}

# Read in and prepare data
read_prepare_data <- function(key) {
  # Get file path and names
  data_path <- "./data"
  files <- dir(data_path, pattern = "Exercise Names Survey*")

  # Map reading them to a tibble
  data <- tibble(survey = files) |>
    mutate(file_contents = map(survey,
                               ~ read_csv(file.path(data_path, .))))

  # Unnest and add survey name
  data <- unnest(data, cols = file_contents) |>
    mutate(
      survey = case_when(
        survey == files[1] ~ "NSCA",
        survey == files[2] ~ "Social Media",
        survey == files[3] ~ "Vitruvian"
      )
    ) |>

    # Pivot longer so responses for both question types (recognise, name) are tidy
    pivot_longer(
      cols = c(23:64, 66:73),
      names_to = "question",
      values_to = "response"
    )

  # Join with survey key
  data <- left_join(data, key, by = "question")

  # Pivot question type (wording) wider so col for recognise and name
  data <- data |>
    mutate(
      question_wording = case_when(
        question_wording == "What do you personally call this exercise? If you are unsure or do not recognise the exercise, what is your best guess as to what this exercise is called? Type your answer in the space below."
        ~ "response_name",
        question_wording == "Do you recognise this exercise?"
        ~ "recognise"
      )
    ) |>
    pivot_wider(
      id_cols = c(ResponseId, book_exercise_name),
      names_from = question_wording,
      values_from = response,
      unused_fn = max
    ) |>

    # Filter to only those who passed the attention check
    filter(Q16 == "Green",

           # And for English is their primary language for discussing exercise
           Q5 == "Yes")
}

# Create demographic characteristics data table
make_demographics_tbl <- function(data) {
  q14_split <- data |>
    select(ResponseId, Q14) |>
    group_by(ResponseId) |>
    slice_head(n=1) |>
    separate(Q14, into = c("a","b","c","d","e"), sep = ",") |>
    pivot_longer(2:6,
                 names_to = "x",
                 values_to = "populations") |>
    mutate(populations = case_when(
      populations == " otherwise healthy)" ~ NA,
      populations == "General population (non-clinical" ~ "genpop",
      populations == "Athletes" ~ "athletes",
      populations == "Clinical populations" ~ "clinpop",
      populations == "I have never instructed or prescribed the use of this type of exercise for others" ~ "none"
    )) |>
    filter(!is.na(populations)) |>
    dummy_cols("populations") |>
    group_by(ResponseId) |>
    summarise(athletes = sum(populations_athletes),
              genpop = sum(populations_genpop),
              clinpop = sum(populations_clinpop),
              none = sum(populations_none))

  tbl <- data |>
    group_by(ResponseId) |>
    slice_head(n=1) |>
    left_join(q14_split, by = "ResponseId") |>
    ungroup() |>
    select(Q2, Q3, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, athletes, genpop, clinpop, none, Q15) |>
    mutate(Q8 = as.numeric(case_when(
      Q8 == "0 days per week" ~ 0,
      Q8 == "1 day per week" ~ 1,
      Q8 == "2 days per week" ~ 2,
      Q8 == "3 days per week" ~ 3,
      Q8 == "4 days per week" ~ 4,
      Q8 == "5 days per week" ~ 5,
      Q8 == "6 days per week" ~ 6,
      Q8 == "7 days per week" ~ 7
    ))) |>
    rename(`Biological sex` = "Q2",
           `Age (years)` = "Q3",
           `Highest level of education` = "Q6",
           `University-level qualification in a field related to exercise, physical activity, or sport?` = "Q7",
           `Typical resistance training frequency (days)` = "Q8",
           `Resistance training experience (years)` = "Q9",
           `Participated in a weightlifting, powerlifting, or strongman competition?` = "Q10",
           `Participated in a bodybuilding competition or other physique-based competition?` = "Q11",
           `Ever employed in a job involving instruction and/or prescription of resistance training` = "Q12",
           `Holds a certification/license qualifying instruction and/or prescription of resistance training` = "Q13",
           `Instructed and/or prescribed resistance training for athletes` = "athletes",
           `Instructed and/or prescribed resistance training for the general population` = "genpop",
           `Instructed and/or prescribed resistance training for clinical populations` = "clinpop",
           `Never instructed and/or prescribed resistance training` = "none",
           `Current or most recent job role` = "Q15"
    )

  tbl
}

# Create tokens
convert_to_tokens <- function(data) {
  # Let's create tokens
  data("stop_words")

  data_tokens <- data |>
    select(
      ResponseId,
      book_exercise_name,
      body_position,
      body_part,
      action,
      equipment,
      equipment_position,
      action_direction,
      misc,
      recognise,
      response_name
    ) |>
    unnest_tokens(word, response_name) |>
    anti_join(stop_words) |>
    filter(!is.na(word) & !is.na(recognise)) |>
    mutate(recognise = factor(recognise, levels = c("YES", "NO")))

  # spelling errors - https://books.psychstat.org/textmining/data.html
  words <- unique(data_tokens$word)
  bad_words <- hunspell(words)
  bad_words <- unique(unlist(bad_words))
  suggest_words <- hunspell_suggest(bad_words)
  suggest_words <- unlist(lapply(suggest_words, function(x)
    x[1]))

  # # # combine and compare suggestions (manually checked obvious errors) - commented out, but left for anyone that wants to check
  # #
  # bad_suggest_words <- bind_cols(bad_words, suggest_words)
  #
  # count_words <- count(data_tokens, word)
  #
  # bad_suggest_words <- inner_join(count_words, bad_suggest_words, by = c(word = "...1"))

  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words <- recode(
    suggest_words,
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


  ### Add the suggestions

  bad_whole_words <- paste0("\\b", bad_words, "\\b")

  data_tokens$word <-
    stri_replace_all_regex(data_tokens$word,
                           bad_whole_words,
                           suggest_words,
                           vectorize_all = FALSE)

  # for all double barrel terms split and unnest again
  data_tokens <-  data_tokens |>
    unnest_tokens(word, word) |>
    filter(!is.na(word)) |>
    # filter(!row_number() %in% 34680) |>
    # slice_head(n=34680) |>
    rowwise() |>
    # Singularise plural words
    mutate(word = SemNetCleaner::singularize(word, dictionary = TRUE))

  # For some reason it changes "press" to "pres" and "raises" to "rais" so we change back manually
  data_tokens$word <- recode(data_tokens$word,
                             "pres" = "press",
                             "rais" = "raise")
  data_tokens
}

# Create bigrams
convert_to_bigrams <- function(data) {
  # Let's create bigrams
  data("stop_words")

  data_bigrams <- data |>
    select(
      ResponseId,
      book_exercise_name,
      body_position,
      body_part,
      action,
      equipment,
      equipment_position,
      action_direction,
      misc,
      recognise,
      response_name
    ) |>
    unnest_tokens(bigram, response_name, token = "ngrams", n = 2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word) |>
    filter(!word2 %in% stop_words$word) |>
    filter(!is.na(word1)) |>
    filter(!is.na(word2)) |>
    rowid_to_column()

  # spelling errors - https://books.psychstat.org/textmining/data.html
  words1 <- unique(data_bigrams$word1)
  bad_words1 <- hunspell(words1)
  bad_words1 <- unique(unlist(bad_words1))
  suggest_words1 <- hunspell_suggest(bad_words1)
  suggest_words1 <- unlist(lapply(suggest_words1, function(x)
    x[1]))

  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words1 <- recode(
    suggest_words1,
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

  bad_whole_words1 <- paste0("\\b", bad_words1, "\\b")


  data_bigrams$word1 <-
    stri_replace_all_regex(data_bigrams$word1,
                           bad_whole_words1,
                           suggest_words1,
                           vectorize_all = FALSE)

  # spelling errors - https://books.psychstat.org/textmining/data.html
  words2 <- unique(data_bigrams$word2)
  bad_words2 <- hunspell(words2)
  bad_words2 <- unique(unlist(bad_words2))
  suggest_words2 <- hunspell_suggest(bad_words2)
  suggest_words2 <- unlist(lapply(suggest_words2, function(x)
    x[1]))

  # Recode the incorrect suggestions manually with more than 2 uses
  # (manually editing original if obvious incorrect spelling)
  suggest_words2 <- recode(
    suggest_words2,
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

  bad_whole_words2 <- paste0("\\b", bad_words2, "\\b")

  data_bigrams$word2 <-
    stri_replace_all_regex(data_bigrams$word2,
                           bad_whole_words2,
                           suggest_words2,
                           vectorize_all = FALSE)

  data_bigrams <- data_bigrams |>
    unite(response_name, c("word1", "word2"), sep = " ") |>
    unnest_tokens(bigram, response_name, token = "ngrams", n = 2) |>
    separate(bigram, into = c("word1", "word2"), sep = " ") |>
    filter(!word1 %in% stop_words$word) |>
    filter(!word2 %in% stop_words$word) |>
    mutate(word1 = na_if(word1, "na"),
           word2 = na_if(word2, "na")) |>
    filter(!is.na(word1)) |>
    filter(!is.na(word2)) |>
    rowwise() |>
    mutate(
      word1 = SemNetCleaner::singularize(word1, dictionary = TRUE),
      word2 = SemNetCleaner::singularize(word2, dictionary = TRUE)
    )

  # for some reason it changes "press" to "pres" and "raises" to "rais" so we change back
  data_bigrams$word1 <- recode(data_bigrams$word1,
                               "pres" = "press",
                               "rais" = "raise")
  data_bigrams$word2 <- recode(data_bigrams$word2,
                               "pres" = "press",
                               "rais" = "raise")

  data_bigrams

}

# Plot simple counts and frequencies for all data
plot_counts_freqs <- function(tokens) {
  # Plot simple counts
  simple_count_plot <- tokens |>
    count(word, sort = TRUE) |>
    mutate(word = reorder(word, n)) |>
    ungroup() |>
    slice(1:20) |>
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL,
         x = "Count (n)",
         title = "Top twenty words used") +
    scale_y_discrete(limits = rev) +
    theme_classic()

  # tf as proportion of all words based on recognition

  count_words <- tokens |>
    count(word, sort = TRUE) |>
    ungroup() |>
    mutate(total = sum(n))

  tf_plot <- ggplot(count_words, aes(n / total)) +
    geom_histogram(show.legend = FALSE) +
    scale_y_continuous(trans = scales::pseudo_log_trans()) +
    labs(y = "Count (n)",
         x = "Term Frequency (n/total)",
         title = "Term frequency of words used") +
    theme_bw()


  # Zipfs law

  freq_by_rank <- count_words |>
    mutate(
      rank = row_number(),
      tf = n / total,
      log10_rank = log10(rank),
      log10_tf = log10(tf)
    ) |>
    ungroup()

  lm <- lm(log10_tf ~ log10_rank, data = freq_by_rank)

  zipf_preds <- predictions(lm)

  zipf_plot <- freq_by_rank |>
    ggplot() +
    geom_line(
      aes(x = rank, y = tf),
      size = 1.1,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = zipf_preds,
      aes(
        x = 10 ^ log10_rank,
        ymin = 10 ^ conf.low,
        ymax = 10 ^ conf.high
      ),
      alpha = 0.25
    ) +
    geom_line(data = zipf_preds,
              aes(x = 10 ^ log10_rank, y = 10 ^ estimate),
              linetype = "dashed") +
    labs(x = "Term Rank (log10 scale)",
         y = "Term Frequency (proportion on log10 scale)",
         title = "Relationship of term frequency and rank for words used") +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()

  (simple_count_plot + tf_plot + zipf_plot) &
    theme(title = element_text(size = 9))
}

# Plot frequencies for data split by recognition
plot_freqs_recognise <- function(tokens) {
  # tf as proportion of all words based on recognition

  recognise_words <- tokens |>
    count(recognise, word, sort = TRUE) |>
    ungroup() |>
    mutate(total = sum(n))

  tf_recognise_plot <-
    ggplot(recognise_words, aes(n / total, fill = recognise)) +
    geom_histogram(show.legend = FALSE) +
    facet_wrap("recognise") +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(trans = scales::pseudo_log_trans()) +
    labs(y = "Count (n)",
         x = "Term Frequency (n/total)",
         title = "Term frequency of words used") +
    theme_bw()


  # Zipfs law

  freq_by_rank <- recognise_words |>
    group_by(recognise) |>
    mutate(
      rank = row_number(),
      tf = n / total,
      log10_rank = log10(rank),
      log10_tf = log10(tf)
    ) |>
    ungroup()

  lm <- lm(log10_tf ~ log10_rank * recognise , data = freq_by_rank)

  zipf_preds <- predictions(lm,
                            variables = list(recognise = c("NO", "YES")))

  zipf_recognise_plot <- freq_by_rank |>
    ggplot() +
    geom_line(
      aes(x = rank, y = tf, color = recognise),
      size = 1.1,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = zipf_preds,
      aes(
        x = 10 ^ log10_rank,
        ymin = 10 ^ conf.low,
        ymax = 10 ^ conf.high
      ),
      alpha = 0.25
    ) +
    geom_line(data = zipf_preds,
              aes(x = 10 ^ log10_rank, y = 10 ^ estimate),
              linetype = "dashed") +
    facet_wrap("recognise") +
    labs(x = "Term Rank (log10 scale)",
         y = "Term Frequency (proportion on log10 scale)",
         title = "Relationship of term frequency and rank for words used") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()

  (tf_recognise_plot + zipf_recognise_plot)
}

# Plot frequencies for data split by exercise (only recognised words)
plot_freqs_exercise <- function(tokens) {
  # tf as proportion of all words for exercise (only recognised words)

  exercise_words <- tokens |>
    filter(recognise == "YES") |>
    count(book_exercise_name, word, sort = TRUE)

  total_words <- exercise_words |>
    group_by(book_exercise_name) |>
    summarize(total = sum(n))

  exercise_words <- left_join(exercise_words, total_words)

  tf_exercise_plot <- ggplot(exercise_words, aes(n / total)) +
    geom_histogram(show.legend = FALSE) +
    facet_wrap("book_exercise_name", scales = "free") +
    scale_y_continuous(trans = scales::pseudo_log_trans()) +
    labs(y = "Count (n)",
         x = "Term Frequency (n/total)",
         title = "Term frequency of words used") +
    theme_bw()

  # Zipfs law

  freq_by_rank <- exercise_words |>
    group_by(book_exercise_name) |>
    mutate(
      rank = row_number(),
      tf = n / total,
      log10_rank = log10(rank),
      log10_tf = log10(tf)
    ) |>
    ungroup()

  lm <-
    lm(log10_tf ~ log10_rank * book_exercise_name, data = freq_by_rank)

  zipf_preds <- predictions(lm,
                            variables = list(book_exercise_name = unique(freq_by_rank$book_exercise_name)))

  zipf_exercise_plot <- freq_by_rank |>
    ggplot() +
    geom_line(
      aes(x = rank, y = tf),
      size = 1.1,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = zipf_preds,
      aes(
        x = 10 ^ log10_rank,
        ymin = 10 ^ conf.low,
        ymax = 10 ^ conf.high
      ),
      alpha = 0.25
    ) +
    geom_line(data = zipf_preds,
              aes(x = 10 ^ log10_rank, y = 10 ^ estimate),
              linetype = "dashed") +
    labs(x = "Term Rank (log10 scale)",
         y = "Term Frequency (proportion on log10 scale)",
         title = "Relationship of term frequency and rank for words used") +
    facet_wrap("book_exercise_name") +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()

  (tf_exercise_plot + zipf_exercise_plot) &
    theme(strip.text = element_text(size = 7))
}

# Plot frequencies for data split by exercise (only recognised words)
plot_freqs_exercise_recognise <- function(tokens) {
  # tf as proportion of all words for exercise (both recognised and unrecognised words)

  exercise_recognise_words <- tokens |>
    count(book_exercise_name, recognise, word, sort = TRUE)

  total_words <- exercise_recognise_words |>
    group_by(book_exercise_name) |>
    summarize(total = sum(n))

  exercise_recognise_words <-
    left_join(exercise_recognise_words, total_words)

  tf_exercise_recognise_plot <-
    ggplot(exercise_recognise_words, aes(n / total, fill = recognise)) +
    geom_histogram(alpha = 0.5) +
    facet_wrap("book_exercise_name", scales = "free") +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(trans = scales::pseudo_log_trans()) +
    labs(
      x = "Proportion of Total Words",
      y = "Count (log10 scale)",
      fill = "Recognised\nExercise?",
      title = "Term frequency of words used"
    ) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 4))

  # Zipfs law

  freq_by_rank <- exercise_recognise_words |>
    group_by(book_exercise_name, recognise) |>
    mutate(
      rank = row_number(),
      tf = n / total,
      log10_rank = log10(rank),
      log10_tf = log10(tf)
    ) |>
    ungroup()

  lm <-
    lm(log10_tf ~ log10_rank * book_exercise_name * recognise, data = freq_by_rank)

  zipf_preds <- predictions(lm,
                            variables = list(
                              book_exercise_name = unique(freq_by_rank$book_exercise_name),
                              recognise = c("NO", "YES")
                            ))

  zipf_exercise_recognise_plot <- freq_by_rank |>
    ggplot() +
    geom_line(
      aes(x = rank, y = tf, color = recognise),
      size = 1.1,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = zipf_preds,
      aes(
        x = 10 ^ log10_rank,
        ymin = 10 ^ conf.low,
        ymax = 10 ^ conf.high,
        group = recognise
      ),
      alpha = 0.25
    ) +
    geom_line(
      data = zipf_preds,
      aes(
        x = 10 ^ log10_rank,
        y = 10 ^ estimate,
        color = recognise
      ),
      linetype = "dashed"
    ) +
    labs(
      y = "Term Frequency (proportion on log10 scale)",
      x = "Term Rank (log10 scale)",
      color = "Recognised\nExercise?",
      title = "Relationship of term frequency and rank for words used"
    ) +
    facet_wrap("book_exercise_name") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_log10() +
    scale_y_log10() +
    guides(color = "none") +
    theme_bw()

  (tf_exercise_recognise_plot + zipf_exercise_recognise_plot) +
    plot_layout(guides = "collect") &
    theme(strip.text = element_text(size = 7))
}

# Plot the term frequency-inverse exercise frequency for recognised exercises
plot_tf_ief <- function(tokens) {
  exercise_words <- tokens |>
    filter(recognise == "YES") |>
    count(book_exercise_name, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_ief <- exercise_words |>
    bind_tf_idf(word, book_exercise_name, n)

  tf_ief_plot <- tf_ief |>
    group_by(book_exercise_name) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, book_exercise_name)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Exercise Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse exercise frequency",
      subtitle = "Measure of how important a word is for naming a particular exercise in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ book_exercise_name, scales = "free") +
    theme_bw() +
    theme(strip.text = element_text(size = 5))

}

# Plot the term frequency-inverse body position frequency for recognised exercises
plot_tf_ibpf <- function(tokens) {
  body_position_words <- tokens |>
    filter(recognise == "YES") |>
    count(body_position, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_ibpf <- body_position_words |>
    bind_tf_idf(word, body_position, n)

  tf_ibpf_plot <- tf_ibpf |>
    group_by(body_position) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, body_position)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Body Position Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse body position frequency",
      subtitle = "Measure of how important a word is for naming an exercise based on body position in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ body_position, scales = "free") +
    theme_bw()

}

# Plot the term frequency-inverse body part frequency for recognised exercises
plot_tf_ibpartf <- function(tokens) {
  body_part_words <- tokens |>
    filter(recognise == "YES") |>
    separate(body_part, into = c("a","b","c"), sep = ",") |>
    pivot_longer(c(a,b,c),
                 names_to = "x",
                 values_to = "body_part") |>
    filter(!is.na(body_part)) |>
    count(body_part, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_ibpartf <- body_part_words |>
    bind_tf_idf(word, body_part, n)

  tf_ibpartf_plot <- tf_ibpartf |>
    group_by(body_part) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, body_part)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Body Part Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse body part frequency",
      subtitle = "Measure of how important a word is for naming an exercise based on body part in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ body_part, scales = "free") +
    theme_bw()

}

# Plot the term frequency-inverse action frequency for recognised exercises
plot_tf_iaf <- function(tokens) {
  action_words <- tokens |>
    filter(recognise == "YES") |>
    count(action, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_iaf <- action_words |>
    bind_tf_idf(word, action, n)

  tf_iaf_plot <- tf_iaf |>
    group_by(action) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, action)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Action Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse action frequency",
      subtitle = "Measure of how important a word is for naming an exercise based on action performed in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ action, scales = "free") +
    theme_bw()

}

# Plot the term frequency-inverse action frequency for recognised exercises
plot_tf_iadf <- function(tokens) {
  action_direction_words <- tokens |>
    filter(recognise == "YES") |>
    count(action_direction, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_iadf <- action_direction_words |>
    bind_tf_idf(word, action_direction, n)

  tf_iadf_plot <- tf_iadf |>
    group_by(action_direction) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, action_direction)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Action Direction Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse action direction frequency",
      subtitle = "Measure of how important a word is for naming an exercise based on action direction performed in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ action_direction, scales = "free") +
    theme_bw()

}

# Plot the term frequency-inverse equipment frequency for recognised exercises
plot_tf_iequipf <- function(tokens) {
  equipment_words <- tokens |>
    filter(recognise == "YES") |>
    count(equipment, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_iequipf <- equipment_words |>
    bind_tf_idf(word, equipment, n)

  tf_iequipf_plot <- tf_iequipf |>
    group_by(equipment) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, equipment)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Equipment Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse equipment frequency",
      subtitle = "Measure of how important a word is for naming an exercise based on equipment used in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ equipment, scales = "free") +
    theme_bw()
}

# Plot the term frequency-inverse equipment frequency for recognised exercises
plot_tf_ibuf <- function(tokens) {
  bi_uni_words <- tokens |>
    filter(recognise == "YES") |>
    count(misc, word, sort = TRUE)

  # inverse exercise frequency for recognised exercises
  tf_ibuf <- bi_uni_words |>
    filter(!is.na(misc)) |>
    bind_tf_idf(word, misc, n)

  tf_ibuf_plot <- tf_ibuf |>
    group_by(misc) |>
    top_n(5) |>
    ggplot(aes(
      x = tf_idf,
      y = reorder_within(word, tf_idf, misc)
    )) +
    geom_col(show.legend = FALSE) +
    labs(
      x = "Term Frequency - Inverse Bilateral/Unilateral Frequency",
      y = NULL,
      title = "Top five words by term frequency - inverse bilateral/unilateral frequency",
      subtitle = "Measure of how important a word is for naming an exercise based on whether exercise is bilateral/unilateral in the collection of exercises examined"
    ) +
    scale_y_reordered() +
    facet_wrap( ~ misc, scales = "free") +
    theme_bw()
}

# Plot the bigram frequencies for recognised
plot_recognise_bigrams <- function(bigrams) {
  # bigram plots

  exercises <- sort(unique(bigrams$book_exercise_name))

  bigram_plots <- list()

  for(i in exercises) {
    bigram_counts <- bigrams |>
      filter(recognise == "YES") |>
      filter(book_exercise_name == i) |>
      count(word1, word2, sort = TRUE) |>
      select(word1, word2, n) |>
      ungroup() |>
      mutate(total = sum(n)) |>
      slice_max(n, n = 5) |>
      as_tbl_graph() |>
      mutate(title = paste(i))

    set.seed(2020)

    a <- grid::arrow(length = unit(.1, "inches"))

    bigram_plot <- ggraph(bigram_counts, layout = 'grid') +
      geom_edge_arc(aes(edge_colour = n/total,
                        start_cap = label_rect(node1.name),
                        end_cap = label_rect(node2.name)),
                    show.legend = FALSE,
                    strength = 0.5,
                    arrow = arrow(angle = 30, type = "closed", length = unit(2, 'mm')))  +
      geom_node_point(color = NA, size = 10) +
      geom_node_text(aes(label = name), size = 3) +
      facet_grid(.~title) +
      scale_x_continuous(expand = expansion(mult=0.25)) +
      scale_y_continuous(expand = expansion(mult=0.25)) +
      scale_edge_color_gradient(low = "grey", high = "black") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(t = 5,  # Top margin
                                 r = 5,  # Right margin
                                 b = 5,  # Bottom margin
                                 l = 5)) # Left margin

    bigram_plots[[i]] <- bigram_plot

  }

  bigram_plots$`back squat`


  (
    bigram_plots$`back squat` +
      bigram_plots$`barbell biceps curl` +
      bigram_plots$`bent-over row` +
      bigram_plots$`flat barbell bench press` +
      bigram_plots$`flat dumbbell fly` +
      bigram_plots$`hammer curl` +
      bigram_plots$`hip sled (machine)` +
      bigram_plots$`incline dumbbell bench press` +
      bigram_plots$`lat pulldown (machine)` +
      bigram_plots$`lateral shoulder raise` +
      bigram_plots$`leg (knee) extension (machine)` +
      bigram_plots$`one-arm dumbbell row` +
      bigram_plots$`pec deck (machine)` +
      bigram_plots$`seated barbell shoulder press` +
      bigram_plots$`seated calf (heel) raise (machine)` +
      bigram_plots$`seated row (machine)` +
      bigram_plots$`shoulder press (machine)` +
      bigram_plots$`standing calf (heel) raise (machine)` +
      bigram_plots$`upright row` +
      bigram_plots$`vertical chest press (machine)` +
      bigram_plots$`vitruvian back squat with bar` +
      bigram_plots$`vitruvian bench press with bar` +
      bigram_plots$`vitruvian bent-over row with bar` +
      bigram_plots$`vitruvian biceps curl with bar` +
      bigram_plots$`wrist curl`
  ) +
    plot_annotation(title = "Top five bigrams by frequency for those who did recognise the exercise",
                    caption = "Note, bigrams with higher frequencies indicated by darker edges between nodes")
}

# Plot the bigram frequencies for not recognised
plot_did_not_recognise_bigrams <- function(bigrams) {
  # bigram plots

  exercises <- sort(unique(bigrams$book_exercise_name))

  bigram_plots <- list()

  for(i in exercises) {
    bigram_counts <- bigrams |>
      filter(recognise == "NO") |>
      filter(book_exercise_name == i) |>
      count(word1, word2, sort = TRUE) |>
      select(word1, word2, n) |>
      ungroup() |>
      mutate(total = sum(n)) |>
      slice_max(n, n = 5) |>
      as_tbl_graph() |>
      mutate(title = paste(i))

    set.seed(2020)

    a <- grid::arrow(length = unit(.1, "inches"))

    bigram_plot <- ggraph(bigram_counts, layout = 'grid') +
      geom_edge_arc(aes(edge_colour = n/total,
                        start_cap = label_rect(node1.name),
                        end_cap = label_rect(node2.name)),
                    show.legend = FALSE,
                    strength = 0.5,
                    arrow = arrow(angle = 30, type = "closed", length = unit(2, 'mm')))  +
      geom_node_point(color = NA, size = 10) +
      geom_node_text(aes(label = name), size = 3) +
      facet_grid(.~title) +
      scale_x_continuous(expand = expansion(mult=0.25)) +
      scale_y_continuous(expand = expansion(mult=0.25)) +
      scale_edge_color_gradient(low = "grey", high = "black") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(t = 5,  # Top margin
                                 r = 5,  # Right margin
                                 b = 5,  # Bottom margin
                                 l = 5)) # Left margin

    bigram_plots[[i]] <- bigram_plot

  }

  bigram_plots$`back squat`


  (
    bigram_plots$`back squat` +
      bigram_plots$`barbell biceps curl` +
      bigram_plots$`bent-over row` +
      tibble(title = "flat barbell bench press") |>
      ggplot() +
      facet_grid(.~title) +
      theme_bw() +
      annotate("text", label = "All respondents recognised this exercise",
               x = 0.5, y = 0.5, size = 3) +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(t = 5,  # Top margin
                                 r = 5,  # Right margin
                                 b = 5,  # Bottom margin
                                 l = 5)  # Left margin
            ) +
      bigram_plots$`flat dumbbell fly` +
      bigram_plots$`hammer curl` +
      bigram_plots$`hip sled (machine)` +
      bigram_plots$`incline dumbbell bench press` +
      bigram_plots$`lat pulldown (machine)` +
      bigram_plots$`lateral shoulder raise` +
      bigram_plots$`leg (knee) extension (machine)` +
      bigram_plots$`one-arm dumbbell row` +
      bigram_plots$`pec deck (machine)` +
      bigram_plots$`seated barbell shoulder press` +
      bigram_plots$`seated calf (heel) raise (machine)` +
      bigram_plots$`seated row (machine)` +
      bigram_plots$`shoulder press (machine)` +
      bigram_plots$`standing calf (heel) raise (machine)` +
      bigram_plots$`upright row` +
      bigram_plots$`vertical chest press (machine)` +
      bigram_plots$`vitruvian back squat with bar` +
      bigram_plots$`vitruvian bench press with bar` +
      bigram_plots$`vitruvian bent-over row with bar` +
      bigram_plots$`vitruvian biceps curl with bar` +
      bigram_plots$`wrist curl`
  ) +
    plot_annotation(title = "Top five bigrams by frequency for those who did not recognise the exercise",
                    caption = "Note, bigrams with higher frequencies indicated by darker edges between nodes")
}

# Plot likert questions about exercise names
plot_likert_exercise_names <- function(data) {
  data |>
    group_by(ResponseId) |>
    slice_head(n=1) |>
    ungroup() |>
    select(Q68_1,Q68_2,Q68_3,Q68_4,Q68_5) |>
    rename(
      "Exercise names are important" = Q68_1,
      "Exercises are named inconsistently" =  Q68_2,
      "Exercise names impact how information about exercise is learned" = Q68_3,
      "I sometimes call the same exercise by different names" = Q68_4,
      "A system that standardizes exercise names would be beneficial" = Q68_5
    ) |>
    mutate(across(everything(), ~ factor(.x, levels = c(
      "Strongly disagree",
      "Disagree",
      "Neutral",
      "Agree",
      "Strongly agree"
    )))) |>
    gglikert(totals_accuracy = TRUE) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 25))
}
