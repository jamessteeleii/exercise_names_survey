# Read in survey key
read_key <- function() {
  key <- read_csv("data/Key.csv")
}

# Read in and prepare data
read_prepare_data <- function(key) {

  # Get file path and names
  data_path <- "./data"
  files <- dir(data_path, pattern = "Exercise Names Survey*")

  # Map reading them to a tibble
  data <- tibble(survey = files) |>
    mutate(file_contents = map(survey,
                               ~ read_csv(file.path(data_path, .)))
    )

  # Unnest and add survey name
  data_useful <- unnest(data, cols = file_contents) |>
    mutate(survey = case_when(survey == files[1] ~ "NSCA",
                              survey == files[2] ~ "Social Media",
                              survey == files[3] ~ "Virtruvian")
    )

  # Pivot longer so responses for both question types (recognise, name) are tidy
  data_long <- pivot_longer(
    data = data_useful,
    cols = c(24:65,67:74),
    names_to = "question",
    values_to = "response"

  )

  # Join with survey key
  data_long <- left_join(data_long, key, by="question")

  # Pivot question type (wording) wider so col for recognise and name
  data_wide_response <- data_long |>
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

    # Filter to only those who passed the attention check
    filter(Q16 == "Green")
}

# Create tokens
convert_to_tokens <- function(data) {
  # Let's create tokens
  data("stop_words")

  data_tokens <- data |>
    select(ResponseId, book_exercise_name,
           body_position, body_part, action, equipment, equipment_position, action_direction, misc,
           recognise, response_name) |>
    unnest_tokens(word, response_name) |>
    anti_join(stop_words) |>
    filter(!is.na(word) & !is.na(recognise))

  # Spelling errors - https://books.psychstat.org/textmining/data.html
  words <- unique(data_tokens$word)
  bad_words <- hunspell(words)
  bad_words <- unique(unlist(bad_words))
  suggest_words <- hunspell_suggest(bad_words)
  suggest_words <- unlist(lapply(suggest_words, function(x) x[1]))

        # # combine and compare suggestions (manually checked obvious errors)
        #
        # bad_suggest_words <- bind_cols(bad_words, suggest_words)
        #
        # count_words <- count(data_tokens, word)
        #
        # bad_suggest_words <- inner_join(count_words, bad_suggest_words, by = c(word = "...1"))

  # Recode the incorrect suggestions manually with more than 5 uses
  # (manually editing original if incorrect spelling)
  suggest_words <- recode(suggest_words,
                          "flue" = "fly",
                          "flyes" = "fly",
                          "pend lay" = "pendlay",
                          "probated" = "pronated",
                          "DEC" = "deck",
                          "hop" = "ohp",
                          "felt" = "delt",
                          "dead lift" = "deadlift",
                          "pull downs" = "pull down",
                          "antediluvian" = "vitruvian",
                          "soles" = "soleus",
                          "trice" = "tricep",
                          "pendent" = "pendlay",
                          "kinetics" = "isokinetic",
                          "fliers" = "flyers",
                          "font" = "dont",
                          "selector" = "selectorized"
  )


  ### Add checking the suggestions

  library(stringi)

  bad_whole_words <- paste0("\\b", bad_words, "\\b")

  data_tokens$word <- stri_replace_all_regex(data_tokens$word, bad_whole_words, suggest_words,
                                                   vectorize_all = FALSE)
}
