library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(purrr)
library(scales)

# base url
reply_all_url <- "https://www.gimletmedia.com/reply-all/all#all-episodes-list"

# get episode titles
episode_names <- reply_all_url %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  data.frame()

colnames(episode_names) <- "episode"

# filter out episodes
episode_names <- episode_names %>%
  filter(
    str_detect(episode, "episode-player"), # must have episode player link
    !str_detect(episode, "re-broadcast"), # no re-broadcasts
    !str_detect(episode, "rebroadcast"), # or rebroadcasts
    !str_detect(episode, "revisited"), # or revisits
    !str_detect(episode, "presents"), # or presentations of other shows
    !str_detect(episode, "introducing"), # or introductions of other shows
    !str_detect(episode, "-2#"), # or replays
    !str_detect(episode, "updated") # or updated versions of old episodes
  ) %>%
  distinct() # and after all of that, no duplicates

# get full URL link to episodes
ep_data <- episode_names %>%
  mutate(
    episode_link = paste0("https://www.gimletmedia.com", episode)
  )

getTranscript <- function(episode_link) {

  print(episode_link)

  # get the transcript from the webpage using the node '.episode-transcript'
  transcript <- episode_link %>%
    read_html() %>%
    html_nodes(".episode-transcript") %>%
    html_text()

  text <- setNames(data_frame(unlist(strsplit(transcript, "[^.a-z]+:", perl = T))), "text") %>%
    mutate(
      text = trimws(text),
      text = str_replace_all(text, "Transcript\n        ", "")
    ) %>%
    filter(
      text != "Transcript",
      text != "[Theme music",
      text != "[theme music",
      text != "[Intro Music"
    )

  ## ok this is gross, but handling some bad laling in specific episodes
  if (episode_link == "https://www.gimletmedia.com/reply-all/79-boy-in-photo#episode-player") {
    speaker <- rbind("PJ", data.frame(setNames(gsubfn::strapply(transcript, "[^.a-z]+:", c, perl = TRUE), "speaker"), stringsAsFactors = FALSE))
  } else if (episode_link == "https://www.gimletmedia.com/reply-all/52-raising-the-bar#episode-player") {
    speaker <- rbind("PJ", data.frame(setNames(gsubfn::strapply(transcript, "[^.a-z]+:", c, perl = TRUE), "speaker"), stringsAsFactors = FALSE))
    text <- text %>% mutate(text = str_replace_all(text, "Transcript\n        PJ Vogt: ", ""))
  } else if (episode_link == "https://www.gimletmedia.com/reply-all/31-bonus-the-reddit-implosion-explainer#episode-player") {
    speaker <- rbind("PJ", data.frame(setNames(gsubfn::strapply(transcript, "[^.a-z]+:", c, perl = TRUE), "speaker"), stringsAsFactors = FALSE))
  } else if (episode_link == "https://www.gimletmedia.com/reply-all/2-instagram-for-doctors#episode-player") {
    speaker <- rbind("PJ", data.frame(setNames(gsubfn::strapply(transcript, "[^.a-z]+:", c, perl = TRUE), "speaker"), stringsAsFactors = FALSE))
    text <- text %>% mutate(text = str_replace_all(text, "[THEME SONG]PJ Vogt: ", ""))
  } else {
    speaker <- setNames(gsubfn::strapply(transcript, "[^.a-z]+:", c, perl = TRUE), "speaker")
  }

  transcript_clean <- data.frame(text, speaker) %>%
    mutate(
      speaker = trimws(speaker),
      speaker = str_replace_all(speaker, "[^A-Z ]", ""),
      speaker = str_replace_all(speaker, "THEME MUSIC", ""),
      speaker = str_replace_all(speaker, "RING", ""),
      speaker = str_replace_all(speaker, "MUSIC", ""),
      speaker = str_replace_all(speaker, "BREAK", "")
    )

  # do some light cleaning of the transcript
  transcript_new <- transcript_clean %>%
    mutate(
      linenumber = row_number() # get the line number (1 is the first line, 2 is the second, etc.)
    ) %>%
    select(speaker, text, linenumber)

  # convert normal boring text into exciting cool tidy text
  transcript_tidy <- transcript_new %>%
    unnest_tokens(word, text)

  return(transcript_tidy)
}

# use purrr to map the 'getTranscript' function over all of the URLS in the ep_data data frame
# takes ~5 minutes to run
ep_data <- ep_data %>%
  mutate(
    transcript = map(episode_link, getTranscript)
  )

tidy_ep_data <- ep_data %>%
  unnest(transcript)

# import 'stop words' from tidytext, e.g. words like 'a' or 'the' or 'I'
data("stop_words")

# remove stop words with an anti_join
tidy_ep_data_clean <- tidy_ep_data %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words)

tidy_ep_data_clean <- tidy_ep_data_clean %>%
  mutate(
    speaker = trimws(speaker),
    speaker = case_when(
      speaker == "ALEX" ~ "ALEX GOLDMAN",
      speaker == "GOLDMAN" ~ "ALEX GOLDMAN",
      speaker == "AG" ~ "ALEX GOLDMAN",
      speaker == "PJ" ~ "PJ VOGT",
      speaker == "BLUMBERG" ~ "ALEX BLUMBERG",
      speaker == "SRUTHI" ~ "SRUTHI PINNAMANENI",
      TRUE ~ speaker
    )
  ) %>%
  filter(
    n() > 100,
    speaker != "FADES OUTCREDITS SONG PLAYSCREDITS",
    speaker != "CREDITSALEX",
    speaker != "CREDITSALEX GOLDMAN",
    speaker != "CREDITSPJ",
    speaker != "AD PJ",
    speaker != "ADPJ",
    speaker != "OUTPJ"
  )

# quick check of the top words accross all episodes
tidy_ep_data_clean %>%
  group_by(word) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 500)

# plot words used 1000 times
tidy_ep_data_clean %>% group_by(speaker) %>%
  group_by(word) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 500) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
      title = "Words used in Reply All Episodes",
      x = "",
      y = "Word Frequency"
    ) +
    theme_bw(base_size = 16)

# Compare word frequency
frequency <- tidy_ep_data_clean %>%
  filter(speaker %in% c("ALEX GOLDMAN", "PJ VOGT")) %>%
  count(speaker, word) %>%
  group_by(speaker) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(speaker, proportion)

# Compare word frequency plot
ggplot(frequency, aes(x = `PJ VOGT`, y = `ALEX GOLDMAN`, color = abs(`ALEX GOLDMAN` - `PJ VOGT`))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(y = "Alex", x = "PJ", title = "Comparing word frequencies of PJ and Alex")

sentiment_data <- tidy_ep_data_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(episode, index = linenumber %/% 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(sentiment_data, aes(index, sentiment, fill = episode)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode, scales = "free_x")

# contribution to sentiment
bing_word_counts <- tidy_ep_data_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    y = "\nContribution to sentiment",
    x = NULL
    ) +
  coord_flip() +
  theme_bw()
