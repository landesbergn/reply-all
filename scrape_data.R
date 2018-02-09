library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(purrr)

# base url
reply_all_url <- "https://gimletmedia.com/show/reply-all/all/"

# get nav bar to determine how many pages to scrape
nav <- reply_all_url %>%
  read_html() %>%
  html_nodes("nav.feed__pagination") %>%
  html_text() %>%
  str_split("\\\n")

nav <- as.numeric(nav[[1]])
max_page <- max(na.omit(nav))

# scrape page for episode names

page_1_url <- "https://gimletmedia.com/show/reply-all/all/"

episode_names <- page_1_url %>%
  read_html() %>%
  html_nodes("h3") %>%
  html_text() %>%
  str_replace_all("[\\\n]", "") %>%
  str_replace_all("[\\\t]", "") %>%
  data.frame()

colnames(episode_names) <- "episode"

episode_names <- episode_names %>% filter(str_detect(episode, "\\#"))

episode_link <- episode_names$episode %>%
  str_replace_all("#", "") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all(" of ", "") %>%
  str_replace_all("the", "") %>%
  str_replace_all("the", "") %>%
  str_replace_all("The ", "") %>%
  str_replace(" I$", "") %>%
  str_to_lower() %>%
  str_replace_all(" ", "-")

episode_link

ep_data <- data.frame(episode_names, episode_link) %>%
  mutate(episode_link_full = paste0("https://gimletmedia.com/episode/", episode_link, "/"))

transcript <- ep_data$episode_link_full[1] %>%
  read_html() %>%
  html_nodes("div.episode__transcript") %>%
  html_text() %>%
  str_split("\\\n") %>%
  data.frame(stringsAsFactors = FALSE)

colnames(transcript) <- "line"

transcript$line <- transcript$line %>%
  str_replace_all("[\\\n]", "") %>%
  str_replace_all("[\\\t]", "") %>%
  str_trim()

transcript_new <- transcript %>%
  filter(
    nchar(line) > 0,
    !str_detect(line, "^\\[")
  ) %>%
  mutate(
    linenumber = row_number(),
    speaker = str_extract(line, "^[[:upper:]]+"),
    line = str_replace_all(line, "^[[:upper:]]+ [[:upper:]]+:", ""),
    text = str_replace_all(line, "^[[:upper:]]+:", "")
  ) %>%
  select(speaker, text, linenumber)

transcript_tidy <- transcript_new %>%
  unnest_tokens(word, text)

data("stop_words")
transcript_clean <- transcript_tidy %>%
  anti_join(stop_words)

transcript_clean %>%
  count(word, sort = TRUE)

bing <- get_sentiments("bing")

reply_all_sentiment <- transcript_tidy %>%
  inner_join(bing) %>%
  count(index = linenumber %/% 5, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(reply_all_sentiment, aes(index, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal()

##########
##########
########## It was at this point... gimlet changed their web pages ########
##########
##########

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


# function to get a nicely formatted transcript
#   input: full URL to page of individiaul episode
#   output: list, which has a 'tidy' version of the episode transcript
getTranscript <- function(episode_link) {

  # get the transcript from the webpage using the node '.episode-transcript'
  transcript <- episode_link %>%
    read_html() %>%
    html_nodes(".episode-transcript") %>%
    html_text() %>%
    str_split("\\\n") %>% # split the transcript into more manageable blocks (1 line)
    data.frame(stringsAsFactors = FALSE) # turn to a DF for easier manipulation

  colnames(transcript) <- "line"

  # remove web formatting for new lines or tabs
  transcript$line <- transcript$line %>%
    str_replace_all("[\\\n]", "") %>% # remove new lines
    str_replace_all("[\\\t]", "") %>% # remove tabs
    str_trim() # remove leading and trailing whitespace

  # do some light cleaning of the transcript
  transcript_new <- transcript %>%
    filter(
      nchar(line) > 0, # make sure lines have _some_ content in them
      !str_detect(line, "^\\[") # make sure the line isn't non-spoken, aka '[Laughs']
    ) %>%
    mutate(
      linenumber = row_number(), # get the line number (1 is the first line, 2 is the second, etc.)
      speaker = str_extract(line, "^[[:upper:]]+"), # speaker of the line is refered to at the start of a line, e.g. '[ALEX]' or '[PJ]'
      line = str_replace_all(line, "^[[:upper:]]+ [[:upper:]]+:", ""), # get rid of extra crap in lines TODO find exmaple
      text = str_replace_all(line, "^[[:upper:]]+:", "") # TODO what is this again?
    ) %>%
    select(speaker, text, linenumber)

  # convert normal boring text into exciting cool tidy text
  transcript_tidy <- transcript_new %>%
    unnest_tokens(word, text)

  return(transcript_tidy)
}


pb <- progress_estimated(nrow(ep_data$episode_link))

# use purrr to map the 'getTranscript' function over all of the URLS in the ep_data data frame
ep_data <- ep_data %>%
  mutate(
    transcript = map(episode_link, getTranscript)
  )

# import 'stop words' from tidytext, e.g. words like 'a' or 'the' or 'I'
data("stop_words")

# remove stop words with an anti_join
transcript_clean <- transcript_tidy %>%
  anti_join(stop_words)

# quick check of the top words
transcript_clean %>%
  count(word, sort = TRUE)

# import sentiment information, via tidytext
bing <- get_sentiments("bing")

# create a dataframe the joins words to sentiment, divides sentiment
#   accross the length of the transcript, and organizes the data for plotting
reply_all_sentiment <- transcript_tidy %>%
  inner_join(bing) %>%
  count(index = linenumber %/% 5, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  mutate(
    sentiment = positive - negative,
  )

# guick plot of sentiment over the length of the episode
ggplot(reply_all_sentiment, aes(x = index, y = sentiment, group = index)) +
  geom_line(stat = "identity", show.legend = FALSE, aes(color = sentiment_cat)) +
  theme_minimal()

