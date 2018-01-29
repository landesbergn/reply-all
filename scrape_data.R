library(rvest)
library(dplyr)
library(stringr)

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

transcript <- transcript %>%
  filter(nchar(line) > 0) %>%
  mutate(
    speaker = str_extract(line, "^[[:upper:]]+"),
    line = str_replace_all(line, "^[[:upper:]]+ [[:upper:]]+:", ""),
    line = str_replace_all(line, "^[[:upper:]]+:", "")
  ) %>%
  select(speaker, line)

