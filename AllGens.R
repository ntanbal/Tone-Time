library(dplyr)
library(ggplot2)
library(tidytext)
library(SentimentAnalysis)
library(xlsx)
library(plotly)
library(cluster)
library(tidyverse)
library(readxl)
library(vars)
library(sentimentr)
library(forecast)
library(data.table)
library(fmsb)
library(plotly)


all <- read_csv("C:/Users/ntanb/Desktop/MusicHistory.csv")

events_data <- read_excel("C:/Users/ntanb/Desktop/HistoricalEvents.xlsx", sheet = "Sheet1")

all <- all %>%
  mutate(Score = sentiment_by(Lyrics)$ave_sentiment)

all <- all %>%
  mutate(
    # Create categories for generations
    generations = case_when(
      Year <= 1964 ~ "Boomers",
      Year > 1964 & Year <= 1980 ~ "Gen X",
      Year > 1980  & Year <= 1996 ~ "Millennials",
      Year > 1996 & Year <= 2012 ~ "Gen Z",
      Year > 2012 ~ "Gen Alpha"
    ),
    # Convert to factor
    generations = factor(
      generations,
      level = c("Boomers", "Gen X", "Millennials", "Gen Z", "Gen Alpha")
    )
  ) 




events <- events_data %>% 
  mutate(
    # Create categories
    generations  = dplyr::case_when(
      Year <= 1964            ~ "Boomers",
      Year > 1964 & Year <= 1980 ~ "Gen X",
      Year > 1980  & Year <= 1996 ~ "Millennials",
      Year > 1996 & Year <= 2012 ~ "Gen Z",
      Year > 2012 ~ "Gen Alpha"
    ),
    # Convert to factor
    generations = factor(
      generations,
      level = c("Boomers", "Gen X","Millennials","Gen Z", "Gen Alpha")
    )
  )







all_result <- all %>%
  group_by(Year) %>%
  summarise(max_sentiment = max(Score),
            min_sentiment = min(Score),
            mean_sentiment = mean(Score))


all_plot <- ggplot(data = all_result, aes(x = Year)) +
  geom_point(aes(y = max_sentiment), color = "blue", size = 3) +
  geom_point(aes(y = min_sentiment), color = "red", size = 3) +
  geom_point(aes(y = mean_sentiment), color = "gray", size = 4) +
  geom_segment(aes(y = max_sentiment, xend = Year, yend = mean_sentiment), color = "blue") +
  geom_segment(aes(y = min_sentiment, xend = Year, yend = mean_sentiment), color = "red") +
  labs(x = "Year", y = "Sentiment Score", title = "Sentiment Scores for All Generations") +
  scale_x_continuous(breaks = seq(min(all_result$Year), max(all_result$Year), by = 3)) +
  theme_minimal() +
  theme(
    text = element_text(size = 16, face = "bold"),  # Bigger and bolder text
    plot.title = element_text(size = 24, face = "bold")  # Bigger and bolder title
  )

print(all_plot)


unique(all$Genre)

genre_colors <- c(
  "Pop" = "red",
  "Jazz" = "blue",
  "Country" = "green",
  "Rock and Roll" = "orange",
  "Rock" = "purple",
  "Blues" = "yellow",
  "Doo-Wop" = "cyan",
  "R&B" = "magenta",
  "Folk" = "brown",
  "Disco" = "pink",
  "Hip Hop" = "darkgreen",
  "Freestyle" = "darkblue",
  "Reggae" = "darkred",
  "Rap" = "darkorange",
  "Soul" = "darkpurple",
  "Electro House" = "darkcyan",
  "Alternative" = "darkmagenta",
  "Trap" = "lightgreen",
  "Funk" = "lightblue",
  "Dancehall" = "lightred",
  "Reggaeton" = "lightorange"
)

# SPLIT BY GENRE

library(ggplot2)
library(dplyr)

# Assuming 'boomers' is your dataset
spider_data <- all %>%
  dplyr::select(Song, Artist, Genre, Danceability, Energy, Liveness, Speechiness, Valence) %>%
  mutate(across(c(Danceability, Energy, Liveness, Speechiness, Valence), as.numeric))

# Reshape the data using pivot_longer
spider_data_long <- spider_data %>%
  pivot_longer(cols = c(Danceability, Energy, Liveness, Speechiness, Valence), names_to = "Attribute", values_to = "Value")

main <- ggplot(data = spider_data_long, aes(x = reorder(Attribute, -Value), y = Value, group = Song)) +
  geom_polygon(aes(fill = Genre), alpha = 0.4, color = "black", size = 1) + # Fill with genre colors
  scale_x_discrete(limits = c('Danceability', 'Energy', 'Liveness', 'Speechiness', 'Valence')) +
  coord_polar(clip = "off") +
  theme_minimal() +
  labs(title = "Radar Chart of Song Attributes by Genre",
       subtitle = "Analysis of song attributes") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(size = 12),
        strip.text.x = element_text(size = 12), # Adjust x-axis text size
        strip.background = element_rect(fill = "white", color = "white", size = 2), # Add more space between facets
        panel.spacing = unit(1, "inch")) + 
  ylim(0, 1) +
  facet_wrap(~ Genre)

print(main)




# Genre Count of Boomers
boomers_genre_counts <- table(boomers$Genre)
boomers_genre_counts_df <- as.data.frame(boomers_genre_counts)
colnames(boomers_genre_counts_df) <- c("Genre", "Count")
print(boomers_genre_counts_df)

# SENTIMENT ANALYSIS ON BOOMERS
nrc <- get_sentiments("nrc")

# Tokenize the lyrics column
boomer_words <- boomers %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
boomers_words <- boomer_words %>%
  anti_join(stop_words)

boomer_word_freq <- boomers_words %>%
  count(word, sort = TRUE)

boomer_word_table <- inner_join(boomer_word_freq, nrc)

boomer_word_table$n <- as.numeric(boomer_word_table$n)

colnames(boomer_word_table)[2] = "Frequency"


#WORD COUNT 
ggplot(head(boomer_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words in Boomer Generation Songs", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))




#ALL DATA TOGETHER

spider_data <- boomers %>%
  dplyr::select(Song, Artist, Danceability, Energy, Liveness, Speechiness, Valence) %>%
  mutate(across(c(Danceability, Energy, Liveness, Speechiness, Valence), as.numeric))

# Reshape the data using pivot_longer
spider_data_long <- spider_data %>%
  pivot_longer(cols = c(Danceability, Energy, Liveness, Speechiness, Valence), names_to = "Attribute", values_to = "Value")



#ALL DATA TOGETHER

spider_data <- boomers %>%
  dplyr::select(Song, Artist, Danceability, Energy, Liveness, Speechiness, Valence) %>%
  mutate(across(c(Danceability, Energy, Liveness, Speechiness, Valence), as.numeric))

# Reshape the data using pivot_longer
spider_data_long <- spider_data %>%
  pivot_longer(cols = c(Danceability, Energy, Liveness, Speechiness, Valence), names_to = "Attribute", values_to = "Value")

# Plotting
main <- ggplot(data = spider_data_long, aes(x = reorder(Attribute, -Value), y = Value, group = Song)) +
  geom_polygon(alpha = 0, color = "#bc6c25", size = 1) + # Only draw the lines in black
  scale_x_discrete(limits = c('Danceability', 'Energy', 'Liveness', 'Speechiness', 'Valence')) +
  coord_polar(clip = "off") +
  theme_minimal() +
  labs(title = "Radar Chart of Spider Data",
       subtitle = "Analysis of song attributes") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),  # Bolder title
        plot.subtitle = element_text(size = 16, hjust = 0.5, face = "bold"),  # Bolder subtitle
        axis.text.x = element_text(size = 12, face = "bold")  # Bolder x-axis text
  ) +
  ylim(0, 1) +
  geom_polygon(data = spider_data_long %>% 
                 group_by(Song, Attribute) %>% 
                 summarise(Value = mean(Value)),
               aes(x = reorder(Attribute, -Value), y = Value, group = Song),
               alpha = 0.2, fill = NA) # No fill

main
