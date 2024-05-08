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


# Subsets of Boomers, Gex X, Millennials, Gen Z, Gen Alpha
alpha <- subset(all, generations == "Gen Alpha")

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





alpha_result <- alpha %>%
  group_by(Year) %>%
  summarise(max_sentiment = max(Score),
            min_sentiment = min(Score),
            mean_sentiment = mean(Score))


alpha_plot <- ggplot(data = alpha_result, aes(x = Year)) +
  geom_point(aes(y = max_sentiment), color = "blue", size = 3) +
  geom_point(aes(y = min_sentiment), color = "red", size = 3) +
  geom_point(aes(y = mean_sentiment), color = "gray", size = 4) +
  geom_segment(aes(y = max_sentiment, xend = Year, yend = mean_sentiment), color = "blue") +
  geom_segment(aes(y = min_sentiment, xend = Year, yend = mean_sentiment), color = "red") +
  labs(x = "Year", y = "Sentiment Score", title = "Sentiment Scores for alpha Generation") +
  scale_x_continuous(breaks = unique(alpha_result$Year), labels = unique(alpha_result$Year)) +
  theme_minimal() +
  theme(
    text = element_text(size = 16, face = "bold"),  # Bigger and bolder text
    plot.title = element_text(size = 24, face = "bold")  # Bigger and bolder title
  )

print(alpha_plot)


unique(alpha$Genre)


# Define custom colors for each genre
genre_colors <- c(
  "Hip Hop" = "orange",
  "Country" = "yellow",
  "Rap" = "purple",
  "Funk" = "green",
  "Reggaeton" = "#4B0082",   # Indigo
  "Pop" = "red",
  "R&B" = "#00008B",    # Dark blue
  "Rock and Roll" = "#8B0000",  # Dark red
  "Rock" = "#FF6666",    # Light red
  "Soul" = "darkorange3",
  "Trap" = "blueviolet",
  "Alternative" = "darkgreen",
  "Dancehall" = "pink",
  "Disco" = "grey"
)

# SPLIT BY GENRE

library(ggplot2)
library(dplyr)

spider_data <- alpha %>%
  dplyr::select(Song, Artist, Genre, Danceability, Energy, Liveness, Speechiness, Valence) %>%
  mutate(across(c(Danceability, Energy, Liveness, Speechiness, Valence), as.numeric))

# Reshape the data using pivot_longer
spider_data_long <- spider_data %>%
  pivot_longer(cols = c(Danceability, Energy, Liveness, Speechiness, Valence), names_to = "Attribute", values_to = "Value")

# Plotting with facet_wrap by Genre
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
        strip.text = element_text(size = 20), # Adjust strip text size
        strip.background = element_rect(fill = "white", color = "white"), # Add more space between plots
        panel.spacing = unit(1, "inch")) + 
  ylim(0, 1) +
  facet_wrap(~ Genre)

print(main)




# Genre Count of Boomers
alpha_genre_counts <- table(alpha$Genre)
alpha_genre_counts_df <- as.data.frame(alpha_genre_counts)
colnames(alpha_genre_counts_df) <- c("Genre", "Count")
print(alpha_genre_counts_df)

# SENTIMENT ANALYSIS ON BOOMERS
nrc <- get_sentiments("nrc")

# Tokenize the lyrics column
alpha_words <- alpha %>%
  unnest_tokens(word, Lyrics) %>%
  filter(nchar(word) >= 2)

# Remove common English stopwords
alpha_words <- alpha_words %>%
  anti_join(stop_words)

alpha_word_freq <- alpha_words %>%
  count(word, sort = TRUE)

alpha_word_table <- inner_join(alpha_word_freq, nrc)

alpha_word_table$n <- as.numeric(alpha_word_table$n)

colnames(alpha_word_table)[2] = "Frequency"


#WORD COUNT 
ggplot(head(alpha_word_table, 10), aes(x = word, y = Frequency, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Top 10 Words in alpha Generation Songs", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))




#ALL DATA TOGETHER

spider_data <- alpha %>%
  dplyr::select(Song, Artist, Danceability, Energy, Liveness, Speechiness, Valence) %>%
  mutate(across(c(Danceability, Energy, Liveness, Speechiness, Valence), as.numeric))

# Reshape the data using pivot_longer
spider_data_long <- spider_data %>%
  pivot_longer(cols = c(Danceability, Energy, Liveness, Speechiness, Valence), names_to = "Attribute", values_to = "Value")



#ALL DATA TOGETHER

spider_data <- alpha %>%
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
