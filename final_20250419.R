# Clear environment
cat("\014")
rm(list = ls())
options(scipen = 100)

# Load packages
install.packages("e1071")  # Only once if not installed
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(e1071)

# Read the dataset
socialcultural <- read.csv("cleaned_sociocultural_adapatative.csv", skip = 1, header = TRUE, stringsAsFactors = FALSE)



# Identify Pre/Post item columns
pre_cols <- grep("^X\\.Pre\\.", colnames(socialcultural), value = TRUE)
post_cols <- grep("^X\\.Post\\.", colnames(socialcultural), value = TRUE)

# Convert to numeric
socialcultural[, pre_cols] <- sapply(socialcultural[, pre_cols], as.numeric)
socialcultural[, post_cols] <- sapply(socialcultural[, post_cols], as.numeric)

# Show number of missing values in each column
colSums(is.na(socialcultural))

# Check for rows where all values are NA
all_na_rows <- apply(socialcultural[, pre_cols], 1, function(x) all(is.na(x)))
sum(all_na_rows)
# Remove rows where all values in the 'Pre' columns are NA
socialcultural <- socialcultural[!all_na_rows, ]

pre_imputed <- t(apply(socialcultural[, pre_cols], 1, function(x) {
  if (all(is.na(x))) return(rep(NA, length(x)))
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}))
pre_imputed <- as.data.frame(pre_imputed)
colnames(pre_imputed) <- pre_cols


post_imputed <- t(apply(socialcultural[, post_cols], 1, function(x) {
  if (all(is.na(x))) return(rep(NA, length(x)))
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}))
post_imputed <- as.data.frame(post_imputed)
colnames(post_imputed) <- post_cols

socialcultural[, pre_cols] <- pre_imputed
socialcultural[, post_cols] <- post_imputed
colSums(is.na(socialcultural))

View(socialcultural)


# Category question mappings
interpersonal <- c(1, 3, 6, 11, 13, 16, 21)
academic <- c(2, 7, 12, 17)
interests <- c(4, 8, 14, 18)
ecological <- c(5, 9, 15, 19)
language <- c(10, 20)

# Function to extract columns based on question numbers
get_columns <- function(prefix, numbers) {
  colnames(socialcultural)[sapply(numbers, function(n) grep(paste0("^", prefix, "\\.", n, "\\."), colnames(socialcultural)))]
}

# Calculate sums for each category
socialcultural$Interpersonal_Pre <- rowSums(socialcultural[, get_columns("X.Pre", interpersonal)], na.rm = TRUE)
socialcultural$Interpersonal_Post <- rowSums(socialcultural[, get_columns("X.Post", interpersonal)], na.rm = TRUE)

socialcultural$Academic_Pre <- rowSums(socialcultural[, get_columns("X.Pre", academic)], na.rm = TRUE)
socialcultural$Academic_Post <- rowSums(socialcultural[, get_columns("X.Post", academic)], na.rm = TRUE)

socialcultural$Interest_Pre <- rowSums(socialcultural[, get_columns("X.Pre", interests)], na.rm = TRUE)
socialcultural$Interest_Post <- rowSums(socialcultural[, get_columns("X.Post", interests)], na.rm = TRUE)

socialcultural$Ecological_Pre <- rowSums(socialcultural[, get_columns("X.Pre", ecological)], na.rm = TRUE)
socialcultural$Ecological_Post <- rowSums(socialcultural[, get_columns("X.Post", ecological)], na.rm = TRUE)

socialcultural$Language_Pre <- rowSums(socialcultural[, get_columns("X.Pre", language)], na.rm = TRUE)
socialcultural$Language_Post <- rowSums(socialcultural[, get_columns("X.Post", language)], na.rm = TRUE)


View(socialcultural)

# Compute the sum of Pre and Post scores for each participant
socialcultural$Pre_Sum <- rowSums(socialcultural[, pre_cols], na.rm = TRUE)
socialcultural$Post_Sum <- rowSums(socialcultural[, post_cols], na.rm = TRUE)

View(socialcultural)

# Clean Native Language
socialcultural$Native.language <- trimws(tolower(socialcultural$Native.language))
socialcultural$Native.language <- ifelse(socialcultural$Native.language == "chinese", "Chinese", "Other")
socialcultural$Native.language <- factor(socialcultural$Native.language, levels = c("Other", "Chinese"))  # Set "Other" as reference
socialcultural$Native.language 
# Clean High School Language
socialcultural$High.School.Language <- socialcultural$High.School.Language %>%
  tolower() %>%
  trimws()
socialcultural$High.School.Language <- ifelse(socialcultural$High.School.Language == "english", "English", "Other")
socialcultural$High.School.Language <- factor(socialcultural$High.School.Language, levels = c("Other", "English"))  # Set "Other" as reference
socialcultural$High.School.Language
# Recode Time in the US
socialcultural$Time.in.the.US <- case_when(
  socialcultural$Time.in.the.US %in% c("Less than one month", "One to three months", "Three months to one year") ~ "Less than 1 year",
  socialcultural$Time.in.the.US == "One to two years" ~ "1-2 years",
  socialcultural$Time.in.the.US == "More than two years" ~ "More than 2 years",
  TRUE ~ NA_character_
)
socialcultural$Time.in.the.US <- factor(socialcultural$Time.in.the.US,
                                        levels = c("Less than 1 year", "1-2 years", "More than 2 years"))
socialcultural$Time.in.the.US
colSums(is.na(socialcultural))

#regression model for time change 
socialcultural <- socialcultural %>%
  mutate(change_score = Post_Sum - Pre_Sum)
View(socialcultural)



# Create change scores in wide format
socialcultural <- socialcultural %>%
  mutate(
    Interpersonal_Change = Interpersonal_Post - Interpersonal_Pre,
    Academic_Change = Academic_Post - Academic_Pre,
    Interest_Change = Interest_Post - Interest_Pre,
    Ecological_Change = Ecological_Post - Ecological_Pre,
    Language_Change = Language_Post - Language_Pre
  )

View(socialcultural)

#check skewness of change_score in social cultural data
raw_social_change_skew <- skewness(socialcultural$change_score, na.rm = TRUE)
cat("Skewness of raw Change Score: ", round(raw_social_change_skew, 3), "\n")


# Reshape to long format for change scores
long_change <- socialcultural %>%
  select(participant.ID, Native.language, High.School.Language, Time.in.the.US,
         Interpersonal_Change, Academic_Change, Interest_Change,
         Ecological_Change, Language_Change) %>%
  pivot_longer(
    cols = ends_with("_Change"),
    names_to = "Category",
    names_pattern = "(.*)_Change",
    values_to = "Change_Score"
  )
View(long_change)


ggplot(long_change, aes(x = Category, y = Change_Score, fill = Native.language)) +
  geom_boxplot(color = "black") +
  labs(title = "Change Scores by Category and Native Language",
       x = "Adaptation Category", y = "Change Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")
ggplot(long_change, aes(x = Category, y = Change_Score, fill = High.School.Language)) +
  geom_boxplot(color = "black") +
  labs(title = "Change Scores by Category and High School Language",
       x = "Adaptation Category", y = "Change Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2")
ggplot(long_change, aes(x = Category, y = Change_Score, fill = Time.in.the.US)) +
  geom_boxplot(color = "black") +
  labs(title = "Change Scores by Category and Time in the U.S.",
       x = "Adaptation Category", y = "Change Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



# Create a multivariate regression model
mmr_model <- lm(cbind(Interpersonal_Change, Academic_Change, Interest_Change, Ecological_Change, Language_Change) ~ 
                  Native.language + High.School.Language + Time.in.the.US, 
                data = socialcultural)

# View the summary of the multivariate model (separate regressions)
summary(mmr_model)

# Conduct MANOVA to test overall multivariate effects
manova_model <- manova(cbind(Interpersonal_Change, Academic_Change, Interest_Change, Ecological_Change, Language_Change) ~ 
                         Native.language + High.School.Language + Time.in.the.US, 
                       data = socialcultural)

summary(manova_model, test = "Pillai")  # Pillai's Trace is a robust test

#regression model for time change 
socialcultural <- socialcultural %>%
  mutate(change_score = Post_Sum - Pre_Sum)
View(socialcultural)
model_change <- lm(change_score ~ Native.language + High.School.Language + Time.in.the.US+ Pre_Sum, data = socialcultural)
summary(model_change)
model_change_int <- lm(change_score ~ Native.language + High.School.Language + Time.in.the.US+ Pre_Sum+ Pre_Sum*Native.language, data = socialcultural)
summary(model_change_int)
# Reshape to long format
long_data <- socialcultural %>%
  select(participant.ID, Pre_Sum, Post_Sum, Native.language, High.School.Language, Time.in.the.US) %>%
  pivot_longer(cols = c(Pre_Sum, Post_Sum), names_to = "Time", values_to = "Score") %>%
  mutate(Time = ifelse(Time == "Pre_Sum", 0, 1))  # 0 = Pre, 1 = Post

# Drop zero scores (if any)
long_data <- long_data %>%
  filter(Score != 0)
long_data
# Create transformed variables
long_data$log_score <- log(long_data$Score)
long_data$sqrt_score <- sqrt(long_data$Score + 1)  # Avoid sqrt(0)

# check skewness of long_data score

raw_skew <- skewness(long_data$Score, na.rm = TRUE)
cat("Skewness of Score: ", round(raw_skew, 3), "\n")

# Run regression with score
model_original <- lm(Score ~ Time + Native.language + High.School.Language + Time.in.the.US, data = long_data)
summary(model_original)
# Run regression with score interaction
model_original_interaction <- lm(Score ~ Time + Native.language + High.School.Language + Time.in.the.US+ Native.language*Time, data = long_data)
summary(model_original_interaction)



ggplot(long_data, aes(x = factor(Time, labels = c("Pre", "Post")), y = Score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Sociocultural Adaptation: Pre vs. Post",
       x = "Time", y = "Score") +
  theme_minimal()

ggplot(long_data, aes(x = Native.language, y = Score)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(title = "Sociocultural Adaptation by Native Language: Chinese vs. Other Languages",
       x = "Native Language", y = "Score") +
  theme_minimal()
ggplot(long_data, aes(x = High.School.Language, y = Score)) +
  geom_boxplot(fill = "lightyellow", color = "black") +
  labs(title = "Sociocultural Adaptation by High School Language: English vs. Other Languages",
       x = "High School Language", y = "Score") +
  theme_minimal()




ggplot(long_data, aes(x = Time.in.the.US, y = Score)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Sociocultural Adaptation by Time in the US",
       x = "Time in the US", y = "Score") +
  theme_minimal()

# Raw
ggplot(long_data, aes(x = Score)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Raw Scores", x = "Score", y = "Frequency") +
  theme_minimal()




ggplot(socialcultural, aes(x = Native.language, y = change_score)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(title = "Change in Sociocultural Adaptation by Native Language: Chinese vs. Other Languages",
       x = "Native Language", y = "Change Score") +
  theme_minimal()


ggplot(socialcultural, aes(x = High.School.Language, y = change_score)) +
  geom_boxplot(fill = "lightyellow", color = "black") +
  labs(title = "Change in Sociocultural Adaptation by High School Language: English vs. Other",
       x = "High School Language", y = "Change Score") +
  theme_minimal()


ggplot(socialcultural, aes(x = Time.in.the.US, y = change_score)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Change in Sociocultural Adaptation: Time spent in the US",
       x = "Time in the US", y = "Change Score") +
  theme_minimal()

library(lme4)

# Mixed-effects model with random intercept for each participant
model_mixed <- lmer(Score ~ Time + Native.language + High.School.Language + Time.in.the.US + (1 | participant.ID), data = long_data)

summary(model_mixed)


# Histogram of score
ggplot(long_data, aes(x = Score)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Scores", x = "Score", y = "Frequency") +
  theme_minimal()
# Histogram of change in score
ggplot(socialcultural, aes(x = change_score)) +
  geom_histogram(binwidth = 2, fill = "lightgrey", color = "black") +
  labs(title = "Histogram of Change in Scores", x = "Change in Score", y = "Frequency") +
  theme_minimal()


