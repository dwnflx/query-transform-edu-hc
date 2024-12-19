
# Libraries ---------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("car")
# install.packages("reshape2")
# install.packages("rstatix")
# install.packages("effsize")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("knitr")
# install.packages("patchwork")
# install.packages("ggsignif")
# install.packages("ARTool")
# install.packages("effectsize")
# install.packages("here")
# install.packages("pwr")
# install.packages("rstudioapi")

# Load necessary libraries
library(tidyverse)
library(car)
library(reshape2)
library(rstatix)
library(effsize)
library(ggplot2)
library(dplyr)
library(knitr)
library(patchwork)
library(ggsignif)
library(ARTool)
library(effectsize)
library(here)
library(pwr)
library(rstudioapi)


# Data preparation --------------------------------------------------------
# Load the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("notebooks/data/healthcare/processed/3_healthcare_analysis.csv")

# Ensure column names are unique
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Identify metric columns (those containing "_" and specific metrics)
metric_cols <- grep("_", colnames(data), value = TRUE)
metric_cols <- metric_cols[grepl("Recall.k|MRR|nDCG.k|gpt_score|total_token_size|total_latency", 
                                 metric_cols)]

# Filter relevant columns
data_filtered <- data %>%
  dplyr::select(topic_lda, all_of(metric_cols))

# Pivot to long format
long_data <- data_filtered %>%
  pivot_longer(
    cols = -topic_lda,
    names_to = "Transformation_Metric",
    values_to = "Metric_Value"
  ) %>%
  # Split the combined column into "Transformation" and "Metric"
  separate(
    col = Transformation_Metric,
    into = c("Transformation", "Metric"),
    sep = "_",
    extra = "merge" # Keeps "gpt_score" intact
  )

head(long_data)

## SEPERATE CODE FOR METRICS
# Group metrics into two categories
retrieval_data <- long_data %>%
  filter(Metric %in% c("Recall.k", "nDCG.k", "MRR"))

gpt_data <- long_data %>%
  filter(Metric == "gpt_score")



# Transformation Impact on Primary Metrics ----------------------------------------

# Kruskal-Wallis test for retrieval metrics
kruskal_retrieval <- kruskal.test(Metric_Value ~ Transformation, data = retrieval_data)
print(kruskal_retrieval)

transformations <- unique(retrieval_data$Transformation)

# Mann-Whitney U tests for retrieval metrics
pairwise_retrieval <- list()

for (i in 1:(length(transformations) - 1)) {
  for (j in (i + 1):length(transformations)) {
    group1 <- transformations[i]
    group2 <- transformations[j]
    
    subset_data <- retrieval_data %>%
      filter(Transformation %in% c(group1, group2))
    
    test_result <- wilcox.test(
      Metric_Value ~ Transformation,
      data = subset_data,
      paired = FALSE,
      exact = FALSE
    )
    
    pairwise_retrieval[[paste(group1, "vs", group2)]] <- test_result$p.value
  }
}
print(pairwise_retrieval)

# Kruskal-Wallis test for gpt_score
kruskal_gpt <- kruskal.test(Metric_Value ~ Transformation, data = gpt_data)
print(kruskal_gpt)

transformations <- unique(gpt_data$Transformation)

# Mann-Whitney U tests for gpt_score
pairwise_gpt <- list()

for (i in 1:(length(transformations) - 1)) {
  for (j in (i + 1):length(transformations)) {
    group1 <- transformations[i]
    group2 <- transformations[j]
    
    subset_data <- gpt_data %>%
      filter(Transformation %in% c(group1, group2))
    
    test_result <- wilcox.test(
      Metric_Value ~ Transformation,
      data = subset_data,
      paired = FALSE,
      exact = FALSE
    )
    
    pairwise_gpt[[paste(group1, "vs", group2)]] <- test_result$p.value
  }
}

adjusted_pvalues_retrieval <- p.adjust(unlist(pairwise_retrieval), method = "bonferroni")
print(adjusted_pvalues_retrieval)

adjusted_pvalues_gpt <- p.adjust(unlist(pairwise_gpt), method = "bonferroni")
print(adjusted_pvalues_gpt)

# TABLE RESULT
# Combine non-adjusted and adjusted p-values for retrieval metrics
retrieval_table <- data.frame(
  Comparison = c("Baseline vs Compression", "Baseline vs Rewriting", 
                 "Baseline vs Hyde", "Compression vs Rewriting", 
                 "Compression vs Hyde", "Rewriting vs Hyde"),
  Non_Adjusted_P = unlist(pairwise_retrieval),
  Adjusted_P = adjusted_pvalues_retrieval
)

# Combine non-adjusted and adjusted p-values for GPT score
gpt_table <- data.frame(
  Comparison = c("Baseline vs Compression", "Baseline vs Rewriting", 
                 "Baseline vs Hyde", "Compression vs Rewriting", 
                 "Compression vs Hyde", "Rewriting vs Hyde"),
  Non_Adjusted_P = unlist(pairwise_gpt),
  Adjusted_P = adjusted_pvalues_gpt
)

# Print the retrieval table
cat("Retrieval Metrics (P-Values):\n")
kable(retrieval_table, format = "markdown", digits = 4)

# Print the GPT score table
cat("\nGPT Score (P-Values):\n")
kable(gpt_table, format = "markdown", digits = 4)




# Transformation Impact on Latency and Tokens -----------------------------

# Reshape the data into long format
long_data <- data %>%
  pivot_longer(
    cols = ends_with(c("_total_token_size", "_total_latency")),
    names_to = c("Transformation", ".value"),
    names_pattern = "(.*)_(total_token_size|total_latency)"
  ) %>%
  rename(Token_Size = total_token_size, Latency = total_latency)


# Latency
# ANOVA for Latency
anova_latency <- aov(Latency ~ Transformation, data = long_data)
summary(anova_latency)

# Check assumptions for ANOVA
leveneTest(Latency ~ Transformation, data = long_data)  # Test for homogeneity of variance
shapiro.test(residuals(anova_latency))                 # Test for normality of residuals

kruskal_latency <- kruskal.test(Latency ~ Transformation, data = long_data)
print(kruskal_latency)

pairwise_latency <- pairwise.wilcox.test(long_data$Latency, 
                                         long_data$Transformation, 
                                         p.adjust.method = "bonferroni")
print(pairwise_latency)

median_latency <- long_data %>%
  group_by(Transformation) %>%
  summarise(
    Median_Latency = median(Latency, na.rm = TRUE),
    IQR_Latency = IQR(Latency, na.rm = TRUE) # Interquartile Range for additional context
  )
print(median_latency)


# TokenSize
# Kruskal-Wallis Test for Token Size
kruskal_token_size <- kruskal.test(Token_Size ~ Transformation, data = long_data)
print(kruskal_token_size)

# Pairwise Wilcoxon Tests for Token Size
pairwise_token_size <- pairwise.wilcox.test(long_data$Token_Size, long_data$Transformation, p.adjust.method = "bonferroni")
print(pairwise_token_size)

# Descriptive Statistics for Token Size
median_token_size <- long_data %>%
  group_by(Transformation) %>%
  summarise(
    Median_Token_Size = median(Token_Size, na.rm = TRUE),
    IQR_Token_Size = IQR(Token_Size, na.rm = TRUE)
  )
print(median_token_size)
print(median_latency)


# Combine both Latency and Token Information
# Define consistent color palette
color_palette <- c("baseline" = "#4DAF4A",  
                   "compression" = "#E41A1C",
                   "rewriting" = "#377EB8",
                   "hyde" = "#984EA3")

# Define the original levels and desired display labels
raw_levels <- c("baseline", "compression", "rewriting", "hyde") 
display_levels <- c("Baseline", "Compression", "Rewriting", "HyDE")

# 1. Boxplot for Latency
p1 <- ggplot(long_data, aes(x = factor(Transformation, levels = raw_levels), 
                            y = Latency, fill = factor(Transformation, levels = raw_levels))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 14)) +
  scale_fill_manual(values = color_palette, labels = display_levels) + 
  theme_minimal() +
  labs(title = "Latency Across Transformations",
       x = NULL,
       y = "Latency (s)") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = display_levels)

# 2. Boxplot for Token Size
p2 <- ggplot(long_data, aes(x = factor(Transformation, levels = raw_levels), 
                            y = Token_Size, fill = factor(Transformation, levels = raw_levels))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  coord_cartesian(ylim = c(1500, 4500)) + 
  scale_fill_manual(values = color_palette, labels = display_levels) +
  theme_minimal() +
  labs(title = "Token Size Across Transformations",
       x = NULL,
       y = "Token Size") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = display_levels) 

# 3. Scatter Plot for Trade-Offs
combined_summary <- long_data %>%
  group_by(Transformation) %>%
  summarise(
    Median_Latency = median(Latency, na.rm = TRUE),
    Median_Token_Size = median(Token_Size, na.rm = TRUE)
  )

p3 <- ggplot(combined_summary, aes(x = Median_Token_Size, y = Median_Latency, 
                                   label = factor(Transformation, levels = raw_levels))) +
  geom_point(size = 4, aes(fill = factor(Transformation, levels = raw_levels)), 
             shape = 21, color = "black", stroke = 1.0) +
  scale_fill_manual(values = color_palette, labels = display_levels) +
  theme_minimal() +
  labs(title = "Trade-Off Between Latency and Token Size",
       x = "Median Token Size",
       y = "Median Latency (s)",
       fill = "Transformation") +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.1, 0))


# Combine Plots (Legend from p3 is retained)
(p1 / p2) | p3





# Transformation Variance in Topics ---------------------------------------

# Prepare a clean dataset for gpt_score
# Ensure topic_lda, question_class, and _gpt_score columns are selected
gpt_long_data <- data %>%
  pivot_longer(
    cols = matches("_gpt_score"),
    names_to = "Transformation",
    values_to = "gpt_score"
  ) %>%
  mutate(
    Transformation = gsub("_gpt_score", "", Transformation),
    Transformation = factor(Transformation, levels = c("baseline", "compression", "rewriting", "hyde")),
    topic_lda = factor(topic_lda)
  )

# Summarize data for plotting
interaction_summary_gpt <- gpt_long_data %>%
  group_by(Transformation, topic_lda) %>%
  summarise(
    Mean_gpt_score = mean(gpt_score, na.rm = TRUE),
    SE_gpt_score = sd(gpt_score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Generate smoothed data using cubic spline for each Transformation
smoothed_data <- interaction_summary_gpt %>%
  group_by(Transformation) %>%
  do({
    spline_data <- spline(
      x = as.numeric(.$topic_lda), 
      y = .$Mean_gpt_score, 
      n = 100
    )
    tibble(
      Smooth_Topic = spline_data$x,
      Smooth_Mean = spline_data$y,
      Smooth_SE_Lower = spline(
        x = as.numeric(.$topic_lda), 
        y = .$Mean_gpt_score - .$SE_gpt_score, 
        n = 100
      )$y,
      Smooth_SE_Upper = spline(
        x = as.numeric(.$topic_lda), 
        y = .$Mean_gpt_score + .$SE_gpt_score, 
        n = 100
      )$y,
      Transformation = .$Transformation[1] 
    )
  }) %>%
  ungroup()

ggplot(smoothed_data, aes(x = Smooth_Topic, y = Smooth_Mean, group = Transformation, color = Transformation)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_ribbon(aes(ymin = Smooth_SE_Lower, ymax = Smooth_SE_Upper, fill = Transformation),
              alpha = 0.2, color = NA) +
  geom_point(data = interaction_summary_gpt, aes(x = as.numeric(topic_lda), y = Mean_gpt_score, color = Transformation), 
             size = 4, shape = 21, fill = "white") +
  scale_color_manual(
    values = c("#4DAF4A", "#E41A1C", "#377EB8", "#984EA3"),
    labels = c("Baseline", "Compression", "Rewriting", "HyDE")
  ) +
  scale_fill_manual(
    values = c("#4DAF4A", "#E41A1C", "#377EB8", "#984EA3"),
    labels = c("Baseline", "Compression", "Rewriting", "HyDE"),
    guide = "none"
  ) +
  labs(
    title = "Interaction Plot: Topic vs. Transformation related to GPT-Score (Education)",
    x = "Topic",
    y = "Mean GPT score",
    color = "Transformation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.70, 0.2),
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    legend.box.background = element_rect(fill = "white", color = "black", size = 0.5),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  scale_y_continuous(
    limits = c(0.6, 0.8),
    breaks = seq(0.6, 0.9, by = 0.05)
  ) +
  scale_x_continuous(
    breaks = 1:2,
    labels = c("CanGen", "HeaEpi"),
    expand = c(0.05, 0.05)
  )




# (ART) ANOVA Analysis regarding Answer Quality ---------------------------------

# Fit the ART model with all three factors
anova_model <- aov(gpt_score ~ Transformation * topic_lda, data = gpt_long_data)

# 2.2: Check for normality of residuals
shapiro_test <- shapiro.test(residuals(anova_model))
print(shapiro_test)

# 2.3: Check for homogeneity of variances
levene_test <- leveneTest(gpt_score ~ Transformation * topic_lda, data = gpt_long_data)
print(levene_test)

# Visual checks for residuals
par(mfrow = c(1, 2))
hist(residuals(anova_model), main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "blue")


# Fit ART Model
art_model_full <- art(gpt_score ~ Transformation * topic_lda, data = gpt_long_data)

# Get the ANOVA table
anova_results_full <- anova(art_model_full)

# Calculate partial eta-squared effect sizes
effect_sizes <- eta_squared(anova_results_full, partial = TRUE)

# Combine ANOVA results with effect sizes
anova_table_with_eta <- cbind(anova_results_full,
                              "Eta2 (partial)" = effect_sizes$Eta2_partial)

# Function to calculate power
calculate_power <- function(df_numerator, df_denominator, eta_squared_partial, sig.level = 0.05) {
  f_squared <- eta_squared_partial / (1 - eta_squared_partial)
  power_result <- pwr.f2.test(u = df_numerator,
                              v = df_denominator,
                              f2 = f_squared,
                              sig.level = sig.level)
  return(power_result$power)
}

# Add power to the ANOVA table
anova_table_with_eta$Power <- mapply(calculate_power,
                                     anova_table_with_eta$Df,
                                     anova_table_with_eta$Df.res,
                                     anova_table_with_eta$`Eta2 (partial)`)

# Print the enhanced ANOVA table
print(anova_table_with_eta)

# Generate the Combined Contrasts table with educational interpretations
# Pairwise comparisons for topic_lda and question_class
pairwise_topic <- art.con(art_model_full, "Transformation")

# Extract summaries as dataframes
summary_pairwise_topic <- summary(pairwise_topic) %>% as.data.frame()

# Combine all pairwise contrasts into a single dataframe
all_contrasts <- rbind(
  summary_pairwise_topic %>% mutate(Category = "Topic")
)

# Sort by p-value and select the top 15 contrasts
top_n_contrasts <- all_contrasts %>%
  arrange(p.value) %>%
  head(n = 15)

# Print the top 15 contrasts table
print(top_n_contrasts)
