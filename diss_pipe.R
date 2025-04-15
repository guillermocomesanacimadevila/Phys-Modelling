##############################################################
# Biomarker Data Pipeline: Human Performance & Recovery Study
# Author: Guillermo Comesaña | 2023
# BSc Dissertation
# Reproducible R Pipeline: Data Exploration, Modelling, Clustering
##############################################################

# Load Required Libraries
library(tidyverse)    # Data handling & visualisation
library(corrplot)     # Correlation matrix
library(GGally)       # Pairwise plots
library(cluster)      # Clustering tools
library(factoextra)   # Clustering visualisation
library(caret)        # Data scaling & preprocessing

##############################################################
# Data Import
##############################################################

# Load Dataset
df <- read.csv("athlete_biomarker_dataset.csv")

# Overview of Dataset
glimpse(df)
summary(df)

##############################################################
# Descriptive Statistics
##############################################################

# Summary Table of Key Variables
df %>%
  select(VO2max, Blood_Lactate, Haematocrit, HR_Recovery, Sleep_Quality, Recovery_Time) %>%
  summary()

##############################################################
# Correlation Analysis
##############################################################

# Compute Correlation Matrix
corr_matrix <- cor(df[, c('VO2max', 'Blood_Lactate', 'Haematocrit', 'HR_Recovery', 'Sleep_Quality', 'Recovery_Time')])

# Visualise Correlation Matrix
corrplot(corr_matrix, method = "color", type = "upper", tl.cex=0.8)

# Extract Specific Correlations
cor(df$VO2max, df$Recovery_Time)
cor(df$Sleep_Quality, df$Recovery_Time)

##############################################################
# Linear Regression Model
##############################################################

# Fit Linear Model
model <- lm(Recovery_Time ~ VO2max + Blood_Lactate + Haematocrit + HR_Recovery + Sleep_Quality, data = df)

# Model Summary
summary(model)

# Residual Plots
par(mfrow=c(2,2))
plot(model)

##############################################################
# Clustering Analysis: Athlete Profiling
##############################################################

# Scale Relevant Features
scaled_data <- scale(df[, c('VO2max', 'Blood_Lactate', 'Haematocrit', 'HR_Recovery', 'Sleep_Quality')])

# Determine Optimal Number of Clusters
fviz_nbclust(scaled_data, kmeans, method = "wss")

# Apply K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Append Cluster Labels
df$Cluster <- as.factor(kmeans_result$cluster)

# Visualise Clusters
fviz_cluster(kmeans_result, data = scaled_data)

##############################################################
# Pairwise Exploration of Biomarkers by Cluster
##############################################################

ggpairs(df, columns = c('VO2max', 'Blood_Lactate', 'Haematocrit', 'HR_Recovery', 'Sleep_Quality', 'Recovery_Time'),
        aes(color = Cluster))

##############################################################
# Key Scatter Plot: VO2max vs Recovery Time
##############################################################

ggplot(df, aes(x = VO2max, y = Recovery_Time, color = Cluster)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title="VO2max vs Recovery Time by Cluster",
       x="VO2max (ml/kg/min)",
       y="Recovery Time (Hours)") +
  scale_color_brewer(palette="Dark2")

##############################################################
# Export Cleaned & Clustered Dataset
##############################################################

write.csv(df, "processed_athlete_biomarker_dataset.csv", row.names = FALSE)

##############################################################
# End of Pipeline
##############################################################

