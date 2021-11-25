#Logistic Regression

# install.packages("GlmSimulatoR")
# loading the libraries required for this code
library(tidyverse)
library(olsrr)
library(corrplot)
library(smotefamily)

library(GlmSimulatoR)
library(ggplot2)
library(dplyr)
library(stats)

# Read Github_most_stars.csv into a tibble called githubStar
githubStar <- read_csv(file="Github_most_stars.csv",
                       col_types = "cccnnnnnncc",
                       col_names = TRUE)
# Display githubStar in the console
print(githubStar)

# Display the structure of githubStar in the console
# Display the summary of githubStar in the console
print(str(githubStar))
print(summary(githubStar))


githubStar <- githubStar %>%
  mutate(starsScaled = 
           ((stars - min(stars))
            /(max(stars) - min(stars))) * (1 - 0) + 0
  )

githubStar <- githubStar %>%
  mutate(forksCountScaled = 
           ((forks_count - min(forks_count))
            /(max(forks_count) - min(forks_count))) * (1 - 0) + 0
  )


# githubStar <- githubStar %>%
#   mutate(networkCountScaled = 
#            ((network_count - min(network_count))
#             /(max(network_count) - min(network_count))) * (1 - 0) + 0
#         )

githubStar <- githubStar %>%
  mutate(suscribersCountScaled = 
           ((subscribers_count - min(subscribers_count))
            /(max(subscribers_count) - min(subscribers_count))) * (1 - 0) + 0
  )

# githubStar <- githubStar %>%
#   mutate(watchersCountScaled = 
#            ((watchers_count - min(watchers_count))
#             /(max(watchers_count) - min(watchers_count))) * (1 - 0) + 0
#       )

githubStar <- githubStar %>%
  mutate(issueCountScaled = 
           ((issue_count - min(issue_count))
            /(max(issue_count) - min(issue_count))) * (1 - 0) + 0
  )


githubStarMedian <- githubStar%>%
  mutate(isTarget = 
           ifelse(
             (starsScaled * 0.3 + forksCountScaled * 0.3 
              + issueCountScaled * 0.2 + suscribersCountScaled * 0.2  ) >
               median(starsScaled * 0.3 + forksCountScaled * 0.3 
                      + issueCountScaled * 0.2 + suscribersCountScaled * 0.2 ),
             TRUE,FALSE
           )
  )

githubStarMean <- githubStar%>%
  mutate(isTarget = 
           ifelse(
             (starsScaled * 0.3 + forksCountScaled * 0.3 
              + issueCountScaled * 0.2 + suscribersCountScaled * 0.2   ) >
               mean(starsScaled * 0.3 + forksCountScaled * 0.3  
                    + issueCountScaled * 0.2  + suscribersCountScaled * 0.2  ),
             TRUE,FALSE
           )
  )


githubStarMedian <- githubStarMedian %>% 
  select( starsScaled, forksCountScaled, 
          issueCountScaled, suscribersCountScaled,   isTarget
  )


githubStarMedian$isTarget <- as.integer(as.logical(githubStarMedian$isTarget))

print(summary(githubStarMean))
print(summary(githubStarMedian))

# Since there is no class imbalance, no need to create synthetic testing data





displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "black") +
    facet_wrap (~key, scales = "free") +
    theme_minimal ()
}


displayAllHistograms(githubStarMedian)

githubStarMedian$starsScaled <- log10(githubStarMedian$starsScaled)
githubStarMedian$forksCountScaled <- log10(githubStarMedian$forksCountScaled)
githubStarMedian$issueCountScaled <- log10(githubStarMedian$issueCountScaled)
githubStarMedian$suscribersCountScaled <- log10(githubStarMedian$suscribersCountScaled)

# removing non-finite values from the dataset
githubStarMedian <- githubStarMedian[!is.infinite(rowSums(githubStarMedian)),]

sampleSet <- sample(nrow(githubStarMedian),
                    round(nrow(githubStarMedian)*0.75),
                    replace = FALSE
)
# Put 75% sample into training
githubStarTraining  <- githubStarMedian[sampleSet, ]

# Put 25% sample into testing
githubStarTesting  <- githubStarMedian[-sampleSet, ]

# Generate the model
logRegressionModel <- glm(data = githubStarTraining,
                        family = binomial,
                        formula = isTarget ~ .)

summary(logRegressionModel)

# Calculate the odds ratios for each of the 7 independent variable coefficients
exp(coef(logRegressionModel)["starsScaled"])
exp(coef(logRegressionModel)["forksCountScaled"])
exp(coef(logRegressionModel)["issueCountScaled"])
exp(coef(logRegressionModel)["suscribersCountScaled"])

# using the model to predict outcomes of testing dataset
githubTargetPrediction <- predict(logRegressionModel,
                                  githubStarTesting,
                                 type = "response")

# display mobilePhonePrediction in the console
print(githubTargetPrediction)

# treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
githubTargetPrediction <- 
  ifelse(githubTargetPrediction >= 0.5, 1, 0)

print(githubTargetPrediction)

# create a confusion matrix
githubTargetConfusionMatrix <- table(githubStarTesting$isTarget,
                                     githubTargetPrediction)

# display the confusion matrix
print(githubTargetConfusionMatrix)

# false positive rate
githubTargetConfusionMatrix[1, 2] / (githubTargetConfusionMatrix[1, 2] +
                                       githubTargetConfusionMatrix[1, 1])

# false negative rate
githubTargetConfusionMatrix[2, 1] / (githubTargetConfusionMatrix[2, 1] +
                                      githubTargetConfusionMatrix[2, 2])

# total predictive accuracy of the model 
predictiveAccuracy <- sum(diag(githubTargetConfusionMatrix)) / nrow(githubStarTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)
