# MIS 545 Section 02

# Install the required packages
# install.packages("tidyverse")
# install.packages("e1071")


# Load the tidyverse and neuralnet libraries
library(tidyverse)
library(e1071)

# Set the working directory to your Lab12 folder
# setwd("C:/Users/user/Documents/GitHub/MIS545_Github_CyberSecurity")

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

githubStar <- githubStar %>%
  mutate(networkCountScaled = 
           ((network_count - min(network_count))
            /(max(network_count) - min(network_count))) * (1 - 0) + 0
        )

githubStar <- githubStar %>%
  mutate(suscribersCountScaled = 
           ((subscribers_count - min(subscribers_count))
            /(max(subscribers_count) - min(subscribers_count))) * (1 - 0) + 0
      )

githubStar <- githubStar %>%
  mutate(watchersCountScaled = 
           ((watchers_count - min(watchers_count))
            /(max(watchers_count) - min(watchers_count))) * (1 - 0) + 0
      )

githubStarMedian <- githubStar%>%
  mutate(isTarget = 
           ifelse(
             (starsScaled * 0.4 + forksCountScaled * 0.4 
              + suscribersCountScaled * 0.2  ) >
               median(starsScaled * 0.4 + forksCountScaled * 0.4
                      + suscribersCountScaled * 0.2 ),
             TRUE,FALSE
           )
  )

githubStarMean <- githubStar%>%
  mutate(isTarget = 
           ifelse(
             (starsScaled * 0.4 + forksCountScaled * 0.4  
              + suscribersCountScaled * 0.2   ) >
               mean(starsScaled * 0.4 + forksCountScaled * 0.4 
                    + suscribersCountScaled * 0.2  ),
             TRUE,FALSE
           )
  )

print(summary(githubStarMean))
print(summary(githubStarMedian))


githubStarMedian <- githubStarMedian %>% 
            select( starsScaled, forksCountScaled, 
                    issue_count, suscribersCountScaled,   isTarget
            )

set.seed(154)

sampleSet <- sample(nrow(githubStarMedian ),
                    round(nrow(githubStarMedian )*0.75),
                    replace = FALSE
)
# Put 75% sample into training
githubStarMedianTraining  <- githubStarMedian[sampleSet, ]

summary(githubStarMedianTraining)

# Put 25% sample into testing
githubStarMedianTesting  <- githubStarMedian[-sampleSet, ]

# Generate the Naive Bayes model to predict isTarget 
# based on the other variables in the dataset
githubStarModel <- naiveBayes(formula = isTarget ~.,
                            data = githubStarMedianTraining,
                            lapse = 1)

# Build probabilities for each record in the testing dataset 
# and store them in githubStarProbability
githubStarProbability <- predict(githubStarModel,
                                 githubStarMedianTesting,
                                   type = "raw")

# Display githubStarProbability on the console
print(githubStarProbability)

githubStarPrediction <- predict(githubStarModel,
                                githubStarMedianTesting,
                                  type = "class")

# Display githubStarPrediction on the console
print(githubStarPrediction)

# Evaluate the model by forming a confusion matrix
githubStarConfusionMatrix <- table(githubStarMedianTesting$isTarget ,
                                   githubStarPrediction)

# Display the confusion matrix on the console
print(githubStarConfusionMatrix)

# Calculate the model prediction accuracy 
predictiveAccuracy <-sum(diag(githubStarConfusionMatrix)
                         /nrow(githubStarMedianTesting))

# Display the predictive accuracy on the console
print(predictiveAccuracy)

