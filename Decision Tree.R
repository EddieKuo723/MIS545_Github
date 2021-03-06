# MIS 545 Section 02

# Install the required packages
# install.packages("tidyverse")
# install.packages("rpart")
# install.packages("rpart.plot")

library("tidyverse")
library("rpart")
library("rpart.plot")

# Set the working directory to your Lab12 folder
setwd("C:/Users/user/Documents/GitHub/MIS545_Github_CyberSecurity")

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


print(summary(githubStarMean))
print(summary(githubStarMedian))



githubStarMedian <- githubStarMedian %>% 
  select( starsScaled, forksCountScaled, 
          issueCountScaled, suscribersCountScaled,   isTarget
  )



set.seed(370)

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

# Generate the decision tree model to predict isTarget based
# on the other variables in the dataset. Use 0.01 as the complexity parameter.
githubStarModel <- rpart(formula = isTarget ~ .,
                        method = "class",
                        cp = 0.01,
                        data = githubStarTraining)

# Display the decision tree visualization in R
rpart.plot(githubStarModel)


# Predict classes for each record in the testing dataset 
# and store them in githubStarPrediction
githubStarPrediction <- predict(githubStarModel,
                              githubStarTesting,
                              type = "class")

# Display riceFarmsPrediction on the console
print(githubStarPrediction)


# Evaluate the model by forming a confusion matrix
githubStarConfusionMatrix <- table(githubStarTesting$isTarget,
                                  githubStarPrediction)

# Display the confusion matrix on the console
print(githubStarConfusionMatrix)

# Calculate the model prediction accuracy 
predictiveAccuracy <-sum(diag(githubStarConfusionMatrix)
                         /nrow(githubStarTesting))

# Display the predictive accuracy on the console
print(predictiveAccuracy)







