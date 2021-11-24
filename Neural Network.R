# MIS 545 Section 02

# Install the required packages
# install.packages("tidyverse")
# install.packages("neuralnet")


# Load the tidyverse and neuralnet libraries
library("tidyverse")
library("neuralnet")


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


set.seed(591)

sampleSet <- sample(nrow(githubStarMedian),
                    round(nrow(githubStarMedian)*0.75),
                    replace = FALSE
)
# Put 75% sample into training
githubStarMedianTraining <- githubStarMedian[sampleSet, ]

# Put 25% sample into testing
githubStarMedianTesting  <- githubStarMedian[-sampleSet, ]


githubStarMedianTraining

githubStarNeuralNet <- neuralnet(
  formula = isTarget ~ starsScaled + forksCountScaled 
                            + issueCountScaled + suscribersCountScaled,
  
  data = githubStarMedianTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)

# Display the neural network numeric results
print(githubStarNeuralNet$result.matrix)

# Visualize the neural network
plot(githubStarNeuralNet)

# Use fishingCharterNeuralNet to generate probabilities on the 
# fishingCharterTesting data set and store this in fishingCharterProbability
githubStarProbability <- compute(githubStarNeuralNet,
                                     githubStarMedianTesting)

# Display the probabilities from the testing dataset on the console
print(githubStarProbability$net.result)

# Convert probability predictions into 0/1 predictions and 
# store this into fishingCharterPrediction
githubStarPrediction <- 
  ifelse(githubStarProbability$net.result > 0.5,1,0)

# Display the 0/1 predictions on the console
print(githubStarPrediction)

# Evaluate the model by forming a confusion matrix
githubStarConfusionMatrix <- table(githubStarMedianTesting$isTarget,
                                   githubStarPrediction)

# Display the confusion matrix on the console
print(githubStarConfusionMatrix)

# Calculate the model predictive accuracy
predictAccuracy <- sum(diag(githubStarConfusionMatrix)) / 
  nrow(githubStarMedianTesting)

# Display the predictive accuracy on the console
print(predictAccuracy)






