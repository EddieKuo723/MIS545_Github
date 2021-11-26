#Logistic Regression


# loading the libraries required for this code
library(tidyverse)
library(olsrr)
library(corrplot)
library(smotefamily)
library(class)
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
print(str(githubStar))

# Display the summary of githubStar in the console
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


print(summary(githubStarMean))
print(summary(githubStarMedian))

# Since there is no class imbalance, no need to create synthetic testing data


displayAllBoxplots <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_boxplot(mapping = aes(x=value, fill=key),
                            color = "black") +
    facet_wrap (~key, scales = "free") +
    theme_minimal ()
}

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

displayAllBoxplots(githubStarMedian)


githubStarMedian$starsScaled <- log10(githubStarMedian$starsScaled)
githubStarMedian$forksCountScaled <- log10(githubStarMedian$forksCountScaled)
githubStarMedian$issueCountScaled <- log10(githubStarMedian$issueCountScaled)
githubStarMedian$suscribersCountScaled <- log10(githubStarMedian$suscribersCountScaled)

# removing non-finite values from the dataset
githubStarMedian <- githubStarMedian[!is.infinite(rowSums(githubStarMedian)),]


# separate the tibble into 2 lables only
isTargetLabels <- githubStarMedian %>% 
  select(isTarget)

# other variables
githubStarMedian <- githubStarMedian %>% 
  select(-isTarget)



# splitting the data and setting seed
set.seed(777)

sampleSet <- sample(nrow(githubStarMedian),
                    round(nrow(githubStarMedian)*0.75),
                    replace = FALSE
)
# Put 75% sample into training
githubStarTraining  <- githubStarMedian[sampleSet, ]
githubStarTrainingLabels <- isTargetLabels[sampleSet, ]

# Put 25% sample into testing
githubStarTesting  <- githubStarMedian[-sampleSet, ]
githubStarTestingLabels <- isTargetLabels[-sampleSet, ]

# generate the model
isTargetPrediction <- knn(train = githubStarTraining,
                           test = githubStarTesting, 
                           cl = githubStarTrainingLabels$isTarget,
                           k = 31)


# display predictions 
print(isTargetPrediction)

# display summary of the predictions 
print(summary(isTargetPrediction))

# evaluate the model using confusion matrix
githubTargetConfusionMatrix <- table(githubStarTestingLabels$isTarget,
                                  isTargetPrediction)


# display the confusion matrix 
print(githubTargetConfusionMatrix)

# calculate the accuracy
predictiveAccuracy <- sum(diag(githubTargetConfusionMatrix)) /
  nrow(githubStarTesting)


# print the accuracy
print(predictiveAccuracy)

# create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA, nrow = 0, ncol = 2)

# assign column names of "k value" and "Predictive accuracy" 
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# loop through odd values of k from 1 up to the number of records.With 
# each pass through the loop, store the k-value along with 
# its predictive accuracy.
for (kValue in 1:nrow(githubStarTraining)){
  
  # only calculate predictive value if the k value is odd
  if(kValue %% 2 !=0){
    
    # generate the model
    isTargetPrediction <- knn(train = githubStarTraining,
                               test = githubStarTesting, 
                               cl = githubStarTrainingLabels$isTarget,
                               k = kValue)
    
    # generate the confusion matrix
    githubTargetConfusionMatrix <- table(githubStarTestingLabels$isTarget,
                                         isTargetPrediction)
    
    # calculate the accuracy
    predictiveAccuracy <- sum(diag(githubTargetConfusionMatrix)) /
      nrow(githubStarTesting)
    
    # add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
  }
}

# print kValueMatrix
print(kValueMatrix)



