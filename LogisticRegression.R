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


print(summary(githubStarMean))
print(summary(githubStarMedian))



githubStarMedian <- githubStarMedian %>% 
  select( starsScaled, forksCountScaled, 
          issueCountScaled, suscribersCountScaled,   isTarget
  )


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


