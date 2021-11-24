# MIS 545 Section 02

# Install the required packages
# install.packages("tidyverse")


# Load the tidyverse and neuralnet libraries
library("tidyverse")
library(corrplot)

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


# boxplot
githubStar %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = stars, y = keyword), fill = "red") +
  labs(title = "BoxPlot of Stars V/s Language", x = "Stars", y =
         "Lang")

                    
print(githubStar)

# histogram
githubStar %>%
  ggplot() +
  geom_histogram(mapping = aes(x = stars), bins = 30, color = "black", fill = "white") +
  labs(title = "Histogram of Stars", x = "Stars", y =
         "Frequency")


# correlation
githubCorr <- githubStar %>%
  select(stars, forks_count, issue_count, subscribers_count)

# matrix
corrMat <- cor(githubCorr)

# correlation matrix display
corrplot(corrMat, method = 'number')

