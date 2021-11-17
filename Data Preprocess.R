# MIS 545 Section 02

# Install the required packages
# install.packages("tidyverse")


# Load the tidyverse and neuralnet libraries
library("tidyverse")

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


# githubStarStat <- githubStar %>% 
#   select(stars,forks_count, issue_count,network_count,
#          subscribers_count, watchers_count
#       )

# githubStar <- githubStar%>%
#   mutate(isTarget = 
#            ifelse((stars*0.5 + forks_count*0.3 + subscribers_count * 0.2) >
#                   mean(stars*0.5 + forks_count*0.3+ subscribers_count * 0.2),
#                   TRUE,FALSE))

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

githubStar <- githubStar%>%
  mutate(isTarget = 
           ifelse(
                    (starsScaled * 0.3 + forksCountScaled * 0.2 + networkCountScaled * 0.2 
                     + suscribersCountScaled * 0.2 + watchersCountScaled * 0.1 ) >
                     mean(starsScaled * 0.3 + forksCountScaled * 0.2 + networkCountScaled * 0.2 
                          + suscribersCountScaled * 0.2 + watchersCountScaled * 0.1),
                        TRUE,FALSE
                  )
         )


print(githubStar)






