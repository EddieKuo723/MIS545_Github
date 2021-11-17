# MIS 545 Section 02

# Install the required packages
# install.packages("tidyverse")


# Load the tidyverse and neuralnet libraries
library("tidyverse")

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
  select(stars,forks_count, issue_count,network_count,
         subscribers_count, watchers_count
      )

githubStar <- githubStar%>%
  mutate(isTarget = 
           ifelse((stars*0.5 + forks_count*0.3 + subscribers_count * 0.2) >
                  mean(stars*0.5 + forks_count*0.3+ subscribers_count * 0.2),
                  TRUE,FALSE))

summary(githubStar)






