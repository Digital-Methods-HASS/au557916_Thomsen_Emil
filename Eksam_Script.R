library(tidyverse)

abortlink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Abortions_DK_1995_2018.csv"
populationlink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Population_pr_municipalities.csv"

abortions <- read_csv(url(abortlink)) #Reading the CSV files with the links above.
population <- read_csv(url(populationlink))

head(abortions)
head(population)

#______________________________________________________________________________________________________________________________________

# this is Adela's guide without testing or having data, so it may still need troubleshooting, especially in grabbing correct columns

data <- vector() #create empty vector for interim municipality values through time
final <- NULL #create empty object for final values

for (i in population$Municipalities){
  x <- abortions %>% select(distinct(Municipalities == i))  
  for (j in 1995:2018){ # check that correct columns are grabbed, use 2:ncol
    newvalue <- as.numeric(abortions[x,j])/as.numeric(population[i,j])
    data <- c(data, newvalue) # check if it survives outside the loop
  }
  final <- rbind(final, data)
}


