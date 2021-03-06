
library(reshape2) #Is used to restructure data with the function melt
library(tidyverse) #A package for cleaning


abortlink <- "https://raw.githubusercontent.com/Digital-Methods-HASS/au557916_Thomsen_Emil/main/OpenRefine_datasets/Abortions_DK_1995_2018.csv"
populationlink <- "https://raw.githubusercontent.com/Digital-Methods-HASS/au557916_Thomsen_Emil/main/OpenRefine_datasets/Population_pr_municipalities.csv"
incomelink <- "https://raw.githubusercontent.com/Digital-Methods-HASS/au557916_Thomsen_Emil/main/OpenRefine_datasets/Income_male_and_female.csv"
#Saving our data sets, cleaned in OpenRefine from our GitHub account.

abortions <- read_csv(url(abortlink)) #Reading the CSV files with the links above.
population <- read_csv(url(populationlink))
income <- read_csv(url(incomelink))
#Reading our data sets from the links above.

popuDF <- population[!(population$Municipalities == "Fanø" | population$Municipalities =="Samsø" | population$Municipalities =="Læsø"),]
#Removing three municipalities from the population data set as they do not occur in the abortions data set.

popuDF <- popuDF[order(popuDF$Municipalities),] #Now rearranging the data so it is alphabetical according to municipalities.


#We are now creating a function, because otherwise we would have to do this code chunk 8 times with more or less identical code.

ages <- function(agename){ # the argument of the function is the name of the different age groups, that being "allAges", "15-19", etc.
  
  agegroup <-filter(abortions, Alder == agename) #Filtering so we only get the agegroup we wish for.
  agegroup <- agegroup[!(agegroup$Municipalities == "Udland mv."),] #removing data about danish people from foreign countries.
  agegroup <- subset(agegroup, select = -c(Alder) ) #removing the column "Alder" as this function only looks at 1 agename, the one we select when we call the function.
  
  agegroup <- replace(as.data.frame(agegroup), agegroup == "-", 0) #The cells that contains "-" are replaced with "0" 
  agegroup <- replace(as.data.frame(agegroup), agegroup == "<5", 1) #the cells that contain "<5" are replaced with "1" so only numeric charactor appear. This is of course a bias in our data set, that people should know about.
  
  agegroup <- agegroup[order(agegroup$Municipalities),] #Putting our municipalities into alphabetical order.
  
  numAgegroup <- agegroup[,2:length(names(agegroup))] #Removing the Municipalities column so we only have numbers left in the data set
  numAgegroup = as.data.frame(sapply(numAgegroup, as.numeric)) #Now making them numeric
  
  numAgegroup <- numAgegroup[,1:24]/popuDF[,2:25]*100 #dividing the abortions data set with the population data set and multiplying by 100 to get abortions per capita in %
  
  agegroup <- cbind(Municipalities=c(agegroup[,1]),numAgegroup) # combining the Municipalities names with their new values.
  
  agegroup <- tibble::as_tibble(agegroup) #transforming the data into a tibble data frame to make it easier to work with.
  #We found that in other formats, when trying to order the data frame alphabetical the indexes did not change unless the data frame was a tbl_df format
  
  
  agegroup <- melt(data = agegroup, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
  #Here we are using the melt-function from the reshape2 package, found in stackoverflow.com The function takes id.vars and preserve them as they are
  #it takes the measure.vars and melts them into rows instead of columns. that way we instead get one column with all years and one with all abortions-values instead
  
  
  agegroup <- agegroup %>% rename(Year = variable) #Renaming the column variable to "Year"
  #agegroup <- agegroup %>% rename(agename = value)
  #This line does not work, maybe because the rename can't take an argument?
  
}



allAges <- ages("Alle aldre")

allAges <- allAges %>% rename(allAges = value)

age15_19 <- ages("15-19")
age15_19 <- age15_19 %>% rename(age15_19 = value)

age20_24 <- ages("20-24")
age20_24 <- age20_24 %>% rename(age20_24 = value)

age25_29 <- ages("25-29")
age25_29 <- age25_29 %>% rename(age25_29 = value)

age30_34 <- ages("30-34")
age30_34 <- age30_34 %>% rename(age30_34 = value)

age35_39 <- ages("35-39")
age35_39 <- age35_39 %>% rename(age35_39 = value)

age40_44 <- ages("40-44")
age40_44 <- age40_44 %>% rename(age40_44 = value)

age45_49 <- ages("45-49")
age45_49 <- age45_49 %>% rename(age45_49 = value)

#In the lines above we are running the function with all age groups and renaming the last column as we could not make the function do that.

final_df <- merge(allAges, age15_19, by = c("Municipalities", "Year"))
final_df <- merge(final_df, age20_24, by = c("Municipalities", "Year"))
final_df <- merge(final_df, age25_29, by = c("Municipalities", "Year"))
final_df <- merge(final_df, age30_34, by = c("Municipalities", "Year"))
final_df <- merge(final_df, age35_39, by = c("Municipalities", "Year"))
final_df <- merge(final_df, age40_44, by = c("Municipalities", "Year"))
final_df <- merge(final_df, age45_49, by = c("Municipalities", "Year"))

#Lastly we are merging our data sets into one.



#We also want an income column therefore we create one like above and ad it to our final_df

income <- income[!(income$Municipalities == "Fanø" | income$Municipalities =="Samsø" | income$Municipalities =="Læsø"),] #Removing the municipalities not in abortions df
income <- income[order(income$Municipalities),]
income <- melt(data = income, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
income <- income %>% rename(Year = variable, av_Income = value)

final_df <- merge(final_df, income, by =c("Municipalities", "Year"))

#Melting income df to make it a part of the final_df




averageagelink <- "https://raw.githubusercontent.com/Digital-Methods-HASS/au557916_Thomsen_Emil/main/OpenRefine_datasets/Average_age_municipalities.csv"
averageage <- read_csv(url(averageagelink))
#Introducing new data, that contains the average age from the different Municipalities


av_age <- averageage[!(averageage$Municipalities == "Fanø" | averageage$Municipalities =="Samsø" | averageage$Municipalities =="Læsø" | averageage$Municipalities == "Christiansø"),]
#Removing the municipalities, that does not exist in the abortions data set.



av_agemelt <- av_age <- melt(data = av_age, id.vars = c("Municipalities"), mesure.vars = c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
av_agemelt <- av_agemelt %>% rename(Year = variable, av_age = value)
#Rearranging data and renaming columns



av_agemelt <- tibble::as_tibble(av_agemelt) #Transforming df to tbl_df

av_agemelt <- av_agemelt[order(av_agemelt$Municipalities),]#Putting data frame into alphabetical order.


final_df05_18 <- merge(final_df, av_agemelt, by = c("Municipalities", "Year")) #Merging our final_df with the new age df.
#This df only have data containing the years from 2005 - 2018 as we could not find data for the other years back.



write.csv(final_df,"au557916_Thomsen_Emil/final_df_NoDanishLetters.csv", row.names = FALSE)
write.csv(final_df05_18,"au557916_Thomsen_Emil/final_df05_18_NoDanishLetters.csv", row.names = FALSE)
#Here we are writing our files into csv's so they can be downloaded and used by others, in our Git Hub repository.
