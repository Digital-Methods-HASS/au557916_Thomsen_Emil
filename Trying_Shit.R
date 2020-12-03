

library(reshape2) #Is used to restructure data
library(tidyverse)


abortlink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Abortions_DK_1995_2018.csv"
populationlink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Population_pr_municipalities.csv"
incomelink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Income_male_and_female.csv"
#Loading data from our datasets in github

abortions <- read_csv(url(abortlink)) #Reading the CSV files with the links above.
population <- read_csv(url(populationlink))
income <- read_csv(url(incomelink))
#Loading data from our datasets in github

head(abortions) 
head(population)
head(income)
#Checking how our datasets look.



#__________________________________________________________________________________________________________________________________



abort_uden_udland <- abortions[!(abortions$Municipalities == "Udland mv."),]
#here we are filtering out abortions performed on danish people in foreign countries, as they do not exist in our population dataset

pop_uden_øer <- population[!(population$Municipalities == "Fanø" | population$Municipalities =="Samsø" | population$Municipalities =="Læsø"),]
head(pop_uden_øer)
#here we are filturing out 3 islands (municipalities) as our abortions dataset does not contain them.



abort_alle_aldre_uden_udland <- alleAldre[!(alleAldre$Municipalities == "Udland mv."),]
#here we are again filtering out foreign countries but this time in a smaller dataset containing only the acumulated abortions for all ages
#in every year.

abort_alle_aldre_uden_udland = subset(abort_alle_aldre_uden_udland, select = -c(Alder) )
#here we are removing the column called "Alder" from the dataset as we do not need to see it because the datafram contains all ages acumulated.


#______________________________________________________________________________________________________________________________________________



column_list <- abort_alle_aldre_uden_udland[,2:length(names(abort_alle_aldre_uden_udland))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num <- NULL

column_list_num <- as.numeric(column_list_num)
for (col in column_list){
  num_col <- as.numeric(col)
  append(column_list_num, num_col)
}
column_list <- as.data.frame(column_list)
df2 = as.data.frame(sapply(column_list, as.numeric))
AbortNumDF <- cbind(abort_alle_aldre_uden_udland[,1],df2) %>% 
  .[1:96, ]
#In the end we append and cbind the variables to create a data frame called AbortNumDF to the first column of abort_alle_aldre_uden_udland
#And thus creating AbortNumDF which is the data frame for creating and ordering the abortions data 


#___________________________________________________________________________________________________________________________________________

abortDF <- AbortNumDF[order(AbortNumDF$Municipalities),] #Here we struggled with the index numbers not changing after ordering our data frame
#By looking what class is it we aim to solve this

class(abortDF) # Checking if this Data frame is the same as popDF. It was not, so we chaged it by making it into tible like popDF
abortDF2 <- tibble::as_tibble(abortDF)


popDF <- pop_uden_øer[order(pop_uden_øer$Municipalities),]
class(popDF) #popDF are tible and so are abortDF2, this means that we can make the factor now that show us abortions per capita in every municipality


df4 <- abortDF2[,2:25] / popDF[,2:25] * 100
total <- cbind(popDF[,1],df4) %>% 
  .[1:96, ]

income <- income[order(income$Municipalities),]
income_uden_øer <- income[!(income$Municipalities == "Fanø" | income$Municipalities =="Samsø" | income$Municipalities =="Læsø"),]
class(income_uden_øer)
income_uden_øer_melt <- melt(income_uden_øer)
class(income_uden_øer_melt)
income_uden_øer_melt <- income_uden_øer_melt[order(income_uden_øer_melt$Municipalities),]
income_uden_øer_melt <- tibble::as_tibble(income_uden_øer_melt)


total_melt <- melt(total) # rearanges data so all years is put under one variable.

total_melt <- total_melt[order(total_melt$Municipalities),] #They are put into alphabetical order

total_melt <- tibble::as_tibble(total_melt) #Transforming it to tibble
class(total_melt)



final_df <- merge(total_melt, income_uden_øer_melt, by = c("Municipalities", "variable"))



class(final_df)
final_df <- tibble::as_tibble(final_df)

head(final_df)
colnames(final_df)


#Renaming variables
names(final_df)[names(final_df) == "variable"] <- "Year"
names(final_df)[names(final_df) == "value.x"] <- "Abor_pr_capita"
names(final_df)[names(final_df) == "value.y"] <- "Income_pr_capita"



#_____________________________________________________________________________________________________________________________________________

#Now getting all age groupes in the dataset

age15_19 <- filter(abort_uden_udland, Alder == "15-19") 

age15_19 = subset(age15_19, select = -c(Alder) )

age15_19 <- replace(as.data.frame(age15_19), age15_19 == "-", 0)
age15_19 <- replace(as.data.frame(age15_19), age15_19 == "<5", 1)







age20_24 <- filter(abort_uden_udland, Alder == "20-24") 

age20_24 = subset(age20_24, select = -c(Alder) )

age20_24 <- replace(as.data.frame(age20_24), age20_24 == "-", 0)
age20_24 <- replace(as.data.frame(age20_24), age20_24 == "<5", 1)





age25_29 <- filter(abort_uden_udland, Alder == "25-29") 

age25_29 = subset(age25_29, select = -c(Alder) )

age25_29 <- replace(as.data.frame(age25_29), age25_29 == "-", 0)
age25_29 <- replace(as.data.frame(age25_29), age25_29 == "<5", 1)





age30_34 <- filter(abort_uden_udland, Alder == "30-34") 

age30_34 = subset(age30_34, select = -c(Alder) )

age30_34 <- replace(as.data.frame(age30_34), age30_34 == "-", 0)
age30_34 <- replace(as.data.frame(age30_34), age30_34 == "<5", 1)





age35_39 <- filter(abort_uden_udland, Alder == "35-39") 

age35_39 = subset(age35_39, select = -c(Alder) )

age35_39 <- replace(as.data.frame(age35_39), age35_39 == "-", 0)
age35_39 <- replace(as.data.frame(age35_39), age35_39 == "<5", 1)



age40_44 <- filter(abort_uden_udland, Alder == "40-44") 

age40_44 = subset(age40_44, select = -c(Alder) )

age40_44 <- replace(as.data.frame(age40_44), age40_44 == "-", 0)
age40_44 <- replace(as.data.frame(age40_44), age40_44 == "<5", 1)





age45_49 <- filter(abort_uden_udland, Alder == "45-49") 

age45_49 = subset(age45_49, select = -c(Alder) )

age45_49 <- replace(as.data.frame(age45_49), age45_49 == "-", 0)
age45_49 <- replace(as.data.frame(age45_49), age45_49 == "<5", 1)





column_list15_19 <- age15_19[,2:length(names(age15_19))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num15_19 <- NULL

column_list_num15_19 <- as.numeric(column_list_num15_19)
for (col15_19 in column_list15_19){
  num_col15_19 <- as.numeric(col15_19)
  append(column_list_num15_19, num_col15_19)
}
column_list15_19 <- as.data.frame(column_list15_19)
df15_19 = as.data.frame(sapply(column_list15_19, as.numeric))
AbortNumDF15_19 <- cbind(age15_19[,1],df15_19) %>% 
  .[1:96, ]



column_list20_24 <- age20_24[,2:length(names(age20_24))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num20_24 <- NULL

column_list_num20_24 <- as.numeric(column_list_num20_24)
for (col20_24 in column_list20_24){
  num_col20_24 <- as.numeric(col20_24)
  append(column_list_num20_24, num_col20_24)
}
column_list20_24 <- as.data.frame(column_list20_24)
df20_24 = as.data.frame(sapply(column_list20_24, as.numeric))
AbortNumDF20_24 <- cbind(age20_24[,1],df20_24) %>% 
  .[1:96, ]





column_list25_29 <- age25_29[,2:length(names(age25_29))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num25_29 <- NULL

column_list_num25_29 <- as.numeric(column_list_num25_29)
for (col25_29 in column_list25_29){
  num_col25_29 <- as.numeric(col25_29)
  append(column_list_num25_29, num_col25_29)
}
column_list25_29 <- as.data.frame(column_list25_29)
df25_29 = as.data.frame(sapply(column_list25_29, as.numeric))
AbortNumDF25_29 <- cbind(age25_29[,1],df25_29) %>% 
  .[1:96, ]






column_list30_34 <- age30_34[,2:length(names(age30_34))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num30_34 <- NULL

column_list_num30_34 <- as.numeric(column_list_num30_34)
for (col30_34 in column_list30_34){
  num_col30_34 <- as.numeric(col30_34)
  append(column_list_num30_34, num_col30_34)
}
column_list30_34 <- as.data.frame(column_list30_34)
df30_34 = as.data.frame(sapply(column_list30_34, as.numeric))
AbortNumDF30_34 <- cbind(age30_34[,1],df30_34) %>% 
  .[1:96, ]





column_list35_39 <- age35_39[,2:length(names(age35_39))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num35_39 <- NULL

column_list_num35_39 <- as.numeric(column_list_num35_39)
for (col35_39 in column_list35_39){
  num_col35_39 <- as.numeric(col35_39)
  append(column_list_num35_39, num_col35_39)
}
column_list35_39 <- as.data.frame(column_list35_39)
df35_39 = as.data.frame(sapply(column_list35_39, as.numeric))
AbortNumDF35_39 <- cbind(age35_39[,1],df35_39) %>% 
  .[1:96, ]





column_list40_44 <- age40_44[,2:length(names(age40_44))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num40_44 <- NULL

column_list_num40_44 <- as.numeric(column_list_num40_44)
for (col40_44 in column_list40_44){
  num_col40_44 <- as.numeric(col40_44)
  append(column_list_num40_44, num_col40_44)
}
column_list40_44 <- as.data.frame(column_list40_44)
df40_44 = as.data.frame(sapply(column_list40_44, as.numeric))
AbortNumDF40_44 <- cbind(age40_44[,1],df40_44) %>% 
  .[1:96, ]





column_list45_49 <- age45_49[,2:length(names(age45_49))]
#For the abortions we had trouble with it being characters and not numeric. In this code we make variables to take the numeric data and the looping
#Them through every column to create a variable called column_list_num

column_list_num45_49 <- NULL

column_list_num45_49 <- as.numeric(column_list_num45_49)
for (col45_49 in column_list45_49){
  num_col45_49 <- as.numeric(col45_49)
  append(column_list_num45_49, num_col45_49)
}
column_list45_49 <- as.data.frame(column_list45_49)
df45_49 = as.data.frame(sapply(column_list45_49, as.numeric))
AbortNumDF45_49 <- cbind(age45_49[,1],df45_49) %>% 
  .[1:96, ]


#abortDF <- AbortNumDF[order(AbortNumDF$Municipalities),]

AbortNumDF15_19 <- AbortNumDF15_19[order(AbortNumDF15_19$`age15_19[, 1]`),]
AbortNumDF15_19 <- AbortNumDF15_19[,2:25] / popDF[,2:25] * 100



AbortNumDF20_24
AbortNumDF25_29
AbortNumDF30_34
AbortNumDF35_39
AbortNumDF40_44
AbortNumDF45_49
#Here we are making a factor so we get abortions pr. capita in procent.



#All of these loops are the same, but creates different datasets acording to agegroups


names(AbortNumDF15_19)[names(AbortNumDF15_19) == "age15_19[, 1]"] <- "Municipalities"

names(AbortNumDF20_24)[names(AbortNumDF20_24) == "age20_24[, 1]"] <- "Municipalities"

names(AbortNumDF25_29)[names(AbortNumDF25_29) == "age25_29[, 1]"] <- "Municipalities"

names(AbortNumDF30_34)[names(AbortNumDF30_34) == "age30_34[, 1]"] <- "Municipalities"

names(AbortNumDF35_39)[names(AbortNumDF35_39) == "age35_39[, 1]"] <- "Municipalities"

names(AbortNumDF40_44)[names(AbortNumDF40_44) == "age40_44[, 1]"] <- "Municipalities"

names(AbortNumDF45_49)[names(AbortNumDF45_49) == "age45_49[, 1]"] <- "Municipalities"

#Here we are changing a column name that changed during the cleaning


#And now changing them to Tibble format

AbortNumDF15_19 <- tibble::as_tibble(AbortNumDF15_19)
AbortNumDF20_24 <- tibble::as_tibble(AbortNumDF20_24)
AbortNumDF25_29 <- tibble::as_tibble(AbortNumDF25_29)
AbortNumDF30_34 <- tibble::as_tibble(AbortNumDF30_34)
AbortNumDF35_39 <- tibble::as_tibble(AbortNumDF35_39)
AbortNumDF40_44 <- tibble::as_tibble(AbortNumDF40_44)
AbortNumDF45_49 <- tibble::as_tibble(AbortNumDF45_49)



AbortNumDF15_19 <- melt(data = AbortNumDF15_19, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
AbortNumDF20_24 <- melt(data = AbortNumDF20_24, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
AbortNumDF25_29 <- melt(data = AbortNumDF25_29, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
AbortNumDF30_34 <- melt(data = AbortNumDF30_34, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
AbortNumDF35_39 <- melt(data = AbortNumDF35_39, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
AbortNumDF40_44 <- melt(data = AbortNumDF40_44, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
AbortNumDF45_49 <- melt(data = AbortNumDF45_49, id.vars = c("Municipalities"), measure.vars = c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))


names(AbortNumDF15_19)[names(AbortNumDF15_19) == "variable"] <- "Year"
names(AbortNumDF15_19)[names(AbortNumDF15_19) == "value"] <- "Abor_pr_capita15_19"



names(AbortNumDF20_24)[names(AbortNumDF20_24) == "variable"] <- "Year"
names(AbortNumDF20_24)[names(AbortNumDF20_24) == "value"] <- "Abor_pr_capita20_24"



names(AbortNumDF25_29)[names(AbortNumDF25_29) == "variable"] <- "Year"
names(AbortNumDF25_29)[names(AbortNumDF25_29) == "value"] <- "Abor_pr_capita25_29"



names(AbortNumDF30_34)[names(AbortNumDF30_34) == "variable"] <- "Year"
names(AbortNumDF30_34)[names(AbortNumDF30_34) == "value"] <- "Abor_pr_capita30_34"



names(AbortNumDF35_39)[names(AbortNumDF35_39) == "variable"] <- "Year"
names(AbortNumDF35_39)[names(AbortNumDF35_39) == "value"] <- "Abor_pr_capita35_39"



names(AbortNumDF40_44)[names(AbortNumDF40_44) == "variable"] <- "Year"
names(AbortNumDF40_44)[names(AbortNumDF40_44) == "value"] <- "Abor_pr_capita40_44"



names(AbortNumDF45_49)[names(AbortNumDF45_49) == "variable"] <- "Year"
names(AbortNumDF45_49)[names(AbortNumDF45_49) == "value"] <- "Abor_pr_capita45_49"


#All of these variables may not be the best way of organising the data, but it is one way to rearange data so we can get one
#dataset where all different agegroups are in

final_df2 <- merge(final_df, AbortNumDF15_19, by = c("Municipalities", "Year"))
final_df2 <- merge(final_df2, AbortNumDF20_24, by = c("Municipalities", "Year"))
final_df2 <- merge(final_df2, AbortNumDF25_29, by = c("Municipalities", "Year"))
final_df2 <- merge(final_df2, AbortNumDF30_34, by = c("Municipalities", "Year"))
final_df2 <- merge(final_df2, AbortNumDF35_39, by = c("Municipalities", "Year"))
final_df2 <- merge(final_df2, AbortNumDF40_44, by = c("Municipalities", "Year"))
final_df2 <- merge(final_df2, AbortNumDF45_49, by = c("Municipalities", "Year"))

#_____________________________________________________________________________________________________________________________________



#Next step of the proces is to make some beautiful visualisations based on the dataset "total" (which is the data set of abortions pr. capita) 
#and to make visualisations on average income with ggplot (maybe) and then look for patterns and correlations. 

#Writing our "total" Data Frame into a CSV file and naming it "Abor_Per_capita"
write.csv(total,"au557916_Thomsen_Emil/Abor_Per_capita.csv", row.names = FALSE)



