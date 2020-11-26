


library(tidyverse)


abortlink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Abortions_DK_1995_2018.csv"
populationlink <- "https://raw.githubusercontent.com/RasmusVestiH/Final-Project/main/Rigtig_Data_CSV/Population_pr_municipalities.csv"
#Loading data from our datasets in github

abortions <- read_csv(url(abortlink)) #Reading the CSV files with the links above.
population <- read_csv(url(populationlink))
#Loading data from our datasets in github

head(abortions) 
head(population)
#Checking how our datasets look.

alleAldre <- filter(abortions, Alder == "Alle aldre") #Kode til at gemme et data frame så vi kun får alle aldre.

alleAldre

#______________________________________________________________________________________________________________________________________

# this is Adela's guide without testing or having data, so it may still need troubleshooting, especially in grabbing correct columns

data <- vector() #create empty vector for interim municipality values through time
final <- NULL #create empty object for final values


for (i in population$Municipalities){ 
  for (j in 1995:2018){ # check that correct columns are grabbed, use 2:ncol
    data <- NULL
    abort_mun <- abortions %>% # find the municipality row in abortions for that municipality and for Alle aldre
      filter(Municipalities == i, Alder == "Alle aldre") %>% 
      select(as.character(j))
    pop_mun <- population %>% # find the value in population for year and municiality 
      filter(Municipalities == i) %>% 
      select(as.character(j))
    newvalue <- as.numeric(abort_mun)/pop_mun
    #  print(newvalue)}}
    data <- c(data, newvalue) # DOES NOT WORK-Look up 'combining values into a digital object'
  } 
  final <- rbind(final, data)
}

final
view(final)

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
class(abortDF) # Checking if this Data fram is the same as popDF. It was not, so we chaged it by making it into tible like popDF
abortDF2 <- tibble::as_tibble(abortDF)


popDF <- pop_uden_øer[order(pop_uden_øer$Municipalities),]
class(popDF) #popDF are tible and so are abortDF2, this means that we can make the factor now that show us abortions per capita in every municipality


df4 <- abortDF2[,2:25] / popDF[,2:25] * 100
total <- cbind(popDF[,1],df4) %>% 
  .[1:96, ]
#_____________________________________________________________________________________________________________________________________________

#Next step of the proces is to make some beautiful visualisations based on the dataset "total" (which is the data set of abortions pr. capita) 
#and to make visualisations on average income with ggplot (maybe) and then look for patterns and correlations. 

#Writing our "total" Data Frame into a CSV file and naming it "Abor_Per_capita"
write.csv(total,"au557916_Thomsen_Emil/Abor_Per_capita.csv", row.names = FALSE)




