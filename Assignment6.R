#Assignment 6

#Question 1
#Define a defensive function that calculates the Gross Domestic Product of a nation
#from the data available in the gapminder dataset. Using that function, calculate the 
#GDP of Denmark in the following years: 1967, 1977, 1987, 1997, 2007, and 2017.


library(tidyverse)
library(gapminder)

calGDP <- function(dataset, year=NULL, country=NULL) { #Creating a function that is calculating GDP. 
                                                       # The function takes 3 parametes a dataset (gapminder),
                                                       #the year you wish to look at (optional) and the country you wish to look at (also optional)
  if(!is.null(year)) {
    dataset <- dataset[dataset$year %in% year,] # Here we use defensive programming say that if the year parameter is not null the program should
                                                # if the year exist in the dataset in the coulmn year.
  }
  if (!is.null(country)) {
    dataset <- dataset[dataset$country %in% country,] #this is the same as above, just with country instead of year.
  }
  gdp <- dataset$pop * dataset$gdpPercap #in this line the time for calculating the gdp has come, and we save an object which takes the 
                                         # population and multiply it with gdpPercap to get the countries full gdp.
  
  gapminder2 <- cbind(dataset, gdp) # Lastly we use the cbind function to bind together gapminder and gdp, so the full gdp can be red in the table
                             # when the function is called
  return(gapminder2) #The function should return the new dataset which is saved in the object gapminder2.
}

calGDP(gapminder, country = "Denmark") #The function is called, and we get the gdp for Denmark in the wiched years

# Now we can read from the new function that the gdp in the asked years is: 1967 = 77116977700, 1977 = 103920280028
# 1987 = , 1997 = 157476118456, 2007 = 192906627081, and 2017 is not in the dataset and there for not posible to calculate.



#Question 2
# Write a script that loops over each country in the gapminder dataset, tests whether the country starts with a ‘B’,
# and print out whether the life expectancy is smaller than 50, between 50 and 70, or greater than 70.



Bcountries <- grep("^B", unique(gapminder$country), value=TRUE) #The grep function searches for matches with the "^B" pattern in the gapminder dataset.


for (LifeEx in Bcountries) { #The creation of a for loop, that loops through Bcountries
  compare <- mean(gapminder[gapminder$country == LifeEx, "lifeExp"]) # The compare object is created to compare with the numbers later.
    if(compare >= 70){ #Then we use if, if else and else sentences to print different sentences depending on how high the lifeexpectancy is.
    print(paste("If you live in", LifeEx, ",you are gonna live a loooooong time")) }  
  else if (compare>=50) {
    print(paste("If you live in", LifeEx, ",you are going to die in your best age"))}
  else { print(paste("If you live in", LifeEx, ",don't count on living to long..."))
  }
}


