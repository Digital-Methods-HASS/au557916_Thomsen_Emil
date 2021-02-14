##-----------------------------------------------##
##    Author: Adela Sobotkova                    ##
##    Institute of Culture and Society           ##
##    Aarhus University, Aarhus, Denmark         ##
##    adela@cas.au.dk                             ##
##-----------------------------------------------##

#### Goals ####

# - Understand the provided datasets
# - Learn how to reproject spatial data
# - Limit your data into an area of interest
# - Create a new map

# We highlighted all parts of the R script in which you are supposed to add your
# own code with: 

# /Start Code/ #

print("Hello World") # This would be your code contribution

# /End Code/ #

#### Required R libraries ####

# We will use the sf, raster, and tmap packages.
# Additionally, we will use the spData and spDataLarge packages that provide new datasets. 
# These packages have been preloaded to the worker2 workspace.

library(sf)
library(raster)
library(tmap)
library(spData)
library(spDataLarge)

#### Data sets #### 

# We will use two data sets: `srtm` and `zion`.
# The first one is an elevation raster object for the Zion National Park area, and the second one is an sf object with polygons representing borders of the Zion National Park.

srtm <- raster(system.file("raster/srtm.tif", package = "spDataLarge"))
zion <- read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))

# Additionally, the last exercise (IV) will used the masked version of the `lc_data` dataset.

getwd() #checking the directory, in order to navigate to the right and downloade the right files in the next lines.

study_area <- read_sf("data/study_area.gpkg") #THe path to the data in the right directory
lc_data <- raster("data/example_landscape.tif")
lc_data_masked <- mask(crop(lc_data, study_area), study_area)

#### Exercise I ####

# 1. Display the `zion` object and view its structure.
# What can you say about the content of this file?
# What type of data does it store? 
# What is the coordinate system used?
# How many attributes does it contain?
# What is its geometry?
# 2. Display the `srtm` object and view its structure.
# What can you say about the content of this file? 
# What type of data does it store?
# What is the coordinate system used? 
# How many attributes does it contain?
# How many dimensions does it have? 
# What is the data resolution?

# Your solution (type answer to the questions as code comments and the code used)

# /Start Code/ #

# 1. Display the `zion` object and view its structure.
  # 1.1 What can you say about the content of this file?
#Checking the number of columns and rows.
nrow(zion) 
ncol(zion)
# The file consist of 1 row and 12 columns 

#1.2 What type of data does it store?
head(zion)
#Using head it can be viewed that the df consist of mostly characters, though the "DATE_EDIT" col, consists of dates and the geom col consists of "POLYGON [m] data


#1.3 What is the coordinate system used? 
st_crs(zion) #Using st_crs to check the Coordinate Reference system
#Reading in the terminal it can be seen that the coordinate system used is WGS 84.

#1.4 How many attributes does it contain?
# #Checking the number of columns and rows.
nrow(zion) 
ncol(zion)
# The file consist of 1 row and 12 columns, so 12 atributes.

#1.5 what is the geometry?
class(zion$geom)
#By checking the class it can be seen that the geometry is plygon.


#______________________________________________________

# 2. Display the `srtm` object and view its structure.
# 2.1What can you say about the content of this file?
nrow(srtm)
ncol(srtm)
#Checking the columns and rows, we can see that the df, consists of 457 rows and 465 columns.


# 2.2What type of data does it store?
class(srtm)
#checking the class of the srtm df, it can be seen that the file stores Rasterlayer and raster.


# 2.3What is the coordinate system used? 
st_crs(srtm)
#calling st_crs it can be red in the terminal that the coordinate system used is "World Geodetic System 1984"


# 2.4How many attributes does it contain?
length(attributes(srtm))
#checking the length of attributes it can be read in the terminal that there is 13 attributes.

# 2.5How many dimensions does it have? 
dim(srtm)
#checking the dimentions, it can be read in the terminal, that the dimentions is 457, 465 and 1


# 2.6What is the data resolution?
res(srtm)
#checking the resolution it can be read in the terminal that the resolution is 0.0008333333 0.0008333333


# /End Code/ #

#### Exercise II ####

# 1. Reproject the `srtm` dataset into the coordinate reference system used in the `zion` object. 
# Create a new object `srtm2`
# Vizualize the results using the `plot()` function.
# 2. Reproject the `zion` dataset into the coordinate reference system used in the `srtm` object.
# Create a new object `zion2`
# Vizualize the results using the `plot()` function.


# Your solution

# /Start Code/ #

#Saving the zion  Coordinate reference system (crs): 
the_crs <- crs(zion, asText = TRUE)

#Transforming the_crs to srtm2
#strm is a raster, so the projectRaster function can be used.
srtm2 <- projectRaster(srtm, crs = the_crs)


#Plotting the srtm2 to see how it looks with the zion crs.
plot(srtm2)


# chekking the crs of the zion and srtm2 to see if they have the same crs.
crs(zion)
crs(srtm2)



#To flip it and do it with zion data but the crs from srtm 
#we use transform as we now take the crs data from a raster to a vector: 
# Transform the zion CRS to match srtm

#creating the_crs2 by saving the srtm crs coordinates si it matches the zion's.
the_crs2 <- crs(srtm, asText = TRUE)

#creating zion2 with the crs of the_crs2
zion2 <- st_transform(zion, crs = the_crs2)

#This means we have to run the two following plots simultaneously  
plot(srtm)
plot(zion2, add = TRUE)



# /End Code/ #
