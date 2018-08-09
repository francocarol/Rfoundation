# Documentaion ----
# Description: first steps with R
# Author: C Franco
# Date: Aug 7, 2018

# Objects types ----
# 4 or more dashes define a new section/heading
names <- "C Franco"
num <- 2
logi <- TRUE


typeof(logi)
typeof(num)
typeof(names)

# Data structures ----
# Vector have one type
# Matrix 2 dimensional vector, all same type
# data.frame 2 dimesions, 
# each column can hold a different data type

# load packages ----
?library
library(dplyr)
library(tidyverse)

?tidyverse

# Download the dataset ----
?download.file

if (! file.exists("data/gapminder.csv")) # in case the file does not exist, we will download it
  download.file(url = "https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/data/gapminder-FiveYearData.csv",
                destfile = "data/gapminder.csv")

# explore dataset ----
?read.csv
gapminder <- read.csv("data/gapminder.csv")

#str : structure
?str
str(gapminder)
# glimpse
?glimpse # did noy find it
# head
?head
# tail
# dim
?dim # shows dimensions: nrows and ncol
#ncol
?ncol
#nrow
?nrow
# View for less than 100 thousand
View(gaminder)

class(gapminder$pop)

length(gapminder$country)

nrow(gapminder$country)

# Question: what if I want to know the absolute number of countries (without repetitiocs(
?unique

length(unique(gapminder$country))

#Selecting by row and column indexes
head(gapminder[1]) # gives first column
gapminder[1,] # gives first row
gapminder[1:2, 1:3] # first 2 rows and first 3 columns


#Selecting by conditions
gapminder[gapminder$year == 1952,]
gapminder[gapminder$country == "Brazil",]
