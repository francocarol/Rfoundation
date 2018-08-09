# Documentaion ----
# Description: first steps with R
# Author: C Franco
# Date: Aug 8, 2018
#
# Load the packages ----
library(tidyverse)
library(dplyr)
library(plotly)
# Load the gapminder dataset
gapminder <-read.csv(file="data/gapminder.csv")
# Explore the datasets ----
glimpse(gapminder)

# Make some plots! ----
plot(gapminder$gdpPercap, gapminder$lifeExp)

?ggplot2
# A system for 'declaratively' creating graphics, based on 
#"The Grammar of Graphics". You provide the data, tell 'ggplot2'
#how to map variables to aesthetics, what graphical primitives 
#to use, and it takes care of the details.
?ggplot
# ggplot(data = NULL, mapping = aes(), ..., environment = parent.frame())
# data: x=..., y=...
# aes() = aesthetics: everything that make your plot look nicer.
#                     the columns we choose to plot, the specific char shape of one of the columns

??geom

ggplot(data= gapminder, aes(x=gdpPercap, y=lifeExp)) + #first lline: canvas
  geom_point() #2nd line: scatter plot

ggplot(data= gapminder, aes(x=year, y=pop, shape=continent)) + #first lline: canva
  geom_point() #2nd line: scatter plot (or other, if you choose another geom)
# shape: differentiate data from different columns. we can also use color or colour

ggplot(data= gapminder, aes(x=year, y=pop, shape=continent, colour=continent)) + #first lline: canva
  geom_point() #2nd line: scatter plot (or other, if you choose another geom)

ggplot(data= gapminder, aes(x=year, y=pop, shape=continent, colour=continent)) + #first lline: canva
  geom_point() + #2nd line: scatter plot (or other, if you choose another geom)
  scale_x_continuous( breaks=unique(gapminder$year) ) + # optional layer: scale layer (we want the exact year under each point)
  #scale_color_brewer(type="seq", pallete=2) # does not work yet :P
  scale_y_continuous(breaks = c(0, 100000000, 200000000, 500000000, 1000000000),
                     labels = c(0, "100 mi", "200 mi", "500 mi", "1 billon"))

# REMEMBER: we need toadd a + for a new layer

# Histogram plot

ggplot(data=gapminder, aes(x=lifeExp, fill=continent), log10 = "x") + # for hist, we use fill instead of colour
  geom_histogram(binwidth = 1)

# trying to use log scale for pop sizes...did not work very well
ggplot(data=gapminder, aes(x=pop, fill=continent)) + # for hist, we use fill instead of colour
  geom_histogram() +
  scale_x_log10()#(breaks = c(5,6,7,8,9),
                   #  labels = c("100000", "1 million", "10 million", "100 billon", "1 billion"))

# Install
install.packages("RColorBrewer")
# Load
library(RColorBrewer)
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# plotting hist by countries
ggplot(data=gapminder, aes(x=lifeExp, fill=continent), log10 = "x") + # for hist, we use fill instead of colour
  geom_histogram(binwidth = 1) +
  facet_wrap(~continent) + # ~ is for ... ?
  scale_fill_brewer(pallet="cbPalette") + # did not work
  theme_light() 

#Create a line plot of year and life expectation
ggplot(data= gapminder, aes(x=year, y=lifeExp, colour=continent,
                            by=country)) +  # aes by is equivalent to using geom_path
  geom_line() +
  facet_wrap(~continent) +
  scale_x_continuous( breaks=unique(gapminder$year) ) 

# Scatter plot gdpPercap vs lifeExp
ggplot(data= gapminder, aes(x=gdpPercap, y=lifeExp, colour=continent)) +
  geom_point() +
  geom_smooth(method="lm", size=1, colour="black")+
  # if we choose one colour aes in the 1st layer it will be inherited for the next ones, unless you reassign it inside an specific layer
  scale_x_log10() +
  labs(x="Gross domestic product per capita", y="Life expectancy")

# Plot only contries starting with letter B
starts_with <- substr(gapminder$country, start=1, stop=1)
mycountries <- gapminder[starts_with %in% c("B"),] # check if our variables are inside a specified subset (could be more than one letter, in this case)

View(mycountries)

ggplot(data= mycountries, aes(x=gdpPercap, y=lifeExp, colour=country)) +
  geom_point() +
  geom_smooth(size=1)+
  # if we choose one colour aes in the 1st layer it will be inherited for the next ones, unless you reassign it inside an specific layer
  scale_x_log10() +
  #facet_wrap(~country) +
  scale_colour_brewer(palette = "Set1") +
  labs(x="Gross domestic product per capita", y="Life expectancy")

ggplot(data= mycountries, aes(x=year, y=lifeExp, colour=continent)) +
  geom_line() +
  facet_wrap(~country) +
  scale_colour_brewer(palette = "Set1") +
  labs(x="year", y="Life expectancy")

# More than one country
# SAVING THE PLOT

mycountries <- gapminder[starts_with %in% c("E", "H"),] # check if our variables are inside a specified subset (could be more than one letter, in this case)
ggplot(data= mycountries, aes(x=year, y=lifeExp, colour=continent)) +
  geom_line() +
  facet_wrap(~country) +
  scale_colour_brewer(palette = "Set1") +
  labs(x="year", y="Life expectancy")
# Here we can save the plot
ggsave(filename="figures/lifeExp_vs_year_E_H.png")  # by default, saves your last plot

# New functions ----

# funtion name: countriesByLetter
# Parameters:
#     fl: first letter(s)
#     l
countriesByLetter <- function(fl) {
  mycountries <- gapminder[starts_with %in% fl,] # check if our variables are inside a specified subset (could be more than one letter, in this case)
  mycountries
  return(mycountries)
}

# Run the function
countriesAH <- countriesByLetter(fl=c("A", "H"))
View(countriesAH)

# Let's define a function that takes a letter as an argument than produces and saves a plot
plot_countryByLetter <- function(fl){
  mycountries <- gapminder[starts_with %in% fl,] # check if our variables are inside a specified subset (could be more than one letter, in this case)
  ggplot(data= mycountries, aes(x=year, y=lifeExp, colour=continent)) +
    geom_line() +
    facet_wrap(~country) +
    #scale_colour_brewer(palette = "Set1") +
    labs(x="year", y="Life expectancy")
  ggsave(filename=paste("figures/lifeExp_vs_year_",paste(fl, collapse="_"),".png"))  # by default, saves your last plot
}

fl0 = c("F", "U")
plot_countryByLetter(fl=fl0)

# Save data ----

View(mycountries)
write.csv(mycountries, paste("data/gapminder_letters_",paste(fl0, collapse="_"),".csv"))

#-----------------------
# Data Manipulation ----
#-----------------------

?`dplyr-package`
# dplyr provides a flexible grammar of data manipulation

#Pipes:
?'%>%'
# magrittr forward-pipe operator
# Pipe an object forward into a function or call expression.
# like the pipe in the shell, the put 2 commands together: put an input into a function
# shortcut: CNTRL + SHIFT + M

head(gapminder)
# is equivalent to
gapminder %>% head()

# Pipes are useful when you have more then one function

# EX: number of countries in gapminder: 142
gapminder$country %>%
  unique() %>%
  length()
# note that unique NEEDS to come before length function

# select() ----
# Picks variables based on their names
# select() keeps only the variables you mention; rename() keeps all variables.
# does not modify the number of rows, only the number of columns

#EX: select country and continet from the gapminder
?select
newdata <- select(gapminder, country, continent)
#OR
newdata <- gapminder %>% select(country, continent)
head(newdata)

# EX: select those columns that start with letter Cfrom the gapminder
newdata <- gapminder %>% select(starts_with("c"))
head(newdata)
# other helper functions FOR COLUMNS: ends_with, contains, matches, num_range(prefix, range), one_of...

#filter() ----
# Use filter() find rows/cases where conditions are true.
# Unlike base subsetting with [, rows where the condition evaluates to NA are dropped.
# It is analogous to selct, but only affects rows, not columns

?filter

#EX:
newdata <- filter(gapminder, country == 'Brazil')
#OR
newdata <- gapminder %>% filter(country == 'Brazil')
head(newdata)

#EX:
newdata <- filter(gapminder, year == 2002)
length(newdata)
head(newdata)

newdata <- filter(gapminder, year > '2000') # the right is WITHOUT quotes for the number, but it workf
length(newdata)
head(newdata)

# Select your country and the year 2002
newdata <- gapminder %>% filter(country == 'Brazil' & year == 2002)
#OR
newdata <- gapminder %>% filter(country == 'Brazil', year == 2002)
head(newdata)

# Select you country and the year 2002
# Tip: 2 columns and 1 row
newdata <- gapminder %>% 
  select(country, year) %>% 
  filter(country == 'Brazil', year == 2002)
head(newdata)

# Select African countries for lifeExp, country an year
# How many rows does your data have?
newdata <- gapminder %>% 
  filter(continent == 'Africa')  %>% 
  select(country, year, lifeExp) 
head(newdata)
nrow(newdata)

# Save data into anfrican_countries
african_countries <- newdata
dim(african_countries)
write_csv(african_countries, "data/african_countries.csv")

# plot
gapminder %>% 
  filter(continent == 'Africa')  %>% 
  select(country, year, lifeExp) %>% 
  ggplot(aes(x=year, y=lifeExp)) +
    geom_point(colour='blue')
