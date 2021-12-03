# Intro to R

# MUSA 508, Fall, 2021
# Michael Fichman, Instructor
# mfichman@design.upenn.edu

# ------ Resources -------

# A simple guide to data types and basic operations - http://www.r-tutor.com/r-introduction
# Hadley Wickham's "R For Data Science" - http://r4ds.had.co.nz/
# Stack Overflow - stackoverflow.com

# A note - almost any problem you have with your data or code has occurred before in some form,
# and it was likely discussed online and solved!

#  ---- What is R? ------
# It is a programming language used for statistics, data science and data visualization
# It is open source, backed by an enormous community of users
# It is the most quickly growing programming language and community

# ----- How do you run code in R? ------

# Let's examine the R Studio enviornment.

# Code with a "#" is called "commented code"

# Code without will run - either as written in the console or by hitting 
# ctrl+enter on code the code window

print("hello world")

# R is case sensitive - many of your issues can be resolved by checking your syntax

# ----- Data Types -----

# You can assign information to a variable using this sign "<-"

myVariable <- "derp"

# Check it out - it appears in your global environment window.

# What kind of data are these?

typeof(myVariable)

# Let's create another

anotherVariable <- 95.01234

typeof(anotherVariable)

# You can do math with a numeric variable, a double etc.,

anotherVariable + 10

anotherVariable / 4

# Data types - characters, numbers, vectors, lists

# Characters are dealt with in quotes
# Here is a vector of character data, created using the "<-" 
# and the function "c" which creates the vector
# we give it a name of our choice

characterVector <- c("A", "B", "C", "A", "A", "C", "B", "B", "A", "D")

# Numbers do not have quotes

numericVector <- c(3, 135, 37, 21, 26, 11, 15, 20, 1, 10)

# We can bind these together as columns because they are the same length

twoColumns <- cbind(characterVector, numericVector)

# This is starting to look more like the type of data we are familiar with from excel....

#  ------ Exploring the environment, loading data and using data frames  ------

# Loading Data using syntax and the RStudio dropdown - we can import our data using the 
# "Import Dataset" button and loading it From Text (base).

# Let's load some data

# Make sure you select "Yes" in the Headings option

# Let's name our data "dat"

# Alternately, we could load data using R syntax
# Notice that this example has my filepath in it!
# Use the filepath appropriate for your file!

dat <- read.csv("~/GitHub/MUSA_508_Wk1_2021/lab_1_2_data.csv")

# For mac: read.csv("/users/myname/desktop/experiment.csv")

# This is a "dataframe" - it has rows and columns like an excel sheet.

# You can look at it in R Studio like a spreadsheet

View(dat)

# EXAMINING YOUR DATA

# Look at the first five rows
head(dat)

# What are the dimensions of the data?
length(dat)

nrow(dat)

# What are the column names
names(dat)

# Let's call a column using the "$" operator
# Note the syntax - dataframe$column
# What type of data are they?

typeof(dat$total_HU.2010)

# Let's summarize it

summary(dat$total_HU.2010)

# What happens if we calculate the median of one of these columns

median(dat$vacancyPct.2016) # uh oh - this won't work. maybe some NA values?

# NA data can break operations - be aware of them in your data!

# We can omit the NA values if we want

median(dat$vacancyPct.2016, na.rm = TRUE)

# We can write out data

write.csv(dat, "your filepath goes here!")

# Loading packages

install.packages('tidyverse')
library(tidyverse)

# NEXT STEP - ENTER THE TIDYVERSE WITH TODAY'S MARKDOWN - intro_to_tidycensus


### Extras -

# Using the Tidyverse

# The tidyverse is a group of packages for data wrangling and visualization,
# statistical modeling and many other operations.

# An important feature of tidy R is the "pipe" - the %>% operator you see below

# Let's get the median vacancyPct.2016 using tidy methods
# Let's subset some data by "piping" the "filter" command (a function from the tidyverse)
# to the data set dat, and filtering out the NA data
# and then piping in the summarize command to calculate the median

dat %>%
  filter(is.na(vacancyPct.2016) == FALSE) %>%
  summarize(median(vacancyPct.2016))

# Let's pipe the "select" function to the dataframe "dat",
# select only a couple variables, and name that a new data frame, called newDat

newDat <- dat %>%
  select(GEOID, 
         NAME.x,
         total_pop.2010, 
         total_GradDeg.2010,
         pointBreeze)

# Let's overwrite our data frame and rename some columns
# by piping the "rename" function to newDat

newDat <- newDat %>%
  rename(tractName = NAME.x,
         pop_total_2010 = total_pop.2010)

# The "mutate" command is used to create new variables

newDat <- newDat  %>%
  mutate(gradPercent.2010 = total_GradDeg.2010 / pop_total_2010)

# We could output any data frame we create to file using "write.csv"
# Use the filepath appropriate for YOUR file

