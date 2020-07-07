#testing plots for some data sets
#*****************************************************************************
#divorce_margarine
library(tidyverse)
library(dslabs)
data("divorce_margarine")

divorce_margarine %>%
  ggplot(aes(margarine_consumption_per_capita,divorce_rate_maine,color=year))+
  geom_point()

##murders
library(tidyverse)
library(dslabs)
data(murders)

murders %>%
  ggplot(aes(population,total,label=abb,color=region))+
  geom_label()

##brexit_polls trying a plot -- but complicated data set
library(tidyverse)
library(dslabs)
data("brexit_polls")

brexit_polls %>%
  ggplot(aes(poll_type,remain,label=startdate))+
  geom_point()

#******************************************************************************

#examples of some functions
#**********************************************************************************************
ls() #lists the names of objects saved in workspace
log(8) # calculates naturallog of 8, i.e. base =e (exp)
exp(1) # calculates exponential function 1 i.e. e^1
log(exp(1)) #nested function, evaluated inside out
help("log") #help file that gives more info about the func in the parantheses..
?log # for same purpose as above func.
help("+") # help file opens up for all arithmetic funcs
?"+" # same purpose as above. but important to use quotes
args(log) # to know about arguments of a function
log(8, base =2) #instead of natural log, it will calculate for base 2. note  = is used for specifying args and not <-
log(x=8,base=2) # gives same result as above. dint have to mention x before, as R assumes that args r given in the order mentioned in help file or args()
log (8,2) #gives same result as above for the same reasons as above
log2(8) # guves same result as above. 2 is the base set.
2^3 #not all funcs need parantheses. arithmetic and relational operators dont need ()
data() # lists the data sets pre built in R as objects. can be used by just mentioning the object name like CO2
view(mtcars) #opens up the table having data set content.Note Capital V.
pi  # other math objects that are pre built and can be used directly
Inf # other math objects that are pre built and can be used directly
class(a) # to determine the data type of an objectlike numeric, function, data frame..
str(murders) #to know about an object,i.e, what data type, observations, it's rows, variables involved..
names(murders) # just to know the names of the columns in a data frame. no data is displayed, unlike the str function
head(murders) # to see first 6 records of a data frame.
murders$population # $ is accessor symbol to list out all values of that column in tbe murders data table IN SAME ORDER AS FOUND IN TABLE.
murders[["population"]] # can be used instead of accessor symbol. same results
murders["population"] # will return a subset of original data frame having just this column and this new object will be of class 'data.frame' and not a character vector. bcos class(murders[["population"]]) is character.
length(murders$population) # gives the length of the vector (here 51). i.e. murders$population is a numeric vector having '51' entries. . numeric vector bcos class(murders$population) gives result as numeric. and length(murders) gives 5. i.e. there r 5 columns in this data frame.
class(murders$region) # is expected to give characters as the data type but, it is actually "factor" data type. 
#Factor data type is used to store categorical data, for memory efficiency. so they r stored as numeric instead of chars.SO here,there r 4 categories only throughout the data. i.e. 4 levels only
levels(murders$region) # lists the levels.. NorthEast, NorthCentral, South, West.
nlevels(movielens$genres) #to get the no. of levels of the factor 'genres' in moviekens data frame. Can also use length(levels(movielens$genres)) to get same answer.
length(levels(murders$region)) # will give result as 4.
seq(1,5) #results in generation of first 5 integers.
1:5 # same as above
seq(1,5,2) # results in sequence with increments of 2. unless mentioned, increment by default is 1. can be used to generate an Arithmetic progression.
nchar(murders$state) #nchar tells you how many characters long a character vector is. 
identical(a,b) # returns true or false. finds if the 2 variables are identical.
table(murders$region) # function takes vector as input and results in the frequency of each unique element in the vector.
nrow(murders) # gives the no. of rows in the data frame. Note that length(murders) gives the number of columns in the table.
##same as above by assigning a vector and then using table func.
x <- c("a", "a", "b", "b", "b", "c") # how u create a vector. use the 'c' as shown
table(x) # results in a -2, b-3, c-1
##
codes <- c(380,124,818) # creates a numeric vector
countries <- c("italy","canada","egypt") # creates a character vector
country <- c(italy=33,canada=455,egypt=555) #associates codes with countries. by assigning name to entries of vector. result will be same even if u use quotes to italy,canada, egypt.
names(codes) <- countries # the above:assigning name to the entries of a vector, can be done using 'names' function as well.
#SUBSETTING concept: to access elements fromm a vector using single []
codes[2] # gives canada-124
codes[c(1,3)] #to access more than 1 element, create a vector of the elements that u want to access. Result is italy-380, egypt-818
codes[1:2] #use sequence concept to access 1st two elements. italy,canada
codes["canada"] # if elements in the vector has names, can access by names like this.
codes[c("egypt","italy")] # to access more than 1 element from a vector where elements have names assigned.

##Vector coercion concept: R tries to be flexible with data type
x<- c(1,2,"canada") ## declared vector has 2 integer and 1 character element
class(x) ## would return character and if u print x , it would hv converted the integers to character string.
## in above example, coercion was automatic. It can also be induced using functions.
x <- 1:5
y <- as.character(x) # the integer type vector will be converted into a character vector.
z <- as.numeric(y) # to convert back to numeric data type from character type.
##Missing data concept:NAs
x <- c("1","b","2") #defined a character vector x and tryin to coerce into a numeric vector
y <- as.numeric(x) # when executed, throws a warning message: NAs introduced by coercion. and if u print value y now, it shows 1 NA 2. it did not know hoe to convert "b" into numeric, so made it as NA.
## in real time data, NAs are commonly occurring. so understand this well.

x <- seq(0, 100, length.out = 5) # argument length.out indicates the desired length of sequence. here result will be 0,25,50,75,100. i.e. 5 no.s length as indicated.
class(seq(1, 10, 0.5)) # class is shown as numeric and not integer
class(3L) # class is integer and not numeric as shown when class(3) is computed.
3L-3 # gives 0 only of 'numeric' data type
sort(murders$total) #sorts the total entries in incr order/ascending order
sort(murders$total,decreasing = TRUE) #sorts in descending order.
index <- order(murders$total) #gives the index of the total entries in incr order of their value.
#i.e. index has the values of indices of entries which when arranged  will give the value's incr order in a sorted way.
murders$state[index] #the state names will be ordered as per the incr order of the state's corresponding total. 
#So sort(x) and x[order(x)] give the same result. i.e. ascending order of x.
#This way, we get arranged names of states with least to most murders.
# the above code in a single line would be like : 
murders$state[order(murders$total)]
max(murders$total) # gives 1257 i.e largest value of total murders
min(murders$total) # gives 2 i.e smallest value of total murders)
index_max <- which.max(murders$total) #result is 5 .i.e. index of the entry having largest totals value (1257) is 5.
murders$state[index_max] # gives "California" i.e. indexing the state names with the above found index of highest murders.
##above in 1 line is:
murders$state[which.max(murders$total)] # answer is California
index_min <- which.min(murders$total) #result is 46.i.e. index of the entry having smallest totals value (2) is 46.
murders$state[index_min] # gives "Vermont" i.e. indexing the state names with the above found index of smallest murders.
##above in 1 line is:
murders$state[which.min(murders$total)] # answer is Vermont
#Rank
x<- c(31,4,15,92,65)
rank(x) #result is 3 1 2 5 4 . gives rank of original vector as if it had to be sorted.
#rank(x) gives ranks of x from lowest to highest, rank(-x) gives you the ranks from highest to lowest.
x <- c(88, 100, 83, 92, 94)
rank(-x) #result is  4 1 5 3 2, whereas, rank(x) would be 2 5 1 3 4
#
##Can create a data frame using the data.frame function. Here is a quick example:
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)
####

###
states <- murders$state
# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)
# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)
# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(states =states[ind]  , ranks =ranks[ind])
## this shows that California is the most populous and Wyoming is the least populous state.
####

mean(na_example) # to find mean of a data set. or vector
ind <- is.na(na_example) # creates a logical index ind that tells which entries are NA
sum(ind) #calculates sum of logical vector ind here.R assigns True =1, False=0. so with this we can see how many TRUEs i.e. how many NAs were there in data set. and to see how many non-NAs were there, we can do sum(!ind).

# Note what we can do with the ! operator
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind] #results in 1,3. i.e. it only displayed x values that are Not True(!ind)
##Similarly, to find avg of non NA values of data set na_example:
ind <- is.na(na_example)
# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind]) # computes avg of non NA values.
##

x<-c(1,2,3,4)#declare a vector and perform arithmetic operations on them.
2*x # operations happen element wise. so result is 2 4 6 8.
#same happens to arithmetic operations between 2 vectors of same length.

## to find the most dangerous state for murders: we need to consider the rate and not just the total murders.
murder_rate <- murders$total/murders$population*100000
#now ordering the states by murder rate, in decreasing order
murders$state[order(murder_rate,decreasing=TRUE)] #most dangerous state is thus, District of Columbia.
##

#Logical operations on vectors
murder_rate<-murders$total/murders$population*100000
index <- murder_rate<=0.71 #result is false/true
murders$state[index] #displays only those state names where murder_rate<=0.71 i.e index= TRUE.
#This way we were able to subset a vector based on properties of another vector.

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west # creates a logical vector ofthe 51 states and whether they satisfy both conditions or not.
murders$state[index] #displays entries of states that satisfy the murderrate and region. i.e. only TRUE conditions r selected.
###

##indexing functions :which, match, %in%
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x) #result is 2 4 5. it only gives indices of TRUE entries.
# find murder rate of MAssachusetts
index <- which(murders$state == "Massachusetts") # gives index of state massachusettes.
murder_rate[index] #gives 1.802.
#
#in the above code, we could hv done the same without using which func too.
index <- murders$state == "Massachusettes" # stores False/True vector for all 51 states.unlike above index.
murder_rate[index] # gives 1.802
#SO, by this method,index is a biggerobject than when index is computed using which func.
# like in the above example, if we want to find murder rates of multiple states instead of just 1. use Match function
index <- match(c("New York","Florida","Texas"),murders$state)
index # gives the indices of the 3 states in murders table. 33,10,44
murder_rate[index] # gives the murder rates of the 3 states.
#If u want to know if washington, bosto, dakota are in the murders state vector, use the in operator.
c("Washington","Dakota","Boston") %in% murders$state #Result is TRUE FALSE FALSE
##So, the %in% operator can be used to see of each element of a vector is present in another vector.
x <- c("a","b","c","d","e")
y <- c("a","b","f")
y %in% x

#
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000
# Store the murder_rate < 1 in low 
low <- murder_rate < 1
# Get the indices of entries that are below 1
which(low)
#
#
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000
# Store the murder_rate < 1 in low 
low <- murder_rate < 1
# Names of states with murder rates lower than 1
murders$state[low]
#
#to report the states in the Northeast with a murder rate lower than 1.
# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total/murders$population*100000
# Store the `murder_rate < 1` in `low` 
low <- murder_rate < 1
# Create a vector ind for states in the Northeast and with murder rates lower than 1. 
ind <- low & murders$region == "Northeast"
# Names of states in `ind` 
murders$state[ind]
#
#How many states are below the average murder_rate
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000
# Compute the average murder rate using `mean` and store it in object named `avg`
avg <- mean(murder_rate)
# How many states have murder rates below avg ? Check using sum 
sum(murder_rate<avg)
#
#use the match function to identify the states with abbreviations AK, MI, and IA.
# Store the 3 abbreviations in a vector called `abbs` (remember that they are character vectors and need quotes)
abbs <- c("AK","MI","IA")
# Match the abbs to the murders$abb and store in ind
ind <- match(abbs,murders$abb)
# Print state names from ind
murders$state[ind]
#
#Which of the following are actual abbreviations: MA, ME, MI, MO, MU?
# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs <- c("MA","ME","MI","MO","MU")
# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs %in% murders$abb
#
#We are again working with the characters abbs <- c("MA", "ME", "MI", "MO", "MU")previously we computed the index abbs%in%murders$abb. Use which and the ! operator to get the index of the entries of abbs that are not abbreviations.Show the entries of abbs that are not actual abbreviations.
# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 
# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which(!abbs %in% murders$abb)
# Names of abbreviations in `ind`
abbs[ind]
#
#To modify an existing data frame, to addcolumns, use the mutate function.
murders <- mutate(murders,rate=total/population*100000) # 1st argument is the table to be modified and 2nd arg is the new column's name 'rate' that will hold the valueof total/pop*100000.Note: for total and population we r not mentioning murders$ bcos R knows to take it from murders data frame.
head(murders) # check to see if added. To get back original data frame, reload it from the dslabs package like:
library(dslabs)
data(murders)
head(murders) # murders data frame will nomore have the rate column.
#
#To filter rows that have murder rates only less than 0.71, use the filter function.
filter(murders,rate<0.71) # will display only those states that satisfy condition.
#
#To select only a few columns form a huge table, use select func.
new_table <- select(murders,state,region,rate) #created a new object to store this new table. 1st arg is the table that we r selecting columns from.
filter(new_table,rate<0.71) #we filter this new table to get only those rows that satisfy the condition.
#
#We can do the above (i.e. selecting 3 columns from murders and filtering only those states having rate<0.71) buyeven using pipe function (%>%)
murders %>% select(state,region,rate) %>% filter(rate<0.71) #murders dtaa frame is first piped into the select func where only those 3 columns r selected and lastly the results is filtered to satify the condition mentioned.
#
#For many analyses using dplyr pckg, we will need to create data frames like.:
grades <- data.frame(names = c("John","Juan","Yao"),
                     exam_1 = c(90,85,90),
                     exam_2 = c(95,80,95),
                     stringsAsFactors = FALSE)# note that, if we dint give the last arg FALSE, by default the characters will be converted to factors type by the data.frame function.
#
#Use the function mutate to add a column rank containing the rank, from highest to lowest murder rate. Make sure you redefine murders.
# Defining rate
rate <-  murders$total/ murders$population * 100000
# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders,rank=rank(-rate))
#
#Use filter to show the top 5 states with the highest murder rates. After we add murder rate and rank, do not change the murders dataset, just show the result. Note that you can filter based on the rank column.
# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))
# Filter to show the top 5 states with the highest murder rates
filter(murders, rank == 1 | rank ==2 | rank ==3 | rank ==4 | rank ==5)
#
#Create a new data frame called no_south that removes states from the South region.
#How many states are in this category? You can use the function nrow for this.
# Use filter to create a new data frame no_south
no_south <- filter(murders, region != "South")
# Use nrow() to calculate the number of rows
nrow(no_south)
#
#Create a new data frame called murders_nw with only the states from the Northeast and the West.
#How many states are in this category?
# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders,region %in% c("Northeast","West")) #NOte, that here, u cannot interchange region and the vector created (of northeast and west), because this way u wont get the values from the table that need to be passed on to the filter function. remember, the result of an %in% operator is T/F, so we need to get all 51 obs's T/F so that it can be passed to filter function, which will finally only filter the True values and store in this new table name.
# Number of states (rows) in this category 
nrow(murders_nw)
#
#Create a table, call it my_states, that satisfies both the conditions: it is in the Northeast or West and the murder rate is less than 1.
#Use select to show only the state name, the rate and the rank
# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter(murders, rate <1 & region %in% c("Northeast","West"))
# Use select to show only the state name, the murder rate and the rank
select(my_states,state,rate,rank)
#
#Repeat the previous exercise, but now instead of creating a new object, show the result and only include the state, rate, and rank columns. using pipe symbol
# Load library
library(dplyr)
## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# show the result and only include the state, rate, and rank columns, all in one line
murders %>% filter(region %in% c("Northeast","West") & rate < 1) %>% select(state,rate,rank)
#
#Use one line of code to create a new data frame, called my_states, that has murder rate and rank columns, considers only states in the Northeast or West which have a murder rate lower than 1, and contain only the state, rate, and rank columns. The line should have four components separated by three %>% operators
# Loading the libraries
library(dplyr)
data(murders)
# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% mutate(rate=total/population*100000,rank=rank(-rate)) %>% filter(region %in% c("Northeast","West") & rate <1) %>% select(state,rate,rank)
#

## a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)
# a histogram of murder rates
#a histogram gives the graphical summary of a list of 'numbers'that gives u a general overview of the types of values ypu have.
hist(murders$rate) # Note, thearg must be a numeric vector.
# boxplots of murder rates by region
#A boxplot gives more brief summary than histogram, but they r easier to stack against each other.so we can see many distributions in 1 plot. Here we r comparing murder rates in diff regions.
#Boxplots provide a more compact summary of a distribution than a histogram and are more useful for comparing distributions. They can be produced using the boxplot() function.
boxplot(rate~region, data = murders) # we can see that the south region has the highest murder rates of all regions.
##

# load the dslabs dataset heights and determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height.How many are above avg height?
ind <- heights$height > mean(heights$height) # creates a logical vector as asked.
sum(ind) #ans is 532
#How many individuals in the dataset are above average height and are female?
sum(ind & heights$sex=="Female") # and is 31
#If you use mean() on a logical (TRUE/FALSE) vector, it returns the "proportion" of observations that are TRUE.
#What proportion of individuals in the dataset are female?
mean(heights$sex == "Female") # ans is 0.227
#Determine the minimum height in the heights dataset.
min(heights$height) # ans is 50
#Use the match() function to determine the index of the first individual with the minimum height.
match(50,heights$height) #ans is 1032
#subset the sex column of the dataset by the index in 4b to determine the individual's sex.
heights$sex[1032] #ans is Male.
#Determine the maximum height.
max(heights$height) #ans is 82.7
#Which integer values are between the maximum and minimum heights? For example, if the minimum height is 10.2 and the maximum height is 20.8, your answer should be x <- 11:20 to capture the integers in between those values. (If either the maximum or minimum height are integers, include those values too.)
#Write code to create a vector x that includes the integers between the minimum and maximum heights.
x <- 50:82
#How many of the integers in x are NOT heights in the dataset?
#Use the sum() and %in% functions in addition to the ! operator.
#So, we need to negate the (x %in% heights$height) statement with the ! operator to create a logical with TRUE for integers in x that are not a height. Then we sum this logical.
sum(!(x %in% heights$height)) #ans is 3
#Using the heights dataset, create a new column of heights in centimeters named ht_cm. Recall that 1 inch =
#2.54 centimeters. Save the resulting dataset as heights2.
#What is the height in centimeters of the 18th individual (index 18)?
heights2 <- mutate(heights, ht_cm = height*2.54)
heights2$ht_cm[18]
#What is the mean height in centimeters?
mean(heights2$ht_cm) #ans is 174
#Create a data frame females by filtering the heights2 data to contain only female individuals.
females <- filter(heights2, sex == "Female")
#How many females are in the heights2 dataset?
nrow(females) #ans is 238
#What is the mean height of the females in centimeters?
mean(females$ht_cm)
#
#The olive dataset in dslabs contains composition in percentage of eight fatty acids found in the lipid fraction of 572 Italian olive oils:
library(dslabs)
data(olive)
head(olive)
#Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. What relationship do you see?
plot(olive$palmitic, olive$palmitoleic) # ans is There is a positive linear relationship between palmitic and palmitoleic.
#Create a histogram of the percentage of eicosenoic acid in olive.
hist(olive$eicosenoic) # the ans is he most common value of eicosenoic acid is below 0.05%.
#Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.
boxplot(palmitic~region,data=olive)
#Which region has the highest median palmitic acid percentage?
#ans is Southern Italy.The median is identified with the bold line in the middle of the box.
#Which region has the most variable palmitic acid percentage?
#ans is Southern Italy.We can determine variability from the range of values each regions's palmitic acid percentage covers.
#

#Basic COnditionals: if- else, ifelse
# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.") #this will be printed finally as it satisfies the condition.
}
#
## an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate) # note we r doing this bcos in the below state we want to compare the lowest value existing with 0.5
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) #ans is Vermont. Although, New Hampshire also has a vaue below 0.5,, still.. its not the least value i.e. Vermont's.
} else{
  print("No state has murder rate that low")
}
#####** The above can be done wihtout using the conditionals in the foll way. and this way we also get to see all the states with rate<0.5 as opposed to seeing only the state with the least murder_rate.
murder_rate <- murders$total / murders$population*100000
ind <- murder_rate<0.5
murders$state[ind] #result is "New Hampshire" "Vermont". So these 2 states have rates < 0.5
result <- data.frame(state=murders$state[ind],murder_rate=murder_rate[ind]) # creating a data frame to store the states and thrie corresponding low murder rates.
result$state[which.min(result$murder_rate)] #To extract the state name with the lowest murder rate. i.e. Vermont.
######**

## the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA) # to be read as:if the 1st arg(i.e. condition) is satisfied, print the 2nd arg as result, else the 3rd arg is the result.
# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA) # result is: NA 1.0 0.5  NA 0.2
# as seen above, the condition is checked for EACH element in the vector and result is given for each of the elements in the vector.
##The function ifelse is useful because you convert a vector of logicals into something else. For example, some datasets use the number -999 to denote NA. A bad practice! You can convert the -999 in a vector to NA using the following ifelse call:
x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)
#

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas)) # result is 0. i.e. there r no more NAs.
sum(no_nas) # ans is 1968 this is the sum of the na_examples that were numeric b4. excl the NAs which r now 0s..
# this is used generally to replace a value of NA in a vector with a 0.

## the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z) # returns true if even one of the entries is true. So ans here is TRUE
all(z) # returns true only if all entries r true. so ans here is FALSE
#
##Functions
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x) # note that the variables defined inside the function are not created in workspace. they r created only when the func is called and assigned that variable inside the func.
  n <- length(x)
  s/n
}
#
# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = FALSE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
#The above func will calculate srithmetic mean or the geometric mean of x depending on user defined variable 'arithmetic'
#Note that, if u type avg(x) u will get the avg i.e. sum(x)/n and not the geom mean. bcos by default it will take the args as defined i.e. arithmetic=TRUE even if u did not mention that argument while computing it.


## a very simple for-loop
for(i in 1:5){
  print(i)
}
##We want to compute the sums of integers till n( till 1,2,3,..,25) using for loop:

# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector to store our results in
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
# creating a plot for our summation function
n <- 1:m
plot(n, s_n)
# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))
# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2) # adding lines to check with formula and we see that they coincide with the points plotted using our for loop results.
##
##LAST Assessment questions:
#What does the result return?
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives") # this is the answer
}

#Which of the following expressions is always FALSE when at least one entry of a logical vector x is TRUE?
#Options are:
all(x)
any(x)
any(!x)
all(!x) # this is the answer

#Use the ifelse function to write one line of code that assigns to the object new_names the state abbreviation when the state name is longer than 8 characters and assigns the state name when the name is not longer than 8 characters.
# Assign the state abbreviation when the state name is longer than 8 characters 
new_names <- ifelse(nchar(murders$state)>8,murders$abb,murders$state)
#
#define a function sum_n for this exercise.Create a function sum_n that for any given value, say n, creates the vector 1:n, and then computes the sum of the integers from 1 to n.
#Use the function you just defined to determine the sum of integers from 1 to 5,000.
# Create function called `sum_n`
sum_n <- function(n){
  x <- 1:n
  sum(x)
}
# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)
#
##Create a function altman_plot that takes two arguments x and y and plots y-x (on the y-axis) against x+y (on the x-axis).
# Create `altman_plot` 
altman_plot <- function(x,y){
  plot(x+y, y-x)
}
#
##Write a function compute_s_n that for any given n computes the sum Sn=12+22+32+???+n2.
#Report the value of the sum when n=10.
# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Report the value of the sum when n=10
compute_s_n(10)
#
##Define an empty numeric vector s_n of size 25 using s_n <- vector("numeric", 25).
#Compute the the sum when n is equal to each integer from 1 to 25 using the function we defined in the previous exercise: compute_s_n
#Save the results in s_n
# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Create a vector for storing results
s_n <- vector("numeric", 25)
# write a for-loop to store the results in s_n
n <- 25
for(i in 1:n){
  s_n[i] <- compute_s_n(i)
}
#
##If we do the math, we can show that Sn=12+22+32+???+n2=n(n+1)(2n+1)/6
#We have already computed the values of Sn from 1 to 25 using a for loop.
#If the formula is correct then a plot of Sn versus n should look cubic.
#Plot s_n (on the y-axis) against n (on the x-axis).
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Define the vector of n
n <- 1:25
# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}
#  Create the plot 
plot(n,s_n)
#
##Confirm that s_n and n(n+1)(2n+1)/6 are the same using the identical command.
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Define the vector of n
n <- 1:25
# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}
# Check that s_n is identical to the formula given in the instructions.
identical(s_n,n*(n+1)*(2*n+1)/6)
# If we want to check the same by potting graphs:
plot(n,s_n)
lines(n,n*(n+1)*(2*n+1)/6)
#

#***********************************************************************************************

# Rules for naming variables
#********************
#dont use spaces, start with a letter, use lowercases, use '_' instead of spaces, include comments (#), dont use object names that are already defined in R.
#********************

#**************************************************************
#Solving a quadratic equation: ax^2 + bx +c =0
#assign variables
a <- 2
b <- -1
c <- -4
#solution formulae:
solution_1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
solution_2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
# change the assigned variables a,b,c as required.
#**************************************************************

log(1024,4)
log(8,2)
library(dslabs)
data(movielens)
