#Keep in mind that discrete numeric data can be considered ordinal.
#Although this is technically true, we usually reserve the term ordinal data for variables belonging to a small number of different groups, with each group having many members.
#The height variable could be ordinal if, for example, we report a small number of values such as short, medium, and tall. Let's explore how many unique values are used by the heights variable. For this we can use the unique function:
x <- c(3, 3, 3, 3, 4, 4, 2)
unique(x)
#Use the unique and length functions to determine how many unique heights were reported.
library(dslabs)
data(heights)
x <- heights$height
length(unique(x)) #Ans is 139
#
#One of the useful outputs of data visualization is that we can learn about the distribution of variables. For categorical data we can construct this distribution by simply computing the frequency of each unique value. This can be done with the function table
#compute the frequencies of each unique height value.
library(dslabs)
data(heights)
x <- heights$height
tab <- table(x)
#
#To see why treating the reported heights as an ordinal value is not useful in practice we note how many values are reported only once.
#Use logicals and the function sum to count the number of times this happens.
library(dslabs)
data(heights)
tab <- table(heights$height)
sum(tab ==1) # ans is 63
#
#Since there are a finite number of reported heights and technically the height can be considered ordinal, which of the following is true:
#(Ans)It is more effective to consider heights to be numerical given the number of unique values we observe and the fact that if we keep collecting data even more will be observed.
#It is actually preferable to consider heights ordinal since on a computer there are only a finite number of possibilities.
#This is actually a categorical variable: tall, medium or short.
#This is a numerical variable because numbers are used to represent it.
#
#To know the distribution of the sex variable in heights table, use prop.table func.
prop.table(table(heights$sex)) # this gives the proprtion if M and F in the table. ans is Female-0.226 Male -0.773
table(heights$sex) #This will give the number of M and F in the table and not their proprtions. Ans is Female 238 Male-812
#
#A distribution is a function or description that shows the possible values of a variable and how often those values occur.
#For categorical variables, the distribution describes the proportions of each category.
#A frequency table is the simplest way to show a categorical distribution. Use prop.table() to convert a table of counts to a frequency table. Barplots display the distribution of categorical variables and are a way to visualize the information in frequency tables.
#For continuous numerical data, reporting the frequency of each unique entry is not an effective summary as many or most values are unique. Instead, a distribution function is required.
#The cumulative distribution function (CDF) is a function that reports the proportion of data below a value  a  for all values of  a :  F(a)=Pr(x≤a) .
#The proportion of observations between any two values  a  and  b  can be computed from the CDF as  F(b)−F(a) .
#A histogram divides data into non-overlapping bins of the same size and plots the counts of number of values that fall in that interval.
#
#For datasets that are not normal, the CDF can be calculated manually by defining a function to compute the probability above. This function can then be applied to a range of values across the range of the dataset to calculate a CDF. Given a dataset my_data, the CDF can be calculated and plotted like this:
a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)
#
#Normal Distribution represented by mean and standard deviation.
#The normal distribution:
#Is centered around one value, the mean
#Is symmetric around the mean
#Is defined completely by its mean (μ) and standard deviation ( σ )
#Always has the same proportion of observations within a given distance 
#of the mean (for example, 95% within 2 σ)

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))
#actually, there r pre built funcs for this so we use them insted of the above formula.
# built-in mean and sd functions
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)
#
## calculate standard units z= (x-average)/SD
z <- scale(x)
# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)
# to calculate the proportion of values that meet a certain condition,
#use the mean() function on a logical vector. Because TRUE is converted to 1 and
#FALSE is converted to 0, taking the mean of this vector yields the 
#proportion of TRUE. and using sum() will give the count
#of the TRUE values.
#
#Normal distribution has a mathematically defined CDF which can be comouted using pnorm.
#Using pnorm to calculate probabilities
#Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# above, using pull function to extract height column from the  heights table.
#We can estimate the probability that a male is taller than 70.5 inches with:
1 - pnorm(70.5, mean(x), sd(x))
#
#ASSESSMENT EXERCISES
#1#What proportion of the data is between 69 and 72 inches (taller than 69 but shorter or equal to 72)? A proportion is between 0 and 1.
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
mean(x>69 & x<=72) # ans is 0.337
#can use mean to compute the proportion of entries of a logical vector that are TRUE.
#
#2#Suppose you only have avg and stdev below, but no access to x, can you approximate the proportion of the data that is between 69 and 72 inches?
#Use the normal approximation to estimate the proportion the proportion of the data that is between 69 and 72 inches.
library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72,avg,stdev)-pnorm(69,avg,stdev)
#Ans is 0.306
#
#Notice that the approximation calculated in the second question is very close to the exact calculation in the first question. The normal distribution was a useful approximation for this case.
#
#3#The normal distribution was a useful approximation for the above case.
#However, the approximation is not always useful.
#An example is for the more extreme values, often called the "tails" of the distribution.
#Let's look at an example. We can compute the proportion of heights between 79 and 81.
#Question: Use normal approximation to estimate the proportion of heights between 79 and 81 inches and save it in an object called approx.
#Report how many times bigger the actual proportion is compared to the approximation.
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81) # is 0.004926108
approx <- pnorm(81,mean(x),sd(x)) - pnorm(79,mean(x),sd(x)) # is 0.003051617
exact/approx #is 1.614261
#
#4#Someone asks you what percent of seven footers are in the National Basketball Association (NBA). Can you provide an estimate? Let's try using the normal approximation to answer this question.
##First, we will estimate the proportion of adult men that are taller than 7 feet.
##Assume that the distribution of adult men in the world as normally distributed with an average of 69 inches and a standard deviation of 3 inches.
#Question:Using the normal approximation, estimate the proportion of adult men that are taller than 7 feet, referred to as seven footers. Remember that 1 foot equals 12 inches.
#Use the pnorm function. Note that pnorm finds the proportion less than or equal to a given value, but you are asked to find the proportion greater than that value.Print out your estimate; don't store it in an object.
# use pnorm to calculate the proportion over 7 feet (7*12 inches)
1 - pnorm(7*12,69,3)
# ans is 2.866516e-07
#
#5#Now we have an approximation for the proportion, call it p, of men that are 7 feet tall or taller.
##We know that there are about 1 billion men between the ages of 18 and 40 in the world, the age range for the NBA.
##Can we use the normal distribution to estimate how many of these 1 billion men are at least seven feet tall?
#Question: Use your answer to the previous exercise to estimate the proportion of men that are seven feet tall or taller in the world and store that value as p.
#Then multiply this value by 1 billion (10^9) round the number of 18-40 year old men who are seven feet tall or taller to the nearest integer with round. (Do not store this value in an object.)
p <- 1-pnorm(7*12,69,3)
round(p*10^9) 
#without rounding, the ans is 286.6516 and after rounding it is 287
#
#6#There are about 10 National Basketball Association (NBA) players that are 7 feet tall or higher.
##Use your answer to exercise 4 to estimate the proportion of men that are seven feet tall or taller in the world and store that value as p.
##Use your answer to the previous exercise (exercise 5) to round the number of 18-40 year old men who are seven feet tall or taller to the nearest integer and store that value as N.
##Then calculate the proportion of the world's 18 to 40 year old seven footers that are in the NBA. (Do not store this value in an object.)
p <- 1 - pnorm(7*12,69,3)
N <- round(p*10^9)
10/N #Because N is the number of people in the universal data that meets given condition and 10 is the number of people that given for data NBA and same condition. the ratio is the division
#i.e. Number people 7 feet tall AND in the NBA / number of people 7 feet tall total
# ans is 0.03484321
#
#7#In the previous exerceise we estimated the proportion of seven footers in the NBA .Repeat the calculations performed in the previous question for Lebron James' height: 6 feet 8 inches. There are about 150 players, instead of 10, that are at least that tall in the NBA.
#Question:Report the estimated proportion of people at least Lebron's height that are in the NBA.
p <- 1 - pnorm((6*12)+8, 69, 3)
N <- round(p * 10^9)
150/N
#ans is 0.001220842
#
#8#In answering the previous questions, we found that it is not at all rare for a seven footer to become an NBA player.
#Question: What would be a fair critique of our calculations?
#Options#:
#Practice and talent are what make a great basketball player, not height.
#The normal approximation is not appropriate for heights.
#ANS# As seen in exercise 3, the normal approximation tends to underestimate the extreme values. It's possible that there are more seven footers than we predicted.
#As seen in exercise 3, the normal approximation tends to overestimate the extreme values. It's possible that there are less seven footers than we predicted.
#
#********QUANTILES, PERCENTILES AND BOXPLOTS************************#
library(dslabs)
data(heights)
#Use summary() on the heights$height variable to find the quartiles:
summary(heights$height)
#Find the percentiles of heights$height:
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
#Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles. 
#Note that quantile() returns a named vector. You can access the 25th and 75th percentiles like this (adapt the code for other percentile values):
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
#note, that percentiles is like a table with column percentages, so names(percentiles)==25% will
#select that particular row and so, percentiles[above chosen record i.e. "25%"] will give the ht at which 25% is found.
#
#***************###########
#The qnorm() function gives the theoretical value of a quantile with probability p of observing a value equal to or less than that quantile value given a normal distribution with mean mu and standard deviation sigma:
qnorm(p, mu, sigma)
#By default, mu=0 and sigma=1. Therefore, calling qnorm() with no arguments gives quantiles for the standard normal distribution.
qnorm(p)
#The pnorm() function gives the probability that a value from a standard normal distribution will be less than or equal to a z-score value z. Consider:
pnorm(-1.96)  #≈0.025 
#The result of pnorm() is the quantile. Note that:
qnorm(0.025) # ≈−1.96 
#qnorm() and pnorm() are inverse functions:
pnorm(qnorm(0.025)) # =0.025
#
#You can use qnorm() to determine the theoretical quantiles of a dataset: that is, the theoretical value of quantiles assuming that a dataset follows a normal distribution. Run the qnorm() function with the desired probabilities p, mean mu and standard deviation sigma. 
#Suppose male heights follow a normal distribution with a mean of 69 inches and standard deviation of 3 inches. The theoretical quantiles are: 
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
#Theoretical quantiles can be compared to sample quantiles determined 
#with the quantile function in order to evaluate whether the sample follows a normal distribution.
#
##q-q plots:
#In a QQ-plot, the sample quantiles in the observed data are compared 
#to the theoretical quantiles expected from the normal distribution. 
#If the data are well-approximated by the normal distribution, 
#then the points on the QQ-plot will fall near the identity line(sample = theoretical).
# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
# proportion of data below 69.5
mean(x <= 69.5) # ans is 0.515, so almost 50% observations r below 69.5 inches.
# calculate observed and theoretical quantiles to see whether normal distribution of male ht data is good enough an approximation
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1) #If they fall on the identity line, then its bcos normal distribution is good for this data
# make QQ-plot with scaled values to make the above code simpler.
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
#
##ASSESSMENT QUESTIONS ON QUANTILES, PERCENTILES AND BOXPLOTS
#1#When analyzing data it's often important to know the number of measurements you have for each category.
#Define a variable male that contains the male heights.Define a variable female that contains the female heights.
#Report the length of each variable.
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male) # ANS IS 812
length(female) #ans is 238
#
#2#Suppose we can't make a plot and want to compare the distributions side by side. If the number of data points is large, listing all the numbers is inpractical. A more practical approach is to look at the percentiles. We can obtain percentiles using the quantile
#Qustion: Create two five row vectors showing the 10th, 30th, 50th, 70th, and 90th percentiles for the heights of each sex called these vectors female_percentiles and male_percentiles.
#Then create a data frame called df with these two vectors as columns. The column names should be female and male and should appear in that order.
#Take a look at the df by printing it. This will provide some information on how male and female heights differ.
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
female_percentiles <- quantile(female,seq(0.1,0.9,0.2))
male_percentiles <- quantile(male,seq(0.1,0.9,0.2))
df <- data.frame(female =  female_percentiles, male= male_percentiles)
df
#result is       female     male
#10% 61.00000 65.00000
#30% 63.00000 68.00000
#50% 64.98031 69.00000
#70% 66.46417 71.00000
#90% 69.00000 73.22751
#
#3#Study the boxplots summarizing the distributions of populations sizes by country.
#Which continent has the country with the largest population size?
#Options are AFRICA, ASIA, AMERICAS, OCEANIA, EUROPE
# ANS IS Asia.. as seen in boxplot.
#
#4# Study the boxplots summarizing the distributions of populations sizes by country.
#Which continent has the largest median population? Same options as above.
# ans is Africa, by seeing the plot, it has the highest median middle line.
#
#5#Again, look at the boxplots summarizing the distributions of populations sizes by country. To the nearest million, what is the median population size for Africa?
# as seen by the middle line in the box plot, its 10 million.
#
#6#Examine the following boxplots and report approximately what proportion of countries in Europe have populations below 14 million:
# options are 0.75, 0.5,0.25,0.01
#as seen by the graph the top of the box, i.e. 75% is a little above 10 units in y axis.
#
#7#Using the boxplot as guidance, which continent shown below has the largest interquartile range for log(population)?
# options are  AFRICA, ASIA, AMERICAS, OCEANIA, EUROPE
# ans is Americas, has the largest width of the boxplot i.e. interqartile range.
#
#****Assessment: Robust summaries with outliers**********#
#1#For this chapter, we will use height data collected by Francis Galton for his genetics studies. Here we just use height of the children in the dataset:
#Compute the average and median of these data. Note: do not assign them to a variable.
library(HistData)
data(Galton)
x <- Galton$child
mean(x) # ans is 68.08847
median(x) # ans is 68.2
#
#2#Now for the same datalibrary(HistData)
data(Galton)
x <- Galton$child
sd(x) # is 2.517941
mad(x) # is 2.9652
# MAD is a simple way to quantify variation in our data. MAD calculation
#find the median of data, calculate how far each value is from the median value (i.e. take absolute value of difference between the value and the median).Now find the median of that set of differences. result is Median absolute deviation.
#
#3#In the previous exercises we saw that the mean and median are very similar and so are the standard deviation and MAD. This is expected since the data is approximated by a normal distribution which has this property.
#Now suppose that Galton made a mistake when entering the first value, forgetting to use the decimal point. You can imitate this error
#The data now has an outlier that the normal approximation does not account for. Let's see how this affects the average.
#Question: Report how many inches the average grows after this mistake. Specifically, report the difference between the average of the data with the mistake x_with_error and the data without the mistake x.
library(HistData)
data(Galton)
x <- Galton$child
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
mean(x_with_error)-mean(x) # 68.68685 - 68.08847
#ans is 0.5983836
#
#4#In the previous exercise we saw how a simple mistake in 1 out of over 900 observations can result in the average of our data increasing more than half an inch, which is a large difference in practical terms. Now let's explore the effect this outlier has on the standard deviation.
#Report how many inches the SD grows after this mistake. Specifically, report the difference between the SD of the data with the mistake x_with_error and the data without the mistake x.
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
sd(x_with_error) - sd(x) # 18.19255 - 2.517941
# ans is 15.6746
#
#5#In the previous exercises we saw how one mistake can have a substantial effect on the average and the standard deviation.
#Now we are going to see how the median and MAD are much more resistant
#to outliers. For this reason we say that they are robust summaries.
#Report how many inches the median grows after the mistake. Specifically, report the difference between the median of the data with the mistake x_with_error and the data without the mistake x.
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
median(x_with_error) - median(x) #68.2 - 68.2
#ans is 0
#
#6#We saw that the median barely changes. Now let's see how the MAD is affected.
#Report how many inches the MAD grows after the mistake. Specifically, report the difference between the MAD of the data with the mistake x_with_error and the data without the mistake x.
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
mad(x_with_error) - mad(x) # 2.9652 - 2.9652
#ans is 0
#
#7#Usefulness of EDA
#How could you use exploratory data analysis to detect that an error was made?
#Options are:
#Since it is only one value out of many, we will not be able to detect this.
#We would see an obvious shift in the distribution.
# Ans- A boxplot, histogram, or qq-plot would reveal a clear outlier.
#A scatter plot would show high levels of measurement error.
#
#8#Using EDA to explore changes
#We have seen how the average can be affected by outliers. But how large can this effect get? 
#This of course depends on the size of the outlier and the size of the dataset.
#To see how outliers can affect the average of a dataset, let's 
#write a simple function that takes the size of the outlier as input and returns the average.
#Write a function called error_avg that takes a value k and returns the average of the vector x after the first entry changed to k. Show the results for k=10000 and k=-10000.
x <- Galton$child
error_avg <- function(k){
  x[1] <- k
  mean(x)
}
error_avg(10000) #ans is 78.79784
error_avg(-10000) # ans is 57.24612
#mean(x) was 68.08847 before changing the 1st element.
#notice the massive change in mean observed by changing the first element.
#
#**********GG PLOT######################
#define a ggplot object to start a new plot and define the data component. in the below 3 ways:
library(tidyverse)
library(dslabs)
data(murders)

ggplot(data = murders)

ggplot(murders)

murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
print(p)    # this is equivalent to simply typing p
p
###
#Code: Adding layers to a plot
library(tidyverse)
library(dslabs)
data(murders)
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))
# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))
# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))
#Code: Example of aes behavior
# no error from this call
p_test <- p + geom_text(aes(population/10^6, total, label = abb))
# error - "abb" is not a globally defined variable and cannot be found outside of aes
p_test <- p + geom_text(aes(population/10^6, total), label = abb)
#
# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))
# move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)
# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)
# local aesthetics override global aesthetics
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))
#
#
#Code: Log-scale the x- and y-axis
# define p
library(tidyverse)
library(dslabs)
data(murders)
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
# efficient log scaling of the axes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()
#Code: Add labels and title
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
#Code: Change color of the points
# redefine p to be everything except the points layer
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
# make all points blue
p + geom_point(size = 3, color = "blue")
# color points by region
p + geom_point(aes(col = region), size = 3)
#Code: Add a line with average murder rate
# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)
# basic line with average murder rate for the country
p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1
# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)
#Code: Change legend title
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title
#
#
#Code: Adding themes
# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()
# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website
#
#Code: Putting it all together to assemble the plot
# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate
# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()
#
#OTHER PLOTS

#Code: Histograms in ggplot2
# load heights data
library(tidyverse)
library(dslabs)
data(heights)
# define p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))
# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)
# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")
#
#Code: Smooth density plots in ggplot2
p + geom_density()
p + geom_density(fill = "blue",col="red")
#
#Code: Quantile-quantile plots in ggplot2
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()
# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()
# QQ-plot of scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()
#
#Code: Grids of plots with the gridExtra package
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
#
#ASSESSMENT: INTRO TO GGPLOT2:
#
#1# ggplot2 basics
#Start by loading the dplyr and ggplot2 libraries as well as the murders data.
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
#Note that you can load both dplyr and ggplot2, as well as other packages, by installing and loading the tidyverse package.
#With ggplot2 plots can be saved as objects. For example we can associate a dataset with a plot object like this
p <- ggplot(data = murders)
#Because data is the first argument we don't need to spell it out. So we can write this instead:
p <- ggplot(murders)
#or, if we load dplyr, we can use the pipe:
p <- murders %>% ggplot()
#Now let us get an introduction to ggplot.
#QUESTION: What is the class of the object p?
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(murders)
class(p)
#And is "gg" "ggplot"
#
#2#Print the object p defined in exercise one
p <- ggplot(murders)
#and describe what you see.
#Options are NOTHING HAPPENS, A BLANK SLATE PLOT, A SCATTER PLOT, A HISTOGRAM
#ANS IS A BLANK SLATE PLOT
#
#3#Using the pipe %>%, create an object p associated with the heights dataset instead of with the murders dataset as in previous exercises.
data(heights)
# define ggplot object called p like in the previous exercise but using a pipe 
p <- heights %>% ggplot()
#
#4#Now we are going to add layers and the corresponding aesthetic mappings. For the murders data, we plotted total murders versus population sizes in the videos.
#Explore the murders data frame to remind yourself of the names for the two variables (total murders and population size) we want to plot and select the correct answer.
str(murders)
#Options are:
#state and abb
#total_murders and population_size
#ANS :total and population
#murders and size
#
#5#To create a scatter plot, we add a layer with the function geom_point. The aesthetic mappings require us to define the x-axis and y-axis variables respectively.
#except we have to fill in the blanks to define the two variables x and y.
#QUESTION: Fill out the sample code with the correct variable names to plot total murders versus population size.
## Fill in the blanks
murders %>% ggplot(aes(x = population, y =total )) +
  geom_point()
#
#6#Note that if we don't use argument names, we can obtain the same plot by making sure we enter the variable names in the desired order
#QUESTION:Remake the plot but flip the axes so that total is on the x-axis and population is on the y-axis.
murders %>% ggplot(aes(total, population)) + geom_point()
#
#7#If instead of points we want to add text, we can use the geom_text() or geom_label() geometries. However, note that the following code
murders %>% ggplot(aes(population, total)) +
  geom_label()
#will give us the error message: Error: geom_label requires the following missing aesthetics: label
#Why is this?
#OPtions are:
#ANS IS: We need to map a character to each point through the label argument in aes
#We need to let geom_label know what character to use in the plot
#The geom_label geometry does not require x-axis and y-axis values.
#geom_label is not a ggplot2 command
#
#8#You can also add labels to the points on a plot.
#Rewrite the code from the previous exercise to:
#add a label aesthetic to aes equal to the state abbreviation
#use geom_label instead of geom_point
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
## edit the next line to add the label
murders %>% ggplot(aes(population, total,label=abb)) +
  geom_label()
#
#9#Now let's change the color of the labels to blue. How can we do this?
#OPTIONS ARE:
#By adding a column called blue to murders
#By mapping the colors through aes because each label needs a different color
#By using the color argument in ggplot
#ANS IS: By using the color argument in geom_label because we want all colors to be blue so we do not need to map colors
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
## edit the next line to add the label
murders %>% ggplot(aes(population, total,label=abb)) +
  geom_label(color="blue")
#
#10#Now let's go ahead and make the labels blue. We previously wrote this code to add labels to our plot:
murders %>% ggplot(aes(population, total, label= abb)) +
  geom_label()
#Now we will edit this code.
#Rewrite the code above to make the labels blue by adding an argument to geom_label.
#You do not need to put the color argument inside of an aes col.
#Note that the grader expects you to use the argument color instead of col; these are equivalent.
murders %>% ggplot(aes(population, total,label= abb)) +
  geom_label(color="blue")
#
#11#Now suppose we want to use color to represent the different regions. So the states from the West will be one color, states from the Northeast another, and so on. In this case, which of the following is most appropriate:
#OPTIONS ARE:
#Adding a column called color to murders with the color we want to use
#ANS IS: Mapping the colors through the color argument of aes because each label needs a different color
#Using the color argument in ggplot
#Using the color argument in geom_label because we want all colors to be blue so we do not need to map colors
#
#12#We previously used this code to make a plot using the state abbreviations as labels:
murders %>% ggplot(aes(population, total, label = abb)) +
  geom_label()
#We are now going to add color to represent the region.
#Rewrite the code above to make the label color correspond to the state's region. Because this is a mapping, you will have to do this through the aes function. Use the existing aes function inside of the ggplot function.
## edit this code
murders %>% ggplot(aes(population, total, label = abb, color=region)) +
  geom_label()
#
#13#Now we are going to change the axes to log scales to account for the fact that the population distribution is skewed.
#Change both axes to be in the log scale on a single graph. Make sure you do not redefine p - just add the appropriate layers.
p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) + geom_label()
## add layers to p here
p + scale_x_log10() + scale_y_log10()
#
#14#Edit the code above to add the title "Gun murder data" to the plot.
p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()
# add a layer to add title to the next line
p + scale_x_log10() + 
  scale_y_log10() + ggtitle("Gun murder data")
#
#15#We are going to shift our focus from the murders dataset to explore the heights dataset.
#We use the geom_histogram function to make a histogram of the heights in the heights data frame. When reading the documentation for this function we see that it requires just one mapping, the values to be used for the histogram.
#What is the variable containing the heights in inches in the heights data frame?
#OPTIONS ARE:
#sex
#heights
#ANS IS: height
#heights$height
#
#16#We are now going to make a histogram of the heights so we will load the heights dataset.
#Create a ggplot object called p using the pipe to assign the heights data to a ggplot object.
#Assign height to the x values through the aes function.
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
# define p here
p <- heights %>% ggplot(aes(x=height))
#
#17#Add a layer to the object p (created in the previous exercise) using the geom_histogram function to make the histogram.
p <- heights %>% 
  ggplot(aes(height))
## add a layer to p
p+geom_histogram()
#
#18#Note that when we run the code from the previous exercise we get the following warning:
#stat_bin() using bins = 30. Pick better value with binwidth.
#Use the binwidth argument to change the histogram made in the previous exercise to use bins of size 1 inch.
p <- heights %>% 
  ggplot(aes(height))
## add the geom_histogram layer but with the requested argument
p + geom_histogram(binwidth=1)
#
#19#Now instead of a histogram we are going to make a smooth density plot. In this case, we will not make an object p. Instead we will render the plot using a single line of code. In the previous exercise, we could have created a histogram using one line of code like this:
heights %>% 
  ggplot(aes(height)) +
  geom_histogram()
#Now instead of geom_histogram we will use geom_density to create a smooth density plot.
#Add the appropriate layer to create a smooth density plot of heights.
## add the correct layer using +
heights %>% 
  ggplot(aes(height)) + geom_density()
#
#20#Now we are going to make density plots for males and females separately. We can do this using the group argument within the aes mapping. Because each point will be assigned to a different density depending on a variable from the dataset, we need to map within aes.
#Create separate smooth density plots for males and females by defining group by sex. Use the existing aes function inside of the ggplot function.
## add the group argument then a layer with +
heights %>% 
  ggplot(aes(height,group=sex)) + geom_density()
#
#21#In the previous exercise we made the two density plots, one for each sex, using:
heights %>% 
  ggplot(aes(height, group = sex)) + 
  geom_density()
#We can also assign groups through the color or fill argument. For example, if you type color = sex ggplot knows you want a different color for each sex. So two densities must be drawn. You can therefore skip the group = sex mapping. Using color has the added benefit that it uses color to distinguish the groups.
#Change the density plots from the previous exercise to add color.
## edit the next line to use color instead of group then add a density layer
heights %>% 
  ggplot(aes(height, color = sex)) + geom_density()
#
#22#We can also assign groups using the fill argument. When using the geom_density geometry, color creates a colored line for the smooth density plot while fill colors in the area under the curve.
#We can see what this looks like by running the following code:
  heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density() 
#However, here the second density is drawn over the other. We can change this by using something called alpha blending.
#Set the alpha parameter to 0.2 in the geom_density function to make this change.
  heights %>% ggplot(aes(height,fill=sex)) + geom_density(alpha=0.2)
#Without alpha parameter or with alpha =1,1 graph is on top of other so, intersection isnot seen, so we use te alpha bending parameter alpha.
#
  #summarize() function of dplyr
  library(tidyverse)
  library(dslabs)
  data(heights)
  # compute average and standard deviation for males
  s <- heights %>%
    filter(sex == "Male") %>%
    summarize(average = mean(height), standard_deviation = sd(height))
  # access average and standard deviation from summary table
  s$average
  s$standard_deviation
  # compute median, min and max
  heights %>%
    filter(sex == "Male") %>%
    summarize(median = median(height),
              minimum = min(height),
              maximum = max(height))
  # alternative way to get min, median, max in base R
  quantile(heights$height, c(0, 0.5, 1))
  # generates an error: summarize can only take functions that return a single value
  heights %>%
    filter(sex == "Male") %>%
    summarize(range = quantile(height, c(0, 0.5, 1)))  
#
#'.' placeholder
  ##
  library(tidyverse)
  library(dslabs)
  data(murders)
  murders <- murders %>% mutate(murder_rate = total/population*100000)
  summarize(murders, mean(murder_rate))
  # calculate US murder rate, generating a data frame
  us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum(population) * 100000)
  us_murder_rate
  # extract the numeric US murder rate with the dot operator
  us_murder_rate %>% .$rate
  # calculate and extract the murder rate with one pipe
  us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum(population) * 100000) %>%
    .$rate
  #
#group by then summarize
  # libraries and data
  library(tidyverse)
  library(dslabs)
  data(heights)
  data(murders)
  # compute separate average and standard deviation for male/female heights
  heights %>%
    group_by(sex) %>%
    summarize(average = mean(height), standard_deviation = sd(height))
  # compute median murder rate in 4 regions of country
  murders <- murders %>%
    mutate(murder_rate = total/population * 100000)
  murders %>%
    group_by(region) %>%
    summarize(median_rate = median(murder_rate))
#
# arrange, desc and top_n funcs
  # libraries and data
  library(tidyverse)
  library(dslabs)
  data(murders)
  # set up murders object
  murders <- murders %>%
    mutate(murder_rate = total/population * 100000)
  # arrange by population column, smallest to largest
  murders %>% arrange(population) %>% head()
  # arrange by murder rate, smallest to largest
  murders %>% arrange(murder_rate) %>% head()
  # arrange by murder rate in descending order
  murders %>% arrange(desc(murder_rate)) %>% head()
  # arrange by region alphabetically, then by murder rate within each region
  murders %>% arrange(region, murder_rate) %>% head()
  # show the top 10 states with highest murder rate, not ordered by rate
  murders %>% top_n(10, murder_rate)
  # show the top 10 states with highest murder rate, ordered by rate
  murders %>% arrange(desc(murder_rate)) %>% top_n(10)
  #
#
#***SUMMARIZING WITH DPLYR: ASSESSMENT
  ##To practice our dplyr skills we will be working with data from the survey collected by the United States National Center for Health Statistics (NCHS). This center has conducted a series of health and nutrition surveys since the 1960’s.
#  Starting in 1999, about 5,000 individuals of all ages have been interviewed every year and then they complete the health examination component of the survey. Part of this dataset is made available via the NHANES package which can be loaded this way:
  library(NHANES)
  data(NHANES)
#  The NHANES data has many missing values. Remember that the main summarization function in R will return NA if any of the entries of the input vector is an NA. Here is an example:
  library(dslabs)
  data(na_example)
  mean(na_example)
  sd(na_example)
  #To ignore the NAs, we can use the na.rm argument:
    mean(na_example, na.rm = TRUE)
  sd(na_example, na.rm = TRUE)
#  Try running this code, then let us know you are ready to proceed with the analysis.
#
  #1#Let's explore the NHANES data. We will be exploring blood pressure in this dataset.
  #First let's select a group to set the standard. We will use 20-29 year old females. Note that the category is coded with 20-29, with a space in front of the 20! The AgeDecade is a categorical variable with these ages.
#To know if someone is female, you can look at the Gender variable.
#Filter the NHANES dataset so that only 20-29 year old females are included and assign this new data frame to the object tab.
#Use the pipe to apply the function filter, with the appropriate logicals, to NHANES.
#Remember that this age group is coded with 20-29, which includes a space. You can use head to explore the NHANES table to construct the correct call to filter.
  library(dplyr)
  library(NHANES)
  data(NHANES)
  ## fill in what is needed
  tab <- NHANES %>% filter(AgeDecade ==" 20-29" & Gender == "female")
  #
  #2#ow we will compute the average and standard deviation for the subgroup we defined in the previous exercise (20-29 year old females), which we will use reference for what is typical.
  #You will determine the average and standard deviation of systolic blood pressure, which are stored in the BPSysAve variable in the NHANES dataset.
  #Complete the line of code to save the average and standard deviation of systolic blood pressure as average and standard_deviation to a variable called ref.
  #Use the summarize function after filtering for 20-29 year old females and connect the results using the pipe %>%. When doing this remember there are NAs in the data!
  library(dplyr)
  library(NHANES)
  data(NHANES)
  ## complete this line of code.
  ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))  
  #
  #3#Now we will repeat the exercise and generate only the average blood pressure for 20-29 year old females. For this exercise, you should review how to use the place holder . in dplyr or the pull function.
  #Modify the line of sample code to assign the average to a numeric variable called ref_avg using the . or pull.
  library(dplyr)
  library(NHANES)
  data(NHANES)
  ## modify the code we wrote for previous exercise.
  ref_avg <- NHANES %>%
    filter(AgeDecade == " 20-29" & Gender == "female") %>%
    summarize(average = mean(BPSysAve, na.rm = TRUE), 
              standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average
  #
  #4#Let's continue practicing by calculating two other data summaries: the minimum and the maximum.
  #Again we will do it for the BPSysAve variable and the group of 20-29 year old females.
  #Report the min and max values for the same group as in the previous exercises.
  #Use filter and summarize connected by the pipe %>% again. The functions min and max can be used to get the values you want.
  #Within summarize, save the min and max of systolic blood pressure as minbp and maxbp.
  library(dplyr)
  library(NHANES)
  data(NHANES)
  ## complete the line
  NHANES %>%
    filter(AgeDecade == " 20-29"  & Gender == "female") %>% summarize(minbp = min(BPSysAve, na.rm = TRUE), maxbp = max(BPSysAve, na.rm = TRUE))
  #
  #5#Now let's practice using the group_by function.
  #What we are about to do is a very common operation in data science: you will split a data table into groups and then compute summary statistics for each group.
#  We will compute the average and standard deviation of systolic blood pressure for females for each age group separately. Remember that the age groups are contained in AgeDecade.
#  Use the functions filter, group_by, summarize, and the pipe %>% to compute the average and standard deviation of systolic blood pressure for females for each age group separately.
 # Within summarize, save the average and standard deviation of systolic blood pressure (BPSysAve) as average and standard_deviation.
#  Note: ignore warnings about implicit NAs. This warning will not prevent your code from running or being graded correctly.
  library(dplyr)
  library(NHANES)
  data(NHANES)
  ##complete the line with group_by and summarize
  NHANES %>%
    filter(Gender == "female") %>%group_by(AgeDecade) %>% summarize(average = mean(BPSysAve,na.rm = TRUE), standard_deviation = sd(BPSysAve,na.rm = TRUE))
  #
  #6#Now let's practice using group_by some more. We are going to repeat the previous exercise of calculating the average and standard deviation of systolic blood pressure, but for males instead of females.
  #This time we will not provide much sample code. You are on your own!
  #Calculate the average and standard deviation of systolic blood pressure for males for each age group separately using the same methods as in the previous exercise.
  #Note: ignore warnings about implicit NAs. This warning will not prevent your code from running or being graded correctly.
  library(dplyr)
  library(NHANES)
  data(NHANES)
  
  NHANES %>% filter(Gender == "male") %>% group_by(AgeDecade) %>% summarize(average = mean(BPSysAve,na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))
  #
  #7#We can actually combine both of these summaries into a single line of code. This is because group_by permits us to group by more than one variable.
 #We can use group_by(AgeDecade, Gender) to group by both age decades and gender.
  #Create a single summary table for the average and standard deviation of systolic blood pressure using group_by(AgeDecade, Gender).
  #Note that we no longer have to filter!
  #Your code within summarize should remain the same as in the previous exercises.
  #Note: ignore warnings about implicit NAs. This warning will not prevent your code from running or being graded correctly.
  library(NHANES)
  data(NHANES)
  NHANES %>% group_by(AgeDecade, Gender) %>% summarize(average=mean(BPSysAve,na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))
  #
  #8#Now we are going to explore differences in systolic blood pressure across races, as reported in the Race1 variable.
  #We will learn to use the arrange function to order the outcome acording to one variable.
  #Note that this function can be used to order any table by a given outcome. Here is an example that arranges by systolic blood pressure.
  NHANES %>% arrange(BPSysAve)
  #If we want it in descending order we can use the desc function like this:
    NHANES %>% arrange(desc(BPSysAve))
  #In this example, we will compare systolic blood pressure across values of the Race1 variable for males between the ages of 40-49.
  #Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49.
  #Order the resulting table from lowest to highest average systolic blood pressure.
  #Use the functions filter, group_by, summarize, arrange, and the pipe %>% to do this in one line of code.
  #Within summarize, save the average and standard deviation of systolic blood pressure as average and standard_deviation.
  
    library(dplyr)
    library(NHANES)
    data(NHANES)
    NHANES %>% filter(Gender=="male" & AgeDecade==" 40-49") %>% group_by(Race1) %>% summarize(average=mean(BPSysAve,na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE)) %>% arrange(average)
    #
    #
    #GAPMINDER: World health and Economics
    # load and inspect gapminder data
    library(dslabs)
    data(gapminder)
    head(gapminder)
    
    # compare infant mortality in Sri Lanka and Turkey
    gapminder %>%
      filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
      select(country, infant_mortality)
    #
#A#  Prevalent worldview is that the world is divided into two groups of countries:
  #  Western world: high life expectancy, low fertility rate
  #  Developing world: lower life expectancy, higher fertility rate
  #  Gapminder data can be used to evaluate the validity of this view.
  #  A scatterplot of life expectancy versus fertility rate in 1962 suggests that this viewpoint was grounded in reality 50 years ago. Is it still the case today?
    # basic scatterplot of life expectancy versus fertility
    ds_theme_set()    # set plot theme
    filter(gapminder, year == 1962) %>%
      ggplot(aes(fertility, life_expectancy)) +
      geom_point()
    # add color as continent
    filter(gapminder, year == 1962) %>%
      ggplot(aes(fertility, life_expectancy, color = continent)) +
      geom_point()      
    #
    #
#Faceting:
  #Faceting makes multiple side-by-side plots stratified by some variable. This is a way to ease comparisons.
  # The facet_grid() function allows faceting by up to two variables, with rows faceted by one variable and columns faceted by the other variable. To facet by only one variable, use the dot operator as the other variable.
  # The facet_wrap() function facets by one variable and automatically wraps the series of plots so they have readable dimensions.
  #  Faceting keeps the axes fixed across all plots, easing comparisons between plots.
  # The data suggest that the developing versus Western world view no longer makes sense in 2012.
  #
    # facet by continent and year
    filter(gapminder, year %in% c(1962, 2012)) %>%
      ggplot(aes(fertility, life_expectancy, col = continent)) +
      geom_point() +
      facet_grid(continent ~ year)
    # facet by year only
    filter(gapminder, year %in% c(1962, 2012)) %>%
      ggplot(aes(fertility, life_expectancy, col = continent)) +
      geom_point() +
      facet_grid(. ~ year)
    # facet by year, plots wrapped onto multiple rows
    years <- c(1962, 1980, 1990, 2000, 2012)
    continents <- c("Europe", "Asia")
    gapminder %>%
      filter(year %in% years & continent %in% continents) %>%
      ggplot(aes(fertility, life_expectancy, col = continent)) +
      geom_point() +
      facet_wrap(~year)  
  #
  # TIME SERIES PLOT
#    Time series plots have time on the x-axis and a variable of interest on the y-axis.
#    The geom_line() geometry connects adjacent data points to form a continuous line. A line plot is appropriate when points are regularly spaced, densely packed and from a single data series.
#   You can plot multiple lines on the same graph. Remember to group or color by a variable so that the lines are plotted independently.
#    Labeling is usually preferred over legends. However, legends are easier to make and appear by default. Add a label with geom_text(), specifying the coordinates where the label should appear on the graph.
  #Code: Single time series
    # scatterplot of US fertility by year
    gapminder %>%
      filter(country == "United States") %>%
      ggplot(aes(year, fertility)) +
      geom_point()
    # line plot of US fertility by year
    gapminder %>%
      filter(country == "United States") %>%
      ggplot(aes(year, fertility)) +
      geom_line()
  #  Code: Multiple time series
    # line plot fertility time series for two countries- only one line (incorrect)
    countries <- c("South Korea", "Germany")
    gapminder %>% filter(country %in% countries) %>%
      ggplot(aes(year, fertility)) +
      geom_line()
    # line plot fertility time series for two countries - one line per country
    gapminder %>% filter(country %in% countries) %>%
      ggplot(aes(year, fertility, group = country)) +
      geom_line()
    # fertility time series for two countries - lines colored by country
    gapminder %>% filter(country %in% countries) %>%
      ggplot(aes(year, fertility, col = country)) +
      geom_line()
#    Code: Adding text labels to a plot
    # life expectancy time series - lines colored by country and labeled, no legend
    labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
    gapminder %>% filter(country %in% countries) %>%
      ggplot(aes(year, life_expectancy, col = country)) +
      geom_line() +
      geom_text(data = labels, aes(x, y, label = country), size = 5) +
      theme(legend.position = "none")
  ####  
##We use GDP data to compute income in US dollars per day, adjusted for inflation.
  #  Log transformations convert multiplicative changes into additive changes.
  #  Common transformations are the log base 2 transformation and the log base 10 transformation. The choice of base depends on the range of the data. The natural log is not recommended for visualization because it is difficult to interpret.
  #  The mode of a distribution is the value with the highest frequency. The mode of a normal distribution is the average. A distribution can have multiple local modes.
  #  There are two ways to use log transformations in plots: transform the data before plotting or transform the axes of the plot. Log scales have the advantage of showing the original values as axis labels, while log transformed values ease interpretation of intermediate values between labels.
  #  Scale the x-axis using scale_x_continuous() or scale_x_log10() layers in ggplot2. Similar functions exist for the y-axis.
  #  In 1970, income distribution is bimodal, consistent with the dichotomous Western versus developing worldview.
  #  Code
    # add dollars per day variable
    gapminder <- gapminder %>%
      mutate(dollars_per_day = gdp/population/365)
    # histogram of dollars per day
    past_year <- 1970
    gapminder %>%
      filter(year == past_year & !is.na(gdp)) %>%
      ggplot(aes(dollars_per_day)) +
      geom_histogram(binwidth = 1, color = "black")
    # repeat histogram with log2 scaled data
    gapminder %>%
      filter(year == past_year & !is.na(gdp)) %>%
      ggplot(aes(log2(dollars_per_day))) +
      geom_histogram(binwidth = 1, color = "black")
    
    # repeat histogram with log2 scaled x-axis
    gapminder %>%
      filter(year == past_year & !is.na(gdp)) %>%
      ggplot(aes(dollars_per_day)) +
      geom_histogram(binwidth = 1, color = "black") +
      scale_x_continuous(trans = "log2")
    ##
    #
#Make boxplots stratified by a categorical variable using the geom_boxplot() geometry.
  #  Rotate axis labels by changing the theme through element_text(). You can change the angle and justification of the text labels.
  #  Consider ordering your factors by a meaningful value with the reorder() function, which changes the order of factor levels based on a related numeric vector. This is a way to ease comparisons.
   # Show the data by adding data points to the boxplot with a geom_point() layer. This adds information beyond the five-number summary to your plot, but too many data points it can obfuscate your message.
  #  Code: Boxplot of GDP by region
    # add dollars per day variable
    gapminder <- gapminder %>%
      mutate(dollars_per_day = gdp/population/365)
    # number of regions
    length(levels(gapminder$region))
    # boxplot of GDP by region in 1970
    past_year <- 1970
    p <- gapminder %>%
      filter(year == past_year & !is.na(gdp)) %>%
      ggplot(aes(region, dollars_per_day))
    p + geom_boxplot()
    # rotate names on x-axis
    p + geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
#    Code: The reorder function
    # by default, factor order is alphabetical
    fac <- factor(c("Asia", "Asia", "West", "West", "West"))
    levels(fac)
    # reorder factor by the category means
    value <- c(10, 11, 12, 6, 4)
    fac <- reorder(fac, value, FUN = mean)
    levels(fac)
  #  Code: Enhanced boxplot ordered by median income, scaled, and showing data
    # reorder by median income and color by continent
    p <- gapminder %>%
      filter(year == past_year & !is.na(gdp)) %>%
      mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
      ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("")
    p
    # log2 scale y-axis
    p + scale_y_continuous(trans = "log2")
    # add data points
    p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)    
##
  #Use intersect() to find the overlap between two vectors.
  #  To make boxplots where grouped variables are adjacaent, color the boxplot by a factor instead of faceting by that factor. This is a way to ease comparisons.
  #  The data suggest that the income gap between rich and poor countries has narrowed, not expanded.
  #  Code: Histogram of income in West versus developing world, 1970 and 2010
    # add dollars per day variable and define past year
    gapminder <- gapminder %>%
      mutate(dollars_per_day = gdp/population/365)
    past_year <- 1970
    
    # define Western countries
    west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
    
    # facet by West vs devloping
    gapminder %>%
      filter(year == past_year & !is.na(gdp)) %>%
      mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
      ggplot(aes(dollars_per_day)) +
      geom_histogram(binwidth = 1, color = "black") +
      scale_x_continuous(trans = "log2") +
      facet_grid(. ~ group)
    
    # facet by West/developing and year
    present_year <- 2010
    gapminder %>%
      filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
      mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
      ggplot(aes(dollars_per_day)) +
      geom_histogram(binwidth = 1, color = "black") +
      scale_x_continuous(trans = "log2") +
      facet_grid(year ~ group)
  #  Code: Income distribution of West versus developing world, only countries with data 
    # define countries that have data available in both years
    country_list_1 <- gapminder %>%
      filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
    country_list_2 <- gapminder %>%
      filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
    country_list <- intersect(country_list_1, country_list_2)
    
    # make histogram including only countries with data available in both years
    gapminder %>%
      filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
      mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
      ggplot(aes(dollars_per_day)) +
      geom_histogram(binwidth = 1, color = "black") +
      scale_x_continuous(trans = "log2") +
      facet_grid(year ~ group)
    #Code: Boxplots of income in West versus developing world, 1970 and 2010
    p <- gapminder %>%
      filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
      mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
      ggplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("") + scale_y_continuous(trans = "log2")
    
    p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
      facet_grid(year ~ .)
    
    # arrange matching boxplots next to each other, colored by year
    p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))  
  #
  ##
#
    #Density Plots
  #  Change the y-axis of density plots to variable counts using ..count.. as the y argument.
  #  The case_when() function defines a factor whose levels are defined by a variety of logical operations to group data.
  #  Plot stacked density plots using position="stack".
  #  Define a weight aesthetic mapping to change the relative weights of density plots - for example, this allows weighting of plots by population rather than number of countries.
  #  Code: Faceted smooth density plots
    # see the code below the previous video for variable definitions
    
    # smooth density plots - area under each curve adds to 1
    gapminder %>%
      filter(year == past_year & country %in% country_list) %>%
      mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
      summarize(n = n()) %>% knitr::kable()
    
    # smooth density plots - variable counts on y-axis
    p <- gapminder %>%
      filter(year == past_year & country %in% country_list) %>%
      mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
      ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
      scale_x_continuous(trans = "log2")
    p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)
  #  Code: Add new region groups with case_when
    # add group as a factor, grouping regions
    gapminder <- gapminder %>%
      mutate(group = case_when(
        .$region %in% west ~ "West",
        .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
        .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
        .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
        TRUE ~ "Others"))
    
    # reorder factor levels
    gapminder <- gapminder %>%
      mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
  #  Code: Stacked density plot
    # note you must redefine p with the new gapminder object first
    p <- gapminder %>%
      filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
      ggplot(aes(dollars_per_day, fill = group)) +
      scale_x_continuous(trans = "log2")
    
    # stacked density plot
    p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
      facet_grid(year ~ .)
   # Code: Weighted stacked density plot
    # weighted stacked density plot
    gapminder %>%
      filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
      group_by(year) %>%
      mutate(weight = population/sum(population*2)) %>%
      ungroup() %>%
      ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
      scale_x_continuous(trans = "log2") +
      geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)
  
##
    #
    #Ecological Fallacy
#The breaks argument allows us to set the location of the axis labels and tick marks.
#    The logistic or logit transformation is defined as f(p)=log
#    (p/1−p)
    
#    , or the log of odds. This scale is useful for highlighting differences near 0 or near 1 and converts fold changes into constant increases.
#    The ecological fallacy is assuming that conclusions made from the average of a group apply to all members of that group.
#    Code
    # define gapminder
    library(tidyverse)
    library(dslabs)
    data(gapminder)
    
    # add additional cases
    gapminder <- gapminder %>%
      mutate(group = case_when(
        .$region %in% west ~ "The West",
        .$region %in% "Northern Africa" ~ "Northern Africa",
        .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
        .$region == "Southern Asia" ~ "Southern Asia",
        .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
        .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
        .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
    
    # define a data frame with group average income and average infant survival rate
    surv_income <- gapminder %>%
      filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
      group_by(group) %>%
      summarize(income = sum(gdp)/sum(population)/365,
                infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
    surv_income %>% arrange(income)
    
    # plot infant survival versus income, with transformed axes
    surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
      scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
      scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                         breaks = c(.85, .90, .95, .99, .995, .998)) +
      geom_label(size = 3, show.legend = FALSE)
  ##
    #
  #### ASSESSMENT: Exploring the Gapminder Dataset
    
#1#Using ggplot and the points layer, create a scatter plot of life expectancy versus fertility for the African continent in 2012.
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    ## fill out the missing parts in filter and aes
    gapminder %>% filter( year == 2012 & continent == "Africa") %>%
      ggplot(aes(fertility,life_expectancy)) +
      geom_point()
  
#2# Remake the plot from the previous exercises but this time use color to distinguish the different regions of Africa to see if this explains the clusters. Remember that you can explore the gapminder data to see how the regions of Africa are labeled in the data frame!
    #Use color rather than col inside your ggplot call - while these two forms are equivalent in R, the grader specifically looks for color.
    
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    gapminder %>% filter(year == 2012 & continent =="Africa") %>% 
      ggplot(aes(fertility,life_expectancy, color=region)) + geom_point() 

#3# While many of the countries in the high life expectancy/low fertility cluster are from Northern Africa, three countries are not.
#    Create a table showing the country and region for the African countries (use select) that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
#    Assign your result to a data frame called df.
    library(dplyr)
    library(dslabs)
    data(gapminder)
    df <- data.frame(gapminder %>% 
          filter(continent == "Africa" & year == 2012 & fertility <= 3 & life_expectancy >= 70) %>% select(country,region))
    
#4# The Vietnam War lasted from 1955 to 1975. Do the data support war having a negative effect on life expectancy? We will create a time series plot that covers the period from 1960 to 2010 of life expectancy for Vietnam and the United States, using color to distinguish the two countries. In this start we start the analysis by generating a table.
#    Use filter to create a table with data for the years from 1960 to 2010 in Vietnam and the United States.
#    Save the table in an object called tab.
    library(dplyr)
    library(dslabs)
    data(gapminder)
    tab <- data.frame(gapminder %>%
          filter(year >= 1960 & year<= 2010 & country %in% c("Vietnam","United States")))    
#5# Now that you have created the data table in Exercise 4, it is time to plot the data for the two countries.
#    Use geom_line to plot life expectancy vs year for Vietnam and the United States and save the plot as p. The data table is stored in tab.
#    Use color to distinguish the two countries.
#    Print the object p.    

    p <- tab %>% ggplot(aes(year, life_expectancy, color = country)) + geom_line()
    # code for your plot goes here - the data table is stored as `tab`
    p
#6# Cambodia was also involved in this conflict and, after the war, Pol Pot and his communist Khmer Rouge took control and ruled Cambodia from 1975 to 1979. He is considered one of the most brutal dictators in history. Do the data support this claim?
    #Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    gapminder %>% filter( year >= 1960 & year <= 2010 & country == "Cambodia")  %>% ggplot(aes(year,life_expectancy)) + geom_line()

#7# Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.
#    In the first part of this analysis, we will create the dollars per day variable.
#    Use mutate to create a dollars_per_day variable, which is defined as gdp/population/365.
#    Create the dollars_per_day variable for African countries for the year 2010.
 #   Remove any NA values.
#    Save the mutated dataset as daydollars.
    
    library(dplyr)
    library(dslabs)
    data(gapminder)
    daydollars <- gapminder %>% filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365)
    # write your code here
#8#Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.
#    In the second part of this analysis, we will plot the smooth density plot using a log (base 2) x axis.
 #   The dataset including the dollars_per_day variable is preloaded as daydollars.
#    Create a smooth density plot of dollars per day from daydollars.
 #   Use scale_x_continuous to change the x-axis to a log (base 2) scale.
    
    daydollars %>% ggplot(aes(dollars_per_day)) + geom_density() + scale_x_continuous(trans ="log2")
    
#9# Now we are going to combine the plotting tools we have used in the past two exercises to create density plots for multiple years.
  #  Create the dollars_per_day variable as in Exercise 7, but for African countries in the years 1970 and 2010 this time.
  #  Make sure you remove any NA values.
  #  Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
  #  Use facet_grid to show a different density plot for 1970 and 2010.
    
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    
    gapminder %>% filter(year %in% c(1970,2010) & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day= gdp/population/365) %>% ggplot(aes(dollars_per_day)) + geom_density() + scale_x_continuous(trans="log2") + facet_grid (year~.)    

  #10# Now we are going to edit the code from Exercise 9 to show a stacked density plot of each region in Africa.
#    Much of the code will be the same as in Exercise 9:
 #     Create the dollars_per_day variable as in Exercise 7, but for African countries in the years 1970 and 2010 this time.
#    Make sure you remove any NA values.
#    Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
#    Use facet_grid to show a different density plot for 1970 and 2010.
#    Make sure the densities are smooth by using bw = 0.5.
#    Use the fill and position arguments where appropriate to create the stacked density plot of each region.  
    
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    gapminder %>% filter(year %in% c(1970,2010) &
                           continent == "Africa" & !is.na(gdp))  %>% mutate(dollars_per_day= gdp/population/365) %>%
      ggplot(aes(dollars_per_day, fill = region)) 
    + geom_density(bw = 0.5, position = "stack") + scale_x_continuous(trans="log2") + facet_grid (year~.)

#11# We are going to continue looking at patterns in the gapminder dataset by plotting infant mortality rates versus dollars per day for African countries.
#    Generate dollars_per_day using mutate and filter for the year 2010 for African countries.
#    Remember to remove NA values.
#    Store the mutated dataset in gapminder_Africa_2010.
#    Make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#    Use color to denote the different regions of Africa.
    
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    gapminder_Africa_2010 <- gapminder %>% filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365)
    # create the mutated dataset
    # now make the scatter plot
    gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day,infant_mortality, color = region)) +geom_point()

#12# Now we are going to transform the x axis of the plot from the previous exercise.
#    The mutated dataset is preloaded as gapminder_Africa_2010.
#    As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#    As in the previous exercise, use color to denote the different regions of Africa.
#    Transform the x axis to be in the log (base 2) scale.
 
    gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day,infant_mortality, color = region)) +geom_point() + scale_x_continuous(trans ="log2")

#13# Note that there is a large variation in infant mortality and dollars per day among African countries.
#    As an example, one country has infant mortality rates of less than 20 per 1000 and dollars per day of 16, while another country has infant mortality rates over 10% and dollars per day of about 1.
#    In this exercise, we will remake the plot from Exercise 12 with country names instead of points so we can identify which countries are which.    The mutated dataset is preloaded as gapminder_Africa_2010.
#    As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#    As in the previous exercise, use color to denote the different regions of Africa.
#    As in the previous exercise, transform the x axis to be in the log (base 2) scale.
#    Add a geom_text layer to display country names in addition to of points.
    
    gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day,infant_mortality, color = region)) + scale_x_continuous(trans = "log2") + geom_point() + geom_text(aes(label=country))

#14# Note that there is a large variation in infant mortality and dollars per day among African countries.
#    As an example, one country has infant mortality rates of less than 20 per 1000 and dollars per day of 16, while another country has infant mortality rates over 10% and dollars per day of about 1.
#    In this exercise, we will remake the plot from Exercise 12 with country names instead of points so we can identify which countries are which.
#    The mutated dataset is preloaded as gapminder_Africa_2010.
#    As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#    As in the previous exercise, use color to denote the different regions of Africa.
#    As in the previous exercise, transform the x axis to be in the log (base 2) scale.
#    Add a geom_text layer to display country names in addition to of points.
    
    gapminder_Africa_2010 %>% 
      ggplot(aes(dollars_per_day,infant_mortality, color = region, label = country)) + scale_x_continuous(trans = "log2") + geom_point() + geom_text()  
    
#15# Now we are going to look at changes in the infant mortality and dollars per day patterns African countries between 1970 and 2010.
#    Generate dollars_per_day using mutate and filter for the years 1970 and 2010 for African countries.
#    Remember to remove NA values.
#    As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
#    As in the previous exercise, use color to denote the different regions of Africa.
 #   As in the previous exercise, transform the x axis to be in the log (base 2) scale.
#    As in the previous exercise, add a layer to display country names instead of points.
#    Use facet_grid to show different plots for 1970 and 2010. Align the plots vertically.

    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(gapminder)
    gapminder %>% 
      filter(year %in% c(1970,2010) & continent == "Africa" & !is.na(gdp))%>% 
      mutate(dollars_per_day = gdp/population/365) %>% 
      filter( !is.na(dollars_per_day) & !is.na(infant_mortality)) %>% 
      ggplot(aes(dollars_per_day,infant_mortality, color = region,label = country)) +
      geom_point() + scale_x_continuous(trans= "log2") + 
      geom_text() + facet_grid(year~.)
###
    ##*******
    #Module 5:
    #Show Data
  #  A dynamite plot - a bar graph of group averages with error bars denoting standard errors - provides almost no information about a distribution.
  #  By showing the data, you provide viewers extra information about distributions.
  #  Jitter is adding a small random shift to each point in order to minimize the number of overlapping points. To add jitter, use the  geom_jitter() geometry instead of geom_point(). (See example below.)
  #  Alpha blending is making points somewhat transparent, helping visualize the density of overlapping points. Add an alpha argument to the geometry.
  #  Code
    # dot plot showing the data
    heights %>% ggplot(aes(sex, height)) + geom_point()
    
    # jittered, alpha blended point plot
    heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)
#Ease comparisons : Use common axes
  
# Ease comparisons by keeping axes the same when comparing data across multiple plots.
# Align plots vertically to see horizontal changes. Align plots horizontally to see vertical changes.
# Bar plots are useful for showing one number but not useful for showing distributions.
#

  # Consider Transformations
# Use transformations when warranted to ease visual interpretation.
# The log transformation is useful for data with multiplicative changes. The logistic transformation is useful for fold changes in odds. The square root transformation is useful for count data.
# We learned how to apply transformations earlier in the course.
  
#Ease Comparisons: Compared Visual Cues Should Be Adjacent
  
#    When two groups are to be compared, it is optimal to place them adjacent in the plot.
#    Use color to encode groups to be compared.
#    Consider using a color blind friendly palette like the one in this video.
#    Code
    color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
      ggplot(aes(x, y, color = col)) +
      geom_point(size = 5)
    p1 + scale_color_manual(values = color_blind_friendly_cols)  
#
  #**********************
##Assessment: Data Visualization Principles, Part 2 
    
  #1#To make the plot on the right in the exercise from the last set of assessments, we had to reorder the levels of the states' variables.
#    Redefine the state object so that the levels are re-ordered by rate.
#    Print the new object state and its levels (using levels) so you can see that the vector is now re-ordered by the levels.
    
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    dat <- us_contagious_diseases %>%
      filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
    state <- reorder(dat$state,rate, FUN=median) 
    rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
    state
    levels(state)

  #2#Now we are going to customize this plot a little more by creating a rate variable and reordering by that variable instead.
#    Add a single line of code to the definition of the dat table that uses mutate to reorder the states by the rate variable.
#    The sample code provided will then create a bar plot using the newly defined dat.  

    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data(us_contagious_diseases)
    dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
      mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% mutate(state = reorder(state,rate, FUN=median))
    dat %>% ggplot(aes(state, rate)) +
      geom_bar(stat="identity") +
      coord_flip()
    #
  #3#Say we are interested in comparing gun homicide rates across regions of the US. We see this plot:
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data("murders")
    murders %>% mutate(rate = total/population*100000) %>%
      group_by(region) %>%
      summarize(avg = mean(rate)) %>%
      mutate(region = factor(region)) %>%
      ggplot(aes(region, avg)) +
      geom_bar(stat="identity") +
      ylab("Murder Rate Average")
#    and decide to move to a state in the western region. What is the main problem with this interpretaion?  
  #Options:
  #  The categories are ordered alphabetically.
  #  The graph does not show standard errors.
  #ANS: It does not show all the data. We do not see the variability within a region and it's possible that the safest states are not in the West.
  #   The Northeast has the lowest average.
  #
    #5#To further investigate whether moving to the western region is a wise decision, let's make a box plot of murder rates by region, showing all points.
#    Order the regions by their median murder rate by using mutate and reorder.
#    Make a box plot of the murder rates by region.
#    Show all of the points on the box plot.
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    data("murders")
    murders %>% mutate(rate = total/population*100000) %>% mutate(region = reorder(region,rate,FUN=median))  %>% ggplot(aes(region,rate)) + geom_boxplot() +geom_point()
#
#
  #Slope Charts
#    Consider using a slope chart or Bland-Altman plot when comparing one variable at two different time points, especially for a small number of observations.
#    Slope charts use angle to encode change. Use geom_line() to create slope charts. It is useful when comparing a small number of observations.
 #   The Bland-Altman plot (Tukey mean difference plot, MA plot) graphs the difference between conditions on the y-axis and the mean between conditions on the x-axis. It is more appropriate for large numbers of observations than slope charts.
#    Code: Slope chart
    library(tidyverse)
    library(dslabs)
    data(gapminder)
    
    west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
    
    dat <- gapminder %>%
      filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
    
    dat %>%
      mutate(location = ifelse(year == 2010, 1, 2),
             location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                               location + 0.22, location),
             hjust = ifelse(year == 2010, 1, 0)) %>%
      mutate(year = as.factor(year)) %>%
      ggplot(aes(year, life_expectancy, group = country)) +
      geom_line(aes(color = country), show.legend = FALSE) +
      geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
      xlab("") +
      ylab("Life Expectancy") 
  #  Code: Bland-Altman plot
    library(ggrepel)
    dat %>%
      mutate(year = paste0("life_expectancy_", year)) %>%
      select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
      mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
             difference = life_expectancy_2015 - life_expectancy_2010) %>%
      ggplot(aes(average, difference, label = country)) +
      geom_point() +
      geom_text_repel() +
      geom_abline(lty = 2) +
      xlab("Average of 2010 and 2015") +
      ylab("Difference between 2015 and 2010")
  ##
  #Encoding a Third value
    
  #  Encode a categorical third variable on a scatterplot using color hue or shape. Use the shape argument to control shape.
  #  Encode a continuous third variable on a using color intensity or size.  
  #
##Case Study : Vaccines
#    Vaccines save millions of lives, but misinformation has led some to question the safety of vaccines. The data support vaccines as safe and effective. We visualize data about measles incidence in order to demonstrate the impact of vaccination programs on disease rate.
#    The RColorBrewer package offers several color palettes. Sequential color palettes are best suited for data that span from high to low. Diverging color palettes are best suited for data that are centered and diverge towards high or low values.
#    The geom_tile() geometry creates a grid of colored tiles.
#    Position and length are stronger cues than color for numeric values, but color can be appropriate sometimes.
#    Code: Tile plot of measles rate by year and state
    # import data and inspect
    library(tidyverse)
    library(dslabs)
    data(us_contagious_diseases)
    str(us_contagious_diseases)
    
    # assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
    the_disease <- "Measles"
    dat <- us_contagious_diseases %>%
      filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
      mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
      mutate(state = reorder(state, rate))
    
    # plot disease rates per year in California
    dat %>% filter(state == "California" & !is.na(rate)) %>%
      ggplot(aes(year, rate)) +
      geom_line() +
      ylab("Cases per 10,000") +
      geom_vline(xintercept=1963, col = "blue")
    
    # tile plot of disease rate by state and year
    dat %>% ggplot(aes(year, state, fill=rate)) +
      geom_tile(color = "grey50") +
      scale_x_continuous(expand = c(0,0)) +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
      geom_vline(xintercept = 1963, col = "blue") +
      theme_minimal() + theme(panel.grid = element_blank()) +
      ggtitle(the_disease) +
      ylab("") +
      xlab("")
    Code: Line plot of measles rate by year and state
    # compute US average measles rate by year
    avg <- us_contagious_diseases %>%
      filter(disease == the_disease) %>% group_by(year) %>%
      summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
    
    # make line plot of measles rate by year by state
    dat %>%
      filter(!is.na(rate)) %>%
      ggplot() +
      geom_line(aes(year, rate, group = state), color = "grey50", 
                show.legend = FALSE, alpha = 0.2, size = 1) +
      geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
      scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
      ggtitle("Cases per 10,000 by state") +
      xlab("") +
      ylab("") +
      geom_text(data = data.frame(x = 1955, y = 50),
                mapping = aes(x, y, label = "US average"), color = "black") +
      geom_vline(xintercept = 1963, col = "blue")
  ##  
    #In general, pseudo-3D plots and gratuitous 3D plots only add confusion. Use regular 2D plots instead.
##
    #Avoid too many significant digits:
# In tables, avoid using too many significant digits. Too many digits can distract from the meaning of your data.
# Reduce the number of significant digits globally by setting an option. For example, options(digits = 3) will cause all future computations that session to have 3 significant digits.
# Reduce the number of digits locally using round() or signif().  
##
    #Assessment: Data Visualization Principles, Part 3
#
    #1#The sample code given creates a tile plot showing the rate of measles cases per population. We are going to modify the tile plot to look at smallpox cases instead.
    #    Modify the tile plot to show the rate of smallpox cases instead of measles cases.
    #    Exclude years in which cases were reported in fewer than 10 weeks from the plot.
    
    library(dplyr)
    library(ggplot2)
    library(RColorBrewer)
    library(dslabs)
    data(us_contagious_diseases)
    
    the_disease = "Smallpox" #altered in code
    dat <- us_contagious_diseases %>% 
      filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !weeks_reporting < 10) %>%  #added weeks_reporting cndition
      mutate(rate = count / population * 10000) %>% 
      mutate(state = reorder(state, rate))
    
    dat %>% ggplot(aes(year, state, fill = rate)) + 
      geom_tile(color = "grey50") + 
      scale_x_continuous(expand=c(0,0)) + 
      scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
      theme_minimal() + 
      theme(panel.grid = element_blank()) + 
      ggtitle(the_disease) + 
      ylab("") + 
      xlab("")
  #
    #2#The sample code given creates a time series plot showing the rate of measles cases per population by state. We are going to again modify this plot to look at smallpox cases instead.
  #  Modify the sample code for the time series plot to plot data for smallpox instead of for measles.
  #  Once again, restrict the plot to years in which cases were reported in at least 10 weeks.
      
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    library(RColorBrewer)
    data(us_contagious_diseases)
    
    the_disease = "Smallpox" #altered here
    dat <- us_contagious_diseases %>%
      filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10 ) %>% ##altered here in weeks_reporting
      mutate(rate = count / population * 10000) %>%
      mutate(state = reorder(state, rate))
    
    avg <- us_contagious_diseases %>%
      filter(disease==the_disease) %>% group_by(year) %>%
      summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
    
    dat %>% ggplot() +
      geom_line(aes(year, rate, group = state),  color = "grey50", 
                show.legend = FALSE, alpha = 0.2, size = 1) +
      geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
      scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
      ggtitle("Cases per 10,000 by state") + 
      xlab("") + 
      ylab("") +
      geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
      geom_vline(xintercept=1963, col = "blue")
    #
    #3#Now we are going to look at the rates of all diseases in one state. Again, you will be modifying the sample code to produce the desired plot.
  #  For the state of California, make a time series plot showing rates for all diseases.
  #  Include only years with 10 or more weeks reporting.
  #  Use a different color for each disease.
  #  Include your aes function inside of ggplot rather than inside your geom layer.

    library(dplyr)
    library(ggplot2)
    library(dslabs)
    library(RColorBrewer)
    data(us_contagious_diseases)
    
    us_contagious_diseases %>% filter(state=="California" & weeks_reporting >= 10) %>% 
      group_by(year, disease) %>%
      summarize(rate = sum(count)/sum(population)*10000) %>%
      ggplot(aes(year, rate, color=disease)) + 
      geom_line()
  #
    #4#Now we are going to make a time series plot for the rates of all diseases in the United States. For this exercise, we have provided less sample code - you can take a look at the previous exercise to get you started.
  #  Compute the US rate by using summarize to sum over states. Call the variable rate.
  #  The US rate for each disease will be the total number of cases divided by the total population.
  #  Remember to convert to cases per 10,000.
  #  You will need to filter for !is.na(population) to get all the data.
  #  Plot each disease in a different color.
    
    library(dplyr)
    library(ggplot2)
    library(dslabs)
    library(RColorBrewer)
    data(us_contagious_diseases)
    
    us_contagious_diseases %>% filter(!is.na(population)) %>% 
      group_by(year, disease) %>%
      summarize(rate = sum(count)/sum(population)*10000) %>%
      ggplot(aes(year, rate, color=disease)) + 
      geom_line()
  #
    