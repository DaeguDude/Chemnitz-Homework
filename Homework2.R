# Import ggplot2 library
library(ggplot2)

# Import economics data
economics <- economics
# (A) Create a graph showing the relationship between unemployment and the saving rate shows
ggplot(data=economics, aes(x=psavert, y=unemploy)) + geom_point()
#--------------------------------------------------------------------------------------------------


# (B) Complete the graph (a) for the influence of the population
ggplot(data=economics, aes(x=pop, y=unemploy)) + geom_point()
#--------------------------------------------------------------------------------------------------


# (C) Fill in the graph (a) for the influence of the date by coloring the dots accordingly.
ggplot(data=economics, aes(x=date, y=unemploy, col=date)) + geom_point()
#--------------------------------------------------------------------------------------------------


# (D) Examine the influence of the date more closely. Does the month or the year have an influence? 
# To do this, create new factors that containt the month, year, or decade, and create new
# graphics for them as shown in (C)
# sort it by year
economics_years <- format(economics$date, "%Y")
ggplot(data=economics, aes(x=economics_years, y=unemploy)) + geom_point()


# sort it by months
economics_months <- format(economics$date, "%m")
ggplot(data=economics, aes(x=economics_months, y=unemploy)) + geom_point()


# Sort it by decade
# Change economics_years to numeric value
economics_years <- as.numeric(format(economics$date, "%Y"))

# Sort it by decade
economics_decade <- cut(economics_years, seq(from=1960, to=2020, by=10))

# Because it is in ugly factor, change the name of the factor
# I will use mapvalues() from the plyr package
library(plyr)
economics_decade <- mapvalues(economics_decade, 
                              from=c("(1.96e+03,1.97e+03]", "(1.97e+03,1.98e+03]", "(1.98e+03,1.99e+03]",
                                     "(1.99e+03,2e+03]", "(2e+03,2.01e+03]", "(2.01e+03,2.02e+03]"),
                              to=c("1960~1970", "1970~1980", "1980~1990", "1990~2000", "2000~2010", "2010~2020"))

# Finally make a plot of decades
ggplot(data=economics, aes(x=economics_decade, y=unemploy)) + geom_point()
#--------------------------------------------------------------------------------------------------


# (E) For the graph (d), which shows the relationship between unemployment 
# and savings rate for the individual decades, complete linear regression lines
# for the corresponding decades as well as the total regression line of all data points

# (E)-1 Regression line by decade
ggplot(data=economics, aes(x=psavert, y=unemploy, col=economics_decade)) + geom_point() +
  geom_smooth(method='lm', se=FALSE)

# (E)-2 Total Regression line
ggplot(data=economics, aes(x=psavert, y=unemploy)) + geom_point() +
  geom_smooth(method='lm', se=FALSE)
#--------------------------------------------------------------------------------------------------


# (F) For the graph, add (d), which shows the relationship between unemployment
# and savings rate for each month, linear regression lines for all months, and the overall
# regression line of all data points.

# (F)-1 Regression line by month
ggplot(data=economics, aes(x=psavert, y=unemploy, col=economics_months)) + geom_point() +
  geom_smooth(method='lm', se=FALSE)

# (F)-2 Total Regression line
ggplot(data=economics, aes(x=psavert, y=unemploy)) + geom_point() +
  geom_smooth(method='lm', se=FALSE)
