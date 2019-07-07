# Download the package ggplot2. Find out what’s going on by the code
# ggplot(mpg, aes(class)) + geom_bar(aes(weight=displ))
# is shown exacctly. Use R to calculate the displayed values independently
install.packages("ggplot2")
library(ggplot2)

# Number of cars in each class
g <- ggplot(mpg, aes(class))
# Total engine displacement of each class
ggplot(mpg, aes(class)) + geom_bar(aes(weight=displ))

# '2 seater' displayed value
displ_2seater <- mpg[mpg$class=='2seater', 'displ']
total_displ_2seater <- sum(displ_2seater$displ)

# 'compact' displayed value
displ_compact <- mpg[mpg$class=='compact', 'displ']
total_displ_compact <- sum(displ_compact$displ)

# 'midsize' displayed value
displ_midsize <- mpg[mpg$class=='midsize', 'displ']
total_displ_midsize <- sum(displ_midsize$displ)

# 'minivan' displayed value
displ_minivan <- mpg[mpg$class=='minivan', 'displ']
total_displ_minivan <- sum(displ_minivan$displ)

# 'pickup' displayed value
displ_pickup <- mpg[mpg$class=='pickup', 'displ']
total_displ_pickup <- sum(displ_pickup$displ)

# 'subcompact' displayed value
displ_subcompact <- mpg[mpg$class=='subcompact', 'displ']
total_displ_subcompact <- sum(displ_subcompact$displ)

# 'suv' displayed value
displ_suv <- mpg[mpg$class=='suv', 'displ']
total_displ_suv <- sum(displ_suv$displ)


# Choose your own record3 and make a suitable one for each
# bar graph, box plot, Histogram with density estimator

# make a bar graph of the count of 'manufacturer'
ggplot(mpg, aes(manufacturer)) + geom_bar()

# make a box plot of pickup truck 'cty'
ggplot(mpg, aes(x=class, y=cty), main='cty of each class') +
  geom_boxplot()

# make a histogram of the two variables of 'cty' with denstiy estimator
ggplot(mpg, aes(x=cty)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") 

# For the record you selected in (b), show the relationship between 3 variables.
