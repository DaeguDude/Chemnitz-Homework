# Installing
# install.packages("readr")

# Loading
library("readr")

#----------------------------------------------------------------------
----------------------------------------------------------------------
# (A) Import this to R
diet_data <- read_tsv('DietWeigthLoss.txt')

# I need some modification to work better with 
diet_data$Diet = as.factor(diet_data$Diet)


#----------------------------------------------------------------------
----------------------------------------------------------------------
# (B) Determine
# (B-1) How many participants participated in each diet
num_A = nrow(diet_data[diet_data$Diet=='A', ])
num_B = nrow(diet_data[diet_data$Diet=='B', ])
num_C = nrow(diet_data[diet_data$Diet=='C', ])
num_D = nrow(diet_data[diet_data$Diet=='D', ])

# (B-2) the group effect as well(Overall Mean)
overall_Y = round(mean(diet_data$WeightLoss), 2)
# (B-3) the group mean values

diet_A = as.numeric(unlist(diet_data[diet_data$Diet=='A',
                                     'WeightLoss']))
diet_B = as.numeric(unlist(diet_data[diet_data$Diet=='B',
                                     'WeightLoss']))
diet_C = as.numeric(unlist(diet_data[diet_data$Diet=='C',
                            'WeightLoss']))
diet_D = as.numeric(unlist(diet_data[diet_data$Diet=='D',
                            'WeightLoss']))


mean_A = round(mean(as.numeric(unlist(diet_data[diet_data$Diet=='A',
                                                'WeightLoss']))), 2)

mean_B = round(mean(as.numeric(unlist(diet_data[diet_data$Diet=='B',
                                                'WeightLoss']))), 2)

mean_C = round(mean(as.numeric(unlist(diet_data[diet_data$Diet=='C',
                                                'WeightLoss']))), 2)

mean_D = round(mean(as.numeric(unlist(diet_data[diet_data$Diet=='D',
                                                'WeightLoss']))), 2)
# sd_A = round(sd(as.numeric(unlist(diet_data[diet_data$Diet=='A',
# 'WeightLoss']))), 2)


#----------------------------------------------------------------------
----------------------------------------------------------------------
# (C) Create a graph representing all group averages and the total mean
# Let's first conduct one way ANOVA for this data
anova1 = aov(diet_data$WeightLoss ~ diet_data$Diet)
library("gplots")
plotmeans(WeightLoss ~ Diet, data = diet_data, frame = FALSE,
          xlab = "Diet", ylab = "WeightLoss",
          main="Each mean plot with confidence intervals") 
abline(h=overall_Y, col='red')


#----------------------------------------------------------------------
----------------------------------------------------------------------
# (D) Does the diet have an effect on the assumed weight? To do this, 
# create a model in R that describes the dependencies. In addition to task (c), 
# create another graph representing this dependency.
diet.regression <- lm(WeightLoss ~ Diet, data=diet_data)
plot(diet.regression)



#----------------------------------------------------------------------
----------------------------------------------------------------------
# (E) Check the above question with a suitable statistical test.
# For this, formulate the hypotheses and prerequisites of the test. 
# Check this. Evaluate the test results. 

# I have to show the process of calculating F statistic 

# variability SSbetween(EXPLAIN)
# you have to multiply sample size by the end
sample_size_each_group = 15
SSbetween = round(( (mean_A-overall_Y)^2 + (mean_B-overall_Y)^2 +
                            (mean_C-overall_Y)^2 + (mean_D-overall_Y)^2 ) * sample_size_each_group, 2)
# k-1
df_between = 4-1
variance_SSbetween = round(SSbetween / df_between, 2)

# variability SSbetween(UNEXPLAINED)
SSwithin = round(sum((diet_A - mean_A)^2) + sum((diet_B - mean_B)^2) +
                         sum((diet_C - mean_C)^2) + sum((diet_D - mean_D)^2), 2)
# n-k
df_within = 60-4
variance_SSwithin = round((SSwithin/df_within), 2)

# Now we are comapring the variability between groups to
# overall means, and variability between individuals to 
# each group means
# if something that can be explained by diets(Signal) is bigger than
# something that can not be explained by diets(Noise), it means
# there is actually some differences.
F_STAT = round(variance_SSbetween / variance_SSwithin, 2)


#----------------------------------------------------------------------
----------------------------------------------------------------------
# (F) Create a plot that offsets the predicted values of the model against the residuals

# I assume this means I have to plot residuals?
plot(diet.regression$residuals)

