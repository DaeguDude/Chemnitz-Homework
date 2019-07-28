library("readr")

# (A) Import and display the relationship between all three variables
infomatik_data <- read.table('informatik.txt', header=TRUE)
# Because, it adds first column as number, I will erase first column and put it into
# new data frame
infomatik_data <- infomatik_data[, 2:ncol(infomatik_data)]

plot(infomatik_data)

# (B) Calculate the correlation matrix
cor_note_Dozent = cor(infomatik_data$Note, infomatik_data$Dozent)
cor_note_Aus = cor(infomatik_data$Note, infomatik_data$Ausstattung)
cor_dozent_Aus = cor(infomatik_data$Dozent, infomatik_data$Ausstattung)

# (C) Perform a linear regression to explain the influence of the equipment(Ausstattung)
# on the grade(Note).
# Graph the regression line and the data points.
plot(infomatik_data$Ausstattung, infomatik_data$Note)
aus_Note.regression <- lm(Note ~ Ausstattung, data=infomatik_data)
abline(aus_Note.regression, col="blue")

# (D) In addition, perform a linear regression to explain the influence of the instructors 
# on the grade. Also graph this regression line and the data points.
plot(infomatik_data$Dozent, infomatik_data$Note)
dozent_Note.regression <- lm(Note ~ Dozent, data=infomatik_data)
abline(dozent_Note.regression, col="blue")

# (E) Evaluate both linear models. Be aware of all statistical tests conducted by R. 
# Formulate the hypotheses and evaluate the tests.

# Hypoethesis testing for both of them
# NULL - There is no relationship between them
# ALT - There is a relationship between them

# Summary of Note~Ausstattung says that
# R squared value = 0.89, F-statistic = 300, p-value = 2.2e-16
# So we conclude that there is a positive linear relationship between them,
# And since P value is so small, we can say this R squared value is legitimate
summary(aus_Note.regression)

# Summary of Note~Dozent says that
# R squared value = 0.74, F-statistic = 110, p-value = 1.188e-12
# So we conclude that there is a positive linear relationship between them,
# And since P value is so small, we can say this R squared value is legitimate
summary(dozent_Note.regression)

# (F) For task ((c)) and task ((d)), specify the sums of squares of the model and the residuals.

# Sum of squared of means and Sum of squared residuals for (C)
SS_mean_ausNote = round(sum( (infomatik_data$Note - mean(infomatik_data$Note))^2 ), 2)
SS_fit_ausNote= round(sum( (aus_Note.regression$residuals)^2 ), 2)

# Sum of squared of means and Sum of squared residuals for (D)
SS_mean_dozentNote = round(sum( (infomatik_data$Note - mean(infomatik_data$Note))^2 ), 2)
SS_fit_dozentNote= round(sum( (dozent_Note.regression$residuals)^2 ), 2)

# (g) What do the determinacy measures of both models say?

# Answer to (g): I don't understand what 'determinacy' means here, but if 'determinacy'
# means that examins the conditions under certain theory, it will be F and p value.
# So both model says, there are strong positive relationship between them.

# (h) Is it possible to add the coefficients of determination of both models to the
# degree of certainty of the linear model, which describes the grade depending on equipment
# and lecturers? Justify your answer.
multiple.regression <- lm(Note ~ Dozent + Ausstattung, data=infomatik_data)
# I will use variance inflation factor which assesses the multicollinearity
libary(car)
vif_Dozent = car::vif(multiple.regression)[1]
vif_Ausstattung = car::vif(multiple.regression)[2]
summary(multiple.regression)
# Conclusion:
# Although two predictors are correlated at the value of 0.72,
# our summary of multiple regression shows that both of variables
# are significant. 
# And VIF of two variables also don't exceed the threshold of 5,
# So I will conclude that it is safe to use both variables in this
# regression model.
# So, it is possible.

# (i) Model a more comprehensive model to explain the score and evaluate it.
# Also consider the squares sums for this model. What influence does the order of the predictors have?

# I don't think I understand this question [i]. Because I think I already have done multiple regression.


