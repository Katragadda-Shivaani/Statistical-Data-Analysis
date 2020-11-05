#STUDENT NAME : SHIVAANI KATRAGADDA
#STUDENT NUMBER : R00183214
#SUBJECT : STATISTICAL DATA ANALYSIS
#subject code : STAT9004
#****************************************************************************
install.packages("readr")
install.packages("DataExplorer")
install.packages("dplyr")
install.packages("inspectdf")
install.packages("caret")
install.packages("GGally")
install.packages("skimr")
install.packages("funModeling")
install.packages("scatterplot3d")
install.packages("car")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("VIM")
install.packages("outliers")
install.packages("psych")
install.packages("leaps")

library(readr)
library(DataExplorer)
library(dplyr)
library(inspectdf)
library(caret)
library(GGally)
library(skimr)
library(funModeling)
library(scatterplot3d)
library(car)
library(corrplot)
library(RColorBrewer)
library(VIM)
library(outliers)
library(psych)
library(leaps)
#**************************************************************************************

# QUESTION 1
#A researcher would like to explore the relationship between the diameter of a tree trunk and the volume of the trunk for a particular species of tree. The tree.xlsx dataset is available on Canvas. The dataset consists of n observations of 2 variables, Diam and Vol which measure the diameter (m) and the volume (m3) of n tree trunks from the same species.
#The aim of the question is to find out whether the variable Diam, is of use in predicting the value of Vol. Please write up the analysis in the form of a report.
#As per the given question the main aim of the question is to find out whether Diam(Diameter) is useful to predict the value of vol(volume).
#Therefore I loaded the first dataset treeB.csv by using readr package which will be helpful for reading .csv files.And assigned file to TreeeB variable.


#loading the treeb.csv file and storing it in variable TreeB
TreeB<-read.csv("F:/semester2/SDA/treeB.csv")
View(TreeB)
#checking the class of the TreeB
class(TreeB)

# Question 1(a)
#Make a numerical and graphical summary of the data, commenting on the results.
#Include: boxplots, histograms, scatterplot and the correlation coefficient.

# Solution
#(i) Understanding your variables (Numerical data Analysis)
#Looking in to the data by using dim(which gives dimensions of data),names(column names), head,tail,structure and the summary of the data tReeB
dim(TreeB)
names(TreeB)
head(TreeB)
tail(TreeB)
str(TreeB)
summary(TreeB)
glimpse(TreeB)
#For each variable it returns, Quantity and percentage of zeros,NA values (q_NA/p_na), and infinite values (q_inf/p_inf)
df_status(TreeB)
#prints a concise statistical summary
describe(TreeB)
#provide summary statistics about variables
skim(TreeB)
#descriptive stastics for the data mean,median,standard deviation,variance,IQR and normality by using shaprio test
mean(TreeB$Diam)
mean(TreeB$Vol)
median(TreeB$Diam)
median(TreeB$Vol)
sd(TreeB$Diam)
sd(TreeB$Vol)
var(TreeB$Diam)
var(TreeB$Vol)
IQR(TreeB$Diam)
IQR(TreeB$Vol)
#NORMALITY
shapiro.test(TreeB$Diam)
shapiro.test(TreeB$Vol)
#From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality
ggpairs(TreeB)#Make a matrix of plots with  data set
#The  ggcorr() function draws a correlation matrix plot using ggplot2.
ggcorr(TreeB, palette = "RdBu", label = TRUE)


##Introduce() function will give the outline of the data it tells about the number of rows,columns,missing values,discrete columns,continous columns,all missing columns,complete rows,total observations and memory usage 
introduce(TreeB)
#It prints  the dataset 
plot_str(TreeB)
#plot_intro() will plot a graph which will give the percentage of  discrete columns,continous columns,All missing columns,complete rows and missing observations
plot_intro(TreeB)
plot_num(TreeB)
profiling_num(TreeB)
#prints histogram for numerical data
plot_histogram(TreeB)
#prints density plot 
plot_density(TreeB)
#Q-Q plot
qq_data <-TreeB
plot_qq(qq_data)#plotting qq plot for qq_data
plot_qq(qq_data, by = "Diam")
plot_qq(qq_data, by = "Vol")

#inspect the types of data
a<-inspect_types(TreeB)
show_plot(a)
#prints the data in the form of graphs and prints column size
b<-inspect_mem(TreeB)
show_plot(b)
#inspects the numerical data
b2<-inspect_num(TreeB)
show_plot(b2)
#pearson's correlation 
b5<-inspect_cor(TreeB)
show_plot(b5)

## Pairs plot
##function to put histograms on the diagonal
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   r <- abs(cor(x, y,use = "everything"))
   txt <- format(c(r, 0.123456789), digits = digits)[1]
   txt <- paste0(prefix, txt)
   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
   text(0.5, 0.5, txt, cex = cex.cor * r)
}
# this function is taken from the help documentation, it will create 
#histograms along the diagonal
panel.hist <- function(x, ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5) )
   h <- hist(x, plot = FALSE)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$counts; y <- y/max(y)
   rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
# plot scatter plots for variables 1:7
pairs(TreeB, lower.panel=panel.cor, diag.panel = panel.hist)

#(ii)Cleaning your dataset
#Looking in to the missing data  

#shows the amount of missing data for each variable and the frequency of combinations of missing values
aggr(TreeB, prop = FALSE, number=TRUE) 
#creates a barchart showing the values of the variable missing values
barMiss(TreeB)
# Creates a scatterplot between two variables with information about missing values in the margins
marginplot(TreeB)
#Next we create a margin plot 
marginmatrix(TreeB)
#plot_missing() function shows the percentage of missing values of each column present in the dataset
plot_missing(TreeB)
profile_missing(TreeB)

#THERE IS NO MISSING DATA so there is no need to clean the data

#(iii)Analyzing relationships between variables(graphical data Analysis)

#GRAPHICAL SUMMARY
#Include: boxplots, histograms, scatterplot and the correlation coefficient.
#histograms
hist(TreeB$Diam,col="red")
hist(TreeB$Vol,col="blue")
#boxplots
boxplot(TreeB$Diam,col="yellow")
boxplot(TreeB$Vol,col="pink")
boxplot(TreeB$Diam,TreeB$Vol,col=c("yellow","pink"))
#scatterplots
plot(TreeB$Diam,col="orange")
plot(TreeB$Vol,col="red")
plot(TreeB$Diam,TreeB$Vol)
scatterplot(Diam~Vol, data=TreeB,
            xlab="Diam", ylab="Vol",
            main="Enhanced Scatter Plot")
pairs(~Diam+Vol,data=TreeB,
      main="Simple Scatterplot Matrix",col="red")
scatterplot3d(TreeB$Diam,TreeB$Vol, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")
#corelation coefficient
M <-cor(TreeB)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(M, method="number")
corrplot(M, method="color")

pairs.panels(TreeB, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses
plot_correlation(TreeB)

#From the density graph I observed that the variables are highly skewed. To correct the skewness we can use log10 transformations
#log Transformations - The log transformation can be used to make highly skewed distributions less skewed. This can be valuable both for making patterns in the data more interpretable and for helping to meet the assumptions of inferential statistics.
#And from boxplots we can see that there are some outliers. Those outliers will be reduced after applying log transformation because we are reducing highly skewed data to less skewed data

par(mfrow=c(1,2))
plot(density((TreeB$Diam)),main="Diam")
plot(density(TreeB$Vol),main ="vol")
# we can apply a log transformation to correct the skew
TreeB$Diam<-log10(TreeB$Diam)
TreeB$Vol<-log10(TreeB$Vol)
# check the distributions after transformation
par(mfrow=c(1,2))
plot(density(TreeB$Diam),main="Diam")
plot(density(TreeB$Vol),main ="Vol")

#Now the the highly skewed distribution is less skewed.

#After applying log transformation also still there are some outliers present in the Vol, we can see those from the boxplot.

boxplot(TreeB$Diam,TreeB$Vol,col=c("yellow","pink"))

#so I will be performing the grubbs.test to see whether the outliers are significant or not


grubbs.test(TreeB$Vol)
#its is a outlier which is not significant because the p value is greater than 0.05.

#I performed the grubbs.test for the vol variable the outliers are significant because the p value is greater than 0.05

#The numerical and graphical summary of data after applying log transformation

#Numerical summary
df_status(TreeB)
describe(TreeB)
skim(TreeB)
mean(TreeB$Diam)
mean(TreeB$Vol)
median(TreeB$Diam)
median(TreeB$Vol)
sd(TreeB$Diam)
sd(TreeB$Vol)
var(TreeB$Diam)
var(TreeB$Vol)
IQR(TreeB$Diam)
IQR(TreeB$Vol)
#NORMALITY
shapiro.test(TreeB$Diam)
shapiro.test(TreeB$Vol)
#GRAPHICAL SUMMARY
#Include: boxplots, histograms, scatterplot and the correlation coefficient.
#histograms
hist(TreeB$Diam,col="red")
hist(TreeB$Vol,col="blue")
#boxplots
boxplot(TreeB$Diam,col="yellow")
boxplot(TreeB$Vol,col="pink")
boxplot(TreeB$Diam,TreeB$Vol,col=c("yellow","pink"))
#scatterplots
plot(TreeB$Diam,col="orange")
plot(TreeB$Vol,col="red")
plot(TreeB$Diam,TreeB$Vol)
scatterplot(Diam~Vol, data=TreeB,
            xlab="Diam", ylab="Vol",
            main="Enhanced Scatter Plot")
pairs(~Diam+Vol,data=TreeB,
      main="Simple Scatterplot Matrix",col="red")
scatterplot3d(TreeB$Diam,TreeB$Vol, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")
#corelation coefficient
M <-cor(TreeB)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(M, method="number")
corrplot(M, method="color")
pairs.panels(TreeB, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses
plot_correlation(TreeB)


# 1(b)
#Fit a model of the form y=$\beta_0$+$\beta_1$x+e and interpret the value of $\beta_1$. Note that you will need to consider the results from your exploratory data analysis in part (a) to fit a valid model.	Fit a model of the form y=$\beta_0$+$\beta_1$x+e and interpret the value of $\beta_1$. Note that you will need to consider the results from your exploratory data analysis in part (a) to fit a valid model.

# Solution


#linear regression model
## Examine the relationship between vol and Diam 
plot(Vol~Diam, data = TreeB)
cor(TreeB$Vol,TreeB$Diam)

#fitting the  model
Tree_model<-lm(Vol~Diam, data = TreeB)
Tree_model

#The histogram for the Tree_model residual

hist(resid(Tree_model),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Tree_model",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
#scatter plot for the model

plot(Vol~Diam ,data = TreeB, col = "grey", pch = 20,
     main = "Data from Model")
abline(Tree_model, col = "darkorange", lwd = 3)

#To create a summary of the fitted model
## summary output
summary(Tree_model)

## extract coefficients and residuals
Tree_model$coefficients
Tree_model$residuals

#To view the ANOVA table we use the summary.aov() function.

#summary ANOVA table
summary.aov(Tree_model)

#I want to examine the model with the data before log transformation also

#Fitting the model by using the data before applying log transformations.
Tree<-read.csv("F:/semester2/SDA/treeB.csv")
Treemodel<-lm(Vol~Diam, data = Tree)
Treemodel
summary(Treemodel)
AIC(Tree_model)
AIC(Treemodel)

# 1(c)
# Solution

#the 95% confidence interval for Diam $\beta_1$ coefficient
confint(Tree_model, 'Diam', level=0.95)

# 1(d)
#Test the hypothesis:
#H0: $\beta_1$=0
#HA: $\beta_1$???0
#What do the results of the hypothesis test imply for the regression model?

# Solution

#Examining the output for summary(Tree_model) we see that the t-statistic associated with $\beta_1$ (Diam) is 6.655,the degree of freedom 58 and the associated p-value is 1.11e-08.The p- value is less than the 5% level of significance. 
#In this instance we reject the null hypothesis at the 5% confidence level and conclude that Diam is strongly associated with Vol.There is enough evidence, based on the given data and a 5% level of significance,we can conclude that there is strong relationship among Diam and Vol. 

# 1(e)
#Plot the regression line onto a scatterplot of the data and plot a 95% prediction band

# Solution

#Plotting the regression line on the original data
plot(TreeB$Vol~TreeB$Diam)
abline(Tree_model,col="red")


#The 95% prediction band:

# 95% confidence interval for fitted value (mean) and predicted value (individual)
predict(Tree_model, interval = 'confidence', se.fit = T)
predict(Tree_model,interval = 'prediction', se.fit = T)
## create a set of 95% confidence interval
dist_ci_band = predict(Tree_model, 
                       newdata = data.frame(Diam=TreeB$Diam), 
                       interval = "confidence", level = 0.95)
## create a set of 95% prediction interval
dist_pi_band = predict(Tree_model, 
                       newdata = data.frame(Diam=TreeB$Diam), 
                       interval = "prediction", level = 0.95) 
## plot scatter graph of Diam against Vol
plot(TreeB$Vol~TreeB$Diam, xlab = "Vol",  ylab = "Diam",  pch  = 20,
     cex  = 0.75, ylim = c(min(dist_pi_band), max(dist_pi_band)))
## add regression line    
abline(Tree_model)
lines(TreeB$Diam, dist_ci_band[,"lwr"], col = "blue", lwd = 1, lty = 2)
lines(TreeB$Diam, dist_ci_band[,"upr"], col = "blue", lwd = 1, lty = 2)
lines(TreeB$Diam, dist_pi_band[,"lwr"], col = "red", lwd = 1, lty = 3)
lines(TreeB$Diam, dist_pi_band[,"upr"], col = "red", lwd = 1, lty = 3)

# 1(f)
#Plot the studentized residuals against the fitted values and identify any outliers.

# Solution

## diagnostics
par(mfrow = c(2,2)) 
#plots for the model
plot(Tree_model)
#Q-Q plot for model
qqPlot(Tree_model,main="QQ Plot")
#studentized residuals
stdres <- rstandard(Tree_model)
#Plot of the studentized residuals against the fitted values
plot(stdres~fitted(Tree_model), xlab = "Vol(fitted values)", ylab =" standardised residuals")
abline(0,0)

#From the figure we can say that there are no outliers.An outlier is a point that falls far from the other data points.Therefore from the plot we can say that there are no outliers.

# 1(g)
#Plot the leverage of each case and identify any observations that have high leverage

# Solution

## calculate leverage
h<- lm.influence(Tree_model)$hat
windows(5,5)
plot(h, xlab = "Observation", ylab = "Leverage")
abline(h=0.1)
identify(h,n=1)
#From the plot we can say that the observation 17 have high leverage

# 1(h)
#Identify the observation that has the largest influence on the estimate of the $\beta_1$ coefficient. Explain why this observation has a large influence.

# Solution


#influential points
dfbetaPlots(Tree_model,id.n = 1)
dfbetaPlots(Tree_model,id.n = 2)
dfbetaPlots(Tree_model,id.n = 3)
dfbetaPlots(Tree_model,id.n = 4)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(TreeB)-length(Tree_model$coefficients)-2))
plot(Tree_model, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(Tree_model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#From the dfbetaplots we can see the top four observations that has the largest influence on the estimate of the $\beta_1$ coefficient.
#The cooks plot provides the three observations that has the largest influence.
#The observations 4,17 and 2 have the largest influence 

#Therefore I can say that  Diam(Diameter) is useful to predict the value of vol(volume).

#***************************************************************************************
# QUESTION 2
#The div dataset is available on Blackboard. The dataset consists of 77 observations and 7 variables.

#loading the divusaB.csv file and storing it in variable divusaB
divusaB<-read.csv("F:/semester2/SDA/divusaB.csv")
View(divusaB)
#checking the class of divusaB
class(divusaB)


# 2(a)
#Make a numerical and graphical summary of the data, commenting on the results. Include: boxplots, histograms, scatterplots and correlation coefficients.

# Solution

#(i) Understanding your variables (Numerical data Analysis)

#looking into the data by using dim,names,head,tail,str,summary,df_status,describe,skim
dim(divusaB)
names(divusaB)
head(divusaB)
tail(divusaB)
str(divusaB)
summary(divusaB)
glimpse(divusaB)
df_status(divusaB)#funmodeling
describe(divusaB)
skim(divusaB)
#Descriptive stastics mean,median,standard deviation,variance,IQR,shaprio test for normality
mean(divusaB$year)
mean(divusaB$divorce)
mean(divusaB$unemployed)
mean(divusaB$femlab)
mean(divusaB$marriage)
mean(divusaB$birth)
mean(divusaB$military)

median(divusaB$year)
median(divusaB$divorce)
median(divusaB$unemployed)
median(divusaB$femlab)
median(divusaB$marriage)
median(divusaB$birth)
median(divusaB$military)

sd(divusaB$year)
sd(divusaB$divorce)
sd(divusaB$unemployed)
sd(divusaB$femlab)
sd(divusaB$marriage)
sd(divusaB$birth)
sd(divusaB$military)


var(divusaB$year)
var(divusaB$divorce)
var(divusaB$unemployed)
var(divusaB$femlab)
var(divusaB$marriage)
var(divusaB$birth)
var(divusaB$military)

IQR(divusaB$year)
IQR(divusaB$divorce)
IQR(divusaB$unemployed)
IQR(divusaB$femlab)
IQR(divusaB$marriage)
IQR(divusaB$birth)
IQR(divusaB$military)
#NORMALITY
shapiro.test(divusaB$year)
shapiro.test(divusaB$divorce)
shapiro.test(divusaB$unemployed)
shapiro.test(divusaB$femlab)
shapiro.test(divusaB$marriage)
shapiro.test(divusaB$birth)
shapiro.test(divusaB$military)

ggpairs(divusaB)#Make a matrix of plots with  data set

#The  ggcorr() function draws a correlation matrix plot using ggplot2.
ggcorr(divusaB, palette = "RdBu", label = TRUE)


#Gives the information about dataset in the form of plots
introduce(divusaB)
plot_str(divusaB)
plot_intro(divusaB)
plot_missing(divusaB)
profile_missing(divusaB)
plot_num(divusaB)
profiling_num(divusaB)

#histogram,density and Q-Q plots
plot_histogram(divusaB)
plot_density(divusaB)
qq_data <-divusaB
plot_qq(qq_data)#plotting qq plot for qq_data
plot_qq(qq_data, by = "year")
plot_qq(qq_data, by = "divorce")
plot_qq(qq_data, by = "unemployed")
plot_qq(qq_data, by = "femlab")
plot_qq(qq_data, by = "marriage")
plot_qq(qq_data, by = "military")
plot_qq(qq_data, by = "birth")

# Gives information about the type,size, numerical data and pearson's correlation of dataset
a<-inspect_types(divusaB)
show_plot(a)
b<-inspect_mem(divusaB)
show_plot(b)
b2<-inspect_num(divusaB)
show_plot(b2)
b5<-inspect_cor(divusaB)
show_plot(b5)
## Pairs plot
##function to put histograms on the diagonal
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   r <- abs(cor(x, y,use = "everything"))
   txt <- format(c(r, 0.123456789), digits = digits)[1]
   txt <- paste0(prefix, txt)
   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
   text(0.5, 0.5, txt, cex = cex.cor * r)
}
# this function is taken from the help documentation, it will create 
#histograms along the diagonal
panel.hist <- function(x, ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5) )
   h <- hist(x, plot = FALSE)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$counts; y <- y/max(y)
   rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
# plot scatter plots for variables 1:7
#windows(10,10)
pairs(divusaB, lower.panel=panel.cor, diag.panel = panel.hist)

#(ii)Cleaning your dataset

#shows the amount of missing data for each variable and the frequency of combinations of missing values
aggr(divusaB, prop = FALSE, number=TRUE) 
#creates a barchart showing the values that are missing
barMiss(divusaB)
#Next we create a margin plot
marginmatrix(divusaB)
plot_missing(divusaB)
profile_missing(divusaB)

#THERE IS NO MISSING DATA so there is no need to clean the data

#(iii)Analyzing relationships between variables(graphical data Analysis)

#PLOTTING THE PLOtS
#GRAPHICAL SUMMARY
#Include: boxplots, histograms, scatterplot and the correlation coefficient.
#histograms
hist(divusaB$year,col="red")
hist(divusaB$divorce,col="blue")
hist(divusaB$unemployed,col="green")
hist(divusaB$femlab,col="orange")
hist(divusaB$marriage,col="purple")
hist(divusaB$birth,col="pink")
hist(divusaB$military,col="tomato")
#boxplots
boxplot(divusaB$year,col="red")
boxplot(divusaB$divorce,col="blue")
boxplot(divusaB$unemployed,col="green")
boxplot(divusaB$femlab,col="orange")
boxplot(divusaB$marriage,col="purple")
boxplot(divusaB$birth,col="pink")
boxplot(divusaB$military,col="tomato")
boxplot(divusaB$year,divusaB$divorce,divusaB$unemployed,divusaB$femlab,divusaB$marriage,divusaB$birth,divusaB$military,col=terrain.colors(7))
#scatterplots
plot(divusaB$year,col="red")
plot(divusaB$divorce,col="blue")
plot(divusaB$unemployed,col="green")
plot(divusaB$femlab,col="orange")
plot(divusaB$marriage,col="purple")
plot(divusaB$birth,col="pink")
plot(divusaB$military,col="tomato")
plot(divusaB$year,divusaB$divorce)
plot(divusaB$unemployed,divusaB$femlab)
plot(divusaB$marriage,divusaB$birth)
plot(divusaB$year,divusaB$military)
pairs(divusaB,main="Simple Scatterplot Matrix",col="red")
#corelation coefficient
M <-cor(divusaB)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(M, method="number")
corrplot(M, method="color")
plot_correlation(divusaB)
pairs.panels(divusaB, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses

#From the density graph I observed that the variables are less skewed.so there is no need to apply log transformation.
#But there are some outliers in the data so I will be applying grubbs.test to see whether the outliers are significant or not

#its is a outlier which is not significant because the p value is greater than 0.05.
grubbs.test(divusaB$year)
#its is a outlier which is not significant because the p value is greater than 0.05.
grubbs.test(divusaB$divorce)
#its is a outlier which is significant because the p value is less than 0.05.
grubbs.test(divusaB$unemployed)
#its is a outlier which is not significant because the p value is greater than 0.05.
grubbs.test(divusaB$femlab)
#its is a outlier which is not significant because the p value is greater than 0.05.
grubbs.test(divusaB$marriage)
#its is a outlier which is not significant because the p value is greater than 0.05.
grubbs.test(divusaB$birth)
#its is a outlier which issignificant because the p value is less than 0.05.
grubbs.test(divusaB$military)


# 2(b)
#Fit the model :
#y=$\beta_0$+$\beta_1$ unemployed+$\beta_2$ femlab+$\beta_3$ marriage+$\beta_4$ birth+$\beta_3$ military+e

# Solution

#fit the model
m1 <- lm(divorce ~ unemployed + femlab + marriage + birth + military, data = divusaB)
m1

#The histogram for the Tree_model residual

hist(resid(m1),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, Tree_model",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)

#To create a summary of the fitted model

## summary output
summary(m1)

## extract coefficients and residuals
m1$coefficients
m1$residuals

#To view the ANOVA table we use the summary.aov() function.

#summary ANOVA table
summary.aov(m1)

# 2b(i)
#Interpret the coefficient for femlab.

# Solution
#The coefficient value of femlab is 0.43393x2
#Therefore for every unit change in x2 the value of y will be increasing by 0.43393 amount
#provided all the other variables are constant
#An increase in percent female participation in labour force aged 16+ will be increased by 0.43393 adjusting for all the other variables 

# 2b(ii)
#Calculate the variance inflation factors for this model and discuss their implications for collinearity in the model. 

# Solution

#variance inflation factors of model m1
vif(m1)

#The VIFs lie between 1 and 2 indicating that collinearity is not having a large impact on the coefficient estimates for this model.

# 2b(iii)
#By fitting alternative models, determine whether collinearity has an impact on the coefficient estimates.

# Solution

#Building different models  
m1 <- lm(divorce ~ unemployed + femlab , data = divusaB)
summary(m1)
vif(m1)
m11<- lm(divorce ~ unemployed + femlab + marriage , data = divusaB)
summary(m11)
vif(m11)
m111 <- lm(divorce ~ unemployed + femlab + marriage + birth, data = divusaB)
summary(m111)
vif(m111)

#For all the three models 
#The VIFs lie between 1 and 2 indicating that collinearity is not having a large impact on the coefficient estimates for this model.

# 2b(iv)
#Create a partial regression plot to examine relationship between birth and divorce adjusted for unemployed, femlab, marriage and military.

# Solution

# Create a partial regression plot showing the relationship between 

m_divorce<-lm(divorce ~ unemployed+femlab+marriage+military, data = divusaB)
# first create the model 
m_birth <-lm(birth ~ unemployed+femlab+marriage+military, data = divusaB)
# plot residuals
plot(m_divorce$res ~ m_birth$res, xlab = "birth ~ unemployed+femlab+marriage+military", 
     ylab = "divorce ~ unemployed+femlab+marriage+military")

# fit a regression model to the residuals
m_res <-lm(m_divorce$res ~ m_birth$res)
abline(m_res)
summary(m_res)

# 2b(v)
#Test the hypothesis:  
# H0: $\beta_1$=$\beta_2$=$\beta_3$=$\beta_4$=$\beta_5$=0
#HA: at least one of the $\beta_i$???0
#What do the results of the hypothesis test imply for the regression model?

# Solution
#Examining the output for summary(m1) we see that the global F-statistic is F(5, 71) = 64.37 and p =< 2.2e-16, 
#this F-statistic compares the fitted model to the null model (also called the intercept only model). 
#In this instance may reject the null hypothesis at the 1% confidence level and conclude that at least one of the predictors is associated with divorce.

# 2b(vi)
#Assess the fit of the model using diagnostic plots, commenting on the assumptions of the regression model and influential points.

# Solution

par(mfrow=c(2,2))
plot(m1)
h<- lm.influence(m1)$hat
plot(h, xlab = "Observation", ylab = "Leverage")
abline(h=0.2)
identify(h,n=4)
dfbetaPlots(m1,id.n = 1)
dfbetaPlots(m1,id.n = 2)
dfbetaPlots(m1,id.n = 3)
dfbetaPlots(m1,id.n = 4)

#I have plotted the model m1 which gives the diagnostic plots are Residuals vs Fitted, Normal Q-Q, Scale_Location and Residuals Vs Leverage.
#For Leverage the observation 23,24,25,26 have high leverage
#And also we can see the top four observations influential points of each variable by using dfbetaPlots.


# 2(c)
#Use an F-test to compare the full model to the model including all variables except unemployment.

# Solution

#performing F-test
#m1 <- lm(divorce ~ unemployed + femlab + marriage + birth + military, data = divusaB)
#summary(m1)
m2<- lm(divorce ~  femlab + marriage + birth + military, data = divusaB)
summary(m2)
anova(m2,m1)

#The result of the test ia as follows
#Res.Df    RSS Df Sum of Sq     F Pr(>F)
#1     71 305.28                          
#2     72 307.66 -1   -2.3864 0.555 0.4587
#The residual sum of square for model1(m2) is 305.28 and model2(m1) is 307.66 with the difference -2.3864 and the p value is 0.4587. So we would fail to reject the null hypothesis and we can conclude that the data does not support the alternative hypothesis that there is difference in fit of two model. So we will conclude that the larger model does not fit better than the smaller model.

# 2(d)
#Compare the predictive accuracy of the two models from part (c) using 50 repeats of 10-fold cross validation

# Solution

#10-fold cross-validation
# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=50)
# train the model
model1 <- train(divorce~., data=divusaB, trControl=train_control, method="lm")
# summarize results
print(model1)

#The result of model is as follows
#RMSE      Rsquared   MAE     
#2.071201  0.8230761  1.718682
#Now,
#The two models from part(c) are 
#m1 <- lm(divorce ~ unemployed + femlab + marriage + birth + military, data = divusaB)
#m2<- lm(divorce ~  femlab + marriage + birth + military, data = divusaB)
#Now I am goig to apply k-fold cross-validation for moth the models

#performing 10-fold cross-validation
m1 <- train(divorce ~ unemployed + femlab + marriage + birth + military, data=divusaB, trControl=train_control, method="lm")
# summarize results
print(m1)
m2 <- train(divorce~femlab + marriage + birth + military, data=divusaB, trControl=train_control, method="lm")
# summarize results
print(m2)

#The result from model m1 is as follows
#RMSE      Rsquared  MAE     
#2.070958  0.825239  1.721155
#The result for the model m2 is as follows
#RMSE      Rsquared   MAE     
#2.062907  0.8234187  1.721076
#From both the models m1 and m2 we can see that the RMSE values are m1=2.07 and m2=2.06
#There is only slight difference between m1 and m2 of 0.1.
#Therefore we can say that model2(m2) performance is better when compare to model1(m1)

# 2(e)
#Can you suggest any improvements to the model?

# Solution
#I want to build two models first and would like to draw conclusion by looking in to those models
#Model1 without marriage

#Linear model without marriage
divusa.lm1 <- lm(divorce ~unemployed+femlab+birth+military, data = divusaB)
summary(divusa.lm1)
vif(divusa.lm1)

#In the absence of marriage variable in the dataset, birth variable is no longer significant due to high p-value.
#VIF value of femlab has decreased to 1.787795. 
#R2 value has decreased to 0.8147.
#Residual standard error(S) also increased to 2.085. This means range increases for predicted interval and confidence interval.

#Model2 without birth

#Linear model birth
divusa.lm2 <- lm(divorce ~ unemployed+femlab+marriage+military, data = divusaB)
summary(divusa.lm2)
vif(divusa.lm2)

#In the absence of birth variable in the dataset, unemployed  variables are no longer significant due to high p-value.
#There is a significant decrease in intercept(??0).
#VIF value of femlab has decreased to 1.681645. 
#R2 value has decreased to 0.8165.
#Residual standard error(S) also increased to 2.075. This means range increases for predicted interval and confidence interval.

#Conclusion: Removal of variables has not improved the model. In fact, it has an adverse effect on parameters R2, Residual standard error(S) and intercept(??0).
#**************************************************************************************
# QUESTION 3
# 3(a)
#Please write two or three paragraphs on the concept of step-wise regression, including a brief description of three different types of step-wise regression and an overview of the different criteria that can be used to decide whether an explanatory variable is to be retained in the final model.  Finally, select and discuss one problem associated with step-wise regression.

# Solution

############please look into RMD File
   
# 3(b)
#Select a step-wise regression function from the statistical software R and give a brief description on how the algorithm is implemented. Include a description of the type of step-wise regression and the criterion used to determine whether a variable is included in the final model.

# Solution

#There are many functions and R packages for computing stepwise regression. These include:
#stepAIC()which is available in MASS package, which choose the best model by AIC. It has an option named direction, which can take the following values:
#i) "both" (for stepwise regression, both forward and backward selection);
#ii)"backward" (for backward selection) and
#iii)"forward" (for forward selection). It return the best final model.
#The stepAic() is implemented in the following way
install.packages("MASS")
library(MASS)
#I am loading swiss dataset which is available in R
data(swiss)
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

#I have used the dataset swiss which is available in R.I built the normal linear model model by using all the explanatory variable and considered Fertility as dependent variable and then i applied stepwise regression model by using stepAIC() function
#In this example both backward and forward selection are used.

# 3(c)
#Use 10-fold cross validation with step-wise regression to select a final model based on minimising the RMSE. Does using cross-validation prevent spurious conclusions?

# Solution
   
#In order to build the model by using 10-fold cross validation with step-wise regression.I need to generate my own data first.
#Generating the data:
#We begin by creating a single data set with 15 predictors, 5 of which are linearly related to the outcome Y and 15 of which are noise. An important consideration in regression analysis is the signal to noise ratio i.e. the magnitude of the effect size (the ?? coefficients) to the variance.To start, set the ?? coefficients that are linearly related to the outcome to 0.5 and set the variance of the error term to 1.We create a data set with 300 observations.

#Let $k$ represent the number of explanatory variables $X_j$  $j = 1...k$   
#Each explanatory variable is drawn from a Standard Normal distribution $X_j \sim N(0,1)$.  
#Let k_ln represent the number of explanatory variables that are linearly related to the response variable $Y$.   
#Let B_mag represent the magnitude of the population $\beta$ coefficients that are linearly related to the response $Y$.    
#The $i_{th}$ observation of the response variable is defined to be \[Y_i = \beta_0 + \beta_1X_{i1} + ... + \beta_kX_{ik} + e_i\] where $e_i \sim N(0,1)$  

set.seed(2)  # so can reproduce results
k = 15   # 15 explanatory variables
k_ln = 5 # 5 explanatory variables are linearly related to Y
B_mag = 0.5 # the magnitude of the explanatory variables that are linearly related to Y
n = 300  # number of observations in data set
#create a vector containing the population beta coefficients
B  <- c(rep(B_mag,k_ln),rep(0,k-k_ln))  
X  <- matrix(rnorm(n*k), nrow=n) # create the explanatory variables 
Y  <- X%*%B + matrix(rnorm(n),nrow=n) # create the response variable 
# combine the response and explanatory variables in a single dataframe
DF <- cbind(Y,X) 
DF<-as.data.frame(DF)
# assign column names
colnames(DF)<-c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11",  
                "X12","X13","X14","X15")

#Now I use 10-fold cross-validation to estimate the average prediction error (RMSE) of each of the 15 models. The RMSE statistical metric is used to compare the 15 models and to automatically choose the best one, where best is defined as the model that minimize the RMSE.

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Y ~., data = DF,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:15),
                    trControl = train.control)
#printing the result
step.model$results

#The output above shows different metrics and their standard deviation for comparing the accuracy of the 15 best models.The Columns are:
   
#nvmax: the number of variable in the model. For example nvmax = 2, specify the best 2-variables model

#RMSE and MAE are two different metrics measuring the prediction error of each model. The lower the RMSE and MAE, the better the model.

#Rsquared indicates the correlation between the observed outcome values and the values predicted by the model. The higher the R squared, the better the model.

#The best model among the 15 model is

#printing the best model
step.model$bestTune

#This indicates that the best model is the one with nvmax = 5 variables. The function summary() reports the best set of variables for each model size, up to the best 4-variables model.

#printing the summary of all variables until the best model
summary(step.model$finalModel)

#An asterisk specifies that a given variable is included in the corresponding model.  

#The regression coefficients of the final model (id = 5) can be accessed as follow:

#printing the coefficients
coef(step.model$finalModel, 4)

#This is the procedure to select a model based on minimising the RMSE by using
#10-fold cross validation with step-wise regression.

#if the particular training set has some spurious correlation with those test points, model will have difficulties determining which correlations are real and which are spurious, because even though the test set changes, the training set doesn't.
#In contrast, less correlated training folds means that the model will be fit to multiple unique datasets.
#Cross validation is a technique that allows us to produce test set like scoring metrics using the training set. That is, it allows us to simulate the effects of "going out of sample" using just our training data, so we can get a sense of how well our model generalizes.