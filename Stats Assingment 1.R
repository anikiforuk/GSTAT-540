# Assingment 1, Marin Stats Lectures Tutorial

#a)
summary(MentalHealth)
class(SES)
MentalImp       LifeEvents         SES       
Min.   :17.00   Min.   : 3.00   Min.   : 3.00  
1st Qu.:23.75   1st Qu.:32.75   1st Qu.:39.75  
Median :27.00   Median :43.00   Median :56.00  
Mean   :27.30   Mean   :44.42   Mean   :56.60  
3rd Qu.:31.00   3rd Qu.:55.50   3rd Qu.:75.75  
Max.   :41.00   Max.   :97.00   Max.   :97.00  

factor(MentalHealth)

boxplot(MentalHealth)
hist(MentalImp)
hist(LifeEvents)
hist(SES)
#b) determinant SES, outcome is MentalImp
model1 <- lm(MentalImp~SES)
summary(model1)
Call:
  lm(formula = MentalImp ~ SES)

Residuals:
  Min       1Q   Median       3Q      Max 
-10.8808  -2.7472   0.2939   2.7382  15.2838 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 32.17201    1.98765  16.186   <2e-16 ***
  SES         -0.08608    0.03213  -2.679   0.0109 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.072 on 38 degrees of freedom
Multiple R-squared:  0.1589,	Adjusted R-squared:  0.1367 
F-statistic: 7.177 on 1 and 38 DF,  p-value: 0.01085

Assumptions of the SLR
plot(model1)

#interpret the model slope
#y=b1+x1

# interpret the model slope
On average the MentalImp decreases by -0.086 for every one unit increases of SES.
y=32.17201 + (-0.08608)SES

cor(SES,LifeEvents)
cor(SES,MentalImp)
-0.3985676
mean(MentalImp)
#Ask mike about this******

#95% CI for the lm
confint(model1)
                2.5 %      97.5 %
(Intercept) 28.1482262 36.19579661
SES         -0.1511251 -0.02103079

0.086-0.151
-0.086+-0.021

#What proportion of mental health impairment can be explained by SES in this example or R^2?
  Multiple R-squared:  0.1589

#The scales are subjective and a one unit increase is not a real or interpretable value, ordinal numeric variables.
#The scales are different, what does a score mean in real life? 

#c
model2<-lm(MentalImp~SES+LifeEvents)
summary(model2)

Call:
  lm(formula = MentalImp ~ SES + LifeEvents)

Residuals:
  Min     1Q Median     3Q    Max 
-8.678 -2.494 -0.336  2.886 10.891 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 28.22981    2.17422  12.984 2.38e-15 ***
  SES         -0.09748    0.02908  -3.351  0.00186 ** 
  LifeEvents   0.10326    0.03250   3.177  0.00300 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.556 on 37 degrees of freedom
Multiple R-squared:  0.3392,	Adjusted R-squared:  0.3034 
F-statistic: 9.495 on 2 and 37 DF,  p-value: 0.0004697

# effect models you can use R^2
# raw data model 1
summary(model1)
Call:
  lm(formula = MentalImp ~ SES)

Residuals:
  Min       1Q   Median       3Q      Max 
-10.8808  -2.7472   0.2939   2.7382  15.2838 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 32.17201    1.98765  16.186   <2e-16 ***
  SES         -0.08608    0.03213  -2.679   0.0109 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.072 on 38 degrees of freedom
Multiple R-squared:  0.1589,	Adjusted R-squared:  0.1367 
F-statistic: 7.177 on 1 and 38 DF,  p-value: 0.01085

# raw data of model2
summary(model2)
Call:
  lm(formula = MentalImp ~ SES + LifeEvents)

Residuals:
  Min     1Q Median     3Q    Max 
-8.678 -2.494 -0.336  2.886 10.891 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 28.22981    2.17422  12.984 2.38e-15 ***
  SES         -0.09748    0.02908  -3.351  0.00186 ** 
  LifeEvents   0.10326    0.03250   3.177  0.00300 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.556 on 37 degrees of freedom
Multiple R-squared:  0.3392,	Adjusted R-squared:  0.3034 
F-statistic: 9.495 on 2 and 37 DF,  p-value: 0.0004697

# comparison of the two models model1 and model2
anova(model1,model2)
Analysis of Variance Table

Model 1: MentalImp ~ SES
Model 2: MentalImp ~ SES + LifeEvents
Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
1     38 977.75                                
2     37 768.16  1    209.58 10.095 0.002998 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# conceptually does the inclusion of life events make sense

#d) Do SES only






SES.sq<-SES^2
#assumptions are not met
#make model with linear function of SES call SES.sq
model2ln<-lm(MentalImp~SES+SES.sq+LifeEvents)
summary(model2ln)

#***** ask mike about this*****

par(mfrow=c(2,2))
plot(model2ln)

par(mfrow=c(2,2))
plot(model2)
# using the squaring function does not make it more linear

anova(model2,model2ln)
Analysis of Variance Table

Model 1: MentalImp ~ SES + LifeEvents
Model 2: MentalImp ~ SES + SES.sq + LifeEvents
Res.Df    RSS Df Sum of Sq      F Pr(>F)
1     37 768.16                           
2     36 767.97  1   0.19615 0.0092 0.9241

# do hypothesis test for partial F-test, the SES.sq model does not make the assumptions more linea


#e) use model2
summary(model2)
Call:
  lm(formula = MentalImp ~ SES + LifeEvents)

Residuals:
  Min     1Q Median     3Q    Max 
-8.678 -2.494 -0.336  2.886 10.891 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 28.22981    2.17422  12.984 2.38e-15 ***
  SES         -0.09748    0.02908  -3.351  0.00186 ** 
  LifeEvents   0.10326    0.03250   3.177  0.00300 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.556 on 37 degrees of freedom
Multiple R-squared:  0.3392,	Adjusted R-squared:  0.3034 
F-statistic: 9.495 on 2 and 37 DF,  p-value: 0.0004697


lm(formula = MentalImp ~ SES + LifeEvents)
y= 28.23+ (-0.097SES)+0.103LifeEvents

#LifeEvents=60
#SES=40
y= 28.23+ (-0.097*40)+0.103*60
#[1] 30.53

# Make a 95% CI for the predictive value
newdata=data.frame(SES=40, LifeEvents=60)
predict(model2,newdata,interval="predict")
fit      lwr     upr
1 30.52636 21.05952 39.9932


#f) Standardized Variables
#Standardize all variables in the dataset you can do this by
#standardized_x<-[x-mean(x)]/sd(x)

or

#standardized_x<-scale(variable,scale=T,center=T)
Stand_MentalImp<-scale(MentalImp,scale=T,center=T)
Stand_SES<-scale(SES,scale=T,center=T)
Stand_LifeEvents<-scale(LifeEvents,scale=T,center=T)


MIX<-Stand_MentalImp+5
SESX<-Stand_SES+5
LEX<-Stand_LifeEvents+5

hist(MIX)

par(mfrow=c(3,3))
hist(MentalImp)
hist(SES)
hist(LifeEvents)
hist(MIX)
hist(SESX)
hist(LEX)
# fit model for the standardized process
model3<-lm(MIX~SESX+LEX)
summary(model3)
Call:
  lm(formula = MIX ~ SESX + LEX)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.58962 -0.45675 -0.06155  0.52872  1.99486 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   5.1173     0.9013   5.677 1.72e-06 ***
  SESX         -0.4513     0.1347  -3.351  0.00186 ** 
  LEX           0.4279     0.1347   3.177  0.00300 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.8346 on 37 degrees of freedom
Multiple R-squared:  0.3392,	Adjusted R-squared:  0.3034 
F-statistic: 9.495 on 2 and 37 DF,  p-value: 0.0004697
m<-lm(MIX~SESX)
summary(m)
Residuals:
  Min       1Q   Median       3Q      Max 
-1.99304 -0.50321  0.05384  0.50155  2.79954 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   6.9928     0.7583   9.222 3.08e-11 ***
  SESX         -0.3986     0.1488  -2.679   0.0109 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9291 on 38 degrees of freedom
Multiple R-squared:  0.1589,	Adjusted R-squared:  0.1367 
F-statistic: 7.177 on 1 and 38 DF,  p-value: 0.01085


boxplot(MentalHealth)
        
        
        
### Question 2#####
# data set is = SalaryData
attach(SalaryData_NoMSC)

#a) Analysis of SalaryData set
names(SalaryData)
dim(SalaryData)
class(SalaryData$sex)
class(SalaryData$degree)
class(SalaryData$yearsdeg)
class(SalaryData$salary)
class(SalaryData$rank)

# the dataset countains 5 levels with n=52 observations
#sex= factor/CAT
#degree=factor/CAT
#yearsdegree=integer, # of years since the degree was earned
#salary=integer

#Summary of the dataset
summary(SalaryData)
ranks are equal in #
sex ~ ratio is 14F:38M
degree~ ratio is 36D:16M

hist(yearsdeg)
# random distribution multi-modal
hist(salary)
#left skewed distrubution, more lower slaries than higher ones

cor(yearsdeg,salary)
[1] 0.659165
# medium strength positive correlation as the years of work expierence increase so does the expected salary 

# it appears that the main outcome variable is ***Salary*** and the determinants are years degree, rank, degree and sex. 

# table of sex and salary
table(sex,degree)
degree
sex      doctorate masters
female        10       4
male          26      12

table(sex,rank)
rank
sex      assistant associate full
female         8         2    4
male           9        13   16

table(degree,rank)
rank
degree      assistant associate full
doctorate        14         7   15
masters           3         8    5

# box plots of degree, salary
boxplot(salary~ degree, main="Salary by degree", xlab="Degree Type", ylab="Salary $", las=1)
there are three outliers in the masters category, they make more salary than their peers
wage.data <- `HourlyWagesUSA.(1)`[-171 , ]
attach(SalaryData)

SalaryD<- SalaryData`[-5 , 9, 10 ]

View(SalaryD)
dim(SalaryD)
boxplot(salary~ degree, main="Salary by degree", xlab="Degree Type", ylab="Salary $", las=1)

# scatter plot of yearsdeg, salary
plot(salary~ yearsdeg, main="Salary by Expierence", xlab="Years of Work Expierence", ylab="Salary $", las=1)
there is an overall, positive correlation variability seems to increase by years of work expierence 

#box plot of rank, salary
boxplot(salary~ rank, main="Salary by Academic Rank", xlab="Rank", ylab="Salary $", las=1)
full professors make the most salary, there is a single outlier in the assistant level. 
mean(salary[rank=="assistant"])
[1] 18042.71
mean(salary[rank=="associate"])
[1] 23057.2
mean(salary[rank=="full"])
[1] 29658.95
# boxplot of sex, salary
boxplot(salary~ sex, main="Salary by Biological Sex", xlab="Sex", ylab="Salary $", las=1)
on average males make more salary than females, there is a single outlier in the female category 
mean(salary[sex=="male"])
[1] 24786.13
mean(salary[sex=="female"])
21706.43
#difference in mean salary between male and female
mean(salary[sex=="male"])-mean(salary[sex=="female"])
[1] 3079.703
#boxplot of degree, rank
tab1<- table(degree,rank)
tab1
mosaicplot(tab1)
# the majority of doctors are assitant or full
# the majority of masters are associates 

Removed Outliers by degree where MSC > 30,000, n=3
Removed Outliers by rank where assistant > 23,000, n=1
Removed Outliers by Sex where female >35,000, n=1

Total data points removed= 48
#### b) ####
mean(salary[sex=="male"])
[1] 24099.34
mean(salary[sex=="female"])
[1] 20462.08
#difference in mean salary between male and female
> mean(salary[sex=="male"])-mean(salary[sex=="female"])
[1] 3637.26
boxplot(salary~ sex, main="Salary by Biological Sex", xlab="Sex", ylab="Salary $", las=1)
sd(salary[sex=="male"])
[1] 5236.664
sd(salary[sex=="female"])
[1] 3914.781
# variance difference assumption is equal
# H0: on average the mean salary of male tenure track professors who work at a small mid-western college in the united states does not
# differ from female tenure track professors at the same institution. y1-y2=0
# HA: on average the mean salary of male tenure track professors who work at a small mid-western college in the united states does
# differ from female tenure track professors at the same institution. y1-y2=>0,<0
t.test(salary[sex=="male"],salary[sex=="female"], var.equal = T,alternative = c("two.sided"),conf.level = 0.95,paired = FALSE)
Two Sample t-test

data:  salary[sex == "male"] and salary[sex == "female"]
t = 2.1982, df = 45, p-value = 0.03312
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  304.6554 6969.8636
sample estimates:
  mean of x mean of y 
24099.34  20462.08 
p < 0.05 therefore we reject the null-hypothesis, there is a significant difference in mean salaries by biological sex.
attach(SalaryData_NoOUT)

#Model the unadjusted effect of sex
ModelSex<-lm(salary~sex)
summary(ModelSex)
Call:
  lm(formula = salary ~ sex)

Residuals:
  Min      1Q  Median      3Q     Max 
-8005.3 -3411.8   227.9  3392.7 12250.7 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    20462       1428  14.331   <2e-16 ***
  sexmale         3637       1655   2.198   0.0331 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4946 on 45 degrees of freedom
Multiple R-squared:  0.09697,	Adjusted R-squared:  0.0769 
F-statistic: 4.832 on 1 and 45 DF,  p-value: 0.03312

plot(ModelSex)

### c)###
mod2.1<-lm(salary~sex+degree+yearsdeg+rank)
summary(mod2.1)

Call:
  lm(formula = salary ~ sex + degree + yearsdeg + rank)

Residuals:
  Min      1Q  Median      3Q     Max 
-4045.1 -1527.2  -409.3  1212.7  6318.5 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)   16087.69     983.67    16.355   < 2e-16 ***
  sexmale        1927.92     945.50   2.039  0.04793 *  
  degreemasters  -522.73    1134.26  -0.461  0.64734    
yearsdeg        117.17      66.49   1.762    0.08549 .  
rankassociate  3445.00    1187.07   2.902    0.00594 ** 
  rankfull       8438.26    1415.54   5.961  4.91e-07 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2578 on 41 degrees of freedom
Multiple R-squared:  0.7766,	Adjusted R-squared:  0.7493 
F-statistic:  28.5 on 5 and 41 DF,  p-value: 2.419e-12
# salary= 16087.69 + 1927.92*sexmale + -522.73*degreemasters + 117.17*yearsdeg + 3445.00*rankassociate + 8438.26*rankfull 
the reference category is

a female, PhD, with zero yrs of work expierence, with the rank of assistant

# check the assumptions of linear regression:
1) independence- knowledge of study deisgn, yes INDP
2) linearity- straight line in residual plot, plot 1
#line curves with greate salary values
3) homoscedasticity- variance in y is constant for any variable of x, check for cloud in residual plot
# variance increases with salary
4) normal distribution- for a given value of x y- is approximately normal, check with a q-q plot
upper end of q-q plot deviates
# check assumptions of mod2.1
plot(mod2.1)

# try transformation by polynomial does not help correct
hist(yearsdeg)
yearsdeg2<-yearsdeg^2
mod2.2<-lm(salary~sex+degree+yearsdeg+yearsdeg2+rank)
plot(mod2.2)

# try tansformation by log
lnyearsdeg<-log(yearsdeg)
mod2.3<-lm(salary~sex+degree+lnyearsdeg+rank)
plot(mod2.3)

#try transformation by squaring
yearsdeg2<-yearsdeg^2
mod2.3<-lm(salary~sex+degree+yearsdeg2+rank)
plot(mod2.3)

hist(yearsdeg2)
# histogram is right skewed, may be approximately more normal

### D)### 
we are removing the variable of rank from out analysis

#research question: is there a difference in salaries between males and females?
mod2d<-lm(salary~sex)
summary(mod2d)
Call:
  lm(formula = salary ~ sex)

Residuals:
  Min      1Q  Median      3Q     Max 
-8005.3 -3411.8   227.9  3392.7 12250.7 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    20462       1428  14.331   <2e-16 ***
  sexmale         3637       1655   2.198   0.0331 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4946 on 45 degrees of freedom
Multiple R-squared:  0.09697,	Adjusted R-squared:  0.0769 
F-statistic: 4.832 on 1 and 45 DF,  p-value: 0.03312
# r squared value is very low, sex only explains ~ 9% of variation in salary

mod2d.2<-lm(salary~sex+yearsdeg)
summary(mod2d.2)
plot(mod2d.2)
Call:
  lm(formula = salary ~ sex + yearsdeg)

Residuals:
  Min      1Q  Median      3Q     Max 
-8703.3 -2294.9  -536.5  1899.4  9080.7 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 15228.98    1438.80  10.584 1.13e-13 ***
  sexmale      3807.13    1274.98   2.986   0.0046 ** 
  yearsdeg      328.78      58.27   5.642 1.13e-06 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3810 on 44 degrees of freedom
Multiple R-squared:  0.476,	Adjusted R-squared:  0.4522 
F-statistic: 19.99 on 2 and 44 DF,  p-value: 6.678e-07
# r squared value is higher low, sex and yearsdeg explain ~ 48% of variation in salary

# compare the models by

anova(mod2d, mod2d.2)
Analysis of Variance Table

Model 1: salary ~ sex
Model 2: salary ~ sex + yearsdeg
Res.Df       RSS Df Sum of Sq      F   Pr(>F)    
1     45 1.101e+09                                 
2     44 6.388e+08  1 462148934 31.832 1.13e-06 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# there is a statistically significant increase in the model fit of mod2d.2(Model 2: salary ~ sex + yearsdeg)

AIC(mod2d);AIC(mod2d.2)
[1] 936.937
[1] 913.3532

BIC(mod2d);BIC(mod2d.2)
[1] 942.4874
[1] 920.7538

R^2 #mod2d.2<-lm(salary~sex+yearsdeg)
Residual standard error: #mod2d.2<-lm(salary~sex+yearsdeg), 3810 to 4946 Model 1: salary ~ sex
Partial F-test #Model 2: salary ~ sex + yearsdeg), 1.13e-06 ***
AIC/BIC #mod2d.2<-lm(salary~sex+yearsdeg), both values are lower
Linear Regression assumptions:
  plot(mod2d.2)

# check the assumptions of linear regression:
1) independence- knowledge of study deisgn, yes INDP
2) linearity- straight line in residual plot, plot 1 #not well met# 
#line curves with greate salary values
3) homoscedasticity- variance in y is constant for any variable of x, check for cloud in residual plot
# variance increases with salary
4) normal distribution- for a given value of x y- is approximately normal, check with a q-q plot
upper end of q-q plot deviates

# transformation of yearsdeg^2 does not increase linearity. 
mod2d.3<-lm(salary~sex+yearsdeg2)
plot(mod2d.3)



### e) ###
mod2d.2<-lm(salary~sex+yearsdeg)
mod2e.1<-lm(salary~sex+yearsdeg+degree)

# summary of the new model 
summary(mod2e.1)
Call:
  lm(formula = salary ~ sex + yearsdeg + degree)

Residuals:
  Min      1Q  Median      3Q     Max 
-7040.3 -2524.5  -662.2  2178.2  8524.0 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   15226.03    1312.01  11.605 7.80e-15 ***
  sexmale        3545.53    1165.59   3.042  0.00400 ** 
  yearsdeg        412.80      59.46   6.942 1.56e-08 ***
  degreemasters -4003.22    1271.34  -3.149  0.00298 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3475 on 43 degrees of freedom
Multiple R-squared:  0.5742,	Adjusted R-squared:  0.5445 
F-statistic: 19.33 on 3 and 43 DF,  p-value: 4.368e-08
# r squared value is higher, sex, yearsdeg and degree explain ~ 57% of variation in salary

# compare the models by

anova(mod2d.2,mod2e.1)
Analysis of Variance Table

Model 1: salary ~ sex + yearsdeg
Model 2: salary ~ sex + yearsdeg + degree
Res.Df       RSS Df Sum of Sq     F   Pr(>F)   
1     44 638801777                               
2     43 519105281  1 119696496 9.915 0.002978 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# there is a statistically significant increase in the model fit of Model 2: salary ~ sex + yearsdeg + degree

AIC(mod2d.2);AIC(mod2e.1)
[1] 913.3532
[1] 905.6013

BIC(mod2d.2);BIC(mod2e.1)
[1] 920.7538
[1] 914.852

R^2:#Model 1: salary ~ sex + yearsdeg, 0.476: 0.5742, Model 2: salary ~ sex + yearsdeg + degree
Residual standard error: # :Model 1: salary ~ sex + yearsdeg, 3810 :3475,Model 2: salary ~ sex + yearsdeg + degree
  Partial F-test: # Model 2: salary ~ sex + yearsdeg + degree, 0.002978
AIC/BIC: # Model 2: salary ~ sex + yearsdeg + degree
Linear Regression assumptions:
  plot(mod2e.1)
# check the assumptions of linear regression:
1) independence- knowledge of study deisgn, yes INDP
2) linearity- straight line in residual plot, plot 1 
# not well fit line curves up and high and low salary value
3) homoscedasticity- variance in y is constant for any variable of x, check for cloud in residual plot
# approximately good
4) normal distribution- for a given value of x y- is approximately normal, check with a q-q plot
# approximately normal

conclusion: include this variable
par(mfrow=c(2,2))
plot(mod2e.1)

### f ###
mod2e.1
Call:
  lm(formula = salary ~ sex + yearsdeg + degree)

Residuals:
  Min      1Q  Median      3Q     Max 
-7040.3 -2524.5  -662.2  2178.2  8524.0 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   15226.03    1312.01  11.605 7.80e-15 ***
  sexmale        3545.53    1165.59   3.042  0.00400 ** 
  yearsdeg        412.80      59.46   6.942 1.56e-08 ***
  degreemasters -4003.22    1271.34  -3.149  0.00298 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3475 on 43 degrees of freedom
Multiple R-squared:  0.5742,	Adjusted R-squared:  0.5445 
F-statistic: 19.33 on 3 and 43 DF,  p-value: 4.368e-08

# reference category is female PHD, 0 yrs of expierence 
# the determinants of Sex, yrs wrk exp, degree type explain ~57% of the variability in salary for...

# calculate prediction (individual) and confidence (population) 95% CI
c<-predict(mod2e.1,interval = "confidence")
p<-predict(mod2e.1,interval = "prediction")

View(c)

> confint(mod2e.1)
                2.5 %    97.5 %
(Intercept)   12580.110 17871.960
sexmale        1194.903  5896.165
yearsdeg        292.887   532.720
degreemasters -6567.127 -1439.316

# Make a 95% CI for the predictive value on the variable of sex= MALE
newdata=data.frame(sex="male", yearsdeg=0,degree="doctorate")
predict(mod2e.1,newdata,interval="predict")
fit      lwr      upr
1 18771.57 11474.08 26069.06

y=15226.0+ 3545.5 *  sexmale +  412.8 * yearsdeg +  -4003.2  + degreemasters  
y=15226.0+ 3545.5 *  sexmale=1 +  412.8 * yearsdeg=0 +  -4003.2  + degreemasters=0
ymale<-15226.0+ 3545.5 
ymale
[1] 18771.5, 95% CI (11474.08-26069.06)
# Make a 95% CI for the predictive value on the variable of sex= feMALE
newdata=data.frame(sex="female", yearsdeg=0,degree="doctorate")
predict(mod2e.1,newdata,interval="predict")
fit      lwr      upr
1 15226.03 7736.095 22715.97

y=15226.0+ 3545.5 *  sexmale +  412.8 * yearsdeg +  -4003.2  + degreemasters  
y=15226.0+ 3545.5 *  sexmale=0 +  412.8 * yearsdeg=0 +  -4003.2  + degreemasters=0
yfemale<-15226.0
yfemale
[1] 15226, 95% CI (7736.095-22715.97)

ydiff<-ymale-yfemale
ydiff
[1] 3545.5

### g) ### Effect Modifiers 
attach(SalaryData_NoOUT)
non parallell lines , the effect of sex is modified by yearsdegree or degree
table(degree,yearsdeg)

Outcome is salary
Determinant is sex
EM: yearsdeg or degree 

Base Model: 
mod2e.1<-lm(salary~sex+yearsdeg+degree)
# use the full colon for the interaction term 
Effect Modifying MLR Model:  
ModEMyrs<-lm(formula = salary ~ sex + yearsdeg + degree +yearsdeg:sex)
ModEMdeg<-lm(formula = salary ~ sex + yearsdeg + degree +degree:sex)
# you can compare three models using the partial F-test
anova(mod2e.1,ModEMyrs)
Analysis of Variance Table

Model 1: salary ~ sex + yearsdeg + degree
Model 2: salary ~ sex + yearsdeg + degree + yearsdeg:sex
Res.Df       RSS Df Sum of Sq      F Pr(>F)
1     43 519105281                           
2     42 513252664  1   5852617 0.4789 0.4927

AIC(mod2e.1,ModEMyrs)
> AIC(mod2e.1,ModEMyrs)
df      AIC
mod2e.1   5 905.6013
ModEMyrs  6 907.0684
# you can compare three models using the partial F-test
anova(mod2e.1,ModEMdeg)
Analysis of Variance Table

Model 1: salary ~ sex + yearsdeg + degree
Model 2: salary ~ sex + yearsdeg + degree + degree:sex
Res.Df       RSS Df Sum of Sq      F Pr(>F)
1     43 519105281                           
2     42 519071283  1     33998 0.0028 0.9584
AIC(mod2e.1,ModEMdeg)
df      AIC
mod2e.1   5 905.6013
ModEMdeg  6 907.5982
# the two methods of entering effect modification produce the same result.
summary(ModEMdeg)
mm<-lm(formula = salary ~ sex*degree + yearsdeg)


summary(mm)



> attach(SalaryData_NoOUT)
> 12/52
[1] 0.2307692
> 4/52
[1] 0.07692308
> 26/52
[1] 0.5
> 10/52
[1] 0.1923077
> 23+8+19+50
[1] 100
> mean(salary==[Sex="1"])
Error: unexpected '[' in "mean(salary==["
> mean(salary==[Sex="Male"])
Error: unexpected '[' in "mean(salary==["
> mean(salary[sex=="male"])
[1] 24099.34
> mean(salary[sex=="female"])
[1] 20462.08
> sd(salary[sex=="male"])
[1] 5236.664
> sd(salary[sex=="female"])
[1] 3914.781
> mean(yearsdeg[sex=="male"])
[1] 15.4
> mean(yearsdeg[sex=="female"])
[1] 15.91667
> sd(yearsdeg[sex=="male"])
[1] 8.892164
> sd(yearsdeg[sex=="female"])
[1] 12.01104
> summary(SalaryData_NoOUT)
sex           degree      yearsdeg         salary             rank   
female:12   doctorate:34   Min.   : 1.00   Min.   :15000   assistant:16  
male  :35   masters  :13   1st Qu.: 7.00   1st Qu.:19145   associate:15  
Median :15.00   Median :23300   full     :16  
Mean   :15.53   Mean   :23171                 
3rd Qu.:22.00   3rd Qu.:25965                 
Max.   :35.00   Max.   :36350                 
Warning messages:
  1: In doTryCatch(return(expr), name, parentenv, handler) :
  display list redraw incomplete
2: In doTryCatch(return(expr), name, parentenv, handler) :
  invalid graphics state
3: In doTryCatch(return(expr), name, parentenv, handler) :
  invalid graphics state
> table(sex,degree)
degree
sex      doctorate masters
female         8       4
male          26       9
> 26/47
[1] 0.5531915
> 8/47
[1] 0.1702128
> 9/47
[1] 0.1914894
> 4/47
[1] 0.08510638
> 19+8+55+17
[1] 99
> mod2e.1
Error: object 'mod2e.1' not found
> mod2e.1<-lm(salary~sex+yearsdeg+degree)
> x<-lm(salary~sex+degree)
> anova(x,mod2e.1)
Analysis of Variance Table

Model 1: salary ~ sex + degree
Model 2: salary ~ sex + yearsdeg + degree
Res.Df        RSS Df Sum of Sq      F   Pr(>F)    
1     44 1100933823                                 
2     43  519105281  1 581828542 48.196 1.56e-08 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> AIC(x,mod2e.1)
df      AIC
x        4 938.9362
mod2e.1  5 905.6013
> summary(x)

Call:
  lm(formula = salary ~ sex + degree)

Residuals:
  Min      1Q  Median      3Q     Max 
-8016.3 -3401.5   256.2  3381.7 12239.7 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   20476.25    1543.49  13.266   <2e-16 ***
  sexmale        3634.02    1677.95   2.166   0.0358 *  
  degreemasters   -42.49    1635.66  -0.026   0.9794    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5002 on 44 degrees of freedom
Multiple R-squared:  0.09698,	Adjusted R-squared:  0.05594 
F-statistic: 2.363 on 2 and 44 DF,  p-value: 0.106

> a<-lm(salary~sex)
> anova(a,x)
Analysis of Variance Table

Model 1: salary ~ sex
Model 2: salary ~ sex + degree
Res.Df        RSS Df Sum of Sq     F Pr(>F)
1     45 1100950711                          
2     44 1100933823  1     16888 7e-04 0.9794
> mod2e.1

Call:
  lm(formula = salary ~ sex + yearsdeg + degree)

Coefficients:
  (Intercept)        sexmale       yearsdeg  degreemasters  
15226.0         3545.5          412.8        -4003.2  

> 15226+3545
[1] 18771
> 3637*0.01
[1] 36.37
> summary(mod2e.1)

Call:
  lm(formula = salary ~ sex + yearsdeg + degree)

Residuals:
  Min      1Q  Median      3Q     Max 
-7040.3 -2524.5  -662.2  2178.2  8524.0 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   15226.03    1312.01  11.605 7.80e-15 ***
  sexmale        3545.53    1165.59   3.042  0.00400 ** 
  yearsdeg        412.80      59.46   6.942 1.56e-08 ***
  degreemasters -4003.22    1271.34  -3.149  0.00298 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3475 on 43 degrees of freedom
Multiple R-squared:  0.5742,	Adjusted R-squared:  0.5445 
F-statistic: 19.33 on 3 and 43 DF,  p-value: 4.368e-08








