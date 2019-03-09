##Assingment 2##
attach(ContraceptiveUseIndicators)
class(ContraceptiveUseIndicators)
Age= Numeric Age
AgeCat=Categorized ages: A<25, B=25-29, C=30-39, D=40-49
Education= 0= low and, 1 = high
WantsMore= indicator if the woman wants more children 0= no, 1= yes
UseContraceptive= an indicator of contraceptive use 0=no, 1 =yes

## a) Univariate Summary
Education<-as.factor(Education)
WantsMore<-as.factor(WantsMore)
summary(Age)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
17.00   25.00   30.00   30.45   36.00   47.00 

boxplot(Age)

levels(AgeCat)
[1] "A" "B" "C" "D"
summary(AgeCat=="A")
Mode   FALSE    TRUE 
logical    1210     397 

summary(AgeCat=="B")
Mode   FALSE    TRUE 
logical    1203     404 

summary(AgeCat=="C")
Mode   FALSE    TRUE 
logical     995     612 

summary(AgeCat=="D")
Mode   FALSE    TRUE 
logical    1413     194 
,194
table(Education)
Education
0   1 
613 994 

table(WantsMore)
WantsMore
0   1 
635 972

table(UseContraceptive)
UseContraceptive
0    1 
1100  507 

## b) Bivariate Summary
pairs(ContraceptiveUseIndicators)
## c) Age Category and Education
t1<-table(Education,AgeCat)
AgeCat
Education   A   B   C   D
0           73 103 302 135
1           324 301 310 59
mosaicplot(t1)
324/(324+73)
301/(301+103)
310/(310+302)
59/(59+135)
# direction of association is one directional and negative, younger persons 
# seem to posess a higher degree of education.
m1<-glm(Education ~ AgeCat, family="binomial")
summary(m1)
Call:
  glm(formula = Education ~ AgeCat, family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.8404  -1.1885   0.6375   0.7672   1.5429  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) "4.4"   1.4903     0.1296  11.503   <2e-16 ***
  AgeCatB   "2.9"   -0.4179     0.1727  -2.420   0.0155 *  
  AgeCatC   "1.03"   -1.4641     0.1527  -9.587   <2e-16 ***
  AgeCatD   "0.44"   -2.3180     0.2028 -11.428   <2e-16 ***
  " undajusted odds of higher education EDC=1"
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 AgeCatA= exp(1.49)
 AgeCatB= exp(1.49-0.42)
 AgeCatC=exp(1.49+(-1.46))
 AgeCatD=exp(1.49+(-2.32))
   
(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2136.6  on 1606  degrees of freedom
Residual deviance: 1924.3  on 1603  degrees of freedom
AIC: 1932.3

Number of Fisher Scoring iterations: 4

## d) Unadjusted Odds Ratio, determinant=education, outcome= use of contraceptive
t2<-table(Education,UseContraceptive)
t2
            UseContraceptive
Education     0   1
            0 412 201
            1 688 306
            
tab2=matrix(c(306,688,201,412),nrow=2,byrow=T)
tab2
epi.2by2(tab2)
> epi.2by2(tab2)
Outcome +    Outcome -      Total        Inc risk *        Odds
Exposed +          306          688        994              30.8       0.445
Exposed -          201          412        613              32.8       0.488
Total              507         1100       1607              31.5       0.461

Point estimates and 95 % CIs:
  -------------------------------------------------------------------
  Inc risk ratio                               0.94 (0.81, 1.09)
Odds ratio                                   '0.91 (0.73, 1.13)'
Attrib risk *                                -2.00 (-6.70, 2.69)
Attrib risk in population *                  -1.24 (-5.60, 3.12)
Attrib fraction in exposed (%)               -6.51 (-23.35, 8.02)
Attrib fraction in population (%)            -3.93 (-13.56, 4.88)
-------------------------------------------------------------------
  X2 test statistic: 0.706 p-value: 0.401
Wald confidence limits
* Outcomes per 100 population units 
# simple linear regression of education and use of contraceptives
m2<-glm(UseContraceptive~Education,family = "binomial")
summary(m2)

Call:
  glm(formula = UseContraceptive ~ Education, family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.8914  -0.8914  -0.8578   1.4934   1.5350  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.71772    0.08604  -8.342   <2e-16 ***
  Education1  -0.09249    0.11011  -0.840    0.401    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2003.7  on 1606  degrees of freedom
Residual deviance: 2003.0  on 1605  degrees of freedom
AIC: 2007

Number of Fisher Scoring iterations: 4

Odds of Contraceptive use EDU=0, exp(-0.718)= ODDS'0.4877'
Odds of Contraceptive use EDU=1, exp(-0.718+-0.0925)= ODDS'0.4446'
Odds Ratio: Odds Exposed/ Odds Unexposed
[1] 0.911626 #they are the same. 


## Adjust for Age Cat in the GLM 
m2<-glm(UseContraceptive~Education,family = "binomial")
summary(m3)
anova(m2, m3, test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education
Model 2: UseContraceptive ~ Education + AgeCat
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1605     2003.0                          
2      1602     1918.3  3    84.65 < '2.2e-16 ***'
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Statistically inclusion of AgeCat makes sense by an analysis of deviance and AIC
Concpetually inclusion of AgeCat makes sense because 1) there is an association between age cat and edcation 
2) AgeCat should theoretically also impact contraceptive use and 3) AgeCat is not on the causal pathway
#AgeCat=Confounder

AIC(m2,m3)
df      AIC
m2  2 2006.990
m3  5 '1928.339'


Call:
  glm(formula = UseContraceptive ~ Education + AgeCat, family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.2313  -0.9304  -0.6474   1.3131   1.9573  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.7562     0.1660 -10.582  < 2e-16 ***
  Education1    0.2999     0.1216   2.465  0.01368 *  
  AgeCatB       0.4821     0.1731   2.784  0.00536 ** 
  AgeCatC       1.1428     0.1595   7.165 7.77e-13 ***
  AgeCatD       1.5822     0.2051   7.716 1.20e-14 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2003.7  on 1606  degrees of freedom
Residual deviance: 1918.3  on 1602  degrees of freedom
AIC: 1928.3

Number of Fisher Scoring iterations: 4
### (f) Adjusted Odds: 
exp(coef(m3)) #to get OR
exp(confint(m3)) #to get CI for OR

(Intercept)  Education1     AgeCatB     AgeCatC     AgeCatD 
0.172707    1.349727    1.619432    3.135651    4.865763

               2.5 %    97.5 %
(Intercept) 0.1239897 0.2377507
Education1  1.0648165 1.7157274
AgeCatB     1.1555442 2.2798258
AgeCatC     2.3036901 4.3075469
AgeCatD     3.2645685 7.2986829

'definition for confounding is  diff<,> 10% in the Odds'
EDC=0                               EDC=1               ORman                OR
AgeCatA
exp(0.17)                         exp(0.17+1.349)       4.57/1.18
1.18, (95%CI 1.13-1.25)           4.57                  3.87 (95%CI 2.87-5.57)

L
exp(0.12)                       exp(0.12+1.06)        3.25/1.13
1.13                            3.25                  2.87
U
exp(0.23)                       exp(0.23+1.71)       6.96/1.25  
1.25                            6.96                 5.57





### (g) Include Wants More?
m3
glm(formula = UseContraceptive ~ Education + AgeCat, family = "binomial")

m4<-glm(UseContraceptive~Education+AgeCat+WantsMore, family = "binomial")
summary(m4)
Call:
  glm(formula = UseContraceptive ~ Education + AgeCat + WantsMore, 
      family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.3429  -0.8819  -0.6129   1.1351   2.0480  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.1332     0.1871  -6.057 1.39e-09 ***
  Education1    0.3250     0.1240   2.620  0.00879 ** 
  AgeCatB       0.3894     0.1759   2.214  0.02681 *  
  AgeCatC       0.9086     0.1646   5.519 3.40e-08 ***
  AgeCatD       1.1892     0.2144   5.546 2.92e-08 ***
  WantsMore1   -0.8330     0.1175  -7.091 1.33e-12 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2003.7  on 1606  degrees of freedom
Residual deviance: 1867.8  on 1601  degrees of freedom
AIC: 1879.8

Number of Fisher Scoring iterations: 4

anova(m3,m4,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1602     1918.3                          
2      1601     1867.8  1   50.501 1.191e-12 ***
AIC(m3,m4)  
> AIC(m3,m4)
df      AIC
m3  5 1928.339
'm4'  6 1879.838

## h) predicted probabilities 
 odds of contraceptive use=exp(-1.1332+0.3250+-0.8330) 
 [1] 0.1937474
 Probability 
 exp(-1.6412)/(1+exp(-1.6412))
 [1] 0.1623018*100
 16.23
 
 or
 
 (0.1937474)/(0.1937474+1)
 0.1623018
 
 ## i) is it okay to predict using this model
 
 Yes it is okay to use our model for prediction because:
   
   1) the study population is not case control, we have not defined the total n
   2) for a predictive model you typically start with all conceptually important x and reduce, we built up
   3) you need ~10 obs per x variable added in the model 
   4) all teh variables make conceptual sense as predictors
   5) is a simpler model avalable?
  
a simpler model may be does the woman want to have more children?
m5<-glm(UseContraceptive~WantsMore,family = "binomial")  
anova(m5,m4,test="LRT")
AIC(m5,m4)
> 301/404
[1] 0.7450495
> 1-0.74
[1] 0.26
> 0.74/0.26
[1] 2.846154
> ## d) Unadjusted Odds Ratio, determinant=education, outcome= use of contraceptive
  > t2<-table(Education,UseContraceptive)
> t2
UseContraceptive
Education   0   1
0 412 201
1 688 306
> epi.2by2(t2)
Outcome +    Outcome -      Total        Inc risk *        Odds
Exposed +          412          201        613              67.2        2.05
Exposed -          688          306        994              69.2        2.25
Total             1100          507       1607              68.5        2.17

Point estimates and 95 % CIs:
  -------------------------------------------------------------------
  Inc risk ratio                               0.97 (0.91, 1.04)
Odds ratio                                   0.91 (0.73, 1.13)
Attrib risk *                                -2.00 (-6.70, 2.69)
Attrib risk in population *                  -0.76 (-4.42, 2.90)
Attrib fraction in exposed (%)               -2.98 (-10.35, 3.89)
Attrib fraction in population (%)            -1.12 (-3.77, 1.47)
-------------------------------------------------------------------
  X2 test statistic: 0.706 p-value: 0.401
Wald confidence limits
* Outcomes per 100 population units 
> tab2=matrix(c(36,190,79,548),nrow=2,byrow=T)
> tab2
[,1] [,2]
[1,]   36  190
[2,]   79  548
> tab2=matrix(c(306,688,201,412),nrow=2,byrow=T)
> tab2
[,1] [,2]
[1,]  306  688
[2,]  201  412
> epi.2by2(tab2)
Outcome +    Outcome -      Total        Inc risk *        Odds
Exposed +          306          688        994              30.8       0.445
Exposed -          201          412        613              32.8       0.488
Total              507         1100       1607              31.5       0.461

Point estimates and 95 % CIs:
  -------------------------------------------------------------------
  Inc risk ratio                               0.94 (0.81, 1.09)
Odds ratio                                   0.91 (0.73, 1.13)
Attrib risk *                                -2.00 (-6.70, 2.69)
Attrib risk in population *                  -1.24 (-5.60, 3.12)
Attrib fraction in exposed (%)               -6.51 (-23.35, 8.02)
Attrib fraction in population (%)            -3.93 (-13.56, 4.88)
-------------------------------------------------------------------
  X2 test statistic: 0.706 p-value: 0.401
Wald confidence limits
* Outcomes per 100 population units 
> m2<-glm(UseContraceptive~Education,family = "binomial")
> summary(m2)

Call:
  glm(formula = UseContraceptive ~ Education, family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.8914  -0.8914  -0.8578   1.4934   1.5350  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.71772    0.08604  -8.342   <2e-16 ***
  Education1  -0.09249    0.11011  -0.840    0.401    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2003.7  on 1606  degrees of freedom
Residual deviance: 2003.0  on 1605  degrees of freedom
AIC: 2007

Number of Fisher Scoring iterations: 4

> exp(-0.718)
[1] 0.4877267
> exp(-0.718+-0.0925)
[1] 0.4446357
> 0.4446/0.4877
[1] 0.911626

exp(-0.09249)
> m3<-glm(UseContraceptive~Education+AgeCat, family = "binomial")
> summary(m3)

Call:
  glm(formula = UseContraceptive ~ Education + AgeCat, family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.2313  -0.9304  -0.6474   1.3131   1.9573  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.7562     0.1660 -10.582  < 2e-16 ***
  Education1    0.2999     0.1216   2.465  0.01368 *  
  AgeCatB       0.4821     0.1731   2.784  0.00536 ** 
  AgeCatC       1.1428     0.1595   7.165 7.77e-13 ***
  AgeCatD       1.5822     0.2051   7.716 1.20e-14 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2003.7  on 1606  degrees of freedom
Residual deviance: 1918.3  on 1602  degrees of freedom
AIC: 1928.3

Number of Fisher Scoring iterations: 4

> anova(m2, m3, test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education
Model 2: UseContraceptive ~ Education + AgeCat
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1605     2003.0                          
2      1602     1918.3  3    84.65 < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> AIC(m2,m3)
df      AIC
m2  2 2006.990
m3  5 1928.339
> exp(coef(m3))
(Intercept)  Education1     AgeCatB     AgeCatC     AgeCatD 
0.172707    1.349727    1.619432    3.135651    4.865763 
Warning messages:
  1: In doTryCatch(return(expr), name, parentenv, handler) :
  display list redraw incomplete
2: In doTryCatch(return(expr), name, parentenv, handler) :
  invalid graphics state
3: In doTryCatch(return(expr), name, parentenv, handler) :
  invalid graphics state
> exp(confint(m3))
Waiting for profiling to be done...
2.5 %    97.5 %
  (Intercept) 0.1239897 0.2377507
Education1  1.0648165 1.7157274
AgeCatB     1.1555442 2.2798258
AgeCatC     2.3036901 4.3075469
AgeCatD     3.2645685 7.2986829
> ### (g) Include Wants More?
  > m3

Call:  glm(formula = UseContraceptive ~ Education + AgeCat, family = "binomial")

Coefficients:
  (Intercept)   Education1      AgeCatB      AgeCatC      AgeCatD  
-1.7562       0.2999       0.4821       1.1428       1.5822  

Degrees of Freedom: 1606 Total (i.e. Null);  1602 Residual
Null Deviance:	    2004 
Residual Deviance: 1918 	AIC: 1928
> m4<-glm(UseContraceptive~Education+AgeCat+WantsMore, family = "binomial")
> m4<-glm(UseContraceptive~Education+AgeCat+WantsMore, family = "binomial")
> summary(m4)

Call:
  glm(formula = UseContraceptive ~ Education + AgeCat + WantsMore, 
      family = "binomial")

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.3429  -0.8819  -0.6129   1.1351   2.0480  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.1332     0.1871  -6.057 1.39e-09 ***
  Education1    0.3250     0.1240   2.620  0.00879 ** 
  AgeCatB       0.3894     0.1759   2.214  0.02681 *  
  AgeCatC       0.9086     0.1646   5.519 3.40e-08 ***
  AgeCatD       1.1892     0.2144   5.546 2.92e-08 ***
  WantsMore1   -0.8330     0.1175  -7.091 1.33e-12 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2003.7  on 1606  degrees of freedom
Residual deviance: 1867.8  on 1601  degrees of freedom
AIC: 1879.8

Number of Fisher Scoring iterations: 4

> anova(m3,m4,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1602     1918.3                          
2      1601     1867.8  1   50.501 1.191e-12 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 2      1601     1867.8  1   50.501 1.191e-12 ***
  Error: unexpected numeric constant in "2      1601"
> AIC(m3,m4)
df      AIC
m3  5 1928.339
m4  6 1879.838
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Error: unexpected symbol in "Signif. codes"
> 
  > exp(-1.1332+0.3250+-0.8330) 
[1] 0.1937474
> Probability
Error: object 'Probability' not found
> 
  > 
  > exp(logit)/(1+exp(logit))
Error: object 'logit' not found
> -1.1332+0.3250+-0.8330
[1] -1.6412
> exp(-1.6412)/(1+exp(-1.6412))
[1] 0.1623018
> 0.1623018*100
[1] 16.23018
> (0.1937474)/(0.1937474+1)
[1] 0.1623018
> m1

Call:  glm(formula = Education ~ AgeCat, family = "binomial")

Coefficients:
  (Intercept)      AgeCatB      AgeCatC      AgeCatD  
1.4903      -0.4179      -1.4641      -2.3180  

Degrees of Freedom: 1606 Total (i.e. Null);  1603 Residual
Null Deviance:	    2137 
Residual Deviance: 1924 	AIC: 1932
> m2

Call:  glm(formula = UseContraceptive ~ Education, family = "binomial")

Coefficients:
  (Intercept)   Education1  
-0.71772     -0.09249  

Degrees of Freedom: 1606 Total (i.e. Null);  1605 Residual
Null Deviance:	    2004 
Residual Deviance: 2003 	AIC: 2007
> 5) is a simpler model avalable?
  Error: unexpected ')' in "5)"
>   
  >   AIC(m2,m4)
df      AIC
m2  2 2006.990
m4  6 1879.838
> 5) is a simpler model avalable?
  Error: unexpected ')' in "5)"
>   
  > a simpler model may be does the woman want to have more children?
  Error: unexpected symbol in "a simpler"
> m5<-glm(UseContraceptive~WantsMore,family = "binomial")
> anova(m4,m5,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat + WantsMore
Model 2: UseContraceptive ~ WantsMore
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1601     1867.8                          
2      1605     1912.0 -4  -44.181 5.884e-09 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> anova(m5,m4,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ WantsMore
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1605     1912.0                          
2      1601     1867.8  4   44.181 5.884e-09 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> AIC(m5,m4)
df      AIC
m5  2 1916.019
m4  6 1879.838

> exp(-0.18636+-1.04863)
[1] 0.2908377
> (0.2908377)/(0.2908377+1)
[1] 0.2253093
# using m5 the P of someone who wants more kids of using contrception is ~23% wich is greater then the 
#whole model.

Crude Rates of Model 4, Contraceptives~Education+AgeCat+WantsMore
summary(m4)
y= -1.13+0.32*EDC1+(0.39*catB)+(0.91*catC)+(1.19*catD)+(-0.83*WantsMore1)

EDC=0,WM=0                EDC=1, WM=0             EDC=0,WM=1            EDC=1,WM=1
exp(-1.13)                exp(-1.13+0.32)         exp(-1.13+-0.83)      exp(-1.13+0.32+-0.83) 
 #CatA, 0.32                0.44                       0.14                     0.19             
exp(-1.13+0.39)           exp(-1.13+0.39+0.32)    exp(-1.13+0.39-0.83)  exp(-1.13+0.39+0.32-0.83) 
#CatB, 0.48                 0.66                       0.21                     0.29
exp(-1.13+0.91)           exp(-1.13+0.91+0.32)    exp(-1.13+0.91-0.83)   exp(-1.13+0.91+0.32-0.83) 
#CatC, 0.80                 1.10                       0.35                     0.48
exp(-1.13+1.19)           exp(-1.13+1.19+0.32)    exp(-1.13+1.19-0.83)   exp(-1.13+1.19+0.32-0.83) 
#CatD,1.06                  1.46                       0.46                     0.64
## j) strata specific odds ratios, EFFECT MODIFICATION
m4
You could invesitgate the interaction of AgeCAT and desire 4 children on children on education.


Education:wants more
m6<- glm(UseContraceptive~Education+AgeCat+WantsMore+Education:WantsMore, family = "binomial")
summary(m6)
Call:
  glm(formula = UseContraceptive ~ Education + AgeCat + WantsMore + 
        Education:WantsMore)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.5433  -0.3486  -0.1737   0.5176   0.9405  

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)            0.307577   0.038247   8.042 1.71e-15 ***
  Education1            -0.002235   0.036544  -0.061   0.9512    
AgeCatB                0.067518   0.031673   2.132   0.0332 *  
  AgeCatC                0.174856   0.030611   5.712 1.33e-08 ***
  AgeCatD                0.235767   0.042808   5.507 4.23e-08 ***
  WantsMore1            -0.248069   0.036972  -6.710 2.70e-11 ***
  Education1:WantsMore1  0.116448   0.046886   2.484   '0.0131 *'  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.1981133)

Null deviance: 347.04  on 1606  degrees of freedom
Residual deviance: 316.98  on 1600  degrees of freedom
AIC: 1967.9

Number of Fisher Scoring iterations: 2

anova(m4,m6,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat + WantsMore
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore + Education:WantsMore
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1601    1867.84                          
2      1600     316.98  1   1550.9 < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

AIC(m4,m6)
EDC=0, Kids=0
> AIC(m4,m6)
df      AIC
m4  6 1879.838
m6  8 1967.855

#AgeCatA
y=0.31
exp(0.31)
1.36
#AgeCatB
y=0.31+0.067
exp(0.377)
1.45
#AgeCatC
y=0.31+0.17
exp(0.48)
#AgeCatD
y=0.31+0.23
exp(0.54)
1.72

EDC=0, Kids=1
#AgeCatA
y=0.31+(-0.25)
exp(0.06)
1.06
#AgeCatB
y=0.31+0.067+(-0.25)
exp(0.127)
1.13
#AgeCatC
y=0.31+0.17+(-0.25)
exp(0.23)
1.25
#AgeCatD
y=0.31+0.23+(-0.25)
exp(0.29)
1.34

EDC=1, Kids=0
#AgeCatA
y=0.31+(-0.002)
exp(0.308)
1.36
#AgeCatB
y=0.31+0.067+(-0.002)
exp(0.375)
1.45
#AgeCatC
y=0.31+0.17+(-0.002)
exp(0.478)
1.61
#AgeCatD
y=0.31+0.23+(-0.002)
exp(0.538)
1.71

EDC=1, Kids=1
#AgeCatA
y=0.31+(-0.136)
exp(0.174)
1.19
#AgeCatB
y=0.31+0.067+(-0.136)
exp(0.241)
1.27
#AgeCatC
y=0.31+0.17+(-0.136)
exp(0.344)
1.41
#AgeCatD
y=0.31+0.23+(-0.136)
exp(0.404)
1.49

#Education:AgeCat
m7<- glm(UseContraceptive~Education+AgeCat+WantsMore+Education:AgeCat, family = "binomial")
summary(m7)
Call:
  glm(formula = UseContraceptive ~ Education + AgeCat + WantsMore + 
        Education:AgeCat)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.7091  -0.3165  -0.1831   0.5061   0.8970  

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)         0.28037    0.05564   5.039 5.22e-07 ***
  Education1          0.05554    0.05766   0.963  0.33554    
AgeCatB             0.08010    0.06812   1.176  0.23985    
AgeCatC             0.17898    0.05858   3.055  '0.00229 **' 
  AgeCatD             0.17351    0.06580   2.637  0.00844 ** 
  WantsMore1         -0.17740    0.02422  -7.326 3.75e-13 ***
  Education1:AgeCatB -0.02372    0.07685  -0.309  0.75759    
Education1:AgeCatC -0.02104    0.06797  -0.310  0.75696    
Education1:AgeCatD  0.19971    0.09027   2.212  '0.02709 *'  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.1980512)

Null deviance: 347.04  on 1606  degrees of freedom
Residual deviance: 316.49  on 1598  degrees of freedom
AIC: 1969.3

Number of Fisher Scoring iterations: 2
anova(m4,m7,test="LRT")
AIC(m4,m7)

anova(m7,m6,test="LRT")

# Education:Wantsmore,Education:AgeCat
m8<-glm(UseContraceptive~Education+AgeCat+WantsMore+ Education:WantsMore+Education:AgeCat, family = "binomial")
summary(m8)
Call:
  glm(formula = UseContraceptive ~ Education + AgeCat + WantsMore + 
        Education:WantsMore + Education:AgeCat)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.6919  -0.3464  -0.1703   0.4954   0.9151  

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)            0.356622   0.060353   5.909 4.20e-09 ***
  Education1            -0.072589   0.069989  -1.037  0.29982    
AgeCatB                0.071627   0.067976   1.054  0.29217    
AgeCatC                0.148027   0.059204   2.500  0.01251 *  
  AgeCatD                0.125910   0.067262   1.872  0.06140 .  
WantsMore1            -0.271753   0.038039  -7.144 '1.37e-12 ***'
  Education1:WantsMore1  0.158016   0.049228   3.210  '0.00135 **' 
  Education1:AgeCatB    -0.007584   0.076790  -0.099  0.92134    
Education1:AgeCatC     0.028110   0.069482   0.405  0.68585    
Education1:AgeCatD     0.281918   0.093583   3.012  '0.00263 **' 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.1969048)

Null deviance: 347.04  on 1606  degrees of freedom
Residual deviance: 314.46  on 1597  degrees of freedom
AIC: 1961

Number of Fisher Scoring iterations: 2

#Use of LRT to evaluate models m4,m6,m7,m8
m4= no EM
m6= EDC:WM
m7= EDC:AgeCat
m8= EDC:WM and AgeCat

anova(m4,m6,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat + WantsMore
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore + Education:WantsMore
Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1      1601    1867.84                          
2      1600     316.98  1   1550.9 < '2.2e-16 ***'
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(m6,m7,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat + WantsMore + Education:WantsMore
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore + Education:AgeCat
Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1      1600     316.98                     
2      1598     316.49  2  0.49553   '0.2862'

anova(m6,m8,test="LRT")
Analysis of Deviance Table

Model 1: UseContraceptive ~ Education + AgeCat + WantsMore + Education:WantsMore
Model 2: UseContraceptive ~ Education + AgeCat + WantsMore + Education:WantsMore + 
  Education:AgeCat
Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
1      1600     316.98                        
2      1597     314.46  3   2.5243 '0.005043 **'
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

AIC(m6,m8)
df      AIC
m6  8 1967.855
m8 11 '1961.006'

AIC(m4,m8)
anova(m7,m8,test="LRT")
calculate rates for the model m8:
y=0.36+(-0.07*EDC=1)+(0.07*catB)+(0.15*catC)+(0.13*catD)+(-0.27*WM=1)+(0.15*EDC=1:WM=1)+(-0.0076*EDC=1:catB)
+(0.028*EDC=1:catC)+(0.28*EDC=1:catD)

EDC=0, WM=0
#AgeCat A
y=0.36
exp(0.36)
1.43
#AgeCat B
y=0.36+0.07
exp(0.43)
1.54
#AgeCat C
y=0.36+(0.15)
exp(0.51)
1.66
#AgeCat D
y=0.36+(0.13)
exp(0.49)

EDC=1, WM=0
#AgeCat A
y=0.36+(-0.07)
exp(0.29)
1.34
#AgeCat B
y=0.36+0.07+(-0.07)
exp(0.36)
1.43
#AgeCat C
y=0.36+(0.15)+(-0.07)
exp(0.44)
1.55
#AgeCat D
y=0.36+(0.13)+(-0.07)
exp(0.42)
1.52

EDC=0, WM=1
#AgeCat A
y=0.36+(-0.27)
exp(0.09)
1.09
#AgeCat B
y=0.36+0.07+(-0.27)
exp(0.16)
1.17
#AgeCat C
y=0.36+(0.15)+(-0.27)
exp(0.24)
1.27
#AgeCat D
y=0.36+(0.13)+(-0.27)
exp(0.22)
1.25

y=0.36+(-0.07*EDC=1)+(0.07*catB)+(0.15*catC)+(0.13*catD)+(-0.27*WM=1)+(0.15*EDC=1:WM=1)+(-0.0076*EDC=1:catB)
+(0.028*EDC=1:catC)+(0.28*EDC=1:catD)


EDC=1, WM=1
#AgeCat A
y=0.36+(-0.07*EDC=1)+(-0.27*WM=1)+(0.15*EDC=1:WM=1)
y=0.36+(-0.07)+(-0.27)+(0.15)
exp(0.17)
1.18
#AgeCat B
y=0.36+0.07+(-0.07)+(-0.27)+(0.15)+(-0.0076)
exp(0.23)
1.26
#AgeCat C
y=0.36+(-0.07)+(0.15)+(-0.27)+(0.15)+(0.028)
exp(0.348)
1.42
#AgeCat D
y=0.36+(-0.07)+(0.13)+(-0.27)+(0.15)+(0.28)
exp(0.58)
1.79

Age Cat	EDC=0, WM=0	EDC=1, WM=0	EDC=0, WM=1	EDC=1, WM=1
A	1.43	1.34	1.09	1.18
B	1.54	1.43	1.17	1.26
C	1.66	1.55	1.27	1.42
D	1.63	1.52	1.25	1.79



Variable Units Coefficient         CI.95     p-value 
(Intercept)                         0.36   (0.24,0.47)     < 1e-04 
Education(0): WantsMore(1 vs 0)             -0.27 (-0.35,-0.20)     < 1e-04 
Education(1): WantsMore(1 vs 0)             -0.11 (-0.18,-0.05)   0.0002814 
WantsMore(0): Education(1 vs 0)             -0.07  (-0.21,0.06)   0.2998228 
WantsMore(1): Education(1 vs 0)              0.09  (-0.03,0.20)   0.1426201 
Education(0): AgeCat(B vs A)              0.07  (-0.06,0.20)   0.2921694 
Education(0): AgeCat(C vs A)              0.15   (0.03,0.26)   0.0125083 
Education(0): AgeCat(D vs A)              0.13  (-0.01,0.26)   0.0613987 
Education(1): AgeCat(B vs A)              0.06  (-0.01,0.13)   0.0731884 
Education(1): AgeCat(C vs A)              0.18   (0.10,0.25)     < 1e-04 
Education(1): AgeCat(D vs A)              0.41   (0.28,0.54)     < 1e-04 
AgeCat(A): Education(1 vs 0)             -0.07  (-0.21,0.06)   0.2998228 
AgeCat(B): Education(1 vs 0)             -0.08  (-0.20,0.04)   0.1926219 
AgeCat(C): Education(1 vs 0)             -0.04  (-0.13,0.04)   0.3069229 
AgeCat(D): Education(1 vs 0)              0.21   (0.07,0.35)   0.0031217 