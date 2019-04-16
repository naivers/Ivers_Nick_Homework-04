###################################################################################################

                                        #HOMEWORK 04#

###################################################################################################



Kamilar_Cooper <- read.csv("~/Desktop/Development/Assignment_4/Kamilar_Cooper.csv")
View(Kamilar_Cooper)

KC <- Kamilar_Cooper

#Remove any Nas 
KC <- na.omit(KC)

#Run a basic linear model to see the initial pattern
plot(data = KC, log(HomeRange_km2) ~ log(Body_mass_female_mean))

#Run a linear model of the interaction
m1 <- lm(log(HomeRange_km2) ~ log(Body_mass_female_mean), data = KC)
m1
#model output: Beta0(Intercept) = -9.354
            # Beta 1 = 1.024
print(coef(summary(m1))[,"Std. Error"])
#Standard Error from m1 linear model:
    #Intercept = 1.6380707    
    #log(Body_Mass_Female) = 0.1868741

confint(m1)
#OUTPUT:
#                                 2.5 %    97.5 %
#  (Intercept)                -12.7511475 -5.956846
#log(Body_mass_female_mean)   0.6364542  1.411560

#QUestion 2  Bootstrapping



#from https://www.rdocumentation.org/packages/simpleboot/versions/1.1-7/topics/lm.boot
#library(simpleboot)
#lm.object <- lm(log(HomeRange_km2) ~ log(Body_mass_female_mean), data = KC)
#R <- 1000
#m2 <- lm.boot(lm.object, R, rows = TRUE)

#from https://rdrr.io/cran/car/man/Boot.html
library(car)
data=na.omit(KC)
m2 <- Boot(m1, R=1000, method=c("case"))
summary(m2)
#Output
#Number of bootstrap replications R = 999 
#                           original  bootBias  bootSE bootMed
#(Intercept)                  -9.354 -0.319302 1.73792 -9.5455
#log(Body_mass_female_mean)    1.024  0.036161 0.19585  1.0460

confint(m2)
#Output
#Bootstrap bca confidence intervals
#                                 2.5 %    97.5 %
#  (Intercept)                -12.3244897 -5.921240
#log(Body_mass_female_mean)   0.6406987  1.352249

hist(m2)



#### Question 3 #####

#Estimate the standard error for each of your β coefficients as the standard deviation of the sampling distribution from your bootstrap.

summary(m2)

#coef(summary(m2))[,"bootSE"]
#coef(summary(m2))

# I want to pull out the results for each iteration of the bootstrap. 
#Should be a matrix with 1000 rows (1000 iterations), and 2 columns, one for each B coefficient
m2_rep_results <- m2$t
m2_rep_results <- as.data.frame(m2_rep_results)

# Now calculate the Standard Error from the sampling distribution
#m2_rep_se <- sd(m2_rep_results$Intercept)/sqrt(length(m2_rep_results$Intercept))

#m2_Intercept_se <- sd(m2_rep_results[, 1])/sqrt(length(m2_rep_results[, 1]))
#print(m2_rep_se)
#SE for the "Intercept" sampling distribution is 0.05498

#m2_bodymass_se <- sd(m2_rep_results[, 2])/sqrt(length(m2_rep_results[, 2]))
#print(m2_bodymass_se)
#SE for log(Body_Mass_Female) sampling distribution is 0.05498529




sd_m2_intercept <- sd(m2_rep_results$`(Intercept)`)

sd_m2_bodymass <- sd(m2_rep_results$`log(Body_mass_female_mean)`)

se_m2_intercept <- sd_m2_intercept/sqrt(length(m2_rep_results))
print(se_m2_intercept)
#Output:  1.150876

se_m2_bodymass <- sd_m2_bodymass/sqrt(length(m2_rep_results))
print(se_m2_bodymass)
#Output: 0.1289406


#### Question 4 ######

# Also determine the 95% CI for each of your β coefficients based on the appropriate quantiles from your sampling distribution.


# 'boot' R package should have already generated the confidence interval. I just need to call it.
library(boot)
boot.ci(m2, conf=0.95, type="basic")
#95% CI = (-12.251, -6.010)
boot.ci(m2, conf=0.95, type="bca")
#95% CI = (-12.647,  -6.336 ) 




