#1.Step: Insert Fighter Data.
#Add the number of significant strikes
#Add the fight duration
#Talbott vs Lima

#Duration in Minutes

fighter1=data.frame(sig_strikes=c(49,13,79,28,145),
                    duration=c(15,6,6,11,15 )
)

#Duration in Minutes

fighter2=data.frame(sig_strikes=c(55,37),
                    duration=c(15,12)
)

#New Parameter Sig_Strikes_Per_Minutes
fighter1$STR_PM=fighter1$sig_strikes/fighter1$duration
fighter2$STR_PM=fighter2$sig_strikes/fighter2$duration

head(fighter1)

#Gamma Poisson Code
update_gamma=function(prior_alpha, prior_beta, total_strikes_PM, fights) {
  # Calculate posterior parameters
  posterior_alpha <- prior_alpha + total_strikes_PM
  posterior_beta <- prior_beta + fights
  return(list(alpha = posterior_alpha, beta = posterior_beta))
}


#Observed Data for fighter1
fighter1_sig_PM=sum(fighter1$STR_PM)
fighter2_sig_PM=sum(fighter2$STR_PM)
#Observed Data for fighter2
fights_fighter1 = nrow(fighter1)
fights_fighter2 = nrow(fighter2)

#Insert Parameters(shape,rate) for fighter1
#Uninformed
shape1=1 
rate1=1  



#Insert Parameters(shape,rate) for fighter2
#Uninformed
shape2=1
rate2= 1


#Updated distributions for fighter1
posterior_fighter1=update_gamma(shape1,rate1,fighter1_sig_PM,fights_fighter1)
cat('Prior:Alpha=',posterior_fighter1$alpha,
    "Beta=",posterior_fighter1$beta,"\n")
#Updated distributions for fighter2
posterior_fighter2=update_gamma(shape2,rate2,fighter2_sig_PM,fights_fighter2)
cat('Prior:Alpha=',posterior_fighter2$alpha,
    "Beta=",posterior_fighter2$beta,"\n")


expected_duration <- function(x, y) {
  over2_5 <- x
  under2_5 <- y
  margin <- 1/over2_5 + 1/under2_5
  fair_over <- (1/over2_5)/margin
  fair_under <- (1/under2_5)/margin
  first_round <- fair_under/2
  second_round <- fair_under/2
  third_round <- fair_over
  
  # Calculating expected number of rounds
  p <- first_round + 2*second_round + 3*third_round
  return(p*5)
}


t=expected_duration(1.40,2.85)
t




#Simulate Lambda from the posterior Gamma Distribution
#Fighter1
lambda_fighter1=rgamma(1000,posterior_fighter1$alpha,posterior_fighter2$beta)
print(lambda_fighter1)
#simulate number of strikes_Per_minute in future fights
future_strikes_f1=rpois(1000,lambda_fighter1*t)
#Create a df for ggplot
future_strikes_fighter1=data.frame(sig_strikes=future_strikes_f1)
#Plot the histogram
library(ggplot2)
ggplot(future_strikes_fighter1,aes(x=sig_strikes))+geom_histogram(binwidth=1,
                                                                  fill="orange",
                                                                  color="black",   
                                                                  alpha=0.7)
mean(future_strikes_f1)
median(future_strikes_f1)     


#Fighter2
lambda_fighter2=rgamma(1000,posterior_fighter2$alpha,posterior_fighter2$beta)
#simulate number of strikes in future fights
future_strikes_f2=rpois(1000,lambda_fighter2*t)
#Create a df for ggplot
future_strikes_fighter2=data.frame(sig_strikes=future_strikes_f2)
#Plot the histogram
library(ggplot2)
ggplot(future_strikes_fighter2,aes(x=sig_strikes))+geom_histogram(binwidth=1,
                                                                  fill="cadetblue",
                                                                  color="black",
                                                                  alpha=0.7)





#Plot for both distributions
# Combine your datasets with a grouping variable
combined_data <- rbind(
  data.frame(sig_strikes = future_strikes_fighter1$sig_strikes, fighter = "Fighter 1"),
  data.frame(sig_strikes = future_strikes_fighter2$sig_strikes, fighter = "Fighter 2")
)

# Create overlapping histograms
ggplot(combined_data, aes(x = sig_strikes, fill = fighter)) +
  geom_histogram(binwidth = 1, 
                 color = "black", 
                 alpha = 0.7, 
                 position = "identity") +
  scale_fill_manual(values = c("Fighter 1" = "orange", "Fighter 2" = "cadetblue")) +
  labs(title = "Distribution of Significant Strikes by Fighter",
       x = "Significant Strikes",
       y = "Frequency")


#Credible intervals for fighter1
credible_interval_gamma1=quantile(lambda_fighter1,probs=c(0.025,0.975))
print(credible_interval_gamma1)
mean_credible_gamma1=mean(lambda_fighter1)
print(mean_credible_gamma1)

#Credible intervals for fighter2
credible_interval_gamma2=quantile(lambda_fighter2,probs=c(0.025,0.975))
print(credible_interval_gamma2)
mean_credible_gamma2=mean(lambda_fighter2)
print(mean_credible_gamma2)

#Pricing Markets
#Odds for Fighter1
median(future_strikes_fighter1$sig_strikes)
mean(future_strikes_fighter1$sig_strikes)
mean(future_strikes_fighter1$sig_strikes>20)
mean(future_strikes_fighter1$sig_strikes>40)
mean(future_strikes_fighter1$sig_strikes>50)
mean(future_strikes_fighter1$sig_strikes>60)
mean(future_strikes_fighter1$sig_strikes>80)


#Odds for Fighter2
median(future_strikes_fighter2$sig_strikes)
mean(future_strikes_fighter2$sig_strikes)
mean(future_strikes_fighter2$sig_strikes>20)
mean(future_strikes_fighter2$sig_strikes>40)
mean(future_strikes_fighter2$sig_strikes>50)
mean(future_strikes_fighter2$sig_strikes>60)
mean(future_strikes_fighter2$sig_strikes>80)

#Odds for "Most Significant Strikes
Winner_fighter1=mean(future_strikes_fighter2$sig_strikes>future_strikes_fighter1$sig_strikes)
Winner_fighter2=mean(future_strikes_fighter1$sig_strikes>future_strikes_fighter2$sig_strikes)
Draw=mean(future_strikes_fighter2$sig_strikes==future_strikes_fighter1$sig_strikes)
#Should be 1
Winner_fighter2+Winner_fighter1+Draw
