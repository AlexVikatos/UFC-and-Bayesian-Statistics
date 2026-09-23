# 1. Step: Insert Fighter Data
fighter1 = data.frame(sig_strikes = c(49, 13, 79, 28, 145),
                      duration = c(15, 6, 6, 11, 15))

fighter2 = data.frame(sig_strikes = c(55, 37),
                      duration = c(15, 12))


# ----Weighted Poisson Update Section ----
# Total strikes and total duration (exposure)
total_strikes_f1 = sum(fighter1$sig_strikes)
total_strikes_f2 = sum(fighter2$sig_strikes)

total_time_f1 = sum(fighter1$duration)
total_time_f2 = sum(fighter2$duration)

# Gamma-Poisson conjugate update using exposure
update_gamma = function(prior_alpha, prior_beta, total_strikes, total_time) {
  posterior_alpha <- prior_alpha + total_strikes
  posterior_beta  <- prior_beta + total_time
  return(list(alpha = posterior_alpha, beta = posterior_beta))
}

# Uninformed priors
shape1 = 1; rate1 = 1
shape2 = 1; rate2 = 1

# Posterior updates using duration (not number of fights)
posterior_fighter1 = update_gamma(shape1, rate1, total_strikes_f1, total_time_f1)
posterior_fighter2 = update_gamma(shape2, rate2, total_strikes_f2, total_time_f2)

cat('Posterior for Fighter 1: Alpha =', posterior_fighter1$alpha, "Beta =", posterior_fighter1$beta, "\n")
cat('Posterior for Fighter 2: Alpha =', posterior_fighter2$alpha, "Beta =", posterior_fighter2$beta, "\n")

# ---- Keep the rest of your original code as-is ----

# Expected fight duration from odds
expected_duration <- function(x, y) {
  over2_5 <- x
  under2_5 <- y
  margin <- 1/over2_5 + 1/under2_5
  fair_over <- (1/over2_5)/margin
  fair_under <- (1/under2_5)/margin
  first_round <- fair_under/2
  second_round <- fair_under/2
  third_round <- fair_over
  p <- first_round + 2*second_round + 3*third_round
  return(p * 5)
}

t = expected_duration(1.40, 2.85)
t

# Simulate Lambda from Posterior Gamma Distributions
lambda_fighter1 = rgamma(1000, posterior_fighter1$alpha, posterior_fighter1$beta)
lambda_fighter2 = rgamma(1000, posterior_fighter2$alpha, posterior_fighter2$beta)

# Simulate future strikes
future_strikes_f1 = rpois(1000, lambda_fighter1 * t)
future_strikes_f2 = rpois(1000, lambda_fighter2 * t)

# DataFrames for plotting
future_strikes_fighter1 = data.frame(sig_strikes = future_strikes_f1)
future_strikes_fighter2 = data.frame(sig_strikes = future_strikes_f2)

# Plot Fighter 1
library(ggplot2)
ggplot(future_strikes_fighter1, aes(x = sig_strikes)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7)

mean(future_strikes_f1)
median(future_strikes_f1)

# Plot Fighter 2
ggplot(future_strikes_fighter2, aes(x = sig_strikes)) +
  geom_histogram(binwidth = 1, fill = "cadetblue", color = "black", alpha = 0.7)

# Combined plot
combined_data <- rbind(
  data.frame(sig_strikes = future_strikes_fighter1$sig_strikes, fighter = "Fighter 1"),
  data.frame(sig_strikes = future_strikes_fighter2$sig_strikes, fighter = "Fighter 2")
)

ggplot(combined_data, aes(x = sig_strikes, fill = fighter)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Fighter 1" = "orange", "Fighter 2" = "cadetblue")) +
  labs(title = "Distribution of Significant Strikes by Fighter",
       x = "Significant Strikes",
       y = "Frequency")

# Credible intervals
credible_interval_gamma1 = quantile(lambda_fighter1, probs = c(0.025, 0.975))
credible_interval_gamma2 = quantile(lambda_fighter2, probs = c(0.025, 0.975))
mean_credible_gamma1 = mean(lambda_fighter1)
mean_credible_gamma2 = mean(lambda_fighter2)

print(credible_interval_gamma1)
print(credible_interval_gamma2)
print(mean_credible_gamma1)
print(mean_credible_gamma2)

# Pricing markets
# Fighter 1
median(future_strikes_fighter1$sig_strikes)
mean(future_strikes_fighter1$sig_strikes)
mean(future_strikes_fighter1$sig_strikes > 25)
mean(future_strikes_fighter1$sig_strikes > 50)
mean(future_strikes_fighter1$sig_strikes > 75)
mean(future_strikes_fighter1$sig_strikes > 100)
mean(future_strikes_fighter1$sig_strikes > 125)

# Fighter 2
median(future_strikes_fighter2$sig_strikes)
mean(future_strikes_fighter2$sig_strikes)
mean(future_strikes_fighter2$sig_strikes > 25)
mean(future_strikes_fighter2$sig_strikes > 50)
mean(future_strikes_fighter2$sig_strikes > 75)
mean(future_strikes_fighter2$sig_strikes > 100)
mean(future_strikes_fighter2$sig_strikes > 125)


# Head-to-head market
Winner_fighter1 = mean(future_strikes_fighter2$sig_strikes > future_strikes_fighter1$sig_strikes)
Winner_fighter2 = mean(future_strikes_fighter1$sig_strikes > future_strikes_fighter2$sig_strikes)
Draw = mean(future_strikes_fighter2$sig_strikes == future_strikes_fighter1$sig_strikes)
Winner_fighter1 + Winner_fighter2 + Draw  # Should equal 1

