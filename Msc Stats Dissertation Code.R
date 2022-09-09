########### STEP 1: Parameter Estimation ###########

## Aiming to describe a model with the correct marginal distributions to generate data with
# the correct distribution parameters as described in the MODULATE trial protocol.

## Binary endpoint parameters:
# Trial powered to detect an absolute difference of 17%:
# Control group success probability of 33%:
true_prob_con <- 0.33
# Intervention group success probability of 50%:
true_prob_int <- 0.50


## Continuous endpoint parameters:
# Trial powered to detect a 16-point difference.
# Assumed standard deviation of 32.
y_con <- rnorm(180, 0, 32)
y_int <- rnorm(180, 16, 32)


## Binary data generated from the continuous outcomes through logistic regression:
# X: binary responses.
# Y: continuous responses.
# logit(P(X = 1)) = beta0 + beta1 * Y

## Margin of error allowed:
margin <- 0.15


## Function to calculate the success probability for some given normally distribtued data:
Binomial_Probability <- function(betas, Y) {
  
  # probability = logistic(beta0 + beta1 * Y):
  prob <- exp(betas[1] + betas[2] * Y) / (1 + exp(betas[1] + betas[2] * Y))
  
  # return success probability:
  return(mean(prob))
}


## Function to find optimal beta pairs from given constraints:
# Need data simulated from the control and intervention groups.
Beta_Find <- function(df, Y_Control, Y_Intervention) {
  
  # calculate the probability in each arm for the pair of betas:
  prob_control <- apply(df, 1, Binomial_Probability, Y = Y_Control)
  prob_intervention <- apply(df, 1, Binomial_Probability, Y = Y_Intervention)
  
  # determine which beta pairs give required binomial probabilities:
  acceptable_pairs <- which((prob_control > (true_prob_con - margin) & prob_control < (true_prob_con + margin)) 
                      & (prob_intervention > (true_prob_int - margin) & prob_intervention < (true_prob_int + margin)))
                      
  # determine which beta pairs give required acceptable difference:
  acceptable_difference <- which( abs(prob_intervention - prob_control) < 0.19 & (prob_intervention - prob_control) > 0.13)
  
  # acceptable beta pairs stored:
  acceptable_pairs_betas <- df[acceptable_pairs, ]
  
  # acceptable beta differences stored:
  acceptable_difference_betas <- df[acceptable_difference, ]
  
  # For the given grid of betas, do any result in the correct specified probability (within a pre-determined margin of error):
  if (length(acceptable_pairs) == 0) {
    
    print("No pairs of betas give the required probabilites")
    
    
  } else {
    
    print(acceptable_pairs_betas)
  } 
  
  if (length(acceptable_difference) == 0) {
    
    print("No pairs of betas give the correct difference in probabilities.")
    
  } else {
    
    print(acceptable_difference_betas)
  }
  
}




## From graphical investigation, an appropriate range of beta values to search between
# is from -5 to 5.
beta0 <- seq(-5, 5, 0.1)
beta1 <- seq(-5, 5, 0.1)

# Create a dataframe of beta pairs:
df <- cbind(beta0, beta1)

# Run search of betas to find optimal pairs:
Beta_Find(df, y_con, y_int)

## No pairs of betas give the correct probabilities of success in both arms.
# However, beta0 = beta1 = 0.1 does give the correct required difference of 17% between the control and intervention.
# Under this model, the probabilities are:
# Control group success probability of 50%.
# Intervention group success probability of 67%.


## WHY DOES THIS HAPPEN?
# Defining improvement in the continuous outcome as a decrease of 16 points, the beta values of beta0 = -0.1 = beta1
#give the correct probabilities. However, this assumes increasing the probability of success at stage 1 reduces the 
#mean outcome at stage 2 which is the opposite relationship of interest and so a model which gives the correct 
#characteristics as defined in the MODULATE protocol cannot be found.
# Instead, the absolute difference required to be observed of 17% is assumed and beta parameters are estimated at 
#beta0 = 0.1 = beta1 and new assumptions of a success probability of 50% in the control and 67% in the intervention is
#assumed.


# What betas satisfy a 33% control arm and 50% intervention arm under no dependence? (CHECK THIS)
df <- cbind(beta0, rep(0, length(beta0)))
Beta_Find(df, y_con, y_int)

# If the two stages had completely independent endpoints, we would expect beta1 = 0. In this scenario, beta0 values of
#-0.6:-0.1 (in 0.1 incriments) all give the required control value. When there is an effect observed, we would vary the
#value of beta1. Assuming a beta0 value of -0.1, the below plot shows how the intervention and control probabilities behave
#as beta1 varies:



## Vary beta1:
betas1 <- seq(-5, 5, 0.01)

betas1prob_con <- numeric(length(betas1))
betas1prob_int <- numeric(length(betas1))

## Need to define normally distributed data to generate binomial probabilities from.
# Generated under the alternate hypothesis of a 16-point difference:
y_con <- rnorm(1000, 0, 32)
y_int <- rnorm(1000, 16, 32)


## Calculating control and intervention probability as beta 1 varies with beta0 set at -0.1:
for (i in 1:length(betas1)) {
  
  #Calculate the probabilities generated:
  betas1prob_con[i] <- Binomial_Probability(c(-0.6, betas1[i]), y_con)
  betas1prob_int[i] <- Binomial_Probability(c(-0.6, betas1[i]), y_int)
  
}

## Graphs observing how the probabilities behave as the beta parameters are altered:

par(mfrow = c(1, 2))
plot(betas1, betas1prob_con, ylim = c(0.2, 0.9), pch = 20, main = "Beta 1 varying - Control")
abline(h = 0.33, col = "red")
plot(betas1, betas1prob_int, ylim = c(0.2, 0.9), pch = 20, main = "Beta 1 varying - Intervention")





#STUCK!




########### STEP 2: Data Generation ############
beta0 <- 0.1; beta1 <- 0.1

set.seed(1904)
y_con_null <- rnorm(180, 0, 32)
y_int_null <- rnorm(180, 0, 32)
## ... and under the alternate
y_con_alt <- rnorm(180, 0, 32)
y_int_alt <- rnorm(180, 16, 32)


x_con_null <- rbinom(180, 1, exp(beta0 + beta1 * y_con_null) 
                     / (1 + exp(beta0 + beta1 * y_con_null)))
x_int_null <- rbinom(180, 1, exp(beta0 + beta1 * y_int_null) 
                     / (1 + exp(beta0 + beta1 * y_int_null)))
## ... and under the alternate
x_con_alt <- rbinom(180, 1, exp(beta0 + beta1 * y_con_alt) 
                     / (1 + exp(beta0 + beta1 * y_con_alt)))
x_int_alt <- rbinom(180, 1, exp(beta0 + beta1 * y_int_alt) 
                     / (1 + exp(beta0 + beta1 * y_int_alt)))

## Means of all simulated probabilities:
# Under the null:
cat("Stage 1 Control = ", mean(x_con_null), "Stage 1 Intervention = ", mean(x_int_null))
cat("Stage 2 Control = ", mean(y_con_null), "Stage 1 Intervention = ", mean(y_int_null))
# Under the alternative:
cat("Stage 1 Control = ", mean(x_con_alt), "Stage 1 Intervention = ", mean(x_int_alt))
cat("Stage 2 Control = ", mean(y_con_alt), "Stage 1 Intervention = ", mean(y_int_alt))


## All probabilities look good!
# Graphing the continuous observations under the alternate hypothesis:
par(mfrow = c(1, 2))
hist(y_con_alt, ylim = c(0, 50), xlim = c(-100, 100), main = "Distribution of participant continuous outcomes", sub = "Control Arm", breaks = 10)
abline(v = 0, col = "red")
hist(y_int_alt, ylim = c(0, 50), xlim = c(-100, 100), main = "Distribution of participant continuous outcomes", sub = "Intervention Arm")
abline(v = 16, col = "red")

# Continuous outcomes in the control given the binomial data:
# Sort scales
hist(y_con_alt[x_con_alt == 0], xlim = c(-100, 100), ylim = c(0, 25), main = "control arm - failure")
abline(v = mean(y_con_alt[x_con_alt == 0]), col = "red")
hist(y_con_alt[x_con_alt == 1], xlim = c(-100, 100), ylim = c(0, 25), breaks = 10, main = "control arm - success")
abline(v = mean(y_con_alt[x_con_alt == 1]), col = "red")


hist(y_int_alt[x_int_alt == 0], xlim = c(-100, 100), ylim = c(0, 25), breaks = 10, main = "intervention arm - failure")
abline(v = mean(y_int_alt[x_int_alt == 0]), col = "red")
hist(y_int_alt[x_int_alt == 1], xlim = c(-100, 100), ylim = c(0, 25), breaks = 10, main = "intervention arm - success")
abline(v = mean(y_int_alt[x_int_alt == 1]), col = "red")

mean(y_con_alt[x_con_alt == 1]) 
mean(y_con_alt[x_con_alt == 0])
mean(y_int_alt[x_int_alt == 1])
mean(y_int_alt[x_int_alt == 0])


## Data frame for ggplot2:
datagenFULL <- data.frame(y_con_null, y_con_alt, y_int_null, y_int_alt, x_con_null, x_int_null, x_con_alt, x_int_alt)

par(mfrow = c(2, 1))
#control_alt <- 
ggplot(datagenFULL, aes(x = y_con_alt)) + 
  geom_histogram(binwidth = 9, fill = rgb(0, 0.35, 0), colour = "grey") +
  labs(x = "Quality of Life", y = "Frequency", ) +
  ggtitle("Stage 2 Outcomes - Control Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 30) +
  geom_vline(xintercept = mean(y_con_alt), col = rgb(1, 0.18, 0.48), size = 1) +
  #geom_label(aes(label = round(mean(y_con_alt))), x = mean(y_con_alt), y = 28, hjust = 1)
  geom_text(aes(mean(y_con_alt), 28, label = round(mean(y_con_alt), 2), hjust = 1.2))

#intervention_alt <- 
ggplot(datagenFULL, aes(x = y_int_alt)) + 
  geom_histogram(binwidth = 9, fill = rgb(0, 0.35, 0), colour = "white") +
  labs(x = "Quality of Life", y = "Frequency", ) +
  ggtitle("Stage 2 Outcomes - Intervention Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 30) +
  geom_vline(xintercept = mean(y_int_alt), col = rgb(1, 0.18, 0.48), size = 1) +
  geom_text(aes(mean(y_int_alt), 28, label = 16.57, hjust = 1.2))


##
ggplot(datagenFULL, aes(x = y_con_alt[x_con_alt == 0])) + 
  geom_histogram(binwidth = 9, fill = rgb(0, 0.35, 0), colour = "white") +
  labs(x = "Quality of Life", y = "Frequency", ) +
  ggtitle("Stage 2 Outcomes - Intervention Group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 30) +
  geom_vline(xintercept = mean(y_int_alt), col = rgb(1, 0.18, 0.48), size = 1) +
  geom_text(aes(mean(y_int_alt), 28, label = 16.57, hjust = 1.2))


########### STEP 3: Simple Trial Example ###########

## Simulating a simple, parallel, two-arm RCT with an assumed joint standard deviation.


## Function that results if a trial rejected the null hypothesis:
Go <- function(n1, n2, mu_con, mu_int, sigma, alpha) {
  
  # generate observations for the control and intervention arms from the normal disribution:
  y_control <- rnorm(n1, mu_con, sigma)
  y_intervention <- rnorm(n2, mu_int, sigma)
  
  # t test 
  result <- t.test(y_intervention, y_control, alternative = "two.sided")$p.value < alpha
  
  # result = TRUE if the null is rejected:
  return(result)
}

Go(63, 63, 0, 16, 32, 0.05)

## Calculating operating characteristics for a given trial:
Errors <- function(n1, n2, mu_null, mu_alt, sigma, alpha) {
  
  # Type I error rate: percentage of trials rejecting the null when simulated under the null:
  TypeI_error_rate <- mean(replicate(10000, Go(n1, n2, mu_null, mu_null, sigma, alpha)))
  
  # Power: percentage of trials rejecting the null when simulated under the alternate:
  Power <- mean(replicate(10000,  Go(n1, n2, mu_null, mu_alt, sigma, alpha)))
  
  # Type II error rate: percentage of trials accepting the null when simulated under the alternate ...
  # ... = 1 - Power
  TypeII_error_rate <- 1 - Power
  
  return(c(TypeI_error_rate, TypeII_error_rate, Power))

}


## Operating characteristics for single stage trial with continuous outcome as
# described in MODULATE:
Errors(90, 90, 0, 16, 32, 0.05)

# 63 per arm 80% power.
# 84 per arm 90% power.





## Plotting characteristics:
# How do they vary with sample size (equal allocation)?
samplesize <- seq(10, 200, 5)   #per arm

# Power for varying sample sizes:
powers <- numeric(length(samplesize))
for (i in 1:length(samplesize)) {
  powers[i] <- mean(replicate(1000, Go(samplesize[i], samplesize[i], 0, 16, 32, 0.05)))
}

# Type I error rate for varying sample size:
alphas <- numeric(length(samplesize))
for (i in 1:length(samplesize)) {
  alphas[i] <- mean(replicate(1000, Go(samplesize[i], samplesize[i], 0, 0, 32, 0.05)))
}




## Plotting (ggplot):
data <- data.frame(samplesize, alphas, powers)

ggplot(data, aes(x = samplesize, y = alphas)) +
  geom_point(y = alphas, col = rgb(0, 0.35, 0)) +
  ylim(0, 1) +
  labs(x = "Sample Size", y = "Type I Error Rate") +
  ggtitle("Sample Size v.s. Type I Error Rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 0, xend = 200, y = 0.05, yend = 0.05), size = 0.1, linetype = "dashed") +
  geom_text(aes(0, 0.05), label = 0.05, hjust = 1.2)

ggplot(data, aes(x = samplesize, y = powers)) +
  geom_point(y = powers, col = rgb(0, 0.35, 0)) +
  geom_smooth(aes(y = powers), col = rgb(0, 0.35, 0), se = FALSE, alpha = 0.01, size = 1) +
  ylim(0, 1) +
  labs(x = "Sample Size", y = "Power") +
  ggtitle(" Sample Size v.s. Power") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_segment(aes(x = 90, xend = 90, y = 0, yend = 0.915), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 90, y = 0.915, yend = 0.915), linetype = "dashed") +
  geom_text(aes(90, 0), label = 90, vjust = 1.3) +
  geom_text(aes(0, 0.915), label = 0.9, hjust = 1.5)

geom_text(aes(mean(y_con_alt), 28, label = round(mean(y_con_alt), 2), hjust = 1.2))


plot(samplesize, powers, pch = 20, col = "seagreen", xlab = "Sample Size", ylab = "Trial Power", ylim = c(0, 1))
#lines(samplesize, powers)
abline(h = 0.80)
plot(samplesize, alphas, ylim = c(0, 0.5), pch = 20, col = "lightblue")
abline(h = 0.05, col = "black")

# How do they vary with allocation ratios?
# For control arm set at 100:

#1:2
powers12 <- numeric(length(samplesize))
for (i in 1:length(samplesize)) {
  powers12[i] <- mean(replicate(1000, Go(samplesize[i], 2 * samplesize[i], 0, 16, 32, 0.05)))
}

#1:3
powers13 <- numeric(length(samplesize))
for (i in 1:length(samplesize)) {
  powers13[i] <- mean(replicate(1000, Go(samplesize[i], 3 * samplesize[i], 0, 16, 32, 0.05)))
}


data2 <- data.frame(samplesize, powers, powers12, powers13)

ggplot(data2, aes(x = samplesize)) +
  geom_point(y = powers, col = "1:1", shape = 20) +
  geom_smooth(aes(y = powers), col = "1:1", size = 0.8, se = FALSE) +
  geom_point(y = powers12, col = "1:2", shape = 2) +
  geom_smooth(aes(y = powers12), col = "1:2", size = 0.8, se = FALSE) +
  geom_point(y = powers13, col = "1:3", shape = 3) +
  geom_smooth(aes(y = powers13), col = "1:3", size = 0.8, se = FALSE) +
  labs(x = "Sample Size", y = "Power") +
  ggtitle("Varied Allocation Ratios: Sample Size v.s. Power") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual("", 
                      values = c("1:1" = rgb(0, 0.35, 0), "1:2" = "blue", "1:3" = "plum3")) +
  xlab(" ")


  
ggplot(data2, aes(x = samplesize)) +
  geom_line(aes(y = powers, col = "1:1")) +
  geom_line(aes(y = powers12, col = "1:2")) +
  geom_line(aes(y = powers13, col = "1:3")) +
  scale_colour_manual("", 
                      values = c("1:1" = rgb(0, 0.35, 0), "1:2" = "blue", "1:3" = "plum3")) +
  xlab(" ") +
  labs(x = "Sample Size", y = "Power") +
  ggtitle("Varied Allocation Ratios: Sample Size v.s. Power") +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  
  

plot(samplesize, powers, pch = 20, col = "seagreen", xlab = "Sample Size", ylab = "Trial Power", ylim = c(0.7, 1))
lines(samplesize, powers, col = "seagreen")

points(samplesize, powers12, pch = "*")
lines(samplesize, powers12, lty = 2)

points(samplesize, powers13, pch = "+")
lines(samplesize, powers13, lty = 2)




### One trial example for values in Section 8.2.2:
#1904 for seed in diss.
set.seed(1)
y_control <- rnorm(90, 0, 32)
y_intervention <- rnorm(90, 0, 32)

sd_control <- sd(y_control)
# 36.05257
sd_intervention <- sd(y_intervention)
# 32.3675

term1 <- sqrt( ((90 - 1) * sd_control^2 + (90 - 1) * sd_intervention^2) / (90 + 90 - 2) )
term2 <- sqrt(1 / 90 + 1 / 90)

se <- term1 * term2
se       #5.11

T <- (abs(mean(y_control) - mean(y_intervention)) - 0) / ( term1 * term2 )
T        #1.01

critval <- qt(1 - 0.05, 90 + 90 - 2)
critval  #1.65

########### STEP 4: Multi-Stage Example ###########

## Simulating a more complex trial that follows the MODULATE trial design.

# Two stage trial with a binary outcome at the end of stage 1 to be assessed and a
#continuous outcome at the end of stage 2.

## Stage 1 analysis calculates the mean number of successes between the groups and 
#compares these to pre-defined futility and efficacy bounds.
# If the difference is lower than the futility bound, the trial is stopped and the
#intervention is dropped.
# If the difference is higher than the efficacy bound, the trial is stopped, the 
#intervention as declared effective and the null hypothesis rejected.
# If the difference is between these boundary values, the trial continues to stage 2.

## If the test proceeds to stage 2, more participants are recruited and so more 
#outcomes are generated.
# A t-test is performed on all continuous outcomes between the two groups.
# The null hypothesis is rejected.


MultiStage_Go <- function(betas, boundaries, Stage1_SampleSize, Stage2_SampleSize, mu_control, mu_intervention, sigma, alpha) {
  
  # Generate normally distributed data for stage 1 participants:
  y_control <- rnorm(Stage1_SampleSize[1], mu_control, sigma)
  y_intervention <- rnorm(Stage1_SampleSize[2], mu_intervention, sigma)
  
  # Generate binomial data for stage 1 participants from desired model:
  x_control <- rbinom(Stage1_SampleSize[2], 1, exp(betas[1] + betas[2] * y_control) 
                      / (1 + exp(betas[1] + betas[2] * y_control)))
  x_intervention <- rbinom(Stage1_SampleSize[2], 1, exp(betas[1] + betas[2] * y_intervention) 
                      / (1 + exp(betas[1] + betas[2] * y_intervention)))
  
  
  ## STAGE 1 ANALYSIS:
  Absolute_Difference <- abs(mean(x_intervention) - mean(x_control))
  
  # Stopping boundaries:
  l1 <- boundaries[1]
  u1 <- boundaries[2]
  
  if (Absolute_Difference < l1) {
    
    # if the difference is lower than the futility boundary, stop the trial and
    #drop the arm:
    result <- FALSE
    
  } else {
    
    if (Absolute_Difference > u1) {
      
      # if the difference is greater than the efficacy boundary, stop the trial
      #and reject the null hypothesis:
      result <- TRUE
      
    } else {
      
      ## STAGE 2 ANALYSIS:
      
      # Need to simulate observations for stage 2 participants:
      y_control2 <- c(y_control, rnorm(Stage2_SampleSize[1], mu_control, sigma))
      y_intervention2 <- c(y_intervention, rnorm(Stage2_SampleSize[2], mu_intervention, sigma))
      
      # t-test:
      result <- t.test(y_intervention2, y_control2, alternative = "two.sided")$p.value < alpha
    }
  }
  
  # result = TRUE if the null is rejected
  return(result)
}

TypeI_Error_Rate <- mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.01, 0.99), c(180, 180), c(40, 40), 0, 0, 32, 0.05)))

Power <- mean(replicate(10000, MultiStage_Go(c(0.1, 0.1), c(0.05, 0.3), c(180, 180), c(40, 40), 0, 16, 32, 0.05)))

TypeII_Error_Rate <- 1 - Power

TypeI_Error_Rate
TypeII_Error_Rate
Power


Errors <- function (betas, boundaries, Stage1_SampleSize, Stage2_SampleSize, mu_null, mu_alternative, sigma, alpha) {
  
  # Type I error rate: percentage of trials rejecting the null when simulated under the null:
  TypeI_error_rate <- mean(replicate(10000, MultiStage_Go(betas, boundaries, Stage1_SampleSize, Stage2_SampleSize, mu_null, mu_null, sigma, alpha)))
  
  # Power: percentage of trials rejecting the null when simulated under the alternate:
  Power <- mean(replicate(10000, MultiStage_Go(betas, boundaries, Stage1_SampleSize, Stage2_SampleSize, mu_null, mu_alternative, sigma, alpha)))
  
  # Type II error rate: percentage of trials accepting the null when simulated under the alternate ...
  # ... = 1 - Power
  TypeII_error_rate <- 1 - Power
  
  return(c(TypeI_error_rate, TypeII_error_rate, Power))
  
}

Errors(c(0.1, 0.1), c(0.05, 0.3), c(50, 50), c(100, 100), 0, 16, 32, 0.05)
Errors(c(0.1, 0.1), c(0.05, 0.25), c(50, 50), c(100, 100), 0, 16, 32, 0.05)






########### STEP 5: Operating Characteristics with Varied Parameters ###########

## Want to calculate Type I and II error for each of the given changes to the design.

## Varying Stage 1 Sample Size
ss_stage1 <- seq(10, 300, 10)

alphas_ss1 <- numeric(length(ss_stage1))
betas_ss1 <- numeric(length(ss_stage1))

for (i in 1:length(ss_stage1)) {
  
  # update vectors to corresponding error rates:
  alphas_ss1[i] <- mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.05, 0.3), c(ss_stage1[i], ss_stage1[i]), c(40, 40), 0, 0, 32, 0.05)))
  betas_ss1[i] <- 1 - mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.05, 0.3), c(ss_stage1[i], ss_stage1[i]), c(40, 40), 0, 16, 32, 0.05)))
  
}

#plot(ss_stage1, alphas_ss1)
#plot(ss_stage1, betas_ss1)


## Varying Stage 2 Sample Size
ss_stage2 <- seq(10, 300, 10)

alphas_ss2 <- numeric(length(ss_stage2))
betas_ss2 <- numeric(length(ss_stage2))

for (i in 1:length(ss_stage2)) {
  
  # update vectors to corresponding error rates:
  alphas_ss2[i] <- mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.05, 0.3), c(50, 50), c(ss_stage2[i], ss_stage2[i]), 0, 0, 32, 0.05)))
  betas_ss2[i] <- 1 - mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.05, 0.3), c(50, 50), c(ss_stage2[i], ss_stage2[i]), 0, 16, 32, 0.05)))
  
}

#plot(ss_stage2, alphas_ss2, ylim = c(0, 0.2))
#plot(ss_stage2, betas_ss2, ylim = c(0, 0.5))

## Varying Futility Boundary
l1 <- seq(0.01, 0.3, 0.005)

alphas_l1 <- numeric(length(l1))
betas_l1 <- numeric(length(l1))

for (i in 1:length(l1)) {
  
  # update vectors to corresponding error rates:
  alphas_l1[i] <- mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(l1[i], 0.99), c(50, 50), c(100, 100), 0, 0, 32, 0.05)))
  betas_l1[i] <- 1 - mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(l1[i], 0.99), c(50, 50), c(100, 100), 0, 16, 32, 0.05)))
  
}

#plot(l1, alphas_l1)
#plot(l1, betas_l1)

## Varying Efficacy Boundary
u1 <- seq(0.01, 0.3, 0.01)

alphas_u1 <- numeric(length(u1))
betas_u1 <- numeric(length(u1))

for (i in 1:length(u1)) {
  
  # update vectors to corresponding error rates:
  alphas_u1[i] <- mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.01, u1[i]), c(50, 50), c(100, 100), 0, 0, 32, 0.05)))
  betas_u1[i] <- 1 - mean(replicate(1000, MultiStage_Go(c(0.1, 0.1), c(0.01, u1[i]), c(50, 50), c(100, 100), 0, 16, 32, 0.05)))
  
}

#plot(l1, alphas_l1)
#plot(l1, betas_l1)

## Combine values to df for ggplots:

#data <- data.frame(ss_stage1, alphas_ss1, betas_ss1, ss_stage2, alphas_ss2, betas_ss2, l1, alphas_l1, betas_l1, u1, alphas_u1, betas_u1)
data_ss1 <- data.frame(ss_stage1, alphas_ss1, betas_ss1)
data_ss2 <- data.frame(ss_stage2, alphas_ss2, betas_ss2)
data_l1 <- data.frame(l1, alphas_l1, betas_l1)
data_u1 <- data.frame(u1, alphas_u1, betas_u1)

## Plotting:
library(ggplot2)

ggplot(data_ss1, aes(x = ss_stage1)) +
  geom_point(aes(y = alphas_ss1), colour = rgb(0, 0.35, 0)) +
  geom_point(aes(y = betas_ss1), colour = "plum3", shape = 3) +
  ylim(0, 0.4) +
  labs(x = "Stage 1 Sample Size", y = "Error Rate") +
  ggtitle("Stage 1 Sample Size v.s. Type I and Type II Error Rate") +
  geom_segment(x = 0, xend = 300, y = 0.05, yend = 0.05, linetype = "dashed") +
  geom_segment(x = 0, xend = 300, y = 0.1, yend = 0.1, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data_ss2, aes(x = ss_stage2)) +
  geom_point(aes(y = alphas_ss2), colour = rgb(0, 0.35, 0)) +
  geom_point(aes(y = betas_ss2), colour = "plum3", shape = 3) +
  ylim(0, 0.4) +
  labs(x = "Stage 2 Sample Size", y = "Error Rate") +
  ggtitle("Stage 2 Sample Size v.s. Type I and Type II Error Rate") +
  geom_segment(x = 0, xend = 300, y = 0.05, yend = 0.05, linetype = "dashed") +
  geom_segment(x = 0, xend = 300, y = 0.1, yend = 0.1, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data_l1, aes(x = l1)) +
  geom_point(aes(y = alphas_l1), colour = rgb(0, 0.35, 0)) +
  geom_point(aes(y = betas_l1), colour = "plum3", shape = 3) +
  labs(x = "Futility Boundary", y = "Error Rate") +
  ylim(0, 0.4) +
  ggtitle("Futility Boundary v.s. Type I and Type II Error Rate") +
  geom_segment(x = 0, xend = 300, y = 0.05, yend = 0.05, linetype = "dashed") +
  geom_segment(x = 0, xend = 300, y = 0.1, yend = 0.1, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data_u1, aes(x = u1)) +
  geom_point(aes(y = alphas_u1), colour = rgb(0, 0.35, 0)) +
  geom_point(aes(y = betas_u1), colour = "plum3", shape = 3) +
  labs(x = "Efficacy Boundary", y = "Error Rate") +
  ylim(0, 1) +
  ggtitle("Efficacy Boundary v.s. Type I and Type II Error Rate") +
  geom_segment(x = 0, xend = 300, y = 0.05, yend = 0.05, linetype = "dashed") +
  geom_segment(x = 0, xend = 300, y = 0.1, yend = 0.1, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) 






