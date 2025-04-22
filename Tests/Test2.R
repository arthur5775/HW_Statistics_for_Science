"
Q1:
The table below shows a particular probability distribution. Calculate the standard deviation of this distribution.
x	0	1	2	3
p(x)	0.22	0.15	0.29	0.34
"
# Define the values of x
x <- c(0, 1, 2, 3)
# Define the corresponding probabilities
p_x <- c(0.22, 0.15, 0.29, 0.34)
# Compute the expected value (mean)
E_X <- sum(x * p_x)
# Compute the expected value of X^2
E_X2 <- sum((x^2) * p_x)
# Compute the standard deviation
std_dev <- sqrt(E_X2 - E_X^2)
# Print the standard deviation
print(std_dev)
# Print the standard deviation rounded to 3 decimal places
print(round(std_dev, 3))
"
1.143
"

"
Q2:
A company submits tenders for jobs.
It is thought that the probability that any particular contract will be awarded is 0.17 independently of each other.
Suppose that the company submits tenders for 3 jobs. What is the probability that at least 2 contracts are awarded (3 d.p.)?
"
# Parameters
n <- 3   # Number of trials
p <- 0.17 # Probability of success
# Compute P(X = 2) and P(X = 3)
prob_2 <- dbinom(2, n, p)
prob_3 <- dbinom(3, n, p)
# Compute P(X >= 2)
prob_at_least_2 <- prob_2 + prob_3
# Print result rounded to 3 decimal places
print(round(prob_at_least_2, 3))
"
0.077
"

"
Q3:
Trams arrive at a tram stop at a frequency of 16 per hour. What is the probability that 5 trams arrive in 15 minutes? Assume a Poisson distribution and give your answer to 3 d.p.
NOTE: You should not round in your intermediate calculations.
"
# Parameters
lambda <- 16/4  # rate (trams per 15 minutes)
k <- 5       # number of trams
# Compute the Poisson probability
prob <- dpois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(prob, 3))
"
0.156
"

"
Q4:
A machine produces glass with a nominal thickness of 3mm.
In fact the thickness is a Normal random variable with mean 3.21mm and standard deviation 0.15mm.
The thickness in mm which 25% of panes exceed is (2 d.p.)?
"
# Parameters
mean_thickness <- 3.21  # mean of the normal distribution
sd_thickness <- 0.15    # standard deviation of the normal distribution
# Find the 75th percentile (because 25% exceed this value)
thickness_75th <- qnorm(0.75, mean = mean_thickness, sd = sd_thickness)
# Print the result rounded to 2 decimal places
print(round(thickness_75th, 2))
"
3.31
"

"
Q5:
A customer service department receives on average 10 complaints about a certain product per week. Assuming that the number of complaints has a Poisson distribution, 
use the Central Limit Theorem to determine the probability that the customer service department will receive at most 31 complaints in a 3 week period.
"
#without CLT
# Parameters
lambda <- 30  # mean of the Poisson distribution for 3 weeks
k <- 31       # the number of complaints we are interested in
# Compute the cumulative probability P(X <= 31)
probability <- ppois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(probability, 3))

#with CLT
# Given values
lambda_per_week <- 10  # mean number of complaints per week
weeks <- 3  # number of weeks
# Mean and standard deviation for 3 weeks
mean_total <- lambda_per_week * weeks  # Total mean complaints over 3 weeks
sd_total <- sqrt(mean_total)  # Standard deviation for total complaints
# Calculate the probability using the normal approximation (CLT) with continuity correction
prob <- pnorm(k + 0.5, mean = mean_total, sd = sd_total)
# Print the result
print(round(prob, 3))
"
0.608
"
################################################################################
"
Q1:
The table below shows a particular probability distribution. Calculate the standard deviation of this distribution.
x	0	1	2	3
p(x)	0.31	0.42	0.15	0.12
"
# Define the values of x
x <- c(0, 1, 2, 3)
# Define the corresponding probabilities
p_x <- c(0.31, 0.42, 0.15, 0.12)
# Compute the expected value (mean)
E_X <- sum(x * p_x)
# Compute the expected value of X^2
E_X2 <- sum((x^2) * p_x)
# Compute the standard deviation
std_dev <- sqrt(E_X2 - E_X^2)
# Print the standard deviation
print(std_dev)
# Print the standard deviation rounded to 3 decimal places
print(round(std_dev, 3))
"
0.966
"

"
Q2:
A company submits tenders for jobs.
It is thought that the probability that any particular contract will be awarded is 0.17 independently of each other.
Suppose that the company submits tenders for 4 jobs. What is the probability that at least 2 contracts are awarded (3 d.p.)?
"
# Parameters
n <- 4   # Number of trials
p <- 0.17 # Probability of success
# Compute P(X = 3) and P(X = 4)
prob_3 <- dbinom(3, n, p)
prob_4 <- dbinom(4, n, p)
# Compute P(X >= 3)
prob_at_least_3 <- prob_3 + prob_4
# Print result rounded to 3 decimal places
print(round(prob_at_least_3, 3))
"
0.017
"

"
Q3:
Trams arrive at a tram stop at a frequency of 13 per hour. What is the probability that 8 trams arrive in 45 minutes? Assume a Poisson distribution and give your answer to 3 d.p.
NOTE: You should not round in your intermediate calculations.
"
# Parameters
lambda <- 13*3/4  # rate (trams per 45 minutes)
k <- 8       # number of trams
# Compute the Poisson probability
prob <- dpois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(prob, 3))
"
0.118
"

"
Q4:
A paint manufacturer produces cans of paint with a nominal volume of 1 litres.
In fact the volume is a Normal random variable with mean 1.05 litres and standard deviation 0.05 litres.
A single can is examined; the probability that it contains less than 1 litre (3 d.p.)?
"
# Parameters
mean_thickness <- 1.05  # mean of the normal distribution
sd_thickness <- 0.05    # standard deviation of the normal distribution
# Probability that a can contains less than 1 litre
prob_less_than_1 <- pnorm(1, mean = mean_thickness, sd = sd_thickness)
# Print the result rounded to 3 decimal places
round(prob_less_than_1, 3)
"
0.159
"

"
Q5:
A customer service department receives on average 7 complaints about a certain product per week. Assuming that the number of complaints has a Poisson distribution, 
use the Central Limit Theorem to determine the probability that the customer service department will receive at most 16 complaints in a 3 week period.
"
#without CLT
# Parameters
lambda <- 21  # mean of the Poisson distribution for 3 weeks
k <- 16       # the number of complaints we are interested in
# Compute the cumulative probability P(X <= 31)
probability <- ppois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(probability, 3))

#with CLT
# Given values
lambda_per_week <- 7  # mean number of complaints per week
weeks <- 3  # number of weeks
# Mean and standard deviation for 3 weeks
mean_total <- lambda_per_week * weeks  # Total mean complaints over 3 weeks
sd_total <- sqrt(mean_total)  # Standard deviation for total complaints
# Calculate the probability using the normal approximation (CLT) with continuity correction
prob <- pnorm(k + 0.5, mean = mean_total, sd = sd_total)
# Print the result
print(round(prob, 3))
"
0.163
"
################################################################################
"
Q1:
The table below shows a particular probability distribution. Calculate the standard deviation of this distribution.
x	0	1	2	3
p(x)	0.38	0.32	0.11	0.19
"
# Define the values of x
x <- c(0, 1, 2, 3)
# Define the corresponding probabilities
p_x <- c(0.38, 0.32, 0.11, 0.19)
# Compute the expected value (mean)
E_X <- sum(x * p_x)
# Compute the expected value of X^2
E_X2 <- sum((x^2) * p_x)
# Compute the standard deviation
std_dev <- sqrt(E_X2 - E_X^2)
# Print the standard deviation
print(std_dev)
# Print the standard deviation rounded to 3 decimal places
print(round(std_dev, 3))
"
1.113
"

"
Q2:
A company submits tenders for jobs.
It is thought that the probability that any particular contract will be awarded is 0.25 independently of each other.
Suppose that the company submits tenders for 3 jobs.
What is the probability that at least 3 contracts are awarded (3 d.p.)?
"
# Parameters
n <- 3   # Number of trials
p <- 0.25 # Probability of success
# Compute P(X = 2) and P(X = 3)
prob_3 <- dbinom(3, n, p)
# Compute P(X >= 3)
prob_at_least_3 <- prob_3
# Print result rounded to 3 decimal places
print(round(prob_at_least_3, 3))
"
0.016
"

"
Q3:
Trams arrive at a tram stop at a frequency of 15 per hour. What is the probability that 5 trams arrive in 30 minutes? Assume a Poisson distribution and give your answer to 3 d.p.
NOTE: You should not round in your intermediate calculations.
"
# Parameters
lambda <- 15/2  # rate (trams per 45 minutes)
k <- 5       # number of trams
# Compute the Poisson probability
prob <- dpois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(prob, 3))
"
0.109
"

"
Q4:
A paint manufacturer produces cans of paint of nominal volume 1 litre.
In fact the volume of paint in a can is a Normal random variable with mean 1.072 litres and standard deviation 0.04 litres.
A single can is examined; the probability that it contains less than 1 litre is (3 d.p.)?
"
# Parameters
mean_thickness <- 1.072  # mean of the normal distribution
sd_thickness <- 0.04    # standard deviation of the normal distribution
# Probability that a can contains less than 1 litre
prob_less_than_1 <- pnorm(1, mean = mean_thickness, sd = sd_thickness)
# Print the result rounded to 3 decimal places
round(prob_less_than_1, 3)
"
0.036
"

"
Q5:
The probability that a seed will germinate is 0.31. Suppose 136 seeds are planted. 
Use the Central Limit Theorem to determine the probability that at most 40 seeds germinate.
"
# Parameters
n <- 136        # number of seeds
p <- 0.31       # probability of germination
# Mean and standard deviation
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
# Apply continuity correction to get Z-score (use X = 40.5 instead of 40)
z <- (40.5 - mu) / sigma
# Cumulative probability using the normal distribution
probability <- pnorm(z)
probability
"
0.3791268
"
################################################################################
"
Q1:
The table below shows a particular probability distribution. Calculate the standard deviation of this distribution.
x	0	1	2	3
p(x)	0.29	0.4	0.15	0.16
"
# Define the values of x
x <- c(0, 1, 2, 3)
# Define the corresponding probabilities
p_x <- c(0.29, 0.4, 0.15, 0.16)
# Compute the expected value (mean)
E_X <- sum(x * p_x)
# Compute the expected value of X^2
E_X2 <- sum((x^2) * p_x)
# Compute the standard deviation
std_dev <- sqrt(E_X2 - E_X^2)
# Print the standard deviation
print(std_dev)
# Print the standard deviation rounded to 3 decimal places
print(round(std_dev, 3))
"
1.024
"

"
Q2:
A company submits tenders for jobs.
It is thought that the probability that any particular contract will be awarded is 0.25 independently of each other.
Suppose that the company submits tenders for 5 jobs.
What is the probability that at least 2 contracts are awarded (3 d.p.)?
"
# Parameters
n <- 5   # Number of trials
p <- 0.25 # Probability of success
# Compute P(X = 2) and P(X = 3) and P(X = 4) and P(X = 5)
prob_2 <- dbinom(2, n, p)
prob_3 <- dbinom(3, n, p)
prob_4 <- dbinom(4, n, p)
prob_5 <- dbinom(5, n, p)
# Compute P(X >= 3)
prob_at_least_2 <- prob_2 + prob_3 + prob_4 + prob_5
# Print result rounded to 3 decimal places
print(round(prob_at_least_2, 3))
"
0.367
"

"
Q3:
Trams arrive at a tram stop at a frequency of 20 per hour. What is the probability that 4 trams arrive in 15 minutes? Assume a Poisson distribution and give your answer to 3 d.p.
NOTE: You should not round in your intermediate calculations.
"
# Parameters
lambda <- 20/4  # rate (trams per 15 minutes)
k <- 4       # number of trams
# Compute the Poisson probability
prob <- dpois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(prob, 3))
"
0.175
"

"
Q4:
A machine produces glass with a nominal thickness of 3mm.
In fact the thickness is a Normal random variable with mean 3.15mm and standard deviation 0.15mm.
The thickness in mm which 20% of panes exceed is (2 d.p.)?
"
# Parameters
mean_thickness <- 3.15  # mean of the normal distribution
sd_thickness <- 0.15    # standard deviation of the normal distribution
# Find the 75th percentile (because 25% exceed this value)
thickness_80th <- qnorm(0.80, mean = mean_thickness, sd = sd_thickness)
# Print the result rounded to 2 decimal places
print(round(thickness_80th, 2))
"
3.28
"

"
Q5:
A customer service department receives on average 9 complaints about a certain product per week. 
Assuming that the number of complaints has a Poisson distribution, 
use the Central Limit Theorem to determine the probability that the customer service department will receive at most 26 complaints in a 3 week period.
"
#without CLT
# Parameters
lambda <- 27  # mean of the Poisson distribution for 3 weeks
k <- 26       # the number of complaints we are interested in
# Compute the cumulative probability P(X <= 31)
probability <- ppois(k, lambda)
# Print the result rounded to 3 decimal places
print(round(probability, 3))

#with CLT
# Given values
lambda_per_week <- 9  # mean number of complaints per week
weeks <- 3  # number of weeks
# Mean and standard deviation for 3 weeks
mean_total <- lambda_per_week * weeks  # Total mean complaints over 3 weeks
sd_total <- sqrt(mean_total)  # Standard deviation for total complaints
# Calculate the probability using the normal approximation (CLT)
prob <- pnorm(k + 0.5, mean = mean_total, sd = sd_total)
# Print the result
print(round(prob, 3))
"
0.462
"