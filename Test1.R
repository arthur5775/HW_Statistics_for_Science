"
Q1:
An insurance company issues policy of two types:
20% are of type A and the remainder are of type B.
For the type A policy, 36% of the policy holders are female, 
while for type B, only 25% of policy holders are female.
If a policy is selected at random from those held by females, what is the probability that it is a type A policy (3d.p.)?
"
# Given probabilities
P_A <- 0.2
P_B <- 0.8
P_F_given_A <- 0.36
P_F_given_B <- 0.25
# Total probability of selecting a female policyholder
P_F <- (P_F_given_A * P_A) + (P_F_given_B * P_B)
# Applying Bayes' theorem
P_A_given_F <- (P_F_given_A * P_A) / P_F
# Print result rounded to 3 decimal places
result <-round(P_A_given_F, 3)
cat("Probability:", result, "\n")
"
0.265
"

"
Q1:
An insurance company issues policy of two types:
20% are of type A and the remainder are of type B.
For the type A policy, 36% of the policy holders are female, 
while for type B, only 25% of policy holders are female.
If a policy is selected at random from those held by females, what is the probability that it is a type A policy (3d.p.)?
"
# Given probabilities
P_A <- 0.2
P_B <- 0.8
P_F_given_A <- 0.4
P_F_given_B <- 0.2
# Total probability of selecting a female policyholder
P_F <- (P_F_given_A * P_A) + (P_F_given_B * P_B)
# Applying Bayes' theorem
P_A_given_F <- (P_F_given_A * P_A) / P_F
# Print result rounded to 3 decimal places
result <-round(P_A_given_F, 3)
cat("Probability:", result, "\n")
"
0.265
"

"
Q2:
The data come from the records of an estate agent and relate to sales of private houses.
A random sample of sales was taken from the records and the following table gives the frequency of each type of sale.
|       |1 bedroom|2 bedrooms|3 bedrooms|4 bedrooms|totals|
|Central|   7     |   8      |    5     |   7      |  27  |
|Suburbs|   7     |   7      |    7     |   5      |  26  |
|Totals |   14    |   15     |    12    |   12     |  53  |
If one of the properties is chosen at random, the probability that it is a 2-bedroom house, given that it has at least 2 bedrooms (3d.p.)
"
# Define the frequency table
sales_data <- matrix(c(7, 8, 5, 7, 7, 7, 7, 5), nrow = 2, byrow = TRUE)
colnames(sales_data) <- c("1 bedroom", "2 bedrooms", "3 bedrooms", "4 bedrooms")
rownames(sales_data) <- c("Central", "Suburbs")
# Compute total for each category
total_2_bedrooms <- sum(sales_data[, "2 bedrooms"])
total_3_or_more <- sum(sales_data[, c("2 bedrooms", "3 bedrooms", "4 bedrooms")])
# Compute conditional probability
probability <- total_2_bedrooms / total_3_or_more
# Print the result rounded to 3 decimal places
cat("Probability:", round(probability, 3), "\n")
"
0.385
"

"
Q3:
A psychologist gave 9 students an IQ test with the following results:
88, 106, 104, 95, 100, 115, 114, 107, 107
what is the interquantile range of the data?
"
# Given IQ test results
iq_scores <- c(88, 106, 104, 95, 100, 115, 114, 107, 107)
# Sort the data
sorted_scores <- sort(iq_scores)
# Compute Q1 (25th percentile)
q1_index <- (1/4) * (length(sorted_scores) + 1)
q1 <- sorted_scores[floor(q1_index)] + (q1_index - floor(q1_index)) * 
  (sorted_scores[ceiling(q1_index)] - sorted_scores[floor(q1_index)])
# Compute Q3 (75th percentile)
q3_index <- (3/4) * (length(sorted_scores) + 1)
q3 <- sorted_scores[floor(q3_index)] + (q3_index - floor(q3_index)) * 
  (sorted_scores[ceiling(q3_index)] - sorted_scores[floor(q3_index)])
# Compute IQR
iqr <- q3 - q1
# Print result
print(iqr)
"
13
"

"
Q4:
The data come from the records of an estate agent and relate to sales of private houses.
A random sample of sales was taken from the records and the following table gives the frequency of each type of sale.
|       |1 bedroom|2 bedrooms|3 bedrooms|4 bedrooms|totals|
|Central|   6     |   9      |    7     |   8      |  30  |
|Suburbs|   6     |   10     |    9     |   4      |  29  |
|Totals |   12    |   19     |    16    |   12     |  59  |
If three of the properties are chosen at random, without replacement, the probability they all have at least 3 bedrooms (3d.p.)
"
# Given values
total_houses <- 59
houses_3_or_more <- 16 + 12
# Compute probability
P <- (houses_3_or_more / total_houses) * ((houses_3_or_more - 1) / (total_houses - 1)) * ((houses_3_or_more - 2) / (total_houses - 2))
# Round to 3 decimal places
result <- round(P, 3)
# Print result
cat("Probability:", result, "\n")
"
0.101
"

"
Q4:
The data come from the records of an estate agent and relate to sales of private houses.
A random sample of sales was taken from the records and the following table gives the frequency of each type of sale.
|       |1 bedroom|2 bedrooms|3 bedrooms|4 bedrooms|totals|
|Central|   6     |   9      |    5     |   6      |  26  |
|Suburbs|   5     |   5      |    6     |   3      |  19  |
|Totals |   11    |   14     |    11    |   9      |  45  |
If three of the properties are chosen at random, without replacement, the probability they do not include a 2 bedrooms house (3d.p.)
"
# Given values
total_houses <- 45
houses_not_2_bedrooms <- 11 + 11 + 9
# Compute probability
P <- (houses_not_2_bedrooms / total_houses) * ((houses_not_2_bedrooms - 1) / (total_houses - 1)) * ((houses_not_2_bedrooms - 2) / (total_houses - 2))
# Round to 3 decimal places
result <- round(P, 3)
# Print result
cat("Probability:", result, "\n")
"
0.317
"