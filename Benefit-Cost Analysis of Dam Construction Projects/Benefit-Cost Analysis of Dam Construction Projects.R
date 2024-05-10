# Preset
# Preset
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

####ALY 6050- Module-2: Benefit-Cost Analysis of Dam Construction Projects####

## Given Data ##

# Matrix for Dam 1 
benefits_dam1 <- matrix(c(1.1, 2, 2.8,
                          8, 12, 14.9,
                          1.4, 1.4, 2.2,
                          6.5, 9.8, 14.6,
                          1.7, 2.4, 3.6,
                          0, 1.6, 2.4), nrow = 6, byrow = TRUE)
cost_dam1 <- matrix(c(13.2, 14.2, 19.1,
                       3.5, 4.9, 7.4), nrow = 2, byrow = TRUE)


# Matrix for Dam 2 
benefits_dam2 <- matrix(c(2.1, 3, 4.8,
                          8.7,12.2, 13.6,
                          2.3, 3, 3,
                          5.9, 8.7, 15,
                          0, 3.4, 3.4,
                          0, 1.2, 1.8), nrow = 6, byrow = TRUE)
cost_dam2 <- matrix(c(12.8, 15.8, 20.1,
                       3.8, 5.7, 8), nrow = 2, byrow = TRUE)

###Part-1####

#(i): Simulation of 10,000 benefit-cost ratios

##Creating a Cost function to perform simulation


cost <- function(benefits_dam, costs_dam)
{
  # Seed to generate the same pseudo random values
  set.seed(20)
  n = 10^4  # 10,000 simulations
  sampels <- numeric(n)
  sum_of_benefits <- numeric(n)
  sum_of_costs <- numeric(n)
  for(i in 1:n)
  {
    total_benefits <- apply(benefits_dam, 1, function(x) runif(1, min = x[1], max = x[3]))
    total_cost <- apply(costs_dam, 1, function(x) runif(1, min = x[1], max = x[3]))
    benefit_to_cost_ratio <- sum(total_benefits) / sum(total_cost )
    sampels[i] <- benefit_to_cost_ratio
    sum_of_benefits[i] <- sum(total_benefits)
    sum_of_costs[i] <- sum(total_cost)
  }
  output <- list(sampels, total_benefits, total_cost)
  return(output)
}


#Cost Benefit analysis

alpha1 <- cost(benefits_dam1, cost_dam1)
alpha1 
alpha2 <- cost(benefits_dam2, cost_dam2)
alpha2

data <- unlist(alpha1)
mean_alpha1 <- mean(data)
mean_alpha1 
min_alpha1 <- min(data)
min_alpha1
mode_alpha1 <- mode(data)
mode_alpha1 
max_alpha1 <- max(data)
max_alpha1

# Define a function to calculate the mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_value <- calculate_mode(data)
mode_value

data2 <- unlist(alpha2)
mean_alpha2 <- mean(data2)
mean_alpha2 
min_alpha2 <- min(data2)
min_alpha2
max_alpha2 <- max(data2)
max_alpha2
mode_value2 <- calculate_mode(data2)
mode_value2

#(ii) tabular and a graphical frequency distribution for alpha1 and 2

alpha1_tabel <- table(cut(alpha1[[1]], breaks = 40))
alpha2_tabel<- table(cut(alpha2[[1]], breaks = 40))
alpha1_tabel
alpha2_tabel

##Histogram of Benefit Costs##

hist(alpha1[[1]], breaks = 40, main = "Histogram of Alpha-1", xlab = "Benefit to Ratio")
hist(alpha2[[1]], breaks = 40, main = "Histogram of Alpha-2", xlab = "Benefit to Cost Ratio")

# Calculating theoretical and observed values.
# Dam 1
# Observed Values

dam1_mean_of_benefits <- mean(alpha1[[2]])
dam1_sd_of_benefits <- sd(alpha1[[2]])

dam1_mean_of_cost <- mean(alpha1[[3]])
dam1_sd_of_cost<- sd(alpha1[[3]])

dam1_mean_of_cost_to_benefit <- mean(alpha1[[1]])
dam1_sd_of_cost_to_benefit  <- sd(alpha1[[1]])

##Function for Theoretical values##

theoretical_probability <- function(matrix_m)
{
  mean <- apply(matrix_m, 1, function(x) ((x[1] + x[3])/2))
  sd <- apply(matrix_m, 1, function(x) ((x[3] - x[1])/(2 * sqrt(3))))
  return (list(mean, sd))
}

benefit_values <- theoretical_probability(benefits_dam1)
mean_benefit <- sum(benefit_values[[1]])
sd_benefit <- sum(benefit_values[[2]])

cost_values <-  theoretical_probability(cost_dam1)
mean_cost <- sum(cost_values[[1]])
sd_cost <- sum(cost_values[[2]])

# Storing values in a dataframe.
# Create a dataframe.
df_dam1 <- data.frame(Dam1 = c("Mean of Total Benefits", "SD of Total Benefits", 
                               "Mean of Total Costs","SD of Total Costs",
                               "Mean of Benefit Cost Ratio", "Standard Deviation of Benefit Cost Ratio"),
                       Observed = c(dam1_mean_of_benefits, dam1_sd_of_benefits, 
                                    dam1_mean_of_cost , dam1_sd_of_cost , 
                                    dam1_mean_of_cost_to_benefit, dam1_sd_of_cost_to_benefit),
                       Theoretical = c(mean_benefit, sd_benefit, mean_cost, sd_cost, '*', '*'))
df_dam1



# Dam 2
# Observed Values
dam2_mean_of_benefits  <- mean(alpha2[[2]])
dam2_sd_of_benefits  <- sd(alpha2[[2]])

dam2_mean_of_cost <- mean(alpha2[[3]])
dam2_sd_of_cost  <- sd(alpha2[[3]])

dam2_mean_of_cost_to_benefit <- mean(alpha2[[1]])
dam2_sd_of_cost_to_benefit <- sd(alpha2[[1]])

benefit_values2 <- theoretical_probability(benefits_dam2)
mean_benefit2 <- sum(benefit_values2[[1]])
sd_benefit2 <- sum(benefit_values2[[2]])

cost_values2 <-  theoretical_probability(cost_dam2)
mean_cost2 <- sum(cost_values2[[1]])
sd_cost2 <- sum(cost_values2[[2]])

# Create a dataframe.
df_dam2 <- data.frame(Dam2 = c("Mean of Total Benefits", "SD of Total Benefits", 
                                 "Mean of Total Costs","SD of Total Costs",
                                 "Mean of Benefit Cost Ratio", "Standard Deviation of Benefit Cost Ratio"),
                       Observed = c(dam2_mean_of_benefits, dam2_sd_of_benefits, 
                                     dam2_mean_of_cost , dam2_sd_of_cost , 
                                     dam2_mean_of_cost_to_benefit, dam2_sd_of_cost_to_benefit),
                       Theoretical = c(mean_benefit2, sd_benefit2, mean_cost2, sd_cost2, '*', '*'))
df_dam2 

#______________________________

###Part-2####

#Chi Square Goodness of Fit#

# Defining parameters for triangular distribution
lower_bound <- min_alpha1
upper_bound <- max_alpha1
mode_value <- mode_value


# Define a custom function to calculate the PDF of the triangular distribution
dtriangular <- function(x, lower, mode, upper) {
  # Calculate PDF of the triangular distribution
  pdf <- ifelse(x < mode,
                2*(x - lower)/((upper - lower)*(mode - lower)),
                2*(upper - x)/((upper - lower)*(upper - mode)))
  pdf[x < lower | x > upper] <- 0  # Set PDF to 0 outside the bounds
  return(pdf)
}

# Generating x values within the range of the triangular distribution
x <- seq(lower_bound, upper_bound, length.out = 100)

# PDF of the triangular distribution
pdf_triangular <- dtriangular(x, lower = lower_bound, mode = mode_value, upper = upper_bound)

# Check if there are any valid PDF values to plot
#if (all(is.nan(pdf_triangular))) {
  #stop("No valid PDF values to plot.")
#}

# Determine y-axis limits for plotting
y_min <- min(pdf_triangular, na.rm = TRUE)
y_max <- max(pdf_triangular, na.rm = TRUE)

# Plot PDF of triangular distribution with specified y-axis limits
plot(x, pdf_triangular, type = "l", col = "blue", lwd = 2,
     ylim = c(y_min, y_max),
     main = "Triangular Distribution for Cost-Benefit Analysis for Aplha-1",
     xlab = "Value", ylab = "Density")

# Add vertical lines for mean, min, max, and mode values
abline(v = mean_alpha1, col = "red", lty = 2, lwd = 2)  # Mean
abline(v = min_alpha1, col = "green", lty = 2, lwd = 2)  # Minimum
abline(v = max_alpha1, col = "black", lty = 2, lwd = 2)  # Maximum
abline(v = mode_value, col = "purple", lty = 2, lwd = 2)  # Mode

# Add legend
legend("topright", legend = c("PDF", "Mean", "Minimum", "Maximum", "Mode"),
       col = c("blue", "red", "green", "black", "purple"), lty = c(1, 2, 2, 2, 2), lwd = 2)


# Dam 1.
# H0: The use of a triangular distribution adequately represents the uncertainty in the costs and benefits associated with cost-benefit analysis analysis.

# H1: The use of a triangular distribution does not adequately represents the uncertainty in the costs and benefits associated with cost-benefit analysis analysis.

alpha1_observed <- hist(alpha1[[1]], breaks = 40, plot = FALSE)$counts
alpha1_expected <- dnorm(hist(alpha1[[1]], breaks = 20, plot = FALSE)$mids, 
                         mean = mean(alpha1[[1]]), sd = sd(alpha1[[1]])) * length(alpha1[[1]])

chisq_test_alpha1 <- chisq.test(alpha1_observed, p = alpha1_expected /sum(alpha1_expected ))
chisq_test_alpha1

if(chisq_test_alpha1$p.value<=0.05){
  cat("p-value is less than 0.05. Therefore, we reject the null hypothesis ")
} else {
  cat("p-value is greater than 0.05. Therefore, we fail to reject the null hypothesis ")
}

#Dam 2.


# Defining parameters for triangular distribution
lower_bound2 <- min_alpha2
upper_bound2 <- max_alpha2
mode_value2 <- mode_value2

# Generating x values within the range of the triangular distribution
x <- seq(lower_bound2, upper_bound2, length.out = 100)

# PDF of the triangular distribution
pdf_triangular2 <- dtriangular(x, lower = lower_bound2, mode = mode_value2, upper = upper_bound2)

# Check if there are any valid PDF values to plot
#if (all(is.nan(pdf_triangular))) {
  #stop("No valid PDF values to plot.")
#}

# Determine y-axis limits for plotting
y_min <- min(pdf_triangular, na.rm = TRUE)
y_max <- max(pdf_triangular, na.rm = TRUE)

# Plot PDF of triangular distribution with specified y-axis limits
plot(x, pdf_triangular, type = "l", col = "blue", lwd = 2,
     ylim = c(y_min, y_max),
     main = "Triangular Distribution for Cost-Benefit Analysis for Aplha-2",
     xlab = "Value", ylab = "Density")

# Add vertical lines for mean, min, max, and mode values
abline(v = mean_alpha1, col = "red", lty = 2, lwd = 2)  # Mean
abline(v = min_alpha1, col = "green", lty = 2, lwd = 2)  # Minimum
abline(v = max_alpha1, col = "orange", lty = 2, lwd = 2)  # Maximum
abline(v = mode_value, col = "purple", lty = 2, lwd = 2)  # Mode

# Add legend
legend("topright", legend = c("PDF", "Mean", "Minimum", "Maximum", "Mode"),
       col = c("blue", "red", "green", "orange", "purple"), lty = c(1, 2, 2, 2, 2), lwd = 2)


# H0: The use of a triangular distribution adequately represents the uncertainty in the costs and benefits associated with the scenario under analysis in the cost-benefit analysis.
# H1: The use of a triangular distribution does not adequately represents the uncertainty in the costs and benefits associated with the scenario under analysis in the cost-benefit analysis.

alpha2_observed <- hist(alpha2[[1]], breaks = 20, plot = FALSE)$counts
alpha2_expected <- dnorm(hist(alpha2[[1]], breaks = 20, plot = FALSE)$mids, 
                    mean = mean(alpha2[[1]]), sd = sd(alpha2[[1]])) * length(alpha2[[1]])

chisq_test_alpha2  <- chisq.test(alpha2_observed, p = alpha2_expected/sum(alpha2_expected))
chisq_test_alpha2

if(chisq_test_alpha2$p.value<=0.05){
  cat("p-value is less than 0.05. Therefore, we reject the null hypothesis ")
} else {
  cat("p-value is greater than 0.05. Therefore, we fail to reject the null hypothesis ")
}

###Part-3####

#Results Simulation#

#Dam 1
#install.packages("moments")
library(moments)

skew1 <- skewness(alpha1[[1]])
p1_alpha1_2 <- mean(alpha1[[1]]>2)
p1_alpha1_1.8 <- mean(alpha1[[1]]>1.8)
p1_alpha1_1.5 <- mean(alpha1[[1]]>1.5)
p1_alpha1_1.2 <- mean(alpha1[[1]]>1.2)
p1_alpha1_1 <- mean(alpha1[[1]]>1)

#For Dam 2
skew2 <- skewness(alpha2[[1]])
p2_alpha2_2 <- mean(alpha2[[1]]>2)
p2_alpha2_1.8 <- mean(alpha2[[1]]>1.8)
p2_alpha2_1.5 <- mean(alpha2[[1]]>1.5)
p2_alpha2_1.2 <- mean(alpha2[[1]]>1.2)
p2_alpha2_1 <- mean(alpha2[[1]]>1)

# Dataframe for Displaying results.
project_results <- data.frame(Dam = c("Minimum", "Maximum","Mean","Median",
                                      "Variance","Standard Deviation","skewness",
                                      "P(alpha>2)","P(alpha>1.8)","P(alpha>1.5)",
                                      "P(alpha>1.2)","P(alpha>1)"),
                          Alpha1 = c(round(min(alpha1[[1]]),3), round(max(alpha1[[1]]),3),
                                     round(mean(alpha1[[1]]),3),round(median(alpha1[[1]]),3),
                                     round(var(alpha1[[1]]),3), round(sd(alpha1[[1]]),3), 
                                     skew1,p1_alpha1_2,p1_alpha1_1.8,
                                     p1_alpha1_1.5,p1_alpha1_1.2,p1_alpha1_1),
                          Alpha2 = c(round(min(alpha2[[1]]),3), round(max(alpha2[[1]]),3),
                                     round(mean(alpha2[[1]]),3),round(median(alpha2[[1]]),3),
                                     round(var(alpha2[[1]]),3), round(sd(alpha2[[1]]),3), 
                                     skew2,p2_alpha2_2,p2_alpha2_1.8,
                                     p2_alpha2_1.5,p2_alpha2_1.2,p2_alpha2_1))

project_results

# Probability that the C-B ratio of Dam 1 will be higher

x <- sum(alpha1[[1]] > alpha2[[1]])
prob <- x / 10000
cat("Estimate that Alpha 1 > Alpha 2 ", prob)

cat("Alpha1 consistently outperforms Alpha2 across various metrics.")
