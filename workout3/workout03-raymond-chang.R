---
title: "workout03-raymond-chang"
author: "Raymond Chang"
date: "4/28/2019"
output: html_document
---
  
# Description: Checks validity of probability value
# Input:
#   prob: probability value
#Output:
#   TRUE if probability value is valid.
#   Error message otherwise.
check_prob <- function(prob){
  if(prob > 1){
    stop("Probability value has to be between 0 and 1.")
  }
  if(prob < 0){
    stop("Probability value has to be between 0 and 1.")
  }
  else{
    return("TRUE")
  }
}
check_prob(0.3)


# Description: Checks validity of trials value
# Input:
#   trials: number of trials which much be a non-negative integer
#Output:
#   TRUE if number of trials is valid.
#   Error message otherwise.
check_trials <- function(trials){
  if(trials < 0){
    stop("Invalid trials value.")
  }
  if( (trials - round(trials)) == 0){
    return("TRUE")
  }
  else{
    stop("Invalid trials value.")
  }
}


# Description: Checks if success is a valid value for number of trials.
# Input:
#   success: number of successes
#   trials: number of trials conducted
#Output:
#   TRUE if trials and success values are valid.
#   Error message otherwise.
check_success <- function(success, trials){
  if(success < 0){
    stop("Invalid success value.")
  }
  if((success - round(success)) != 0){
    stop("Success has to be non-negative integer value.")
  }
  if(success > trials){
    stop("Invalid number of success values.")
  }
  else{
    return("TRUE")
  }
}


# Description: Returns mean of given trials and probability.
# Input:
#   trials: number of trials conducted
#   prob: probability value
#Output:
#   Value of mean
aux_mean <- function(trials, prob){
  mean <- trials*prob
  return(mean)
}
aux_mean(10, 0.3)

# Description: Returns variance of given trials and probability.
# Input:
#   trials: number of trials conducted
#   prob: probability value
#Output:
#   Value of variance
aux_variance <- function(trials, prob){
  variance <- trials*prob*(1-prob)
  return(variance)
}
aux_variance(10, 0.3)

# Description: Returns mode of given trials and probability.
# Input:
#   trials: number of trials conducted
#   prob: probability value
#Output:
#   Value(s) of mode(s)
aux_mode <- function(trials, prob){
  not_mode <- floor(((trials*prob) + prob))
  if(is.integer(((trials*prob) + prob))){
    mode <- c(not_mode, (not_mode-1))
  }
  else{
    mode <- not_mode
  }
  return(mode)
}
aux_mode(10, 0.3)

# Description: Returns skewness of given trials and probability.
# Input:
#   trials: number of trials conducted
#   prob: probability value
#Output:
#   Value of skewness
aux_skewness <- function(trials, prob){
  skewness <- (1-(2*prob)) / ((trials*prob*(1-prob))^0.5)
  return(skewness)
}
aux_skewness(10, 0.3)

# Description: Returns kurtosis of given trials and probability.
# Input:
#   trials: number of trials conducted
#   prob: probability value
#Output:
#   Value of kurtosis
aux_kurtosis <- function(trials, prob){
  kurtosis <- (1-(6*prob*(1-prob))) / (trials*prob*(1-prob))
  return(kurtosis)
}
aux_kurtosis(10, 0.3)

#' @title Possible Combinations Function
#' @description computes the number of possible combinations, similar to n choose k
#' @param n number of trials (numeric) 
#' @param k number of successes (numeric) 
#' @return value of number of possible combinations
bin_choose <- function(n, k){
  if(k > n){
    stop("k can not be greater than n")
  }
  combos <- (factorial(n)/(factorial(k)*factorial(n-k)))
  return(combos)
}
bin_choose(n = 5, k = 2)
bin_choose(5, 0)
bin_choose(5, 1:3)

#' @title Probability of Success in Given Trials Function
#' @description computes the probability of getting some number of success in some trials with some probability
#' @param success number of successes (numeric) 
#' @param trials number of trials (numeric) 
#' @param prob probability value of getting a success (double)
#' @return probability value for given success, trials, and probability of success
bin_probability <- function(success, trials, prob){
  if(check_trials(trials)){
    if(check_prob(prob)){
      if(check_success(success, trials)){
        bin_prob <- bin_choose(n = trials, k = success)*(prob^success)*((1-prob)^(trials - success))
        return(bin_prob)
      }
    }
  }
}
bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability(success = 0:2, trials = 5, prob = 0.5)
bin_probability(success = 55, trials = 100, prob = 0.45)

#' @title Data Frame of Binomial Probability Distribution
#' @description Creates a data frame of binomial probability distributions for given trials and probability
#' @param trials number of trials (numeric) 
#' @param prob probability of success (double) 
#' @return Data frame of binomial probability distribution for given trials and probability
bin_distribution <- function(trials, prob){
  prob_col <- bin_probability(success = c(0:trials), trials = trials, prob = prob)
  df <- data.frame("success" = c(0:trials), "probability" = prob_col)
  class(df) <- c("bindis", "data.frame")
  return(df)
}
bin_distribution(trials = 5, prob = 0.5)

#' @title Barplot of Probability Histogram of Binomial Distribution
#' @description Creates a barplot to visualize the probability histogram of 
#' @param trials number of trials (numeric) 
#' @param prob probability of success (double) 
#' @export 
#' @return Data frame of binomial probability distribution for given trials and probability
plot.bindis <- function(df){
  ggplot(data = df) +
    geom_histogram()
}

#' @title Data Frame of Binomial Cumulative Distribution
#' @description Creates a data frame of binomial cumulative distribution.
#' @param trials number of trials (numeric) 
#' @param prob probability of success (double) 
#' @export 
#' @return Data frame of binomial cumulative distribution for given trials and probability
bin_cumulative <- function(trials, prob){
  col1 <- c(0:trials)
  col2 <- bin_probability(success = c(0:trials), trials = trials, prob = prob)
  col3 <- c()
  for(i in c(0:trials)){
    col3[i+1] <- sum(bin_probability(success = i, trials = trials, prob = prob), col3[i])
  } 
  df <- data.frame("success" = col1, "probability" = col2, "cumulative" = col3)
  return(df)
}
bin_cumulative(trials = 5, prob = 0.5)

#' @title Line Plot of Binomial Cumulative Distribution
#' @description Plots a line plot of the binomial cumulative distribution
#' @param trials number of trials (numeric) 
#' @param prob probability of success (double) 
#' @export ggplot2
#' @return Data frame of binomial cumulative distribution for given trials and probability
plot.bincum <- function(df){
  ggplot(data = df, aes(x = success, y = cumulative)) +
    geom_line() +
    geom_point()
}
plot.bincum(df)

#' @title Line Plot of Binomial Cumulative Distribution
#' @description Returns a binomial random variable object
#' @param trials number of trials (numeric) 
#' @param prob probability of success (double) 
#' @export ggplot2
#' @return Data frame of binomial cumulative distribution for given trials and probability
bin_variable <- function(trials, prob){
  cat('\"Binomial variable\"\n\nParameters\n- number of trials:',trials, "\n- prob of success:", prob)
}
bin_variable(trials = 10, prob = 0.3)

#' @title Summary of Binomial Variable
#' @description Prints summary contents of object "binvar"
#' @param trials number of trials (numeric) 
#' @param prob probability of success (double) 
#' @export 
#' @return Summary content of object "binvar"
print.summary.binvar <- function(trials, prob){
  cat('\"Summary Binomial\"\n\nParameters\n- number of trials:',trials, "\n- prob of success:", prob, 
      "\n\nMeasures\n- mean: ", aux_mean(trials, prob),
      "\n- variance: ", aux_variance(trials, prob),
      "\n- variance: ", aux_mode(trials, prob),
      "\n- variance: ", aux_skewness(trials, prob),
      "\n- variance: ", aux_kurtosis(trials, prob)
      
  )
}
print.summary.binvar(trials = 10, prob = 0.3)


bin_mean <- function(trials, prob){
  ans <-aux_mean(trials, prob)
  return(ans)
}
bin_mean(10, 0.3)

bin_variance <- function(trials, prob){
  ans <-aux_variance(trials, prob)
  return(ans)
}
bin_variance(10, 0.3)

bin_mode <- function(trials, prob){
  ans <-aux_mode(trials, prob)
  return(ans)
}
bin_mode(10, 0.3)

bin_skewness <- function(trials, prob){
  ans <-aux_skewness(trials, prob)
  return(ans)  
}
bin_skewness(10, 0.3)

bin_kurtosis <- function(trials, prob){
  ans <-aux_kurtosis(trials, prob)
  return(ans)
}
bin_kurtosis(10, 0.3)