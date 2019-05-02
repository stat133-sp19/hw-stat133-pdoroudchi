
# check_prob() private function
check_prob <- function(prob) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop('invalid prob value')
  } else {
    return(TRUE)
  }
}



# check_trials() private function
check_trials <- function(trials) {
  if (min(trials) < 0 | max(trials) %% 1 != 0) {
    stop('invalid trials value')
  } else {
    return(TRUE)
  }
}


# check_success() private function
check_success <- function(success, trials) {
  if (max(success) > trials) {
    stop('success cannot be greater than trials')
  } else if (max(success) %% 1 != 0 | min(success) < 0) {
    stop('invalid success value')
  } else if (min(trials) < 0 | max(trials) %% 1 != 0) {
    stop('invalid trials value')
  } else {
    return(TRUE)
  }
}
check_success(1, 3)



# aux_mean() private auxiliary function
aux_mean <- function(trials, prob) {
  return(trials * prob)
}
aux_mean(10, 0.3)



# aux_variance() private auxiliary function
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}
aux_variance(10, 0.3)



# aux_mode() private auxiliary function
aux_mode <- function(trials, prob) {
  return(as.integer(trials * prob + prob))
}
aux_mode(10, 0.3)



# aux_skewness() private auxiliary function
aux_skewness <- function(trials, prob) {
  skewness <- (1 - 2 * prob) / sqrt(trials * prob * (1 - prob))
  skewness <- round(skewness, 7)
  return(skewness)
}
aux_skewness(10, 0.3)



# aux_kurtosis() private auxiliary function
aux_kurtosis <- function(trials, prob) {
  kurtosis <- (1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob))
  kurtosis <- round(kurtosis, 7)
  return(kurtosis)
}
aux_kurtosis(10, 0.3)



#' @title bin_choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials; non-negative integer value
#' @param k number of successes; non-negative integer value
#' @return numeric value of combinations that can occur
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#'
#' bin_choose(5, 0)
#'
#' bin_choose(5, 1:3)
bin_choose <- function(n, k) {
  if (max(k) > n) {
    stop('k cannot be greater than n')
  } else {
    combinations <- factorial(n) / (factorial(k) * factorial(n - k))
    return(combinations)
  }
}
bin_choose(5, 2)
bin_choose(5, 0)
bin_choose(5, 1:3)



#' @title bin_probability
#' @description Calculates the probability of getting k successes in n trials assuming a specified probability value
#' @param success number of successes; non-negative integer value
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return numeric probability value between 0 and 1 (inclusive)
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' bin_probability(0:2, 5, 0.5)
#'
#' bin_probability(55, 100, .45)
bin_probability <- function(success, trials, prob) {
  if (check_success(success, trials) != TRUE) {
    stop('invalid success value')
  } else if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    probability <-
      bin_choose(n = trials, k = success) * prob ^ success * (1 - prob) ^ (trials - success)
    return(probability)
  }
}
bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability(0:2, 5, 0.5)
bin_probability(55, 100, .45)



#' @title bin_distribution
#' @description Calculates values of a probability distribution
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return data frame with two classes: "bindis" (binomial distribution) and "data.frame"
#' @export
#' @examples
#' dis1 <- bin_distribution(5, 0.5)
#' dis1
#'
#' # plot probability histogram of binomial distribution
#'
#' plot(dis1)
bin_distribution <- function(trials, prob) {
  probability <- bin_probability(0:trials, trials, prob)
  successes <- c(0:trials)
  dat <- data.frame(successes, probability)
  class(dat) <- c('bindis', 'data.frame')
  return(dat)
}
dis1 <- bin_distribution(5, 0.5)
dis1


#'@export
plot.bindis <- function(x) {
  library(ggplot2)
  ggplot(x, aes(x = successes, y = probability)) +
    geom_col(fill = 'grey') +
    theme_minimal()
}
plot(dis1)



#' @title bin_cumulative
#' @description Calculates both the probabiltiy distribution and the cumulative probabiltiies of a given number of trials and probability value
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return data frame with two classes: "bincum" (binomial cumulative distribution) and "data.frame"
#' @export
#' @examples
#' dis2 <- bin_cumulative(5, 0.5)
#' dis2
#'
#' # plot cumulative distribution
#'
#' plot(dis2)
bin_cumulative <- function(trials, prob) {
  probability <- bin_probability(0:trials, trials, prob)
  successes <- c(0:trials)
  dat <- data.frame(successes, probability)
  cumulative <- c()
  for (i in 0:trials) {
    cumulative[i + 1] <- sum(head(probability, i + 1))
  }
  dat <- cbind(dat, cumulative)
  class(dat) <- c('bincum', 'data.frame')
  return(dat)
}
dis2 <- bin_cumulative(5, 0.5)
dis2

#' @export
plot.bincum <- function(x) {
  library(ggplot2)
  ggplot(x, aes(x = successes, y = probability)) +
    geom_line(aes(x = successes, y = cumulative)) +
    geom_point(aes(x = successes, y = cumulative), pch = 1, size = 2) +
    theme_minimal()
}
plot(dis2)



#' @title bin_variable
#' @description Returns a binomial random variable list displaying the number of trials and probability of success
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return list of class "binvar" (binomial random variable)
#' @export
#' @examples
#' bin1 <- bin_variable(10, 0.3)
#' bin1
#'
#' # display summary of bin_variable
#'
#' summary(bin1)
bin_variable <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    list <- list(trials = trials, prob = prob)
    class(list) <- 'binvar'
    return(list)
  }
}
bin1 <- bin_variable(10, 0.3)
bin1


#' @export
print.binvar <- function(x) {
  cat('"Binomial variable"\n\n')
  cat('Parameters\n')
  cat(paste0(paste('- number of trials:', x$trials), '\n'))
  cat(paste('- prob of success :', x$prob))
}
bin1


#' @export
summary.binvar <- function(x) {
  trials <- x$trials
  prob <- x$prob
  mean <- aux_mean(x$trials, x$prob)
  variance <- aux_variance(x$trials, x$prob)
  mode <- aux_mode(x$trials, x$prob)
  skewness <- aux_skewness(x$trials, x$prob)
  kurtosis <- aux_kurtosis(x$trials, x$prob)
  list <- list(trials = trials,
               prob = prob,
               mean = mean,
               variance = variance,
               mode = mode,
               skewness = skewness,
               kurtosis = kurtosis)
  class(list) <- 'summary.binvar'
  return(list)
}


#' @export
print.summary.binvar <- function(x) {
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat(paste0(paste('- number of trials:', x$trials), '\n'))
  cat(paste0(paste('- prob of success :', x$prob), '\n\n'))
  cat('Measures\n')
  cat(paste0(paste('- mean    :', x$mean), '\n'))
  cat(paste0(paste('- variance:', x$variance), '\n'))
  cat(paste0(paste('- mode    :', x$mode), '\n'))
  cat(paste0(paste('- skewness:', x$skewness), '\n'))
  cat(paste('- kurtosis:', x$kurtosis))
}
binsum1 <- summary(bin1)
binsum1


#' @title bin_mean
#' @description Computes the expected value of a binomial distribution, or the expected number of successes in n trials
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return numeric value indicating the mean
#' @export
#' @examples
#' mean <- bin_mean(trials = 10, prob = 0.3)
#' mean
bin_mean <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    mean <- aux_mean(trials, prob)
    return(mean)
  }
}
bin_mean(10, 0.3)



#' @title bin_variance
#' @description Computes the expectation of the squared deviation from the mean
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return numeric value indicating variance
#' @export
#' @examples
#' var <- bin_variance(10, 0.3)
#' var
bin_variance <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    variance <- aux_variance(trials, prob)
    return(variance)
  }
}
bin_variance(10, 0.3)



#' @title bin_mode
#' @description Computes the most likely number of success in n independent trials with probability p
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return integer value indicating mode
#' @export
#' @examples
#' mode <- bin_mode(10, 0.3)
#' mode
bin_mode <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    mode <- aux_mode(trials, prob)
    return(mode)
  }
}
bin_mode(10, 0.3)



#' @title bin_skewness
#' @description Calculates the asymmetry of the probabiltiy distribution about its mean; can be positive, negative, or undefined
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return numeric value indicating skewness of distribution
#' @export
#' @examples
#' skew <- bin_skewness(10, 0.3)
#' skew
bin_skewness <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    skewness <- aux_skewness(trials, prob)
    return(skewness)
  }
}
bin_skewness(10, 0.3)



#' @title bin_kurtosis
#' @description Calculates the "tailedness" of the probability distribution; descriptor of the shape of the distribution
#' @param trials number of trials; non-negative integer value
#' @param prob probability of success; must be a numeric value between 0 and 1 (inclusive)
#' @return numeric value indicating kurotis of distribution
#' @export
#' @examples
#' kurtosis <- bin_kurtosis(10, 0.3)
#' kurtosis
bin_kurtosis <- function(trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('invalid trials value')
  } else if (check_prob(prob) != TRUE) {
    stop('invalid prob value')
  } else {
    kurtosis <- aux_kurtosis(trials, prob)
    return(kurtosis)
  }
}
bin_kurtosis(10, 0.3)
