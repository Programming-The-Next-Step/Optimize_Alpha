library(pwr)
library(ggplot2)

#functions to justify alpha levels.
#first run all the code. In the beginning are functions incoroporated in later functions

#call idealAlpha(n, effsize, prior, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4)
#to calculate the alpha level which gives optimal learning
#the output will be alpha, power and info, which is the expected learning

#call alphaConfirmation(n, effsize, prior, postfalse, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4)
#to calculate the alpha level needed to achieve a given posterior after a significant result
#the output will be alpha, power, info, and confirm, which is the probability after a positive result

#alphaDisconfirmation(n, effsize, prior, postfalse, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4)
#to calculate the alpha level needed to achieve a given posterior after a non-significant result
#the output will be alpha power and disconfirm, which is the probability after a negative result

#probPower(effsize, prior, postfalse, posttrue, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4)
#conduct a probability based power analysis. Here posterior after significance and posterior after non-signficance are fixed
#the function will then return the sample size and the alpha level needed
#the output will be alpha, power, disconfirm, confirm, info and n, the sample size per group


#The next functions are defined to be part of the four main functions

#'Calculate the probability that the hypothesis is true, after a significant test result.
#'@param power statistical power of the test.
#'@param alpha alpha level.
#'@param prior prior probability of H1.
#'
#'@examples postConfirm(0.8, 0.05, 0.5)
#'@return posterior probability of hypothesis.
#'@export
postConfirm <- function(power, alpha, prior)  {
        (power * prior) / (power * prior + alpha * (1 - prior))
}

#'Calculate the probability that the hypothesis is true, after a non-significant test result.
#'@param power statistical power of the test.
#'@param alpha alpha level.
#'@param prior prior probability of H1.
#'
#'@examples postDisconfirm(0.8, 0.05, 0.5)
#'@return posterior probability of hypothesis.
#'@export
postDisconfirm <- function(power, alpha, prior) {
  (1 - power) * prior / (( 1 - alpha) * (1 - prior) + (1 - power) * prior)
}

#define power function for t.test and anova based on pwr package
get_power <- function(alpha, effsize, n, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4) { #get_power for different alpha levels, two tail independent samples t-test
if(test == "t-test") {
  power <- pwr.t.test(n, effsize, alpha, type = type, alternative = alternative)
}
else if(test == "anova") {
  power <- pwr.anova.test(k, n, effsize, alpha)
}
  return(power$power)
}

#calculate the expected correct change in believe
#only exportet for testing
#'@export
information <- function(confirm, disconfirm, power, alpha, prior) #calcualte the amount of information per test as %correct change in believe about the hypothesis
            {
      rightupdate <- (prior * power * (confirm - prior) + (1 - prior) * (1 - alpha) * (prior - disconfirm))
      wrongupdate <- ((1 - prior) * alpha * (confirm - prior) + (1 - power) * prior * (prior - disconfirm)) #you could also update incorrectly for false postives and false negatives
      information <- rightupdate - wrongupdate #the expected correct change in belief in the hypothesis
}

#four main functions start here

#justify alpha based on change in odds
#idealAlpha: Calculate the alpha level that optimizes the amount of learning

#' idealAlpha
#'
#' Calculates the alpha level and power which result in the highest informational value of a study.
#'
#' @param n Number of participants per group.
#' @param effsize Effect size corresponding to statistical test. D for t-test and f for anova.
#' @param prior Prior probability of H1 being true.
#' @param test Kind of statistical test. Can be "t-test" and "anova".
#' @param type Indicate whether it is a "one.sample", or "two.sample" test (for t-test only)
#' @param alternative Indicate whether the alternative is "two.sided", "less" or "greater"
#' @param k Number of groups for anova.
#' @examples idealAlpha(100, 0.3, 0.5, test = "anova")
#'
#' @return A data frame with the ideal alpha level, the ideal power, the expected correct change in belief in percent and the
#'expected change in belief under alpha = 0.05.
#' @export
idealAlpha <- function(n, effsize, prior, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4) {
  alpha  <- seq(0, 1, .00001) #supply a vector of alphas
  power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k) # calculate power
  confirm <- postConfirm(power, alpha, prior) #posterior after confirming
  disconf <- postDisconfirm(power, alpha, prior) #posterior after disconfirming
  info <- information(confirm, disconf, power, alpha, prior) #expected correct change in believe
  d <- as.data.frame(cbind(alpha, power, info))
  maxinf <- max(na.omit(info))
  dmax <- as.data.frame(subset(d, info == maxinf)) #return valus with highes correct change in believe
  d05 <- as.data.frame(subset(d, alpha == 0.05)) #compare to 5% alpha
  plot(alpha, info,
       ylab = "Expected Correct Change in Belief",
       xlab = "Significance Level",
       type = "l", main = "Optimal Alpha",
       xlim = c(0,1), axes = F,
       ylim = c(0,round(maxinf + 0.005, digits = 2)))
  abline(v = 0.05, col = "red")
  abline(v = dmax$alpha, col = "blue")
  axis(side= 2, at=c(seq(0, (maxinf + 0.01), 0.01)))
  axis(side= 1, at=c(seq(0, 1, 0.05)))
  mtext(side = 3, paste("Alpha = ", round(dmax$alpha, digits = 2), " Power = ", round(dmax$power, digits = 2), "Learning =", round(dmax$info*100, digits = 2), "%"))
  dmax <- cbind(dmax, d05$info)
  colnames(dmax) <- c("alpha", "power", "Learning", "Learning05")
  if(dmax$alpha > 0.99) stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
  row.names(dmax) <- "Value:"
  return(dmax)
}

#alphaConfirmation: Indicate your posterior probability after finding a positive result.
#The function will return the alpha level needed to achieve this posterior probability
#that has the highest informational value

#' Calculates the intended probability of the hypothesis after a significant test.
#'
#' The user specifies the intended probability of the hypothesis after a significant test result and the alpha level is adjusted accordingly.
#' @param n Number of participants per group.
#' @param effsize Effect size corresponding to statistical test. D for t-test and f for anova.
#' @param prior Prior probability of H1 being true.
#' @param posttrue The posterior probability that is aimed for if the test is significant (should be higher than prior).
#' @param test Kind of statistical test. Can be "t-test" and "anova".
#' @param type Indicate whether it is a "one.sample", or "two.sample" test (for t-test only).
#' @param alternative Indicate whether the alternative is "two.sided", "less" or "greater".
#' @param k Number of groups for anova.
#' @examples alphaConfirmation(100, 0.3, 0.2, 0.5).
#'
#' @return A data frame with the alpha level, the  power, the expected correct change in belief in percent and posterior probability after a significant result.
#' @export
alphaConfirmation <- function(n, effsize, prior, posttrue, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4) {
  if(prior > posttrue) #stop if posterior smaller than prior
  {
    stop("Prior must be smaller than posterior")
  }
  alpha   <- seq(0, 1, .00001) #define vector of alphas
  #calculate powers
  power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k)
  #calculate posteriors after confirming or disconfirming and expected learning
  confirm <- postConfirm(power, alpha, prior)
  disconf <- postDisconfirm(power, alpha, prior)
  info <- information(confirm, disconf, power, alpha, prior)
  df <- data.frame(alpha, power, info, confirm)
  #calculate the smallest confirming posterior that is above the user specified posterior
  #the alpha level corresponding to this posterior is what we are looking for.
  maxinf <- max(na.omit(info))
  postt <- as.data.frame(subset(df, confirm > posttrue))
  if (nrow(postt) == 0) stop("You need more participants to enable the intended in probability")
  postt <- max(postt$info)
  d <- subset(df, info == postt)
  if(nrow(d) > 1) {
          stop ("You have unreasonably high power,
                        therefore, there is not enough precision to calculate
                        a unique best alpha level. Consider a smaller sample size")
  }
  #plot
  plot(alpha,info,
       ylab = "Expected Change in Belief",
       xlab = "Signifcance Level", type = "l",
       main = paste("Alpha for",posttrue, "Probability after Significance") ,
       xlim = c(0,1), axes = F, ylim =c(0,round(maxinf + 0.005, digits = 2)))
  abline(v = 0.05, col = "red")
  abline(v = d$alpha, col = "blue")
  axis(side = 2, at = c(seq(0,(maxinf+0.01),0.01)))
  axis(side = 1, at = c(seq(0,1,0.05)))
  mtext(side = 3, paste("Alpha = ", round(d$alpha, digits = 2), " Power = ", round(d$power, digits = 2), "Learning =", round(d$info * 100, digits = 2), "%"))
  if(d$alpha[1] > 0.99) {
          stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
  }
  if(round(d$confirm,1) != round(posttrue,1)) {
          warning("You have a lot of power, therefore, you might achieve
                  even higher probability than intended")
  }
  row.names(d) <- "Value:"
  return(d)
}

#AlphaDisconfirmation: Indicate the posterior probability you want after a negative result
#The function will return the alpha level needed to achieve this posterior probability
#that has the highest informational value

#'Calculates the intended probability of the hypothesis after a nonsignificant test.
#'
#'The user specifies the intended probability of the hypothesis after a non-significant test result and the alpha level is adjusted accordingly.
#' @param n Number of participants per group.
#' @param effsize Effect size corresponding to statistical test. D for t-test and f for anova.
#' @param prior Prior probability of H1 being true.
#' @param postfalse The posterior probability that is aimed for if the test is not significant (should be higher than prior).
#' @param test Kind of statistical test. Can be "t-test" and "anova".
#' @param type Indicate whether it is a one.sample, or two.sample test (for t-test only).
#' @param alternative Indicate whether the alternative is "two.sided", "less", or "greater".
#' @param k Number of groups for anova.
#' @examples alphaDisconfirmation(100, 0.3, 0.5, 0.2, test = "anova").
#'
#' @return A data frame with the alpha level, the  power, the expected correct change in belief in percent and posterior probability after a significant result.
#' @export
alphaDisconfirmation <- function(n, effsize, prior, postfalse, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4)
{
  if(postfalse > prior)
  {
    stop("Prior must be higher than posterior")
  }
  alpha   <- seq(0, 1, .00001)
  power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k) #power.prop.test(n, p1 = 0.8, p2 = 1, sig.level = alpha)$power
  confirm <- postConfirm(power, alpha, prior)
  disconf <- postDisconfirm(power, alpha, prior)
  info <- information(confirm, disconf, power, alpha, prior)
  df <- data.frame(alpha, power, info, disconf)
  #f <- max(na.omit(info))
  maxinf <- max(na.omit(info))
  postd <- as.data.frame(subset(df, disconf < postfalse))
  if (nrow(postd) == 0) stop("You need more participants to enable the intended in probability")
  postd <- max(postd$info)
  d <- subset(df, info == postd)
  if(nrow(d) > 1) {
  stop ("You have unreasonably high power,
                        therefore, there is not enough precision to calculate
                        a unique best alpha level. Consider a smaller sample size.")
  }
  #plot
  plot(alpha, info,
       ylab = "Expected Change in Belief",
       xlab = "Signifcance Level", type = "l",
       main = paste("Alpha for",round(postd, digits = 2), "Probability after Non-Significance") ,
       xlim = c(0,1), axes = F,
       ylim =c(0,round(maxinf + 0.005, digits = 2)))
  abline(v = 0.05, col = "red")
  abline(v = d$alpha, col = "blue")
  axis(side = 2, at = c(seq(0,(maxinf+0.01),0.01)))
  axis(side = 1, at = c(seq(0,1,0.05)))
  mtext(side = 3, paste("Alpha = ", round(d$alpha, digits = 2), " Power = ", round(d$power, digits = 2), "Learning =", round(d$info*100, digits = 2), "%"))
  if(d$alpha > 0.99) {
        stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
  }
  if(round(d$disconf,1) != round(postfalse,1)) {
          warning("You have a lot of power, therefore, you might achieve
                  even lower probability than intended")
  }
  row.names(d) <- "Value:"
  return(d)
}

#'Probability Based Power Analysis
#'
#'Set both the posterior probability after a positive result and the posterior probability after a negative result
#'The function will calculate the required sample size and alplha level to achieve this.
#' @param effsize Effect size corresponding to statistical test. D for t-test and f for anova.
#' @param prior Prior probability of H1 being true.
#' @param posttrue The posterior probability that is aimed for if the test is significant.
#' @param postfalse The posterior probability that is aimed for if the test is not significant (should be higher than prior).
#' @param test Kind of statistical test. Can be "t-test" and "anova".
#' @param type Indicate whether it is a "one.sample", or "two.sample" test (for t-test only).
#' @param alternative Indicate whether the alternative is "two.sided", "less", or "greater".
#' @param k Number of groups for anova.
#' @examples probPower(0.3, 0.5, 0.2, 0.8, test = "anova").
#'
#' @return A data frame with the alpha level, the  power, the expected probability after a significant result and a non-significant result, the expected amount of learning, and needed sample size.
#' @export
probPower <- function(effsize, prior, postfalse, posttrue, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4) {
  if(postfalse >= prior | prior >= posttrue)  {
    stop("Posterior after disconfirming needs to be smaller than prior, posterior after confirming larger")
  }
  alpha <- seq(.0001, 1, .001)
  #for 10-20 000 particpants calculte the alpha needed to achieve the posterior after a negative result
  #stop the loop when there is an option that at the same time has the needed posterior probability after a positive result
  for (i in 10:1000) {
  power <- get_power(alpha, effsize, i, test = test, type = type, alternative = alternative, k = k)
  #calculate power for all alphas for this sample size
  disconf <- postDisconfirm(power, alpha, prior)
  confirm <- postConfirm(power, alpha, prior)
  info <- information(confirm, disconf, power, alpha, prior)
  maxinf <- max(na.omit(info))
  df <- data.frame(alpha, power, disconf, confirm, info)
  postd <- as.data.frame(subset(df, disconf < postfalse))
  d <- subset(postd, confirm > posttrue)
  n <- i
  if(nrow(d) > 0) {
    break
  }
  }
  d <- subset(d, info == max(info))
  d <- cbind(d,n)
  colnames(d) <- (c("alpha", "power", "disconf", "confirm", "info", "n"))
  plot(df$alpha, df$info,
       ylab = "Expected Correct Change in Belief",
       xlab = "Signifcance Level",
       type = "l",
       main = paste("Probability bas Power Analysis for", posttrue, " probability
       after significance and" ,postfalse, "after non-significance") ,
       xlim = c(0, 1),
       axes = F,
       ylim = c(0, round(maxinf + 0.005, digits = 2)))
  abline(v = 0.05, col = "red")
  abline(v = d$alpha, col = "blue")
  axis(side = 2, at = c(seq(0, (maxinf + 0.01), 0.01)))
  axis(side = 1, at = c(seq(0, 1, 0.05)))
  mtext(side = 3, paste("Sample Size", i, " Alpha = ", round(d$alpha, digits = 2), " Power = ", round(d$power, digits = 2), "Learning =", round(d$info*100, digits = 2), "%"))
  row.names(d) <- "Value:"
  return(d)
}


