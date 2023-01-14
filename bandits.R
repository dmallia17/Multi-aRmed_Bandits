# Author:       Daniel Mallia
# Date Begun:   Jan 12, 2023
# Purpose:      Following Chapter 2 in Richard Sutton and Andrew Barto's
#               "Reinforcement Learning" (2nd ed.), this file contains
#               implementations of:
#                 - Stationary and non-stationary bandit problems

library(ggplot2)

new_multi_armed_bandit <- function(arms=10, mean=0, sd=1, nsmean=0, nssd=0.01, class="stationary") {
  cls <- match.arg(class, c("stationary", "nonstationary"))
  c_vec <- c(cls, "bandit")
  qvals <- rnorm(arms, mean=mean, sd=sd)
  if(cls == "stationary") {
    structure(list(arms=arms, qvals=qvals), class=c_vec)
  } else {
    structure(list(arms=arms, qvals=qvals, nsmean=nsmean, nssd=nssd),
              class=c_vec)
  }
}

pull_arm <- function(mab, arm) {
  if(arm < 1 || arm > mab$arms) {
    stop("Invalid arm choice")
  }
  UseMethod("pull_arm")
}

pull_arm.stationary <- function(mab, arm) {
  list(bandit=mab, reward=rnorm(1, mab$qvals[arm]))
}

pull_arm.nonstationary <- function(mab, arm) {
  new_bandit <- mab
  new_bandit$qvals <- mab$qvals + rnorm(mab$arms, mab$nsmean, mab$nssd)
  list(bandit=new_bandit,
       reward=rnorm(1, mab$qvals[arm]))
}

plot.bandit <- function(mab) {
  distribution_df <- data.frame(
    arm=factor(as.vector(sapply(1:mab$arms, rep, times=1000))),
    qvals=as.vector(sapply(mab$qvals,
                           function(q) {qnorm(seq(0,1, length.out=1000), mean=q) }))
  )
  ggplot(distribution_df, aes(x=arm, y=qvals)) + geom_violin(fill="grey") +
    labs(title=paste0(mab$arms,"-armed bandit configuration"), x="Action",
         y="Reward distribution") +
    geom_hline(aes(yintercept=0), linetype="dashed")
}