# Author:       Daniel Mallia
# Date Begun:   Jan 12, 2023
# Purpose:      Following Chapter 2 in Richard Sutton and Andrew Barto's
#               "Reinforcement Learning" (2nd ed.), this file contains
#               implementations of:
#                 - Stationary and non-stationary bandit problems

library(ggplot2)

# Constructor for multi-armed bandit problems, with a customizable number of
# arms, normal distribution for sampling the means (e.g. initial true q-values,
# q*(a)) of the arm reward distributions (once the mean for each arm is
# sampled, unit variance is assumed for the reward distribution), stationary or
# non-stationary behavior, and, if the latter, normal distribution for
# incrementally modifying the arm rewards.
# @param arms       Number of arms in the problem.
# @param initmean   Mean of the normal distribution for sampling the means of the
#                   reward distributions.
# @param initsd     Standard deviation (same distribution as initmean).
# @param nsmean     Mean of the normal distribution used to incrementally
#                   modify the reward distributions if non-stationary.
# @param nssd       Standard deviation (same distribution as nsmean).
# @param class      String indicating if "stationary" or "nonstationary"
# @return           A multi-armed bandit problem structure.
new_multi_armed_bandit <- function(arms=10, initmean=0, initsd=1, nsmean=0,
                                   nssd=0.01, class="stationary") {
  cls <- match.arg(class, c("stationary", "nonstationary"))
  c_vec <- c(cls, "bandit") # Class vector
  qvals <- rnorm(arms, mean=initmean, sd=initsd) # Sample the q*(a) for all a
  if(cls == "stationary") {
    structure(list(arms=arms, qvals=qvals, initmean=initmean), class=c_vec)
  } else {
    structure(list(arms=arms, qvals=qvals, initmean=initmean, nsmean=nsmean,
                   nssd=nssd), class=c_vec)
  }
}

# Add generic pull_arm function for bandit problems
# @param mab  A multi-armed bandit problem instance
# @param arm  An action selection (i.e. arm choice) (integer between 1 and the
#             the number of arms in the problem, inclusive).
# @return     A list of the bandit problem, which may have changed (if
#             non-stationary) and the reward received for this action.
pull_arm <- function(mab, arm) {
  if(arm < 1 || arm > mab$arms) {
    stop("Invalid arm choice")
  }
  UseMethod("pull_arm")
}

# Define pull_arm for stationary multi-armed bandit problems
pull_arm.stationary <- function(mab, arm) {
  list(bandit=mab, reward=rnorm(1, mab$qvals[arm]))
}

# Define pull_arm for non-stationary multi-armed bandit problems
# Note that this function returns a modified bandit structure
pull_arm.nonstationary <- function(mab, arm) {
  new_bandit <- mab
  new_bandit$qvals <- mab$qvals + rnorm(mab$arms, mab$nsmean, mab$nssd)
  list(bandit=new_bandit,
       reward=rnorm(1, mab$qvals[arm]))
}

# Define a plotting method for bandit problems, stationary and non-stationary;
# in the case of the latter, the plot captures only the initial state of the
# problem. This plot mirrors the violin plot style of representing a bandit
# problem found on page 28 in "Reinforcement Learning".
# @param mab    A multi-armed bandit problem instance
# @return       Violin plot representing the problem configuration
plot.bandit <- function(mab) {
  # Prepare a dataframe for use with what ggplot expects for a violin plot
  # A column indicating the arm under consideration
  # A second column giving a sample of the distribution (to accomplish this
  # we use the quantile function)
  distribution_df <- data.frame(
    arm=factor(as.vector(sapply(1:mab$arms, rep, times=1000))),
    qvals=as.vector(sapply(mab$qvals,
                      function(q) {qnorm(seq(0,1, length.out=1000), mean=q) }))
  )
  # Prepare the plot
  ggplot(distribution_df, aes(x=arm, y=qvals)) + geom_violin(fill="grey") +
    labs(title=paste0(mab$arms,"-armed bandit configuration"), x="Action",
         y="Reward distribution") +
    geom_hline(aes(yintercept=mab$initmean), linetype="dashed")
}

