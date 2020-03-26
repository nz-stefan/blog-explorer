################################################################################
# STM utils copied from https://github.com/mikajoh/tidystm
#
# Author: https://github.com/mikajoh/tidystm
# Created: 2020-03-11 10:04:19
################################################################################


#' Extract effect of covariates on topics
#'
#' Extracts the effect of a covariate on a set of topics selected by
#' the user. Different effect types available depending on type of
#' covariate. Before running this, the user should run a function to
#' simulate necessary confidence intervals. See
#' \code{link{estimateEffect}}.
#'
#' @import stm
#' @importFrom stats quantile sd
#'
#' @param x Output of estimateEffect, which calculates simulated betas
#'   for plotting or extraction.
#' @param covariate String of the name of the main covariate of
#'   interest. Must be enclosed in quotes. All other covariates within
#'   the formula specified in estimateEffect will be kept at their
#'   median.
#' @param model Model output, only necessary if labeltype is "prob",
#'   "frex", "score", or "lift". Models with more than one spline
#'   cannot be used for extract.estimateEffect.
#' @param topics Topics to plot.
#' @param method Method used for plotting.  "pointestimate" estimates
#'   mean topic proportions for each value of the covariate.
#'   "difference" estimates the mean difference in topic proportions
#'   for two different values of the covariate (cov.value1 and
#'   cov.value2 must be specified). "continuous" estimates how topic
#'   proportions vary over the support of a continuous covariate.
#' @param cov.value1 For method "difference", the value or set of
#'   values of interest at which to set the covariate. In the case of
#'   calculating a treatment/control contrast, set the treatment to
#'   cov.value1.
#' @param cov.value2 For method "difference", the value or set of
#'   values which will be set as the comparison group.  cov.value1 and
#'   cov.value2 must be vectors of the same length.
#' @param moderator When two terms are interacted and one variable in
#'   the interaction is the covariate of interest, the user can
#'   specify the value of the interaction with moderator.value, and
#'   the name of the moderator with moderator.
#' @param moderator.value When two terms are interacted and one
#'   variable in the interaction is the covariate of interest, the
#'   user can specify the value of the interaction term.
#' @param npoints Number of unique points to use for simulation along
#'   the support of a continuous covariate. For method "continuous"
#'   only.
#' @param nsims Number of simulations for estimation.
#' @param n Number of words to print if "prob", "score", "lift", or
#'   "frex" is chosen.  to signal how far the function have come.
#' @param ci.level Confidence level for confidence intervals.
#' @param frexw If "frex" labeltype is used, this will be the frex
#'   weight.
#' @param custom.labels A vector of custom.labels if labeltype is
#'   equal to "custom".
#' @param labeltype Determines the labeltype for the topics. The
#'   default is "number" which prints the topic number. Other options
#'   are "prob", which prints the highest probability words, "score",
#'   "lift", and "frex", from labeltopics (see
#'   \code{stm::labeltopics()} for more details). The user can also
#'   select "custom" for custom labels, which should be inputted under
#'   custom.labels. Labels appear in the legend for continous
#'   covariates.
#'
#' @examples
#' \dontrun{
#' prep <- estimateEffect(1:3 ~ treatment, gadarianFit, gadarian)
#' effect <- extract.estimateEffect(prep, "treatment", model = gadarianFit, method = "pointestimate")
#' }
#' @export

extract.estimateEffect <- function(x, covariate, model = NULL,
                                   topics = x$topics,
                                   method = "pointestimate",
                                   cov.value1 = NULL, cov.value2 = NULL,
                                   moderator = NULL, moderator.value = NULL,
                                   npoints = 100, nsims = 100, ci.level = .95,
                                   custom.labels = NULL, labeltype = "numbers",
                                   n = 7, frexw = .5) {
  
  cthis <- stm:::produce_cmatrix(prep = x,
                                 covariate = covariate,
                                 method = method,
                                 cov.value1 = cov.value1,
                                 cov.value2 = cov.value2,
                                 moderator = moderator,
                                 moderator.value = moderator.value)
  simbetas <- stm:::simBetas(parameters = x$parameters,
                             nsims = nsims)
  
  uvals <- cthis$cdata[[covariate]]
  offset <- (1 - ci.level) / 2
  labels <- stm:::createLabels(labeltype = labeltype,
                               covariate = covariate,
                               method = method,
                               cdata = cthis$cdata,
                               cov.value1 = cov.value1,
                               cov.value2 = cov.value2,
                               model = model,
                               n = n,
                               topics = x$topics,
                               custom.labels = custom.labels,
                               frexw = frexw)
  
  out <- lapply(topics, function(i) {
    
    sims <- cthis$cmatrix %*% t(simbetas[[which(x$topics == i)]])
    
    if (method == "difference") {
      
      diff <- sims[1, ] - sims[2, ]
      out_inner <- data.frame(method = method,
                              topic = i,
                              covariate = covariate,
                              covariate.value = paste0(cov.value1, "-", cov.value2),
                              estimate = mean(diff),
                              std.error = sd(diff),
                              ci.level = ci.level,
                              ci.lower = quantile(diff, offset),
                              ci.upper = quantile(diff, 1 - offset),
                              label = labels[which(x$topics == i)])
      
    } else {
      
      out_inner <- data.frame(method = method,
                              topic = i,
                              covariate = covariate,
                              covariate.value = uvals,
                              estimate = apply(sims, 1, mean),
                              std.error = apply(sims, 1, sd),
                              ci.level = ci.level,
                              ci.lower = apply(sims, 1, quantile, probs = offset),
                              ci.upper = apply(sims, 1, quantile, probs = (1 - offset)),
                              label = labels[which(x$topics == i)])
      
    }
    
    if (!is.null(moderator)) {
      out_inner$moderator <- moderator
      out_inner$moderator.value <- moderator.value
    }
    
    rownames(out_inner) <- NULL
    return(out_inner)
    
  })
  out <- do.call("rbind", out)
  
  return(out)
}