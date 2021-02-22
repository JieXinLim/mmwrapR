#' Estimation specification for Mplus
#'
#' @param processors number of processors
#' @param stiterations number of initial stage iterations
#' @param starts_initial number of initial stage starts
#' @param starts_final number of final stage optimizations
#' @param LL_replication minimal number of replications for model log-likelihood
#' @param max_iteration maximum number of iterations for finding the optimal seed
#' @param lrtboot number of bootstrap draws for TECH14
#' @param lrtstarts_initial number of initial stage starts for TECH14
#' @param lrtstarts_final number of final stage optimizations for TECH14
#'
#' @return
#' @export
#'
#' @examples
#' mmControl()
mmControl <- function(processors = 2, stiterations = 50, starts_initial = 100, starts_final = 20, LL_replication = 5, max_iteration = 5, lrtboot = 100, lrtstarts_initial = 100, lrtstarts_final = 20){

  control <- list(processors = processors,
                  stiterations = stiterations,
                  starts_initial = starts_initial,
                  starts_final = starts_final,
                  LL_replication = LL_replication,
                  max_iteration = max_iteration,
                  lrtboot = lrtboot,
                  lrtstarts_initial = lrtstarts_initial,
                  lrtstarts_final = lrtstarts_final)
  control
}
