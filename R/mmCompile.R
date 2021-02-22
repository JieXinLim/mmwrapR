#' Compile Mplus LPA output
#'
#' @param path working directory of the Mplus LPA output files to be compiled
#'
#' @return
#' @export
#'
#' @examples
#' mmCompile(path  = getwd())
mmCompile <- function(path = NULL){

  if (is.null(path)){
    path <- getwd()
  }
  setwd(path)

  # determine the number of classes fitted
  outputfiles <- as.numeric(sub("\\D*(\\d+).*", "\\1", list.files(pattern = ".out")))
  classes <- sort(unique(outputfiles))[-1]

  fit <- data.frame(class = as.numeric(), AIC = as.numeric(), BIC = as.numeric(), aBIC = as.numeric(),
                    Entropy = as.numeric(), VLMR_PValue = as.numeric(), LMR_PValue = as.numeric(), BLRT_PValue = as.numeric(),
                    Maxima_warning = as.logical(), BLRT_warning = as.logical())

  model <- readModels("class1.out", what = "summaries")
  fit[1, ] <- c(1, model$summaries[c("AIC", "BIC", "aBIC")], NA, NA, NA, NA, NA, NA)

  for (class in classes){

    file_name <- paste0("class", class, "_step2.out")

    if (isTRUE(file.exists(file_name))){

      model <- readModels(file_name, what = "summaries")

      T11_VLMR_PValue <- ifelse(is.null(model$summaries$T11_VLMR_PValue), NA, model$summaries$T11_VLMR_PValue)
      T11_LMR_PValue <- ifelse(is.null(model$summaries$T11_LMR_PValue), NA, model$summaries$T11_LMR_PValue)
      BLRT_PValue <- ifelse(is.null(model$summaries$BLRT_PValue), NA, model$summaries$BLRT_PValue)

      tech14 <- readLines(file_name)
      BLRT_warn <- any(grepl("WARNING:", tech14[seq(which(tech14 == "TECHNICAL 14 OUTPUT") + 2, which(tech14 == "PLOT INFORMATION"))]))


      fit[class, ] <- c(class, model$summaries[c("AIC", "BIC", "aBIC", "Entropy")], T11_VLMR_PValue, T11_LMR_PValue, BLRT_PValue, FALSE, BLRT_warn)

    } else{

      fit[class, ] <- c(class, rep(NA, 7), TRUE, NA)

    }
  }

  fit <- fit[which(!is.na(fit$class)), ]
  return(fit)
}
