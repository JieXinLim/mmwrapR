#' Wrapper for latent profile analysis to obtain TECH11 and TECH14 outputs
#'
#' @param classes the range of latent classes to be modeled
#' @param data the data object to be used in the analysis
#' @param usevar the variables in the data object to be used in the analysis
#' @param TECH14 logical, is TECH14 BLRT output required
#' @param control Mplus estimation control parameters
#'
#' @return
#' @export
#'
#' @examples
#' mm(class = 2:3, data = iris, usevar = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
#'
mm <- function(classes, data, usevar = NULL, TECH14 = FALSE, control = mmControl()){

  if (min(classes) == 1){
    classes <- sort(classes)[-1]
  } else {
    classes <- classes
  }

  # create model
  model_c1 <- mplusObject(TITLE = "1-class model",
                          VARIABLE = "CLASSES = c(1);",
                          ANALYSIS = " TYPE = MIXTURE;",
                          usevariable = usevar,
                          rdata = data)
  model_c1$ANALYSIS <- paste(model_c1$ANALYSIS, paste0(" PROCESS = ", control$processors, ";\n",
                                                      " STITER = ", control$stiterations, ";"), sep = "\n")

  mplusModeler(model_c1, dataout = "mm_data.dat", modelout = "class1.inp", run = 1L, hashfilename = TRUE)

  varnames <- unlist(strsplit(readModels("class1.out", what = "input", quiet = TRUE)$input$variable$names, split = " "))
  varnames <- c(varnames[1], varnames[length(varnames)])

  for (class_num in classes){

    # step 1: find optimal seed
    seed <- NA
    iter <- 1
    starts_initial <- control$starts_initial
    starts_final <- control$starts_final

    while (is.na(seed) & iter <= control$max_iteration){

      # update model

      model_step1 <- model_c1
      model_step1$TITLE <- paste0(class_num, "-class model")
      model_step1$VARIABLE <- paste0("CLASSES = c(", class_num, ")")
      model_step1$ANALYSIS <- paste(model_c1$ANALYSIS, paste("STARTS =", starts_initial, starts_final, ";", sep = " "), sep = "\n")

      # run model
      mplusModeler(model_step1, dataout = "mm_data.dat", modelout = paste0("class", class_num, "_step1.inp"), run = 1L, hashfilename = TRUE)

      # read model
      model <- readLines(paste0("class", class_num, "_step1.out"))

      # extract LL & seed information
      upper <- which(model == "Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:")
      lower <- upper + control$LL_replication + 1
      LL <- data.frame(matrix(do.call(rbind, strsplit(model[seq(upper + 2, lower)], " +")), ncol = 4, byrow = FALSE)[, -1])
      names(LL) <- c("LL", "seed", "init")
      LL$LL <- round(as.numeric(LL$LL), digits = 3)
      LL$seed <- as.numeric(LL$seed)
      LL$init <- as.numeric(LL$init)

      if (any(table(LL$LL) >= control$LL_replication)){
        seed <- LL$seed[1]
      }

      # update init and final (just in case we need in the next iteration)
      starts_initial <- starts_initial*2
      starts_final <- starts_initial/5

      # update iteration number
      iter <- iter + 1
    }

    if (is.na(seed)){
      next

    } else{
      # Step 2: engage TECH11 and TECH14
      model_step2 <- model_step1
      model_step2$ANALYSIS <- paste(model_c1$ANALYSIS, paste0(" OPTSEED = ", seed, ";"), sep = "\n")
      model_step2$PLOT <- paste0(" TYPE = PLOT3; \n SERIES = ", varnames[1], "-", varnames[2], " (*);")
      model_step2$SAVEDATA <- paste0(" FILE = class_", class_num, ".csv; \n SAVE = cprob;")
      model_step2$OUTPUT <- " TECH11;"

      if (isTRUE(TECH14)){
        model_step2$ANALYSIS <- paste(model_step2$ANALYSIS, paste0(" LRTBOOT = ", control$lrtboot, ";\n",
                                                                   " LRTSTARTS = 0 0 ", control$lrtstarts_initial, " ", control$lrtstarts_final, ";"), sep = "\n")

        model_step2$OUTPUT <- paste0(model_step2$OUTPUT, " TECH14;")
      }

      # run model
      mplusModeler(model_step2, dataout = "mm_data.dat", modelout = paste0("class", class_num, "_step2.inp"), run = 1L, hashfilename = TRUE)
    }
  }
}
