#' function to build models
#'
#' @param type define the type of model at this state
#' @param model_basis define the basis model
#' @param starts_initial init start
#' @param starts_final init final
#' @param class_num class number
#' @param varnames variable names
#' @param seed seed number
#' @param .data dataset
#' @param .usevar used variables
#' @param .control control variables
#'
#' @return
#' @export
#'
mmModel <- function(type = NULL, model_basis = NULL, ...){

  #starts_initial = NULL, starts_final = NULL, class_num = NULL, varnames = NULL, seed = NULL,

  switch(type,
         base = {
           # base model
           model <- mplusObject(TITLE = "1-class model",
                                VARIABLE = "CLASSES = c(1);",
                                ANALYSIS = " TYPE = MIXTURE;",
                                usevariable = usevar,
                                rdata = data)
           model$ANALYSIS <- paste(model$ANALYSIS, paste0(" PROCESS = ", control$processors, ";\n",
                                                          " STITER = ", control$stiterations, ";"), sep = "\n")
         },

         step1 = {
           # step 1 (model_basis = model_c1)
           model <- model_basis
           model$TITLE <- paste0(class_num, "-class model")
           model$VARIABLE <- paste0("CLASSES = c(", class_num, ")")
           model$ANALYSIS <- paste(model_basis$ANALYSIS, paste("STARTS =", starts_initial, starts_final, ";", sep = " "), sep = "\n")
         },

         step2Tech11 = {
           # step 2 (TECH11 only; model_basis = model_step1)
           model <- model_step1
           model$ANALYSIS <- paste0(" TYPE = MIXTURE;\n OPTSEED = ", seed, ";")
           model$PLOT <- paste0(" TYPE = PLOT3; \n SERIES = ", varnames[1], "-", varnames[2], " (*);")
           model$SAVEDATA <- paste0(" FILE = class_", class_num, ".csv; \n SAVE = cprob;")
           model$OUTPUT <- " TECH11;"
         },

         step2Tech14 = {
           # step2 (with TECH14; Model_basis = model_step2)
           model$ANALYSIS <- paste(model_step2$ANALYSIS, paste0(" LRTBOOT = ", .control$lrtboot, ";\n",
                                                                " LRTSTARTS = 0 0 ", .control$lrtstarts_initial, " ", .control$lrtstarts_final, ";"), sep = "\n")

           model$OUTPUT <- paste0(model$OUTPUT, " TECH14;")
         }
  )

  return(model)
}
