#' Function used to create sysdata.rda (i.e. LUT)
#'
#' @description
#' Function which has been used to create the LUT data of this package.
#' 
#' @name pck_lut
#' 
NULL

pck_lut <- function(){
  mthd_def_lst <- list(
    avNNet = list(fncs = caretFuncs,
                  tunegr = expand.grid(.size = seq(10, 20, 5),
                                       .decay = c(0,0.001,0.1),
                                       #.bag = seq(30, 50, 10)
                                       .bag = "TRUE"),
                  type = "prob"),
    ctree = list(fncs = caretFuncs,
                 tunegr = NULL,
                 type = "prob"),
    cubist = list(fncs = caretFuncs,
                  tunegr = expand.grid(.committees = seq(20, 80, 20),
                                       .neighbors = seq(3, 9, 3)),
                  type = "prob"),
    earth = list(fncs = caretFuncs,
                 tunegr = expand.grid(.nprune = seq(3, 18, 3),
                                      .degree = seq(1, 3, 0.5)),
                 type = "prob"),
    gam = list(fncs = gamFuncs,
               tunegr = expand.grid(.method = c("GCV.Cp", "GACV.Cp", "REML",
                                                "P-REML", "ML", "P-ML"),
                                    .select = c(TRUE, FALSE)),
               type = "skip"),
    gbm = list(fncs = caretFuncs,
               tunegr = expand.grid(.interaction.depth = seq(1, 7, 2),
                                    .n.trees = seq(100, 1000, 100),
                                    .shrinkage = c(0.01, 0.1),
                                    .n.minobsinnode = c(3,4,5)),
               type = "prob"),
    glm = list(fncs = lmFuncs,
               tunegr = NULL,
               type = "skip"),
    knn = list(fncs = caretFuncs,
               tunegr = expand.grid(.k = seq(3, 8, 1))),
    nnet = list(fncs = caretFuncs,
                tunegr = expand.grid(.size = seq(10, 20, 5),
                                     .decay = c(0,0.001,0.1)),
                type = "skip"),
    pcr = list(fncs = caretFuncs,
               tunegr = NULL,
               type = "skip"), #expand.grid(.ncomp = seq(1, 8, 1))),
    pls = list(fncs = caretFuncs,
               tunegr = NULL,
               type = "skip"), #expand.grid(.ncomp = seq(1, 8, 1))),
    rf = list(fncs = caretFuncs,
              tunegr = expand.grid(.mtry = seq(2, 8, 1)),
              type = "prob"),
    svmLinear = list(fncs = caretFuncs,
                     tunegr = expand.grid(.C = seq(0.25, 1, 0.25)),
                     type = "prob"),
    svmRadial = list(fncs = caretFuncs,
                     tunegr = expand.grid(.sigma = seq(0.05, 0.15, 0.01),
                                          .C = seq(0.25, 1.25, 0.25)),
                     type = "prob")
  )
  
  lut <- list(MTHD_DEF_LST = mthd_def_lst)
  usethis::use_data(lut, overwrite = TRUE, internal = TRUE)
}