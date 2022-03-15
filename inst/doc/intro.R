## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-packages, message=FALSE--------------------------------------------
# We set a random seed in this vignette only to ensure
#   that our discussion will match the CV output;
# otherwise, each time we reran the vignette, we'd get different CV folds
set.seed(2022)

library(surveyCV)
data("NSFG_data")
library(survey)
data("api")

## -----------------------------------------------------------------------------
#stratified sample
cv.svy(apistrat, c("api00~ell",
                   "api00~ell+meals",
                   "api00~ell+meals+mobility"),
       nfolds = 5, strataID = "stype", weightsID = "pw", fpcID = "fpc")

## -----------------------------------------------------------------------------
# one-stage cluster sample
cv.svy(apiclus1, c("api00~ell",
                   "api00~ell+meals",
                   "api00~ell+meals+mobility"),
       nfolds = 5, clusterID = "dnum", weightsID = "pw", fpcID = "fpc")

## -----------------------------------------------------------------------------
# simple random sample
cv.svy(apisrs, c("api00~ell",
                 "api00~ell+meals",
                 "api00~ell+meals+mobility"),
       nfolds = 5, fpcID = "fpc")

## -----------------------------------------------------------------------------
# complex sample from NSFG
library(splines)
cv.svy(NSFG_data, c("income ~ ns(age, df = 1)",
                    "income ~ ns(age, df = 2)",
                    "income ~ ns(age, df = 3)",
                    "income ~ ns(age, df = 4)",
                    "income ~ ns(age, df = 5)",
                    "income ~ ns(age, df = 6)"),
       nfolds = 4,
       strataID = "strata", clusterID = "SECU",
       nest = TRUE, weightsID = "wgt")

## -----------------------------------------------------------------------------
#stratified sample
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc)
cv.svydesign(formulae = c("api00~ell",
                          "api00~ell+meals",
                          "api00~ell+meals+mobility"),
             design_object = dstrat, nfolds = 5)

# one-stage cluster sample
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
cv.svydesign(formulae = c("api00~ell",
                          "api00~ell+meals",
                          "api00~ell+meals+mobility"),
             design_object = dclus1, nfolds = 5)

# simple random sample
dsrs <- svydesign(id = ~1, data = apisrs, fpc = ~fpc)
cv.svydesign(formulae = c("api00~ell",
                          "api00~ell+meals",
                          "api00~ell+meals+mobility"),
             design_object = dsrs, nfolds = 5)

## -----------------------------------------------------------------------------
NSFG.svydes <- svydesign(id = ~SECU, strata = ~strata, nest = TRUE,
                         weights = ~wgt, data = NSFG_data)
cv.svydesign(formulae = c("income ~ ns(age, df = 1)",
                          "income ~ ns(age, df = 2)",
                          "income ~ ns(age, df = 3)",
                          "income ~ ns(age, df = 4)",
                          "income ~ ns(age, df = 5)",
                          "income ~ ns(age, df = 6)"),
             design_object = NSFG.svydes, nfolds = 4)

## -----------------------------------------------------------------------------
#stratified sample
glmstrat <- svyglm(api00 ~ ell+meals+mobility, design = dstrat)
cv.svyglm(glmstrat, nfolds = 5)

# one-stage cluster sample
glmclus1 <- svyglm(api00 ~ ell+meals+mobility, design = dclus1)
cv.svyglm(glmclus1, nfolds = 5)

# simple random sample
glmsrs <- svyglm(api00 ~ ell+meals+mobility, design = dsrs)
cv.svyglm(glmsrs, nfolds = 5)

## -----------------------------------------------------------------------------
NSFG.svyglm <- svyglm(income ~ ns(age, df = 3), design = NSFG.svydes)
cv.svyglm(glm_object = NSFG.svyglm, nfolds = 4)

## -----------------------------------------------------------------------------
NSFG.svyglm.logistic <- svyglm(LBW ~ ns(age, df = 3), design = NSFG.svydes,
                               family = quasibinomial())
cv.svyglm(glm_object = NSFG.svyglm.logistic, nfolds = 4)

## -----------------------------------------------------------------------------
cv.svydesign(formulae = c("LBW ~ ns(age, df = 1)",
                          "LBW ~ ns(age, df = 2)",
                          "LBW ~ ns(age, df = 3)",
                          "LBW ~ ns(age, df = 4)",
                          "LBW ~ ns(age, df = 5)",
                          "LBW ~ ns(age, df = 6)"),
             design_object = NSFG.svydes, nfolds = 4,
             method = "logistic")

cv.svy(NSFG_data, c("LBW ~ ns(age, df = 1)",
                    "LBW ~ ns(age, df = 2)",
                    "LBW ~ ns(age, df = 3)",
                    "LBW ~ ns(age, df = 4)",
                    "LBW ~ ns(age, df = 5)",
                    "LBW ~ ns(age, df = 6)"),
       nfolds = 4,
       strataID = "strata", clusterID = "SECU",
       nest = TRUE, weightsID = "wgt",
       method = "logistic")

## ---- eval = FALSE------------------------------------------------------------
#  # Based on example("rpms"):
#  #   model the mean of retirement account value `IRAX` among households with
#  #   reported retirement account values > 0,
#  #   predicted from householder education, age, and urban/rural location
#  library(rpms)
#  data(CE)
#  
#  # Generate fold IDs that account for clustering in the survey design
#  # for the IRAX>0 subset of the CE dataset
#  nfolds <- 5
#  CEsubset <- CE[which(CE$IRAX > 0), ]
#  CEsubset$.foldID <- folds.svy(CEsubset, nfolds = nfolds, clusterID = "CID")
#  
#  # Use CV to tune the bin_size parameter of rpms_forest()
#  bin_sizes <- c(10, 20, 50, 100, 250, 500)
#  
#  # Create placeholder for weighted Sums of Squared Errors
#  SSEs <- rep(0, length(bin_sizes))
#  
#  for(ff in 1:nfolds) { # For every fold...
#    # Use .foldID to split data into training and testing sets
#    train <- subset(CEsubset, .foldID != ff)
#    test  <- subset(CEsubset, .foldID == ff)
#  
#    for(bb in 1:length(bin_sizes)) { # For every value of the tuning parameter...
#      # Fit a new model
#      rf <- rpms_forest(IRAX ~ EDUCA + AGE + BLS_URBN,
#                        data = train,
#                        weights = ~FINLWT21, clusters = ~CID,
#                        bin_size = bin_sizes[bb], f_size = 50)
#      # Get predictions and squared errors
#      yhat <- predict(rf, newdata = test)
#      res2 <- (yhat - test$IRAX)^2
#      # Sum up weighted SSEs, not MSEs yet,
#      # b/c cluster sizes may differ across folds and b/c of survey weights
#      SSEs[bb] <- SSEs[bb] + sum(res2 * test$FINLWT21)
#    }
#  }
#  
#  # Divide entire weighted sum by the sum of weights
#  MSEs <- SSEs / sum(CEsubset$FINLWT21)
#  
#  # Show results
#  cbind(bin_sizes, MSEs)
#  #>      bin_sizes         MSEs
#  #> [1,]        10 204246617270
#  #> [2,]        20 202870633392
#  #> [3,]        50 201393921358
#  #> [4,]       100 201085838446
#  #> [5,]       250 201825549231
#  #> [6,]       500 204155844501

## ---- eval = FALSE------------------------------------------------------------
#  CE.svydes <- svydesign(id = ~CID, weights = ~FINLWT21, data = CEsubset)
#  # Use update() to add variables to a svydesign object
#  CE.svydes <- update(CE.svydes,
#                      .foldID = folds.svydesign(CE.svydes, nfolds = nfolds))
#  # Now the fold IDs are available as CE.svydes$variables$.foldID
#  table(CE.svydes$variables$.foldID)
#  #>   1   2   3   4   5
#  #> 813 923 928 885 960

