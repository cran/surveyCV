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

