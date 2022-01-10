## ---- message=FALSE-----------------------------------------------------------
library(surveyCV)
library(survey)
library(ISLR)

## -----------------------------------------------------------------------------
data(Auto)
with(Auto, plot(horsepower, mpg, pch = '.'))

## -----------------------------------------------------------------------------
# Test the general cv.svy() function

# The first line's results are usually around:
# linear lm:      mean=24.3, se=1.45
# quadratic lm:   mean=19.3, se=1.38
cv.svy(Auto, c("mpg~poly(horsepower,1, raw = TRUE)",
               "mpg~poly(horsepower,2, raw = TRUE)"),
       nfolds = 10)
cv.svy(Auto, c("mpg~poly(horsepower,1,raw=TRUE)",
               "mpg~poly(horsepower,2,raw=TRUE)"),
       nfolds = 10, clusterID = "year")
cv.svy(Auto, c("mpg~poly(horsepower,1, raw = TRUE)",
               "mpg~poly(horsepower,2, raw = TRUE)"),
       nfolds = 10, strataID = "year")

## -----------------------------------------------------------------------------
# Test the cv.svydesign() function, which takes a svydesign object and formulas
auto.srs.svy <- svydesign(ids = ~0,
                          data = Auto)
auto.clus.svy <- svydesign(ids = ~year,
                           data = Auto)
auto.strat.svy <- svydesign(ids = ~0,
                            strata = ~year,
                            data = Auto)

cv.svydesign(formulae = c("mpg~poly(horsepower,1, raw = TRUE)",
                          "mpg~poly(horsepower,2, raw = TRUE)",
                          "mpg~poly(horsepower,3, raw = TRUE)"),
             design_object = auto.srs.svy, nfolds = 10)
cv.svydesign(formulae = c("mpg~poly(horsepower,1, raw = TRUE)",
                          "mpg~poly(horsepower,2, raw = TRUE)",
                          "mpg~poly(horsepower,3, raw = TRUE)"),
             design_object = auto.clus.svy, nfolds = 10)
cv.svydesign(formulae = c("mpg~poly(horsepower,1, raw = TRUE)", 
                          "mpg~poly(horsepower,2, raw = TRUE)",
                          "mpg~poly(horsepower,3, raw = TRUE)"), 
             design_object = auto.strat.svy, nfolds = 10)

## -----------------------------------------------------------------------------
# Test the cv.svyglm() function, which takes one svyglm object
srs.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.srs.svy)
clus.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.clus.svy)
strat.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.strat.svy)

cv.svyglm(glm = srs.model, nfolds = 10)
cv.svyglm(glm = clus.model, nfolds = 10)
cv.svyglm(glm = strat.model, nfolds = 10)

## -----------------------------------------------------------------------------
seed = 20210708

set.seed(seed)
cv.svy(Auto, "mpg~horsepower+I(horsepower^2)+I(horsepower^3)",
       nfolds = 10)

set.seed(seed)
cv.svydesign(formulae = "mpg~horsepower+I(horsepower^2)+I(horsepower^3)",
             design_object = auto.srs.svy, nfolds = 10)

srs.model <- svyglm(mpg~horsepower+I(horsepower^2)+I(horsepower^3), design = auto.srs.svy)
set.seed(seed)
cv.svyglm(glm = srs.model, nfolds = 10)

## -----------------------------------------------------------------------------
Auto$isAmerican <- Auto$origin == 1
with(Auto, plot(horsepower, isAmerican))
cv.svy(Auto, c("isAmerican~horsepower",
               "isAmerican~horsepower+I(horsepower^2)",
               "isAmerican~horsepower+I(horsepower^2)+I(horsepower^3)"),
       nfolds = 10, method = "logistic")

## -----------------------------------------------------------------------------
# Should stop early because method isn't linear or logistic
try(cv.svy(Auto, "mpg~horsepower+I(horsepower^2)+I(horsepower^3)",
           nfolds = 10,
           method = "abcde"))

# Should try to run but crash because response variable isn't 0/1
try(cv.svy(Auto, "mpg~horsepower+I(horsepower^2)+I(horsepower^3)",
           nfolds = 10,
           method = "logistic"))

