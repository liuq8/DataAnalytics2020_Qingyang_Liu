library(cvTools)
library(robustbase)
data(coleman)
call <- call("lmrob", formula = Y ~ .)
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
# perform cross-validation
cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
#vary K and R
#look at cvfits, use densityplot, 
tuning <- list(tuning.psi=seq(2., 6., 20))
cvFitsLmrob <- cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
# look at output
cvFitsLmrob
# summarize
aggregate(cvFitsLmrob, summary)
library(MASS)
data(mammals)

mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals, mammals.glm)$delta)

(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)

# Leave-one-out cross-validation estimate without any extra model-fitting.
muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.esterr <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))

# leave-one-out and 11-fold cross-validation prediction error for
# the nodal data set.  Since the response is a binary variable 
# an appropriate cost function is
library(boot)
data(nodal)

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)
install.packages("robustbase")
install.packages("cvTools")
library("robustbase")
require(cvTools)
data("coleman")
set.seed(1234) # set seed for reproducibility
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine results into one object
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
# "cv" object
ncv(cvFitLts50)
nfits(cvFitLts50)
cvNames(cvFitLts50)
cvNames(cvFitLts50) <- c("improved", "initial")
fits(cvFitLts50)
cvFitLts50
# "cvSelect" object
ncv(cvFitsLts)
nfits(cvFitsLts)
cvNames(cvFitsLts)
cvNames(cvFitsLts) <- c("improved", "initial")
fits(cvFitsLts)
fits(cvFitsLts) <- 1:2
cvFitsLts
# assumes coleman, robustbase and cvTools
set.seed(4321) # set seed for reproducibility
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine results into one object
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
# summary of the results with the 50% subsets
aggregate(cvFitLts50, summary)
# summary of the combined results
aggregate(cvFitsLts, summary)
## evaluate MM regression models tuned for
## 80%, 85%, 90% and 95% efficiency
tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
# set up function call
call <- call("lmrob", formula = Y ~ .)
# perform cross-validation
cvFitsLmrob <- cvTuning(call, data = coleman,
                        y = coleman$Y, tuning = tuning, cost = rtmspe,
                        folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
# summary of results
aggregate(cvFitsLmrob, summary)
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 50)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
# combine results into one object
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
# plot results for the MM regression model
bwplot(cvFitLmrob)
# plot combined results
bwplot(cvFits)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine and plot results
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
bwplot(cvFitsLts)
## via model fit
# fit an MM regression model
fit <- lmrob(Y ~ ., data=coleman)
# perform cross-validation
cvFit(fit, data = coleman, y = coleman$Y, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)
## via model fitting function
# perform cross-validation
# note that the response is extracted from â€™dataâ€™ in
# this example and does not have to be supplied
cvFit(lmrob, formula = Y ~ ., data = coleman, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)
## via function call
# set up function call
call <- call("lmrob", formula = Y ~ .)
# perform cross-validation
cvFit(call, data = coleman, y = coleman$Y, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)
set.seed(2143) # set seed for reproducibility
cvFolds(20, K = 5, type = "random")
cvFolds(20, K = 5, type = "consecutive")
cvFolds(20, K = 5, type = "interleaved")
cvFolds(20, K = 5, R = 10)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, K = 5, R = 10,
                  fit = "both", trim = 0.1, seed = 1234)
# compare original and reshaped object
cvFitLts
cvReshape(cvFitLts)
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
# compare cross-validation results
cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine and plot results
cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
## evaluate MM regression models tuned for 85% and 95% efficiency
tuning <- list(tuning.psi = c(3.443689, 4.685061))
## via model fitting function
# perform cross-validation
# note that the response is extracted from â€™dataâ€™ in
# this example and does not have to be supplied
cvTuning(lmrob, formula = Y ~ ., data = coleman, tuning = tuning,
         cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
         seed = 1234)
## via function call
# set up function call
call <- call("lmrob", formula = Y ~ .)
# perform cross-validation
cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning,6 densityplot.cv
         cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
         seed = 1234)
set.seed(1234) # set seed for reproducibility
# set up function call for an MM regression model
call <- call("lmrob", formula = Y ~ .)
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
# perform cross-validation
cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe,
       folds = folds, costArgs = list(trim = 0.1))
set.seed(1234) # set seed for reproducibility
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 50)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
# combine results into one object
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
# plot results for the MM regression model
densityplot(cvFitLmrob)
# plot combined results
densityplot(cvFits)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine and plot results
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
densityplot(cvFitsLts)
set.seed(1234) # set seed for reproducibility
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
# combine and plot results
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
dotplot(cvFits)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine and plot results
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
dotplot(cvFitsLts)
set.seed(1234) # set seed for reproducibility
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 50)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
# combine results into one object
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
# plot results for the MM regression model
plot(cvFitLmrob, method = "bw")
plot(cvFitLmrob, method = "density")
# plot combined results
plot(cvFits, method = "bw")
plot(cvFits, method = "density")
plot(cvFits, method = "xy")
plot(cvFits, method = "dot")
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# combine results into one object
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
# plot combined results
plot(cvFitsLts, method = "bw")
plot(cvFitsLts, method = "density")
plot(cvFitsLts, method = "xy")
plot(cvFitsLts, method = "dot")
set.seed(1234) # set seed for reproducibility
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
repCV(fitLm, cost = rtmspe, folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman)
repCV(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
repCV(fitLts, cost = rtmspe, folds = folds, trim = 0.1)
repCV(fitLts, cost = rtmspe, folds = folds,
      fit = "both", trim = 0.1)
set.seed(1234) # set seed for reproducibility
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)
# combine results into one object
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
# summary of the results with the 50% subsets
summary(cvFitLts50)
# summary of the combined results
summary(cvFitsLts)
## evaluate MM regression models tuned for
## 80%, 85%, 90% and 95% efficiency
tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
# set up function call
call <- call("lmrob", formula = Y ~ .)
# perform cross-validation
cvFitsLmrob <- cvTuning(call, data = coleman,
                        y = coleman$Y, tuning = tuning, cost = rtmspe,
                        folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
# summary of results
summary(cvFitsLmrob)
set.seed(1234) # set seed for reproducibility
## set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
## compare LS, MM and LTS regression
# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)
# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)
# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)
# combine and plot results
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
xyplot(cvFits)
## compare raw and reweighted LTS estimators for
## 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
# combine and plot results
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
xyplot(cvFitsLts)
## evaluate MM regression models tuned for
## 80%, 85%, 90% and 95% efficiency
tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
# perform cross-validation
cvFitsLmrob <- cvTuning(fitLmrob$call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
# plot results
xyplot(cvFitsLmrob)
require(boot)
# leave-one-out and 6-fold cross-validation prediction error for 
# the mammals data set.
data(mammals, package="MASS")
mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals, mammals.glm)$delta)
(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)

# As this is a linear model we could calculate the leave-one-out 
# cross-validation estimate without any extra model-fitting.
muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.err <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))


# leave-one-out and 11-fold cross-validation prediction error for 
# the nodal data set.  Since the response is a binary variable an
# appropriate cost function is
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)
