########################################
# Description of the dglars() function #
########################################

###################################################
library("dglars")

###################################################
set.seed(321)
n <- 100
p <- 4
s <- 2
X <- matrix(rnorm(n * p), n, p)
bs <- rep(1, s)
Xs <- X[, 1:s]
eta <- drop(1 + drop(Xs %*% bs))
mu <- binomial()$linkinv(eta)
y <- rbinom(n, 1, mu)
dataset <- data.frame(y = y, X = X)
out_dglasso_pc <- dglars(y ~ ., family = "binomial", data = dataset)

###################################################
out_dglasso_pc <- dglars(y ~ X.1 + X.2 + X.3 + X.4, family = "binomial", data = dataset)
class(out_dglasso_pc)

###################################################
out_dglars_pc <- dglars(y ~ ., family = "binomial", data = dataset, control = list(method = "dgLARS"))
out_dglasso_pc

###################################################
g <- out_dglasso_pc$g
coef_path <- coef(out_dglasso_pc)
path <- rbind(g, coef_path)[, 1:10]
print(path, digits = 3)

###################################################
summary(out_dglasso_pc, k = "BIC", complexity = "df")
summary(out_dglasso_pc, k = "AIC", complexity = "df")

###################################################
out_dglasso_pc2 <- dglars(y ~ ., family = "binomial", data = dataset, control = list(dg_max = 0.1))
par(mfrow = c(2, 3))
plot(out_dglasso_pc2, k = "BIC", complexity = "df")
plot(out_dglasso_pc2, k = "AIC", complexity = "df")


####################################################
# dglars: Comparison between PC and ccd algorithms #
####################################################

###################################################
set.seed(321)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
b <- 1:2
eta <- b[1] + X[,1] * b[2]
mu <- binomial()$linkinv(eta)
y <- rbinom(n, 1, mu)
dataset <- data.frame(y = y,X = X)

###################################################

out_dglasso_pc <- dglars(y ~ ., family = "binomial", data = dataset)
out_dglasso_ccd <- dglars(y ~ ., family = "binomial", data = dataset, control = list(algorithm = "ccd"))

###################################################

bic_pc <- out_dglasso_pc$dev + log(n) * out_dglasso_pc$df
g_pc <- out_dglasso_pc$g
bic_ccd <- out_dglasso_ccd$dev + log(n) * out_dglasso_ccd$df
g_ccd <- out_dglasso_ccd$g
plot(g_pc, bic_pc, type = "o", pch = 20, lty = 2, ylim = c(110, 145), xlab = expression(gamma), ylab = "BIC", main = "Model Selection Criterion")
points(g_ccd, bic_ccd, type = "o", pch = 20, lty = 2, col = 2)
legend(x = 0, y = 145, legend = c("PC algorithm", "ccd algorithm"), col = 1:2, lty = 2)

###################################################

out_dglasso_pc <- dglars(y ~ ., family = "binomial", data = dataset, control = list(dg_max = 1.0e-03, np = 1.0e+04))
out_dglasso_ccd <- dglars(y ~ ., family = "binomial", data = dataset, control = list(algorithm = "ccd", np = 1.0e+04))

###################################################

bic_pc <- out_dglasso_pc$dev + log(n) * out_dglasso_pc$df
g_pc <- out_dglasso_pc$g
bic_ccd <- out_dglasso_ccd$dev + log(n) * out_dglasso_ccd$df
g_ccd <- out_dglasso_ccd$g
plot(g_pc, bic_pc, type = "l", pch = 20, lty = 2, ylim = c(110,145), xlab = expression(gamma), ylab = "BIC", main = "Model Selection Criterion")
points(g_ccd, bic_ccd, type = "l", pch = 1, lty = 2, col = 2)
legend(x = 0, y = 145, legend = c("PC algorithm", "ccd algorithm"), col = c(1,2), lty = 2)

###################################################
set.seed(321)
n <- 100
p <- c(10, 100)
s <- 2
X <- matrix(rnorm(n * p[2]), n, p[2])
bs <- rep(2, s)
Xs <- X[, 1:s]
eta <- drop(1 + drop(Xs %*% bs))
mu <- binomial()$linkinv(eta)
y <- rbinom(n, 1, mu)
results.time <- array(0, dim = c(2, 3, 2), dimnames = list(algorithm = c("pc", "ccd"), time = c("user", "system", "elapsed"), p = p))
results.time["pc",, "10"] <- system.time(dglars.fit(X = X[, 1:p[1]], y = y, family = "binomial", control = list(algorithm = "pc", eps = 1e-3,g0 = 0.1)))[1:3]
results.time["ccd",, "10"] <- system.time(dglars.fit(X = X[, 1:p[1]], y = y, family = "binomial", control = list(algorithm = "ccd", eps = 1e-3,g0 = 0.1)))[1:3]
results.time["pc",, "100"] <- system.time(dglars.fit(X = X[, 1:p[2]], y = y, family = "binomial", control = list(algorithm = "pc", eps = 1e-3,g0 = 0.1)))[1:3]
results.time["ccd",, "100"] <- system.time(dglars.fit(X = X[, 1:p[2]], y = y, family = "binomial", control = list(algorithm = "ccd", eps = 1e-3,g0 = 0.1)))[1:3]
results.time

###################################################
set.seed(321)
nsim <- 100
n <- c(100, 200, 300)
p <- c(1000, 800, 600, 400, 200, 100, 50, 25)
g0 <- 0.2
s <- 3
bs <- rep(2, s+1)
ris <- array(0, dim = c(nsim, length(p), length(n), 2), dimnames = list(sim = 1:nsim, p = p, n = n, mthd = c("ccd", "pc")))
for(h in 1:length(n)){
  for(j in 1:length(p)){
    pj <- p[j]
    X <- matrix(rnorm(n[h] * pj), n[h], pj)
    eta <- bs[1] + drop(X[, 1:s] %*% bs[-1])
    mu <- binomial()$linkinv(eta)
    for(i in 1:nsim){
      y <- rbinom(n[h], 1, mu)
      time1 <- system.time(dglars.fit(X = X, y = y, family = "binomial", control = list(algorithm = "ccd", g0 = g0)))[3]
      time2 <- system.time(dglars.fit(X = X, y = y, family = "binomial", control = list(algorithm = "pc", g0 = g0)))[3]
      ris[i, j, h, "ccd"] <- time1
      ris[i, j, h, "pc"] <- time2
      cat("simulation ", i, "with p =", pj, "and n =", n[h], "completed\n")
    }
  }
}
summary.time <- array(0, dim = c(length(n), length(p), 2, 2), dimnames = list(n = n, p = p, c("ccd", "pc"), c("mean", "sd")))
for(h in 1:length(n)){
  for(j in 1:length(p)){
    summary.time[h, j, "ccd", "mean"] <- mean(ris[ , j, h, "ccd"])
    summary.time[h, j, "ccd", "sd"] <- sd(ris[ , j, h, "ccd"])
    summary.time[h, j, "pc", "mean"] <- mean(ris[ , j, h, "pc"])
    summary.time[h, j, "pc", "sd"] <- sd(ris[ , j, h, "pc"])
  }
}
summary.time <- as.table(summary.time)
matplot(p, summary.time[3, , , "mean"], col = 1, type = "b", pch = 20, lty = c(1, 2), main = "Scenario (a) with N = 300", xlab = "Number of predictors p", ylab = "CPU times")
legend("topleft", legend = c("ccd algorithm", "PC algorithm"), lty = c(1, 2))

###################################################
library("MASS")
set.seed(321)
nsim <- 100
n <- c(100, 200, 300)
p <- c(1000, 800, 600, 400, 200, 100, 50, 25)
g0 <- 0.2
s <- 3
r <- 0.9
bs <- rep(2, s+1)
ris <- array(0, dim = c(nsim, length(p), length(n), 2), dimnames = list(sim = 1:nsim, p = p, n = n, mthd = c("ccd", "pc")))
for(h in 1:length(n)){
  for(j in 1:length(p)){
    pj <- p[j]
    S <- outer(1:pj, 1:pj, function(i1, i2) r^abs(i1 - i2))
    X <- mvrnorm(n[h], rep(0, pj), S)
    eta <- bs[1] + drop(X[, 1:s] %*% bs[-1])
    mu <- binomial()$linkinv(eta)
    for(i in 1:nsim){
      y <- rbinom(n[h], 1, mu)
      time1 <- system.time(dglars.fit(X = X, y = y, family = "binomial", control = list(algorithm = "ccd", g0 = g0)))[3]
      time2 <- system.time(dglars.fit(X = X, y = y, family = "binomial", control = list(algorithm = "pc", g0 = g0)))[3]
      ris[i, j, h, "ccd"] <- time1
      ris[i, j, h, "pc"] <- time2
      cat("simulation ", i, "with p =", pj, "and n =", n[h], "completed\n")
    }
  }
}
summary.time <- array(0, dim = c(length(n), length(p), 2, 2), dimnames = list(n = n, p = p, c("ccd", "pc"), c("mean", "sd")))
for(h in 1:length(n)){
  for(j in 1:length(p)){
    summary.time[h, j, "ccd", "mean"] <- mean(ris[ , j, h, "ccd"])
    summary.time[h, j, "ccd", "sd"] <- sd(ris[ , j, h, "ccd"])
    summary.time[h, j, "pc", "mean"] <- mean(ris[ , j, h, "pc"])
    summary.time[h, j, "pc", "sd"] <- sd(ris[ , j, h, "pc"])
  }
}
summary.time <- as.table(summary.time)
matplot(p,summary.time[3, , , "mean"], col = 1, type = "b", pch = 20, lty = c(1, 2), main = "Scenario (b) with N = 300", xlab = "Number of predictors p", ylab = "CPU times")
legend("topleft", legend = c("ccd algorithm", "PC algorithm"), lty = c(1, 2))

#####################################
# Description of the gdf() function #
#####################################

###################################################
set.seed(321)
n <- 100
p <- 4
s <- 2
X <- matrix(rnorm(n * p), n, p)
bs <- rep(1, s)
Xs <- X[, 1:s]
eta <- drop(1 + drop(Xs %*% bs))
mu <- binomial()$linkinv(eta)
y <- rbinom(n, 1, mu)
dataset <- data.frame(y = y,X = X)

out_dglasso_pc <- dglars(y ~ ., family = "binomial", data = dataset, control = list(g0 = 0))
df <- out_dglasso_pc$df
bh <- coef(out_dglasso_pc)
gdfh <- gdf(out_dglasso_pc)
complexity <- rbind(df,gdfh)
rownames(complexity) <- c("df","gdf")
print(complexity[, 1:10], digits = 3)

###################################################
summary(out_dglasso_pc, k = "AIC", complexity = "gdf")

##############################################################
# Description of the cvdglars() and cvdglars.fit() functions #
##############################################################

###################################################
set.seed(321)
n <- 100
p <- 100
s <- 2
X <- matrix(rnorm(n * p), n, p)
bs <- rep(0.5, s)
Xs <- X[, 1:s]
eta <- drop(0.5 + drop(Xs %*% bs))
mu <- poisson()$linkinv(eta)
y <- rpois(n, mu)
dataset <- data.frame(y = y, X = X)

out_cvdglasso_pc <- cvdglars(y ~ ., family = "poisson", data = dataset, control = list(g0 = 0.1, eps = 1.0e-03))
class(out_cvdglasso_pc)
plot(out_cvdglasso_pc)
out_cvdglasso_pc

######################
# Simulation studies #
######################

################
# Scenario (a) #
################
###################################################
library("dglars")
library("glmnet")
library("glmpath")
set.seed(321)
thr <- 1.0e-13
nsim <- 500
n <- 100
p <- c(1000, 500, 100)
s <- 5
ng <- 1000
g0 <- 0.2
b <- rep(2,s)
ris <- array(0, dim = c(nsim, length(p), 4, 4), dimnames = list(nsim = 1:nsim, p = p, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), ris = c("card", "FP", "TP", "Dev")))
for(j in 1:length(p)){
  pj <- p[j]
  X <- matrix(rnorm(n * pj), n, pj)
  Xs <- X[, 1:s]
  eta <- 1 + drop(Xs %*% b)
  mu <- drop(binomial()$linkinv(eta))
  for(h in 1:nsim){
    y <- rbinom(n, 1, mu)
    ytst <- rbinom(n, 1, mu)
    out.cvdglasso <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLASSO", g0 = g0, ng = ng))
    out.cvdglar <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLARS", g0 = g0, ng = ng))
    out.cvglmnet <- cv.glmnet(X, y , family = "binomial", nlambda = ng)
    out.cvglmpath <- cv.glmpath(X, y, family = binomial, mode = "lambda", plot.it = FALSE)
    out.glmpath <- glmpath(X, y, family = binomial)
    bh_dglasso <- coef(out.cvdglasso)
    etah <- drop(cbind(1, X) %*% bh_dglasso)
    muh <- drop(binomial()$linkinv(etah))
    ris[h , j, "dglasso", "card"] <- sum(abs(bh_dglasso[-1]) > thr)
    ris[h, j, "dglasso", "TP"] <- sum(abs(bh_dglasso[2:(s + 1)]) > thr)
    ris[h, j, "dglasso", "FP"] <- ris[h, j, "dglasso", "card"] - ris[h, j, "dglasso", "TP"]
    ris[h, j, "dglasso", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    bh_dglar <- coef(out.cvdglar)
    etah <- drop(cbind(1, X) %*% bh_dglar)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "dglar", "card"] <- sum(abs(bh_dglar[-1]) > thr)
    ris[h, j, "dglar", "TP"] <- sum(abs(bh_dglar[2:(s + 1)]) > thr)
    ris[h, j, "dglar", "FP"] <- ris[h, j, "dglar", "card"] - ris[h, j, "dglar", "TP"]
    ris[h, j, "dglar", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    bh_glmnet <- coef(out.cvglmnet, s = "lambda.min")
    etah <- drop(cbind(1, X) %*% bh_glmnet)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "glmnet", "card"] <- sum(abs(bh_glmnet[-1]) > thr)
    ris[h, j, "glmnet", "TP"] <- sum(abs(bh_glmnet[2:(s + 1)]) > thr)
    ris[h, j, "glmnet", "FP"] <- ris[h, j, "glmnet", "card"] - ris[h, j, "glmnet", "TP"]
    ris[h, j, "glmnet", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    id_s_min <- which.min(out.cvglmpath$cv.error)
    s_min <- out.cvglmpath$fraction[id_s_min]
    bh_glmpath <- as.vector(predict(out.glmpath, s = s_min, type = "coefficients", mode = "lambda.fraction"))
    etah <- drop(cbind(1, X) %*% bh_glmpath)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "glmpath", "card"] <- sum(abs(bh_glmpath[-1]) > thr)
    ris[h, j, "glmpath", "TP"] <- sum(abs(bh_glmpath[2:(s + 1)]) > thr)
    ris[h, j, "glmpath", "FP"] <- ris[h, j, "glmpath", "card"] - ris[h, j, "glmpath", "TP"]
    ris[h, j, "glmpath", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    cat("Simulation", h, "with p =", pj, "completed\n")
  }
}
se <- function(x) sd(x) / sqrt(length(x))
results <- array(0, dim = c(length(p), 4, 5, 2), dimnames = list(p = p, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), msr = c("card", "FDR", "FPR", "FNR", "Dev"), type = c("m", "se")))
for(j in 1:length(p)){
  results[j, "dglasso", "card", "m"] <- mean(ris[ , j, "dglasso", "card"])
  results[j, "dglasso", "card", "se"] <- se(ris[ , j, "dglasso", "card"])
  FDR <- ris[ , j, "dglasso", "FP"] / (ris[ , j, "dglasso", "FP"] + ris[ , j, "dglasso", "TP"])
  results[j, "dglasso", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "dglasso", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "dglasso", "FP"]
  FN <- s - ris[ , j, "dglasso", "TP"]
  FPR <- ris[ , j, "dglasso", "FP"] / (ris[ , j, "dglasso", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "dglasso", "TP"])
  results[j, "dglasso", "FPR", "m"] <- mean(FPR)
  results[j, "dglasso", "FPR", "se"] <- se(FPR)
  results[j, "dglasso", "FNR", "m"] <- mean(FNR)
  results[j, "dglasso", "FNR", "se"] <- se(FNR)
  results[j, "dglasso", "Dev", "m"] <- mean(ris[ , j, "dglasso", "Dev"])
  results[j, "dglasso", "Dev", "se"] <- se(ris[ , j, "dglasso", "Dev"])
  results[j, "dglar", "card", "m"] <- mean(ris[, j, "dglar", "card"])
  results[j, "dglar", "card", "se"] <- se(ris[ , j, "dglar", "card"])
  FDR <- ris[ , j, "dglar", "FP"] / (ris[ , j, "dglar", "FP"] + ris[ , j, "dglar", "TP"])
  results[j, "dglar", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "dglar", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "dglar", "FP"]
  FN <- s - ris[ , j, "dglar", "TP"]
  FPR <- ris[ , j, "dglar", "FP"] / (ris[ , j, "dglar", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "dglar", "TP"])
  results[j, "dglar", "FPR", "m"] <- mean(FPR)
  results[j, "dglar", "FPR", "se"] <- se(FPR)
  results[j, "dglar", "FNR", "m"] <- mean(FNR)
  results[j, "dglar", "FNR", "se"] <- se(FNR)
  results[j, "dglar", "Dev", "m"] <- mean(ris[ , j, "dglar", "Dev"])
  results[j, "dglar", "Dev", "se"] <- se(ris[ , j, "dglar", "Dev"])
  results[j, "glmnet", "card", "m"] <- mean(ris[ , j, "glmnet", "card"])
  results[j, "glmnet", "card", "se"] <- se(ris[ , j, "glmnet", "card"])
  FDR <- ris[ , j, "glmnet", "FP"] / (ris[ , j, "glmnet", "FP"] + ris[ , j, "glmnet", "TP"])
  results[j, "glmnet", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "glmnet", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "glmnet", "FP"]
  FN <- s - ris[ , j, "glmnet", "TP"]
  FPR <- ris[ , j, "glmnet", "FP"] / (ris[ , j, "glmnet", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "glmnet", "TP"])
  results[j, "glmnet", "FPR", "m"] <- mean(FPR)
  results[j, "glmnet", "FPR", "se"] <- se(FPR)
  results[j, "glmnet", "FNR", "m"] <- mean(FNR)
  results[j, "glmnet", "FNR", "se"] <- se(FNR)
  results[j, "glmnet", "Dev", "m"] <- mean(ris[ , j, "glmnet", "Dev"])
  results[j, "glmnet", "Dev", "se"] <- se(ris[ , j, "glmnet", "Dev"])
  results[j, "glmpath", "card", "m"] <- mean(ris[ , j, "glmpath", "card"])
  results[j, "glmpath", "card", "se"] <- se(ris[ , j, "glmpath", "card"])
  FDR <- ris[ , j, "glmpath", "FP"] / (ris[ , j, "glmpath", "FP"] + ris[ , j, "glmpath", "TP"])
  results[j, "glmpath", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "glmpath", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "glmpath", "FP"]
  FN <- s - ris[ , j, "glmpath", "TP"]
  FPR <- ris[ , j, "glmpath", "FP"] / (ris[ , j, "glmpath", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "glmpath", "TP"])
  results[j, "glmpath", "FPR", "m"] <- mean(FPR)
  results[j, "glmpath", "FPR", "se"] <- se(FPR)
  results[j, "glmpath", "FNR", "m"] <- mean(FNR)
  results[j, "glmpath", "FNR", "se"] <- se(FNR)
  results[j, "glmpath", "Dev", "m"] <- mean(ris[ , j, "glmpath", "Dev"])
  results[j, "glmpath", "Dev", "se"] <- se(ris[ , j, "glmpath", "Dev"])  
}
results_a.tbl <- as.table(results)
round(ftable(results_a.tbl, row.vars = c("p", "msr"), col.vars = c("mthd", "type")), 3)

###################################################
################
# Scenario (b) #
################
library("MASS")
library("dglars")
library("glmnet")
library("glmpath")
set.seed(321)
thr <- 1.0e-13
nsim <- 500
n <- 100
p <- c(1000, 500, 100)
s <- 5
ng <- 1000
g0 <- 0.3
b <- rep(2, s)
rho <- 0.9
ris <- array(0, dim = c(nsim, length(p), 4, 4), dimnames = list(nsim = 1:nsim, p = p, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), ris = c("card", "FP", "TP", "Dev")))
for(j in 1:length(p)){
  pj <- p[j]
  Sj <- outer(seq(pj), seq(pj), function(i, j) rho^(abs(i - j)))
  X <- mvrnorm(n, rep(0, pj), Sj)
  Xs <- X[ , 1:s]
  eta <- 1 + drop(Xs %*% b)
  mu <- drop(binomial()$linkinv(eta))
  for(h in 1:nsim){
    y <- rbinom(n, 1, mu)
    ytst <- rbinom(n, 1, mu)
    out.cvdglasso <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLASSO", g0 = g0, ng = ng))
    out.cvdglar <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLARS", g0 = g0, ng = ng))
    out.cvglmnet <- cv.glmnet(X, y, family = "binomial", nlambda = ng)
    out.cvglmpath <- cv.glmpath(X, y, family = binomial, mode = "lambda", plot.it = FALSE)
    out.glmpath <- glmpath(X, y, family = binomial)
    bh_dglasso <- coef(out.cvdglasso)
    etah <- drop(cbind(1, X) %*% bh_dglasso)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "dglasso", "card"] <- sum(abs(bh_dglasso[-1]) > thr)
    ris[h, j, "dglasso", "TP"] <- sum(abs(bh_dglasso[2:(s + 1)]) > thr)
    ris[h, j, "dglasso", "FP"] <- ris[h, j, "dglasso", "card"] - ris[h, j, "dglasso", "TP"]
    ris[h, j, "dglasso", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    bh_dglar <- coef(out.cvdglar)
    etah <- drop(cbind(1, X) %*% bh_dglar)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "dglar", "card"] <- sum(abs(bh_dglar[-1]) > thr)
    ris[h, j, "dglar", "TP"] <- sum(abs(bh_dglar[2:(s + 1)]) > thr)
    ris[h, j, "dglar", "FP"] <- ris[h, j, "dglar", "card"] - ris[h, j, "dglar", "TP"]
    ris[h, j, "dglar", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    bh_glmnet <- coef(out.cvglmnet, s ="lambda.min")
    etah <- drop(cbind(1, X) %*% bh_glmnet)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "glmnet", "card"] <- sum(abs(bh_glmnet[-1]) > thr)
    ris[h, j, "glmnet", "TP"] <- sum(abs(bh_glmnet[2:(s + 1)]) > thr)
    ris[h, j, "glmnet", "FP"] <- ris[h, j, "glmnet", "card"] - ris[h, j, "glmnet", "TP"]
    ris[h, j, "glmnet", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    id_s_min <- which.min(out.cvglmpath$cv.error)
    s_min <- out.cvglmpath$fraction[id_s_min]
    bh_glmpath <- as.vector(predict(out.glmpath, s = s_min, type = "coefficients", mode = "lambda.fraction"))
    etah <- drop(cbind(1, X) %*% bh_glmpath)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "glmpath", "card"] <- sum(abs(bh_glmpath[-1]) > thr)
    ris[h, j, "glmpath", "TP"] <- sum(abs(bh_glmpath[2:(s + 1)]) > thr)
    ris[h, j, "glmpath", "FP"] <- ris[h, j, "glmpath", "card"] - ris[h, j, "glmpath", "TP"]
    ris[h, j, "glmpath", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    cat("Simulation", h, "with p =", pj, "completed\n")
  }
}
se <- function(x) sd(x) / sqrt(length(x))
results <- array(0, dim = c(length(p), 4, 5, 2), dimnames = list(p = p, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), msr = c("card", "FDR", "FPR", "FNR", "Dev"), type = c("m", "se")))
for(j in 1:length(p)){
  results[j, "dglasso", "card", "m"] <- mean(ris[ , j, "dglasso", "card"])
  results[j, "dglasso", "card", "se"] <- se(ris[ , j, "dglasso", "card"])
  FDR <- ris[ , j, "dglasso", "FP"] / (ris[ , j, "dglasso", "FP"] + ris[ , j, "dglasso", "TP"])
  results[j, "dglasso", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "dglasso", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "dglasso", "FP"]
  FN <- s - ris[ , j, "dglasso", "TP"]
  FPR <- ris[ , j, "dglasso", "FP"] / (ris[ , j, "dglasso", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "dglasso", "TP"])
  results[j, "dglasso", "FPR", "m"] <- mean(FPR)
  results[j, "dglasso", "FPR", "se"] <- se(FPR)
  results[j, "dglasso", "FNR", "m"] <- mean(FNR)
  results[j, "dglasso", "FNR", "se"] <- se(FNR)
  results[j, "dglasso", "Dev", "m"] <- mean(ris[ , j, "dglasso", "Dev"])
  results[j, "dglasso", "Dev", "se"] <- se(ris[ , j, "dglasso", "Dev"])
  results[j, "dglar", "card", "m"] <- mean(ris[, j, "dglar", "card"])
  results[j, "dglar", "card", "se"] <- se(ris[ , j, "dglar", "card"])
  FDR <- ris[ , j, "dglar", "FP"] / (ris[ , j, "dglar", "FP"] + ris[ , j, "dglar", "TP"])
  results[j, "dglar", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "dglar", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "dglar", "FP"]
  FN <- s - ris[ , j, "dglar", "TP"]
  FPR <- ris[ , j, "dglar", "FP"] / (ris[ , j, "dglar", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "dglar", "TP"])
  results[j, "dglar", "FPR", "m"] <- mean(FPR)
  results[j, "dglar", "FPR", "se"] <- se(FPR)
  results[j, "dglar", "FNR", "m"] <- mean(FNR)
  results[j, "dglar", "FNR", "se"] <- se(FNR)
  results[j, "dglar", "Dev", "m"] <- mean(ris[ , j, "dglar", "Dev"])
  results[j, "dglar", "Dev", "se"] <- se(ris[ , j, "dglar", "Dev"])
  results[j, "glmnet", "card", "m"] <- mean(ris[ , j, "glmnet", "card"])
  results[j, "glmnet", "card", "se"] <- se(ris[ , j, "glmnet", "card"])
  FDR <- ris[ , j, "glmnet", "FP"] / (ris[ , j, "glmnet", "FP"] + ris[ , j, "glmnet", "TP"])
  results[j, "glmnet", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "glmnet", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "glmnet", "FP"]
  FN <- s - ris[ , j, "glmnet", "TP"]
  FPR <- ris[ , j, "glmnet", "FP"] / (ris[ , j, "glmnet", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "glmnet", "TP"])
  results[j, "glmnet", "FPR", "m"] <- mean(FPR)
  results[j, "glmnet", "FPR", "se"] <- se(FPR)
  results[j, "glmnet", "FNR", "m"] <- mean(FNR)
  results[j, "glmnet", "FNR", "se"] <- se(FNR)
  results[j, "glmnet", "Dev", "m"] <- mean(ris[ , j, "glmnet", "Dev"])
  results[j, "glmnet", "Dev", "se"] <- se(ris[ , j, "glmnet", "Dev"])
  results[j, "glmpath", "card", "m"] <- mean(ris[ , j, "glmpath", "card"])
  results[j, "glmpath", "card", "se"] <- se(ris[ , j, "glmpath", "card"])
  FDR <- ris[ , j, "glmpath", "FP"] / (ris[ , j, "glmpath", "FP"] + ris[ , j, "glmpath", "TP"])
  results[j, "glmpath", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "glmpath", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "glmpath", "FP"]
  FN <- s - ris[ , j, "glmpath", "TP"]
  FPR <- ris[ , j, "glmpath", "FP"] / (ris[ , j, "glmpath", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "glmpath", "TP"])
  results[j, "glmpath", "FPR", "m"] <- mean(FPR)
  results[j, "glmpath", "FPR", "se"] <- se(FPR)
  results[j, "glmpath", "FNR", "m"] <- mean(FNR)
  results[j, "glmpath", "FNR", "se"] <- se(FNR)
  results[j, "glmpath", "Dev", "m"] <- mean(ris[ , j, "glmpath", "Dev"])
  results[j, "glmpath", "Dev", "se"] <- se(ris[ , j, "glmpath", "Dev"])  
}
results_b.tbl <- as.table(results)
round(ftable(results_b.tbl, row.vars = c("p", "msr"), col.vars = c("mthd", "type")), 3)

###################################################
################
# Scenario (c) #
################
library("MASS")
library("dglars")
library("glmnet")
library("glmpath")
set.seed(321)
thr <- 1.0e-13
nsim <- 500
n <- 100
p <- c(1000, 500, 100)
s <- 5
ng <- 1000
g0 <- 0.3
b <- rep(2, s)
block_size <- 10
rho <- 0.5
ris <- array(0, dim = c(nsim, length(p), 4, 4), dimnames = list(nsim = 1:nsim, p = p, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), ris = c("card", "FP", "TP", "Dev")))
for(j in 1:length(p)){
  pj <- p[j]
  block_nmbr <- pj / block_size
  Sj <- matrix(0, pj, pj)
  for(k in 1:block_nmbr) Sj[1:10 + (k-1) * 10, 1:10 + (k - 1) * 10] <- rho
  diag(Sj) <- 1
  X <- mvrnorm(n, rep(0, pj), Sj)
  Xs <- X[ , 1:s]
  eta <- 1 + drop(Xs %*% b)
  mu <- drop(binomial()$linkinv(eta))
  for(h in 1:nsim){
    y <- rbinom(n, 1, mu)
    ytst <- rbinom(n, 1, mu)
    out.cvdglasso <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLASSO", g0 = g0, ng = ng))
    out.cvdglar <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLARS", g0 = g0, ng = ng))
    out.cvglmnet <- cv.glmnet(X, y, family = "binomial", nlambda = ng)
    out.cvglmpath <- cv.glmpath(X, y, family = binomial, mode = "lambda", plot.it = FALSE)
    out.glmpath <- glmpath(X, y, family = binomial)
    bh_dglasso <- coef(out.cvdglasso)
    etah <- drop(cbind(1, X) %*% bh_dglasso)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "dglasso", "card"] <- sum(abs(bh_dglasso[-1]) > thr)
    ris[h, j, "dglasso", "TP"] <- sum(abs(bh_dglasso[2:(s + 1)]) > thr)
    ris[h, j, "dglasso", "FP"] <- ris[h, j, "dglasso", "card"] - ris[h, j, "dglasso", "TP"]
    ris[h, j, "dglasso", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    bh_dglar <- coef(out.cvdglar)
    etah <- drop(cbind(1, X) %*% bh_dglar)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "dglar", "card"] <- sum(abs(bh_dglar[-1]) > thr)
    ris[h, j, "dglar", "TP"] <- sum(abs(bh_dglar[2:(s + 1)]) > thr)
    ris[h, j, "dglar", "FP"] <- ris[h, j, "dglar", "card"] - ris[h, j, "dglar", "TP"]
    ris[h, j, "dglar", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    bh_glmnet <- coef(out.cvglmnet, s = "lambda.min")
    etah <- drop(cbind(1, X) %*% bh_glmnet)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "glmnet", "card"] <- sum(abs(bh_glmnet[-1]) > thr)
    ris[h, j, "glmnet", "TP"] <- sum(abs(bh_glmnet[2:(s + 1)]) > thr)
    ris[h, j, "glmnet", "FP"] <- ris[h, j, "glmnet", "card"] - ris[h, j, "glmnet", "TP"]
    ris[h, j, "glmnet", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    id_s_min <- which.min(out.cvglmpath$cv.error)
    s_min <- out.cvglmpath$fraction[id_s_min]
    bh_glmpath <- as.vector(predict(out.glmpath, s = s_min, type = "coefficients", mode = "lambda.fraction"))
    etah <- drop(cbind(1, X) %*% bh_glmpath)
    muh <- drop(binomial()$linkinv(etah))
    ris[h, j, "glmpath", "card"] <- sum(abs(bh_glmpath[-1]) > thr)
    ris[h, j, "glmpath", "TP"] <- sum(abs(bh_glmpath[2:(s + 1)]) > thr)
    ris[h, j, "glmpath", "FP"] <- ris[h, j, "glmpath", "card"] - ris[h, j, "glmpath", "TP"]
    ris[h, j, "glmpath", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
    cat("Simulation",h,"with p =",pj,"completed\n")
  }
}
se <- function(x) sd(x) / sqrt(length(x))
results <- array(0, dim = c(length(p), 4, 5, 2), dimnames = list(p = p, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), msr = c("card", "FDR", "FPR", "FNR", "Dev"), type = c("m", "se")))
for(j in 1:length(p)){
  results[j, "dglasso", "card", "m"] <- mean(ris[ , j, "dglasso", "card"])
  results[j, "dglasso", "card", "se"] <- se(ris[ , j, "dglasso", "card"])
  FDR <- ris[ , j, "dglasso", "FP"] / (ris[ , j, "dglasso", "FP"] + ris[ , j, "dglasso", "TP"])
  results[j, "dglasso", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "dglasso", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "dglasso", "FP"]
  FN <- s - ris[ , j, "dglasso", "TP"]
  FPR <- ris[ , j, "dglasso", "FP"] / (ris[ , j, "dglasso", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "dglasso", "TP"])
  results[j, "dglasso", "FPR", "m"] <- mean(FPR)
  results[j, "dglasso", "FPR", "se"] <- se(FPR)
  results[j, "dglasso", "FNR", "m"] <- mean(FNR)
  results[j, "dglasso", "FNR", "se"] <- se(FNR)
  results[j, "dglasso", "Dev", "m"] <- mean(ris[ , j, "dglasso", "Dev"])
  results[j, "dglasso", "Dev", "se"] <- se(ris[ , j, "dglasso", "Dev"])
  results[j, "dglar", "card", "m"] <- mean(ris[, j, "dglar", "card"])
  results[j, "dglar", "card", "se"] <- se(ris[ , j, "dglar", "card"])
  FDR <- ris[ , j, "dglar", "FP"] / (ris[ , j, "dglar", "FP"] + ris[ , j, "dglar", "TP"])
  results[j, "dglar", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "dglar", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "dglar", "FP"]
  FN <- s - ris[ , j, "dglar", "TP"]
  FPR <- ris[ , j, "dglar", "FP"] / (ris[ , j, "dglar", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "dglar", "TP"])
  results[j, "dglar", "FPR", "m"] <- mean(FPR)
  results[j, "dglar", "FPR", "se"] <- se(FPR)
  results[j, "dglar", "FNR", "m"] <- mean(FNR)
  results[j, "dglar", "FNR", "se"] <- se(FNR)
  results[j, "dglar", "Dev", "m"] <- mean(ris[ , j, "dglar", "Dev"])
  results[j, "dglar", "Dev", "se"] <- se(ris[ , j, "dglar", "Dev"])
  results[j, "glmnet", "card", "m"] <- mean(ris[ , j, "glmnet", "card"])
  results[j, "glmnet", "card", "se"] <- se(ris[ , j, "glmnet", "card"])
  FDR <- ris[ , j, "glmnet", "FP"] / (ris[ , j, "glmnet", "FP"] + ris[ , j, "glmnet", "TP"])
  results[j, "glmnet", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "glmnet", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "glmnet", "FP"]
  FN <- s - ris[ , j, "glmnet", "TP"]
  FPR <- ris[ , j, "glmnet", "FP"] / (ris[ , j, "glmnet", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "glmnet", "TP"])
  results[j, "glmnet", "FPR", "m"] <- mean(FPR)
  results[j, "glmnet", "FPR", "se"] <- se(FPR)
  results[j, "glmnet", "FNR", "m"] <- mean(FNR)
  results[j, "glmnet", "FNR", "se"] <- se(FNR)
  results[j, "glmnet", "Dev", "m"] <- mean(ris[ , j, "glmnet", "Dev"])
  results[j, "glmnet", "Dev", "se"] <- se(ris[ , j, "glmnet", "Dev"])
  results[j, "glmpath", "card", "m"] <- mean(ris[ , j, "glmpath", "card"])
  results[j, "glmpath", "card", "se"] <- se(ris[ , j, "glmpath", "card"])
  FDR <- ris[ , j, "glmpath", "FP"] / (ris[ , j, "glmpath", "FP"] + ris[ , j, "glmpath", "TP"])
  results[j, "glmpath", "FDR", "m"] <- mean(na.omit(FDR))
  results[j, "glmpath", "FDR", "se"] <- se(na.omit(FDR))
  TN <- p[j] - s - ris[ , j, "glmpath", "FP"]
  FN <- s - ris[ , j, "glmpath", "TP"]
  FPR <- ris[ , j, "glmpath", "FP"] / (ris[ , j, "glmpath", "FP"] + TN)
  FNR <- FN / (FN + ris[ , j, "glmpath", "TP"])
  results[j, "glmpath", "FPR", "m"] <- mean(FPR)
  results[j, "glmpath", "FPR", "se"] <- se(FPR)
  results[j, "glmpath", "FNR", "m"] <- mean(FNR)
  results[j, "glmpath", "FNR", "se"] <- se(FNR)
  results[j, "glmpath", "Dev", "m"] <- mean(ris[ , j, "glmpath", "Dev"])
  results[j, "glmpath", "Dev", "se"] <- se(ris[ , j, "glmpath", "Dev"])  
}
results_c.tbl <- as.table(results)
round(ftable(results_c.tbl, row.vars = c("p", "msr"), col.vars = c("mthd", "type")), 3)

###################################################
################
# Scenario (d) #
################
library("MASS")
library("dglars")
library("glmnet")
library("glmpath")
data(alon)
set.seed(321)
thr <- 1.0e-13
nsim <- 500
s <- 5
ng <- 1000
g0 <- 1
b <- rep(2, s)
ris <- array(0, dim = c(nsim, 4, 4), dimnames = list(nsim = 1:nsim, mthd = c("dglasso", "dglar", "glmnet", "glmpath"), ris = c("card", "FP", "TP", "Dev")))
X <- as.matrix(alon[ , -1])
X <- scale(X)
n <- dim(X)[1]
p <- dim(X)[2]
As <- sort(sample(p, s))
Xs <- X[ , As]
eta <- 1 + drop(Xs %*% b)
mu <- drop(binomial()$linkinv(eta))
for(h in 1:nsim){
  y <- rbinom(n, 1, mu)
  ytst <- rbinom(n, 1, mu)
  out.cvdglasso <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLASSO", g0 = g0, ng = ng))
  out.cvdglar <- cvdglars.fit(X = X, y = y, family = "binomial", control = list(method = "dgLARS", g0 = g0, ng = ng))
  out.cvglmnet <- cv.glmnet(X, y, family = "binomial", nlambda = ng)
  out.cvglmpath <- cv.glmpath(X, y, family = binomial, mode = "lambda", plot.it = FALSE)
  out.glmpath <- glmpath(X, y, family = binomial)
  bh_dglasso <- coef(out.cvdglasso)
  etah <- drop(cbind(1, X) %*% bh_dglasso)
  muh <- drop(binomial()$linkinv(etah))
  ris[h, "dglasso", "card"] <- sum(abs(bh_dglasso[-1]) > thr)
  ris[h, "dglasso", "TP"] <- sum(abs(bh_dglasso[-1][As]) > thr)
  ris[h, "dglasso", "FP"] <- ris[h, "dglasso", "card"] - ris[h, "dglasso", "TP"]
  ris[h, "dglasso", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
  bh_dglar <- coef(out.cvdglar)
  etah <- drop(cbind(1, X) %*% bh_dglar)
  muh <- drop(binomial()$linkinv(etah))
  ris[h, "dglar", "card"] <- sum(abs(bh_dglar[-1]) > thr)
  ris[h, "dglar", "TP"] <- sum(abs(bh_dglar[-1][As]) > thr)
  ris[h, "dglar", "FP"] <- ris[h, "dglar", "card"] - ris[h, "dglar", "TP"]
  ris[h, "dglar", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
  bh_glmnet <- coef(out.cvglmnet, s = "lambda.min")
  etah <- drop(cbind(1, X) %*% bh_glmnet)
  muh <- drop(binomial()$linkinv(etah))
  ris[h, "glmnet", "card"] <- sum(abs(bh_glmnet[-1]) > thr)
  ris[h, "glmnet", "TP"] <- sum(abs(bh_glmnet[-1][As]) > thr)
  ris[h, "glmnet", "FP"] <- ris[h, "glmnet", "card"] - ris[h, "glmnet", "TP"]
  ris[h, "glmnet", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
  id_s_min <- which.min(out.cvglmpath$cv.error)
  s_min <- out.cvglmpath$fraction[id_s_min]
  bh_glmpath <- as.vector(predict(out.glmpath, s = s_min, type = "coefficients", mode = "lambda.fraction"))
  etah <- drop(cbind(1, X) %*% bh_glmpath)
  muh <- drop(binomial()$linkinv(etah))
  ris[h, "glmpath", "card"] <- sum(abs(bh_glmpath[-1]) > thr)
  ris[h, "glmpath", "TP"] <- sum(abs(bh_glmpath[-1][As]) > thr)
  ris[h, "glmpath", "FP"] <- ris[h, "glmpath", "card"] - ris[h, "glmpath", "TP"]
  ris[h, "glmpath", "Dev"] <- sum(binomial()$dev(ytst, muh, 1))
  cat("Simulation",h,"completed\n")
}
se <- function(x) sd(x) / sqrt(length(x))
results <- array(0, dim = c(4, 5, 2), dimnames = list(mthd = c("dglasso", "dglar", "glmnet", "glmpath"), msr = c("card", "FDR", "FPR", "FNR", "Dev"), type = c("m", "se")))
results["dglasso", "card", "m"] <- mean(ris[ , "dglasso", "card"])
results["dglasso", "card", "se"] <- se(ris[ , "dglasso","card"])
FDR <- ris[ , "dglasso", "FP"] / (ris[ , "dglasso", "FP"] + ris[ , "dglasso", "TP"])
results["dglasso", "FDR", "m"] <- mean(na.omit(FDR))
results["dglasso", "FDR", "se"] <- se(na.omit(FDR))
TN <- p - s - ris[ , "dglasso", "FP"]
FN <- s - ris[ , "dglasso", "TP"]
FPR <- ris[ , "dglasso", "FP"] / (ris[ , "dglasso", "FP"] + TN)
FNR <- FN / (FN + ris[ , "dglasso", "TP"])
results["dglasso", "FPR", "m"] <- mean(FPR)
results["dglasso", "FPR", "se"] <- se(FPR)
results["dglasso", "FNR", "m"] <- mean(FNR)
results["dglasso", "FNR", "se"] <- se(FNR)
results["dglasso", "Dev", "m"] <- mean(ris[ , "dglasso", "Dev"])
results["dglasso", "Dev", "se"] <- se(ris[ , "dglasso", "Dev"])
results["dglar", "card", "m"] <- mean(ris[ , "dglar", "card"])
results["dglar", "card", "se"] <- se(ris[ , "dglar", "card"])
FDR <- ris[ , "dglar", "FP"] / (ris[ , "dglar", "FP"] + ris[ , "dglar", "TP"])
results["dglar", "FDR", "m"] <- mean(na.omit(FDR))
results["dglar", "FDR", "se"] <- se(na.omit(FDR))
TN <- p - s - ris[ , "dglar", "FP"]
FN <- s - ris[ , "dglar", "TP"]
FPR <- ris[ , "dglar", "FP"] / (ris[ , "dglar", "FP"] + TN)
FNR <- FN / (FN + ris[ , "dglar", "TP"])
results["dglar", "FPR", "m"] <- mean(FPR)
results["dglar", "FPR", "se"] <- se(FPR)
results["dglar", "FNR", "m"] <- mean(FNR)
results["dglar", "FNR", "se"] <- se(FNR)
results["dglar", "Dev", "m"] <- mean(ris[ , "dglar", "Dev"])
results["dglar", "Dev", "se"] <- se(ris[ , "dglar", "Dev"])
results["glmnet", "card", "m"] <- mean(ris[ , "glmnet", "card"])
results["glmnet", "card", "se"] <- se(ris[ , "glmnet", "card"])
FDR <- ris[ , "glmnet", "FP"] / (ris[ , "glmnet", "FP"] + ris[ , "glmnet", "TP"])
results["glmnet", "FDR", "m"] <- mean(na.omit(FDR))
results["glmnet", "FDR", "se"] <- se(na.omit(FDR))
TN <- p - s - ris[ , "glmnet", "FP"]
FN <- s - ris[ , "glmnet", "TP"]
FPR <- ris[ , "glmnet", "FP"] / (ris[ , "glmnet", "FP"] + TN)
FNR <- FN / (FN + ris[ , "glmnet", "TP"])
results["glmnet", "FPR", "m"] <- mean(FPR)
results["glmnet", "FPR", "se"] <- se(FPR)
results["glmnet", "FNR", "m"] <- mean(FNR)
results["glmnet", "FNR", "se"] <- se(FNR)
results["glmnet", "Dev", "m"] <- mean(ris[ , "glmnet", "Dev"])
results["glmnet", "Dev", "se"] <- se(ris[ , "glmnet", "Dev"])
results["glmpath", "card", "m"] <- mean(ris[ , "glmpath", "card"])
results["glmpath", "card", "se"] <- se(ris[ , "glmpath", "card"])
FDR <- ris[ , "glmpath", "FP"] / (ris[ , "glmpath", "FP"] + ris[ , "glmpath", "TP"])
results["glmpath", "FDR", "m"] <- mean(na.omit(FDR))
results["glmpath", "FDR", "se"] <- se(na.omit(FDR))
TN <- p - s - ris[ , "glmpath", "FP"]
FN <- s - ris[ , "glmpath", "TP"]
FPR <- ris[ , "glmpath", "FP"] / (ris[ , "glmpath", "FP"] + TN)
FNR <- FN / (FN + ris[ , "glmpath", "TP"])
results["glmpath", "FPR", "m"] <- mean(FPR)
results["glmpath", "FPR", "se"] <- se(FPR)
results["glmpath", "FNR", "m"] <- mean(FNR)
results["glmpath", "FNR", "se"] <- se(FNR)
results["glmpath", "Dev", "m"] <- mean(ris[ , "glmpath", "Dev"])
results["glmpath", "Dev", "se"] <- se(ris[ , "glmpath", "Dev"])  
results_d.tbl <- as.table(results)
round(ftable(results_d.tbl, row.vars = "msr", col.vars = c("mthd","type")), 3)

#################################
# Application to real data sets #
#################################

##########################
# Breast cancer data set #
##########################

###################################################
set.seed(321)
data("breast", package = "dglars")
breast_cvdglasso <- cvdglars(status ~ ., family = "binomial", data = breast, control = list(g0 = 1, eps = 1e-3))
breast_cvdglasso

###################################################
plot(breast_cvdglasso)

###################################################
breast_cvdglasso$formula_cv
breast_dglasso <- dglars(breast_cvdglasso$formula_cv, family = "binomial", data = breast, control = list(g0 = 0))
summary(breast_dglasso, k = "BIC", complexity = "gdf")

##############################
# Duke breat cancer data set #
##############################

###################################################
set.seed(123)
data("duke", package = "dglars")

duke_cvdglasso <- cvdglars(y ~ ., family = "binomial", data = duke, control = list(g0 = 0.01))
plot(duke_cvdglasso)

###################################################
duke_dglasso <- dglars(duke_cvdglasso$formula_cv, family = "binomial", data = duke)
summary(duke_dglasso)

###################################################
library("hu6800.db")

gene_id <- duke_cvdglasso$var_cv
gene_summary <- matrix("", 9, 3)
colnames(gene_summary) <- c("Manufacturer ID", "Symbol", "Name")
gene_summary[, 1] <- gene_id
for (i in 1:9) {
  gene_summary[i, 2] <- try(as.list(hu6800SYMBOL[gene_id[i]])[[1]])
  gene_summary[i, 3] <- try(as.list(hu6800GENENAME[gene_id[i]])[[1]])

}

gene_summary[-c(1,5,6),]

