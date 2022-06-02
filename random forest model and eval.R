#Use r version 4.1.3 unclear if all packages will run otherwise
library(httr)
library(tidyverse)
library(tools)
library(dplyr)
library(quanteda)
library(caret)
library(grf)
library(randomForest)
library(RFpredInterval)
library(ranger)
library(tidyverse)
library(Metrics)
library(MLmetrics)
library(randomForestExplainer)
library(hrbrthemes)
library(outliers)
library(tm)
library(rgl)
library(plotly)

rsq <- function(x, y) summary(lm(y~x))$r.squared

percentage_change <- function(previous, new, as_decimal = FALSE) {
  x <- ((new - previous) / previous) * 100
  if (as_decimal) x <- x / 100
  return(x)
}



#load dataset we will be working with
#the training data "financedata" is in the form of yearly financial results for around 1300 companies listed on the nasdaq
#Datapoint going back up to five year are used if available
#The analysis is conducted so that the market cap one year from financedata is the dependent varible
#The analysis currently consist of 3 parts with in and out of sample predictions, 
#Breimans original algorithm (poorly optimised), Athey et al. Generalized Random Forests, and ghosal and hookers generalized boosted forest
#the code implementation of Ghosal and hookers algorithm was graciously provided by Ghosal 
stat0 <- read.csv("C:/Users/hvida/Documents/basicstats.csv")
numberofshares <- read.csv("C:/Users/hvida/Documents/numberofsahres02.csv")
financedata <- read.csv("C:/Users/hvida/Documents/advancedfinance02.csv")
stat1 <- read.csv("C:/Users/hvida/Documents/closingprice02.csv")
#View(stat0)
#View(financedata)
#View(numberofshares)
#View(fin1)


marketcap <- numberofshares$sharesOutstanding*stat1$close
print(marketcap)


newdata <- select(financedata, accountsPayable,cashChange,cashFlow,cashFlowFinancing,
                  costOfRevenue,longTermDebt,
                  depreciation,inventory,
                  longTermInvestments,netBorrowings,netIncome,netTangibleAssets,operatingExpense,
                  otherLiabilities,
                  propertyPlantEquipment,researchAndDevelopment,revenue,sellingGeneralAndAdmin,
                  totalAssets,totalCash,totalDebt,totalInvestingCashFlows,totalLiabilities
)
View(newdata)

basedata <- cbind(marketcap,newdata)
marketcap

shapiro.test(dataset1$cashChange)


#extract marketcap from the secondondary dataset so we can make future out of sample correlation
marketcap2 <- dataset2 %>% select(marketcap)


marketcaptwo<-data.matrix(marketcap2)

marketcap1 <- dataset1 %>% select(marketcap)


trainingdata1 <-data.matrix(dataset1)

trainingdata2 <-data.matrix(dataset2)

#breiman implementation in sample
#note breimans default implementation is very compute heavy, so it might take some time to run it
start_time <- Sys.time()
Brandomin <- randomForest(marketcap~.,data=basedata,proximity=TRUE,localImp = TRUE)
end_time <- Sys.time()
end_time - start_time
str(Brandomin)
Brandomin$mse

#calculate varible importence percentage in sample 
importanceframe <- Brandomin$importance
importanceframe1 <- as.data.frame(importanceframe)
percentage1 <- importanceframe1$`%IncMSE`/sum(importanceframe1$`%IncMSE`)*100
percentage1

#plot
varibles <- unname(percentage)
varibles1 <- varibles2 <- round(varibles,digits = 1)
name <- colnames(newdata)
plotdata <- cbind(name,varibles1)
plotdata1 <- as.data(plotdata)


ImpData <- as.data.frame(importanceSD(Brandomin))
ImpData$Var.Names <- row.names(ImpData)

#insample prediction for breiman
breipredict <- predict(Brandomin, basedata)
breipredict

#evaluation of insample prediction
cor(marketcap,breipredict,  method = "pearson")
RMSPE(marketcap,breipredict)
RRSE(marketcap,breipredict)
min_max_accuracy <- mean(apply(prediction_frame, 1, min) / apply(prediction_frame, 1, max))
rsq(marketcap,breipredict)
#got a resulting correlation value of 0.9848, very good even for in sample test


#breiman implementation out of out of sample prediction 
#new limited model where half the values are removed for prediction evaluation 
dataset1 <- basedata[1:2817, ]
dataset1
dataset2 <- basedata[2818:5634, ]
dataset2

Brandomout <- randomForest(dataset1$marketcap~.,data=dataset1)
str(Brandomout)

#calculate varible importence percentage out of  sample 
#calculate varible importence percentage in sample 
percentage <- Brandomout$importance/sum(Brandomout$importance)*100
percentage
Brandomout$importanceSD

#plot percent
varibles <- unname(percentage)
varibles1 <- varibles2 <- round(varibles,digits = 1)
name <- colnames(newdata)
plotdata <- cbind(name,varibles1)
plotdata1 <- as.data.frame(plotdata)

ggplot(plotdata1, aes(x=name, y=varibles1)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(y= "Importance in percent", x = "")
 
#out of sample prediction for breiman, we select dataset 2
breipredictout <- predict(Brandomout, dataset2)
breipredictout

#evaluation of out of sample prediction
cor(dataset2$marketcap,breipredictout,  method = "pearson")
RMSPE(dataset2$marketcap,breipredictout)
RRSE(dataset2$marketcap,breipredictout)
#out of sample correlation falls to 0.7649






#athey implementation
#in sample 
newdata <-data.matrix(newdata)
marketcap1 <- as.vector(unlist(basedata %>% select(marketcap)))
marketcapone <- as.data.frame(marketcap1)
is.matrix(newdata)
is.data.frame(marketcap1)

atheyrandomin <- regression_forest(newdata,marketcap1)
atheyrandomin

imp5 <- c(0.004, 0.011, 0.255, 0.004, 0.004, 0.002, 0.009, 0.001, 0.006, 0.001, 0.267, 0.013, 0.006, 0.009, 0.001, 0.024, 0.029, 0.013, 0.004, 0.332, 0.001, 0.003, 0.002 )
iwanthome <- imp5*100
iwanthome <- as.vector(iwanthome)
varibles <- as.list(varibles)
varibles <- varibles[,1]
varibles2 <- round(iwanthome,digits = 1)
name <- colnames(newdata)
plotdata <- paste(name,varibles2)
plotdata
plotdata1 <- as.data.frame(plotdata)

atheyimp <- ggplot(plotdata1, aes(x=name, y=varibles2)) +
              geom_bar(stat = "identity")+
              coord_flip() +
              labs(y= "Importance in percent", x = "")

atheyimp 

#athey in sample prediction 
atheypredictin <- predict(atheyrandomin, newdata)
atheypredictin
marketcap10 <- as.numeric(marketcap)
atheyinsample <- cbind(marketcap10,atheypredictin)
atheyinsample1 <- as.data.frame(atheyinsample)


cor(marketcap10,atheypredictin,  method = "pearson")
RMSPE(atheyinsample1$marketcap10,atheyinsample1$predictions)
RRSE(atheyinsample1$marketcap10,atheyinsample1$predictions)
rsq(atheyinsample1$marketcap10,atheyinsample1$predictions)
#resulting cor value is 0.8769
#pretty poor results consindring its for in sample 

#athey out of sample prediction
datasetone <- newdata[1:2817, ]


datasettwo <- newdata[2818:5634, ]


atheymodel <- regression_forest(datasetone,dataset1$marketcap)
atheymodel

atheypredictout <- predict(atheymodel, datasettwo)
atheypredictout

#evaluation of outofsample prediction
cor(dataset2$marketcap,atheypredictout,  method = "pearson")
marketcap10 <- as.numeric(dataset2$marketcap)
atheypred <- as.numeric(atheypredictout)
RMSPE(atheyplot$`dataset2$marketcap`,atheyplot$predictions)
RRSE(atheyplot$`dataset2$marketcap`,atheyplot$predictions)

#out of sample falls to 0.7557, which is suprsingly good considering the results from the insample forecast





#insample test for one step boosted forest model
#the code below is an implementation provided by Indrayudh Ghosal 

find.loglik = function(family) {
  
  if(family == "gaussian") {
    lik0 = function(y, eta, weight=1) {-weight*(y-eta)^2}
    lik1 = function(y, eta, weight=1) {weight*(y-eta)}
    lik2 = function(y, eta, weight=1) {-weight}
  }
  
  # if(family == "binomial") {
  #   lik0 = function(y, q, wt=1) { p = plogis(q); y*log(p) + (wt-y)*log(1-p) }
  #   lik1 = function(y, q, wt=1) { p = plogis(q); y - wt*p }
  #   lik2 = function(y, q, wt=1) { p = plogis(q); wt*(p^2-p) }
  #   dir.deriv = function(y, wt = 1) { (mean(wt)*y - mean(y)*wt)/(mean(y)*(mean(wt) - mean(y))) }
  #   recompute.limits = c(1,-1)*qlogis(.Machine$double.eps)
  # }
  # 
  # if(family == "poisson") {
  #   lik0 = function(y, q, wt=1) { wt*(y*q - exp(q)) }
  #   lik1 = function(y, q, wt=1) { wt*(y - exp(q)) }
  #   lik2 = function(y, q, wt=1) { wt*(-exp(q)) }
  #   dir.deriv = function(y, wt = 1) { wt*y/mean(wt*y) - wt/mean(wt) }
  #   recompute.limits = c(1,-1)*log(.Machine$double.eps)
  # }
  
  return(list(lik = lik0, lik1 = lik1, lik2 = lik2))
  
}


glm.inf = function(formula, data, family = "gaussian", varest = TRUE, ...) {
  output = list()
  model = glm(formula = formula, data = data, family = family,
              y = varest, x = varest, ...)
  output$model = model
  
  pred.function = function(xtest, varest. = varest) {
    result = list()
    result$predictions = predict(model, newdata = xtest, type = "link", ...)
    
    if(varest.) {
      ll = find.loglik(family)
      D1 = diag(ll$lik1(y = model$y, eta = model$linear.predictors,
                        weight = model$prior.weights))
      D2 = diag(ll$lik2(y = model$y, eta = model$linear.predictors,
                        weight = model$prior.weights))
      U1 = model.matrix(model$formula, as.data.frame(xtest)) %*% 
        solve(t(model$x) %*% D2 %*% (model$x), t(model$x) %*% D1)
      result$dir.deriv = -U1 * nrow(data)
    }
    return(result)
  }
  output$pred = pred.function
  # class(output) = "inf.model"
  return(output)
}

# infJack.glm = function(glm.model, family) {}


rf.inf = function(formula, data, varest = TRUE, lh.bias = FALSE,
                  var.corr = varest, ev.cutoff = 0, ...) {
  output = list()
  model = ranger(formula = formula, data = data, keep.inbag = (varest | lh.bias), ...)
  output$model = model
  
  # Returns predictions and covariance matrix
  pred.function = function(xtest, varest. = varest, lh.bias. = lh.bias) {
    result = list()
    p = predict(model, data = xtest, predict.all = varest., type = "response", ...)
    
    if(varest.) {
      result$predictions = rowMeans(p$predictions)
      result$dir.deriv = infJack.forest(pred = p$predictions,
                                        inbag = do.call(cbind, model$inbag.counts),
                                        var.corr = var.corr,
                                        ev.cutoff = ev.cutoff)
    } else {
      result$predictions = p$predictions
    }
    
    if(lh.bias.) {
      train.nodes = predict(model, data, type = "terminalNodes") %>% 
        .$predictions %>% t()
      test.nodes = predict(model, data = xtest, type = "terminalNodes") %>% 
        .$predictions %>% t()
      
      inbag.count = do.call(rbind, model$inbag.counts)
      
      # oob errors
      Dvec = (data %>% pull(all.vars(formula)[1])) - model$predictions
      
      train.nodes[inbag.count != 0] = NA
      Cmat = sapply(1:ncol(test.nodes), function(i) {
        sapply(1:ncol(train.nodes), function(j) {
          sum(test.nodes[,i]==train.nodes[,j], na.rm = T)
        })
      }) %>% t()
      
      if(varest.) {
        r = Cmat %*% diag(Dvec)
        p = rowSums(r)
        q = rowSums(Cmat)
        bias = p/q
        ddmat = (r - Cmat*bias)/q
        
        result$predictions = result$predictions + bias
        result$dir.deriv = result$dir.deriv + (ncol(Cmat) * ddmat)
      } else {
        p = Cmat %*% Dvec
        q = rowSums(Cmat)
        bias = p/q
        
        result$predictions = result$predictions + bias
      }
    }
    return(result)
  }
  output$pred = pred.function
  # class(output) = "inf.model"
  return(output)
}


infJack.forest = function(pred, inbag, var.corr, ev.cutoff = 0) {
  pred.centered = pred - rowMeans(pred)
  N = inbag
  
  U1 = tcrossprod(pred.centered, N)/ncol(N)
  dir.deriv = nrow(N) * U1
  
  if(var.corr) {
    covmat = tcrossprod(U1)
    M1 = tcrossprod(pred.centered)/ncol(pred.centered)
    # scale = (sum(N[,1]) - 1)/(ncol(N))
    scale = (sum(apply(N, 1, var)) - 1)/(ncol(N))
    covmat = covmat - scale*M1
    
    eig = eigen(covmat, symmetric = TRUE, only.values = TRUE)
    #print(mean(eig$values <= ev.cutoff))
    #print(eig$values)
    if(any(eig$values <= ev.cutoff)) {
      eig = eigen(covmat, symmetric = TRUE, only.values = FALSE)
      columns = eig$vectors[,which(eig$values > ev.cutoff)]
      dir.deriv = columns %*% solve(crossprod(columns), crossprod(columns, dir.deriv))
    }
  }
  
  return(dir.deriv)
}

# base can act as an offset. To have GLM with offset put it in formula
gbf = function(formula, data, family = "gaussian", base = NULL, nboost = 1,
               train.weight = 1, newton = TRUE, varest = FALSE, lh.bias = FALSE,
               var.corr = varest, ev.cutoff = 0, oob.residuals = TRUE, ...) {
  
  # if (is.character(family)) 
  #   family <- get(family, mode = "function", envir = parent.frame())
  # if (is.function(family)) 
  #   family <- family()
  # if (is.null(family$family)) {
  #   print(family)
  #   stop("'family' not recognized")
  # }
  
  ll = find.loglik(family)
  var.names = all.vars(formula)
  y = data %>% pull(var.names[1])
  steps = list() # will be a list of length nboost+1
  
  if(length(newton) < nboost) {
    newton = rep(newton, length.out = nboost)
  }
  if(length(lh.bias) < nboost) {
    lh.bias = rep(lh.bias, length.out = nboost)
  }
  if(length(var.corr) < nboost) {
    var.corr = rep(var.corr, length.out = nboost)
  }
  if(length(ev.cutoff) < nboost) {
    ev.cutoff = rep(ev.cutoff, length.out = nboost)
  }
  if(length(oob.residuals) < nboost) {
    oob.residuals = rep(oob.residuals, length.out = nboost)
  }
  
  
  if(is.null(base)) {
    steps[[1]] = list(model = NULL,
                      pred = function(xtest, varest. = varest) {
                        result = list()
                        result$predictions = 0
                        if(varest) {
                          result$dir.deriv = matrix(0, nrow(xtest), nrow(data))
                        }
                        return(result)
                      })
  } else if(is_formula(base)) {
    steps[[1]] = glm.inf(formula = base, data = data, family = family,
                         varest = varest)
    # glm doesn't like to be given extraneous stuff like num_trees, etc
    # The solution could be supply extra ... arguments for glm and ranger
    # separately but I'm too tired to implement it right now
  } else { 
    # user must enter a model and prediction function with
    # similar specifications as within glm.inf and rf.inf
    # i.e., with predictions and also dir.deriv component if needed
    steps[[1]] = base
  }
  
  pred = steps[[1]]$pred(data, varest. = F)$predictions #update at the end of each step
  
  for(i in 1:nboost) {
    residual = ll$lik1(y = y, eta = pred, weight = train.weight)
    if(newton[i]) {
      case.weights = -ll$lik2(y = y, eta = pred, weight = train.weight)
      residual = residual/case.weights
    } else {
      case.weights = NULL
    }
    
    rf.formula = paste(var.names[-1], collapse = "+") %>%
      paste("residual~",.) %>% as.formula()
    steps[[i+1]] = rf.inf(formula = rf.formula, data = cbind.data.frame(residual,data),
                          varest = varest, case.weights = case.weights,
                          lh.bias = lh.bias[i], var.corr = var.corr[i],
                          ev.cutoff = ev.cutoff[i], ...)
    if(oob.residuals[i]) {
      pred = pred + steps[[i+1]]$model$predictions
    } else {
      pred = pred + predict(steps[[i+1]]$model, data)$predictions
      # should this be updated to reflect the lh.bias?
      # pred = pred + steps[[i+1]]$pred(data, varest. = F, lh.bias. = lh.bias[i])$predictions
    }
  }
  
  return(list(steps = steps, predicted = pred))
  class(output) = "gbf"
  return(output)
  
}


predict.gbf = function(object, newdata = NULL, varest = FALSE, each.step = FALSE, ...) {
  result = list()
  
  if(is.null(newdata)) {
    result$predictions = output$predicted
    if(varest) warning("'newdata' needed for variance estimation")
    return(result)
  }
  
  pred_list = object$steps[[1]]$pred(newdata, varest. = varest)
  
  if(each.step) {
    pred = pred_list$predictions
    pred_mat = pred
    if(varest) {
      ddmat = pred_list$dir.deriv
      dir_deriv_arr = ddmat
    }
    for(i in 2:length(object$steps)) {
      pred_list = object$steps[[i]]$pred(newdata, varest. = varest)
      pred = pred + pred_list$predictions
      pred_mat = cbind.data.frame(pred_mat, pred)
      
      if(varest) {
        ddmat = ddmat + pred_list$dir.deriv
        dir_deriv_arr = abind::abind(dir_deriv_arr, ddmat, along = 3)
      }
    }
    colnames(pred_mat) = paste0("step", 1:length(object$steps))
    result$predictions = pred_mat
    if(varest) {
      dimnames(dir_deriv_arr)[[3]] = paste0("step", 1:length(object$steps))
      result$dir.deriv = dir_deriv_arr
    }
  } else {
    pred = pred_list$predictions
    if(varest) {
      ddmat = pred_list$dir.deriv
    }
    for(i in 2:length(object$steps)) {
      pred_list = object$steps[[i]]$pred(newdata, varest. = varest)
      pred = pred + pred_list$predictions
      
      if(varest) {
        ddmat = ddmat + pred_list$dir.deriv
      }
    }
    result$predictions = pred
    if(varest) {
      result$dir.deriv = ddmat
    }
  }
  return(result)
}

# There are problems with this function for repeating the same variable names
# in subsidiary function, specifically the argument varest. Not fixed until now
gbf.inf = function(formula, data, family = "gaussian", base = NULL, nboost = 1,
                   train.weight = 1, newton = TRUE, varest = TRUE,
                   lh.bias = FALSE, var.corr = varest, ev.cutoff = 0,
                   oob.residuals = TRUE, each.step = FALSE, ...) {
  output = list()
  var_est = varest
  model = gbf(formula = formula, data = data, family = family, base = base,
              nboost = nboost, train.weight = train.weight, newton = newton,
              varest = var_est, lh.bias = lh.bias, var.corr = var.corr,
              ev.cutoff = ev.cutoff, oob.residuals = oob.residuals, ...)
  output$model = model
  
  pred.function = function(xtest, varest. = var_est, each.step = each.step, ...) {
    return(predict.gbf(object = model, newdata = xtest, varest = varest.,
                       each.step = each.step, ...))
  }
  output$pred = pred.function
  # class(output) = "inf.model"
  return(output)
}


chisq_test = function(m1, m2, testdata) {
  p1 = m1$pred(testdata)
  p2 = m2$pred(testdata)
  
  return(chisq_compare(p1 = p1$predictions, p2 = p2$predictions,
                       dd1 = p1$dir.deriv, dd2 = p2$dir.deriv))
}

max_diff_test = function(m1, m2, testdata, nsim = 5000) {
  p1 = m1$pred(testdata)
  p2 = m2$pred(testdata)
  
  return(max_diff_compare(p1 = p1$predictions, p2 = p2$predictions,
                          dd1 = p1$dir.deriv, dd2 = p2$dir.deriv, nsim = nsim))
}

chisq_compare = function(p1, p2, dd1, dd2) {
  vec = p1-p2
  mat = tcrossprod(dd1-dd2)/((ncol(dd1))^2)
  
  stat = tryCatch(expr = {
    vec %*% solve(mat, vec)
  }, error = function(e) {
    sprintf("Singular covariance matrix")
    return(vec %*% MASS::ginv(mat) %*% vec)
  })
  p_val = 1 - pchisq(stat, length(vec))
  return(c(stat,p_val))
}

max_diff_compare = function(p1, p2, dd1, dd2, nsim = 5000) {
  vec = p1-p2
  mat = tcrossprod(dd1-dd2)/((ncol(dd1))^2)
  
  sds = sqrt(diag(mat))
  vec_max = (max(abs(vec/sds)))
  mat_corr = (mat/sds) %*% diag(1/sds)
  
  simul_max = MASS::mvrnorm(nsim, rep(0,nrow(mat_corr)), mat_corr) %>%
    apply(1, function(x) max(abs(x)))
  return(c(vec_max, 1 - ecdf(simul_max)(vec_max)))
}




fm = marketcap~.

insampleboost <- gbf(fm, data=basedata,importance="impurity")
View(insampleboost)
insampleboostim <- insampleboost[["steps"]][[2]][["model"]][["variable.importance"]]
insampleboostim
#percentage importance


imp1 <- insampleboost[["steps"]][[2]][["model"]][["variable.importance"]]
tester <- imp1[ , -which(names(imp1) %in% c("marketcap"))]
imp2 <- unname(imp1)
imp2
imp3 <- imp1[-1]
imp3


#ploting percentage importance

percentage <- imp3/sum(imp3)*100
percentage

varibles <- unname(percentage)
varibles1 <- varibles2 <- round(varibles,digits = 1)
name <- colnames(newdata)
plotdata <- cbind(name,varibles1)
plotdata1 <- as.data(plotdata)

ggplot(plotdata1, aes(x=name, y=varibles1)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(y= "Importance in percent", x = "")
  


ggplot(plotdata1, aes(x=reorder(Seller, Num), y=Avg_Cost)) +
  geom_bar(stat='identity') +
  coord_flip()




#eval

predvalue <- insampleboost[["steps"]][[2]][["model"]][["predictions"]]
predvalue
cor(marketcap,predvalue,  method = "pearson")
RMSPE(marketcap,predvalue)
MAE(marketcap,predvalue)
RRSE(marketcap,predvalue)
rsq(marketcap,predvalue)
#got a cor value of 0.9548 for in sample test
#r sqaured value is 0.891

#performing the test agian for out of sample data

fm = marketcap~.

outofsamplemodel <- gbf(fm, data=dataset1,importance="impurity")
str(outofsamplemodel)

gbfoutofsample <- predict.gbf(outofsamplemodel,dataset2)
normalisedprediction <- gbfoutofsample$predictions


cor(dataset2$marketcap,normalisedprediction,  method = "pearson")
RMSPE(dataset2$marketcap,normalisedprediction)
MAE(dataset2$marketcap,normalisedprediction)
RRSE(dataset2$marketcap,normalisedprediction)
R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
RMSD(dataset2$marketcap,normalisedprediction)



#this returned a correlation of 0.9202833 for out of sample predictions,
#a very large improvement over the other two out of sample models 
#View(normalisedprediction)
#plotting 


#calculate r squared
rsq(new_df$V1,new_df$normalisedprediction)
#returns a value of 0.8332429
#outlier removal and plotting ghosal and hooker out of bag
gh_plot_data <- cbind(dataset2$marketcap,normalisedprediction)
Fixed_gh_plot_data <- as.data.frame(gh_plot_data)
new_df <- Fixed_gh_plot_data[-c(340,2057,2060,2058,2059,2056),]

quartiles <- quantile(new_df$V1, probs=c(.05, .95), na.rm = FALSE)
IQR <- IQR(new_df$V1)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(new_df, new_df$V1 > Lower & new_df$V1 < Upper)
data_no_outlier

p4 <- ggplot(data_no_outlier, aes(x=V1, y=normalisedprediction),add = "reg.line") +
  geom_point(size = 0.3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_size_area() +
  xlab("Actual Marketcap") + ylab("Predicted Marketcap")+ theme(text = element_text(size = 12))+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 10)
  )
  

print(p4+ggtitle("Predicted vs Actual, Ghosal and Hooker"))

#outlier removal and plotting ghosal and hooker in bag
datating <- cbind(marketcap,predvalue) 
datating <- as.data.frame(datating)
datating <- datating[-c(3157,4874,4877,4875,4876,4873,2720),]

p41 <- ggplot(datating, aes(x=marketcap, y=predvalue),add = "reg.line") +
  geom_point(size = 0.3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_size_area() +
  xlab("Actual Marketcap") + ylab("Predicted Marketcap")+ theme(text = element_text(size = 12))+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 10)
  )


print(p41+ggtitle("Predicted vs Actual, Ghosal and Hooker"))


#outlier removal and plotting breiman out of bag
breiplot <- cbind(dataset2$marketcap,breipredictout)
rownames(breiplot) <- 1:nrow(as.data.frame(breiplot))
breiplot <- as.data.frame(breiplot)
breiplot <- breiplot[-c(340,2057,2060,2058,2059,2056),]


quartiles <- quantile(breiplot$V1, probs=c(.05, .95), na.rm = FALSE)
IQR <- IQR(breiplot$V1)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier1 <- subset(breiplot, breiplot$V1 > Lower & breiplot$V1 < Upper)
data_no_outlier1

p5 <- ggplot(data_no_outlier1, aes(x=V1, y=breipredictout),add = "reg.line") +
  geom_point(size = 0.3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_size_area() +
  xlab("Actual Marketcap") + ylab("Predicted Marketcap")+ theme(text = element_text(size = 12))+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 10)
  )



print(p5+ggtitle("Predicted vs Actual, Breiman"))

#outlier removal and plotting breiman in bag
datating <- cbind(marketcap,breipredict) 
datating <- as.data.frame(datating)
datating <- datating[-c(3157,4874,4877,4875,4876,4873,2720),]

p51 <- ggplot(datating, aes(x=marketcap, y=breipredict),add = "reg.line") +
  geom_point(size = 0.3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_size_area() +
  xlab("Actual Marketcap") + ylab("Predicted Marketcap")+ theme(text = element_text(size = 12))+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 10)
  )


print(p51+ggtitle("Predicted vs Actual, Breiman"))

#calculate r squared
rsq(breiplot$V1,breiplot$breipredictout)
#this returns an r2 value of 0.6074

#outlier removal and plotting athey
atheyplot <- cbind(dataset2$marketcap,atheypredictout)
atheyplot
atheyplot <- as.data.frame(atheyplot)
atheyplot <- atheyplot[-c(340,2057,2060,2058,2059,2056),]
View(atheyplot)

quartiles <- quantile(atheyplot$V1, probs=c(.05, .95), na.rm = FALSE)
IQR <- IQR(atheyplot$V1)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier1 <- subset(atheyplot, atheyplot$`dataset2$marketcap` > Lower & atheyplot$predictions < Upper)
data_no_outlier1

p6 <- ggplot(data_no_outlier1, aes(x=`dataset2$marketcap`, y=predictions),add = "reg.line") +
  geom_point(size = 0.3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_size_area() +
  xlab("Actual Marketcap") + ylab("Predicted Marketcap")+ theme(text = element_text(size = 12))+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 10)
  )



print(p6+ggtitle("Predicted vs Actual, Athey,\nTibshirani and Wager"))


#outlier removal and plotting athey in bag

datating <- cbind(marketcap,atheypredictin) 
datating <- as.data.frame(datating)
datating <- datating[-c(3157,4874,4877,4875,4876,4873,2720),]

p61 <- ggplot(datating, aes(x=marketcap, y=predictions),add = "reg.line") +
  geom_point(size = 0.3) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  scale_size_area() +
  xlab("Actual Marketcap") + ylab("Predicted Marketcap")+ theme(text = element_text(size = 12))+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 10)
  )

print(p61+ggtitle("Predicted vs Actual, Athey,\nTibshirani and Wager"))

#calculate r squared
rsq(atheyplot$`dataset2$marketcap`,atheyplot$predictions)
#this returns an r2 value of 0.581

#plots illustraiting curse of dimensionality

x1 <- sample(x = 1:100, size  = 50)
y1 <- sample(x = 1:100, size  = 50)
z1 <- sample(x = 1:100, size  = 50)

plotdatad <- cbind(x1,y1,z1)


z2 <- rep(50, 50)

y2 <- rep(50,50)

plot_ly(x=x1, y=y1, z=z1, type="scatter3d", mode="markers", color=x1)

plot_ly(x=x1, y=y1, z=z2, type="scatter3d", mode="markers", color=x1)

plot_ly(x=x1, y=y2, z=z2, type="scatter3d", mode="markers", color=x1)


outliersper <- function(x){
  length(which(x >  mean(x) + 2 * sd(x) | x < mean(x) - 2 * sd(x))  ) / length(x)
}
#3 outliers here
df <- data.frame(col= c(x1,y1,z1,runif(50)))

#function
outliersper(df$col)

#3 outliers here
df <- data.frame(col= c(x1,y1,runif(50)))

#function
outliersper(df$col)

#3 outliers here
df <- data.frame(col= c(x1,runif(50)))

#function
outliersper(df$col)


## the rest of code is not planned to be included in the project
## makeing future prediction from the trained data set breiman

fin1 <- read.csv("C:/Users/hvida/Documents/finansdata.csv")
stats1 <- read.csv("C:/Users/hvida/Documents/basicstats.csv")
View(fin1)



stat2 <- rev(stats1)
stat3 <- stat2 %>% map_df(rev)

#for some reason the two datasets dont exactly match, so we remove one data entry in fin1 at 686 
fin2 <- fin1[-c(686), ]
str(fin2)
str(stat3)

#we use the previous model breipredict agian
futurebrei <- predict(Brandomin, fin2)
futurebrei


currentprice <- as.data.frame(select(stats1,companyName,marketcap))
options(scipen = 100)
diffdata <- cbind(currentprice,futurebrei)
View(diffdata)



RMSPE(varibles1,imp5)


imp2

imp5

varibles1





