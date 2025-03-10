# NEW FUNCTIONS -----------------------------------------------------------

# add new obligatory arguments: MGP_quantities and MGP_quantities_accepted
carteltraintest_new <- function (collusion, prices, MGP_quantities=NULL, MGP_quantities_accepted=NULL, trainshare = 0.75, 
                                 cvfolds = 10, partial = FALSE, xstandardize = 0, nsim = 1, threshold = 0.5, 
                                 numberbids = FALSE, mlmethods = c("SL.cforest", "SL.ipredbagg", 
                                                                   "SL.glmnet", "SL.ksvm", "SL.glm")) 
{
  prices = data.frame(prices)
  
  # add the MGP prices if passed as an argument
  if(!is.null(MGP_quantities)){
    MGP_quantities = data.frame(MGP_quantities)
    MGP_quantities_accepted = data.frame(MGP_quantities_accepted)
  }
  
  print("Calculating screens")
  
  x = screens(prices, MGP_quantities, MGP_quantities_accepted, partial = partial, numberbids = numberbids)
  nobsorig = nrow(x)
  x[sapply(x, is.infinite)] = NA
  yx = cbind(collusion, x)
  yx = na.omit(yx)
  dropped = nobsorig - nrow(x)
  collusion = yx[, 1]
  x = yx[, -1]
  
  # corrections for demeaning and standardization (matrix substraction and division issue)
  meanscreens = colMeans(x)
  meanscreens = rep(meanscreens, each=nrow(x)) # here
  
  sdscreens = apply(x, 2, sd)
  if (xstandardize == 1) 
    x = x - meanscreens
  if (xstandardize == 2) {
    
    x = x - meanscreens # here
    x = sweep(x, 2, sdscreens, "/") # and here
    # some screens in the subsets still produce only NAs. We remove those columns
    cols_to_remove <- colnames(x)[colSums(!is.na(x)) == 0]
    cat("Removed screens: ", cols_to_remove)
    x <- x[, colSums(!is.na(x)) > 0]
  }
  
  SLweights = c()
  SLrisks = c()
  SLcorr = c()
  SLcorr.nocart = c()
  SLcorr.cart = c()
  
  # add a progress bar to visualize progress
  print("Beginning simulations")
  pb = progress::progress_bar$new(total=nsim, format = ":elapsedfull Simulation :current of :total [:bar] :percent :eta")
  for (j in 1:nsim) {
    # progress bar advances
    pb$tick()
    trsample <- sample.int(n = nrow(x), size = floor(trainshare * 
                                                       nrow(x)), replace = F)
    xframe = data.frame(x)
    xframe = rename(xframe, c(X1 = "V1", X2 = "V2", X3 = "V3", 
                              X4 = "V4", X5 = "V5", X6 = "V6", X7 = "V7", X8 = "V8", 
                              X9 = "V9", X10 = "V10", X11 = "V11", X12 = "V12", 
                              X13 = "V13", X14 = "V14", X15 = "V15", X16 = "V16", 
                              X17 = "V17", X18 = "V18", X19 = "V19", X20 = "V20", 
                              X21 = "V21", X22 = "V22", X23 = "V23", X24 = "V24", 
                              X25 = "V25", X26 = "V26", X27 = "V27", X28 = "V28", 
                              X29 = "V29", X30 = "V30", X31 = "V31", X32 = "V32", 
                              X33 = "V33", X34 = "V34", X35 = "V35", X36 = "V36", 
                              X37 = "V37", X38 = "V38", X39 = "V39", X40 = "V40", 
                              X41 = "V41", X42 = "V42", X43 = "V43", X44 = "V44", 
                              X45 = "V45", X46 = "V46", X47 = "V47", X48 = "V48", 
                              X49 = "V49", X50 = "V50", X51 = "V51", X52 = "V52", 
                              X53 = "V53", X54 = "V54", X55 = "V55", X56 = "V56", 
                              X57 = "V57", X58 = "V58", X59 = "V59", X60 = "V60", 
                              X61 = "V61", X62 = "V62", X63 = "V63", X64 = "V64", 
                              X65 = "V65", X66 = "V66", X67 = "V67", X68 = "V68", 
                              X69 = "V69", X70 = "V70", X71 = "V71", X72 = "V72", 
                              X73 = "V73", X74 = "V74", X75 = "V75", X76 = "V76", 
                              X77 = "V77", X78 = "V78", X79 = "V79", X80 = "V80", 
                              X81 = "V81", X82 = "V82", X83 = "V83", X84 = "V84", 
                              X85 = "V85", X86 = "V86", X87 = "V87", X88 = "V88", 
                              X89 = "V89", X90 = "V90", X91 = "V91", X92 = "V92", 
                              X93 = "V93", X94 = "V94", X95 = "V95", X96 = "V96", 
                              X97 = "V97", X98 = "V98", X99 = "V99", X100 = "V100", 
                              X101 = "V101", X102 = "V102", X103 = "V103", X104 = "V104", 
                              X105 = "V105", X106 = "V106", X107 = "V107", X108 = "V108", 
                              X109 = "V109", X110 = "V110", X111 = "V111", X112 = "V112", 
                              X113 = "V113", X114 = "V114", X115 = "V115", X116 = "V116", 
                              X117 = "V117", X118 = "V118", X119 = "V119", X120 = "V120"), 
                    warn_missing = FALSE, warn_duplicated = FALSE)
    slpred = suppressWarnings(SuperLearner(Y = collusion[trsample], 
                                           X = xframe[trsample, ], newX = xframe[-trsample, 
                                           ], family = binomial, SL.library = mlmethods, 
                                           cvControl = list(V = cvfolds)))
    SLweights = rbind(SLweights, slpred$coef)
    SLrisks = rbind(SLrisks, slpred$cvRisk)
    SLtestpredict = slpred$SL.predict
    SLcorrectpred = 1 * (collusion[-trsample] == (1 * (SLtestpredict > 
                                                         threshold)))
    SLcorr = c(SLcorr, mean(SLcorrectpred))
    SLcorr.nocart = c(SLcorr.nocart, mean(SLcorrectpred[collusion[-trsample] == 
                                                          0]))
    SLcorr.cart = c(SLcorr.cart, mean(SLcorrectpred[collusion[-trsample] == 
                                                      1]))
  }
  mlmse = colMeans(SLrisks)
  mlweight = colMeans(SLweights)
  results = rbind((mean(SLcorr)) * 100, (mean(SLcorr.cart)) * 
                    100, (mean(SLcorr.nocart)) * 100)
  rownames(results) = c("correct prediction rate (overall) in %", 
                        "correct prediction rate under collusion in %", "correct prediction rate under competition in %")
  list(results = results, mlmodel = slpred, mlmse = mlmse, 
       mlweight = mlweight, xstandardize = xstandardize, meanscreens = meanscreens, 
       sdscreens = sdscreens, partial = partial, dropped = dropped, 
       numberbids = numberbids, trcollusion = collusion[trsample], 
       trscreens = xframe[trsample, ])
}

# add new obligatory arguments
screens_new <- function (prices, MGP_quantities, MGP_quantities_accepted, partial = FALSE, numberbids = FALSE) 
{
  x = c()
  
  # add a progress bar
  pb = progress::progress_bar$new(total=nrow(prices), format = ":elapsedfull Tender :current of :total [:bar] :percent :eta")
  for (i in 1:nrow(prices)) {
    # progress bar advances
    pb$tick()
    
    tender = c()
    # create additional arrays for MGP quantities
    tender_MGP = c()
    tender_MGP_accepted = c()
    
    for (j in 1:ncol(prices)) {
      if (is.na(prices[i, j]) == 0) 
        tender = c(tender, prices[i, j])
    }
    
    # if an argument is passed, then create the MGP tenders
    if (!is.null(MGP_quantities)){
      # add the MGP quantities tender
      for (k in 1:ncol(MGP_quantities)) {
        if (is.na(MGP_quantities[i, k]) == 0)
          tender_MGP = c(tender_MGP, MGP_quantities[i, k])
      }
      
      # add the MGP quantities accepted tender
      for (l in 1:ncol(MGP_quantities_accepted)) {
        if (is.na(MGP_quantities_accepted[i, l]) == 0)
          tender_MGP_accepted = c(tender_MGP_accepted, MGP_quantities_accepted[i, l])
      }
    }
    
    # create screens based on the current tender AND the MGP tender
    screenstender = genscreen(tender, tender_MGP, tender_MGP_accepted)
    if (numberbids == TRUE) 
      screenstender = c(screenstender, length(tender))
    if (partial == TRUE) {
      subsets = t(combn(x = tender, m = 4))
      screenssubsets = c()
      
      # add multiprocessing to speed up the process (resource-intensive)
      # prepare multi core usage
      future::plan(future::multisession)
      
      # multi core running
      results <- future.apply::future_lapply(1:nrow(subsets), function(k) genscreen(subsets[k, ]))
      screenssubsets <- do.call(rbind, results)
      
      sumsubsets = c(apply(screenssubsets, 2, mean), apply(screenssubsets, 
                                                           2, median), apply(screenssubsets, 2, min), apply(screenssubsets, 
                                                                                                            2, max))
      screenstender = c(screenstender, sumsubsets)
      subsets = t(combn(x = tender, m = 3))
      screenssubsets = c()
      
      # multi core running
      results <- future.apply::future_lapply(1:nrow(subsets), function(k) genscreen(subsets[k, ]))
      screenssubsets <- do.call(rbind, results)
      sumsubsets = c(apply(screenssubsets, 2, mean), apply(screenssubsets, 
                                                           2, median), apply(screenssubsets, 2, min), apply(screenssubsets, 
                                                                                                            2, max))
      screenstender = c(screenstender, sumsubsets)
    }
    x = rbind(x, screenstender)
  }
  x
}

# a function that takes two additional optional parameters. 
# No such argument should be passed when calculating subset screens
genscreen_new <- function(x, tender_MGP, tender_MGP_accepted)
{
  x = sort(x)
  xlose = x[-1]
  lengthx = length(x)
  cv = sd(x)/mean(x)
  spread = (max(x) - min(x))/min(x)
  kurt = kurtosis(x)
  diff = (x[2] - x[1])
  diffp = diff/x[1]
  rd = diff/max(0.5, sd(xlose))
  rdnor = diff/max(0.5, mean(x[-1] - x[-lengthx]))
  rdalt = diff/max(0.5, mean(xlose[-1] - xlose[-length(xlose)]))
  skew = skewness(x)
  standbid = x/sd(x)
  rank = c(1:lengthx)/(lengthx + 1)
  ks = max(max(standbid - rank), max(rank - standbid))
  
  # create a placeholder variable for all the screens
  all_screens = c(var(x), cv, spread, kurt, diff, diffp, rd, rdnor, rdalt, 
                  skew, ks)
  
  # if MGP quantities are passed as an argument, additionally calculate these screens
  if (length(tender_MGP)!=0) {
    tender_MGP = sort(tender_MGP)
    tender_MGP_accepted = sort(tender_MGP_accepted)
    
    MGP_offers = sum(tender_MGP!=0)
    MGP_quantity = sum(tender_MGP)
    
    MGP_accepted_offers = sum(tender_MGP_accepted!=0)
    MGP_accepted_quantity = sum(tender_MGP_accepted)
    
    all_screens = c(all_screens, MGP_offers, MGP_quantity, MGP_accepted_offers, MGP_accepted_quantity)
  }
  
  # return all screens
  all_screens
}


# ASSIGN FUNCTIONS --------------------------------------------------------

environment(carteltraintest_new) <- asNamespace('carteldetection')
assignInNamespace("carteltraintest", carteltraintest_new, ns = "carteldetection")

environment(screens_new) <- asNamespace('carteldetection')
assignInNamespace("screens", screens_new, ns = "carteldetection")

environment(genscreen_new) <- asNamespace('carteldetection')
assignInNamespace("genscreen", genscreen_new, ns = "carteldetection")

# remove functions from global environment
rm(carteltraintest_new, screens_new, genscreen_new)