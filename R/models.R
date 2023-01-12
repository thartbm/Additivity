# general ------

compareModels <- function(df) {
  
  # df <- bindExtraData()
  # df <- df[ which( !df$grouplabel %in% c("stepwise, 15° (N=37)", "stepwise, 30°", "stepwise, 45°") ), ]
  # 
  # df$norm.expl <- df$explicit / df$group_adaptation
  # df$norm.impl <- df$implicit / df$group_adaptation
  
  #df <- df[ which( df$norm.expl > 0.0 & df$norm.expl < 1.0 ), ]
  
  MSEs <- c('additivity'=getAdditivityMSE(df),
           # 'polynomial'=getPolynomialMSE(df),
            'maxlimited'=getMaxlimitedMSE(df),
            'fractionLeft'=getFractionLeftMSE(df))
  
  AICs <- AIC(MSEs, k=c(2,2,1), N=dim(df)[1])
  
  print(MSEs)
  print(AICs)
  print(relativeLikelihood(AICs))
  
}

plotModels <- function() {
  
  
  layout(mat=matrix(c(1,2),nrow=1,ncol=2))
  
  
  df <- bindExtraData()
  df <- df[ which( !df$grouplabel %in% c("stepwise, 15° (N=37)", "stepwise, 30°", "stepwise, 45°") ), ]
  
  df$norm.expl <- df$explicit / df$group_adaptation
  df$norm.impl <- df$implicit / df$group_adaptation
  
  #df <- df[ which( df$norm.expl > 0.0 & df$norm.expl < 1.0 ), ]
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
       bty='n',ax=F,asp=1)
  
  lines(c(0,1),c(1,0),lty=3,lw=2)
  
  points(df$norm.expl, df$norm.impl, pch=16, col='#66669927',cex=2)
  
  # first the two LM-based models:
  a_fit <- fitAdditivity(df)
  p_fit <- fitPolynomial(df)
  
  newdata <- data.frame('norm.expl'=seq(-0,1,0.005))
  
  a_pred <- predict.lm(a_fit, newdata=newdata)
  lines(newdata$norm.expl, a_pred, col='red', lw=2)
  
  #p_pred <- predict.lm(p_fit, newdata=newdata)
  #lines(newdata$norm.expl, p_pred, col='purple', lw=2)

  # this is the first one I came up with:
  l_fit <- fitMaxlimited(df)
  newdata$group_adaptation <- mean(df$group_adaptation)
  l_pred <- maxLimited(par=l_fit, df=newdata)
  lines(newdata$norm.expl, l_pred, col='blue', lw=2)
  
  # and a second one:
  f_fit <- fitFractionLeft(df)
  f_pred <- fractionLeft(par=f_fit, df=newdata)
  lines(newdata$norm.expl, f_pred, col='purple', lw=2)

  title(xlab='explicit [norm]')
  title(ylab='implicit [norm]')
  
  axis(side=1, at=c(0,1))
  axis(side=2, at=c(0,1))
  
  
  
  df <- df[ which( df$norm.expl > 0.0 & df$norm.expl < 1.0 ), ]
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
       bty='n',ax=F,asp=1)
  
  lines(c(0,1),c(1,0),lty=3,lw=2)
  
  points(df$norm.expl, df$norm.impl, pch=16, col='#66669927',cex=2)
  
  # first the two LM-based models:
  a_fit <- fitAdditivity(df)
  p_fit <- fitPolynomial(df)
  
  newdata <- data.frame('norm.expl'=seq(-0,1,0.005))
  
  a_pred <- predict.lm(a_fit, newdata=newdata)
  lines(newdata$norm.expl, a_pred, col='red', lw=2)
  
  #p_pred <- predict.lm(p_fit, newdata=newdata)
  #lines(newdata$norm.expl, p_pred, col='purple', lw=2)
  
  # this is the first one I came up with:
  l_fit <- fitMaxlimited(df)
  newdata$group_adaptation <- mean(df$group_adaptation)
  l_pred <- maxLimited(par=l_fit, df=newdata)
  lines(newdata$norm.expl, l_pred, col='blue', lw=2)
  
  # and a second one:
  f_fit <- fitFractionLeft(df)
  f_pred <- fractionLeft(par=f_fit, df=newdata)
  lines(newdata$norm.expl, f_pred, col='purple', lw=2)
  
  title(xlab='explicit [norm]')
  title(ylab='implicit [norm]')
  
  axis(side=1, at=c(0,1))
  axis(side=2, at=c(0,1))
  
  
}

AIC <- function(MSE, k, N) {
  return( (N * log(MSE)) + (2 * k) )
}

AICc <- function(MSE, k, N) {
  return( AIC(MSE, k, N) * (((2*k^2) + 2*k) / (N - k - 1)) )
}

relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}

# slope -----

getAdditivityMSE <- function(df) {
  
  # since this is a standard `lm()`, I don't need an optim structure,
  # but, in order to fit with the rest, I'll make it behave the same way
  
  mfit <- fitAdditivity(df)
  
  return(MSEadditivity(mfit, df))
  
}

fitAdditivity <- function(df) {
  
  return( lm(norm.impl ~ norm.expl, data=df) )
  
}

MSEadditivity <- function(mfit, df) {
  
  predictions <- predict.lm(mfit)
  
  MSE <- mean( (df$norm.impl - predictions)^2 )
  
  return( MSE )
  
}

# polynomial -----

getPolynomialMSE <- function(df) {
  
  mfit <- fitPolynomial(df)
  
  return(MSEpolynomial(mfit, df))
  
}

fitPolynomial <- function(df) {
  
  return( lm(norm.impl ~ poly(norm.expl, 2), data=df) )
  
}

MSEpolynomial <- function(mfit, df) {
  
  predictions <- predict.lm(mfit)
  
  MSE <- mean( (df$norm.impl - predictions)^2 )
  
  return( MSE )
  
}

# max-limited -----

getMaxlimitedMSE <- function(df) {
  
  mfit <- fitMaxlimited(df)
  
  return(MSEmaxlimited(mfit, df))
  
}

fitMaxlimited <- function(df) {
  
  # search grid:
  maxImpl  <- c(5:30)
  fracExpl <- seq(0.5,0.99,length.out=50)
  
  searchgrid <- expand.grid('maxImpl'  = maxImpl,
                            'fracExpl' = fracExpl)
  
  searchMSE <- apply(searchgrid,
                     FUN=MSEmaxlimited,
                     MARGIN=c(1),
                     df=df)
  
  # this works for single parameters (unusual situation):
  #topgrid <- data.frame('maxImpl'=searchgrid[order(searchMSE)[c(1,3,5)],])
  
  # get best points in the grid:
  topgrid <- searchgrid[order(searchMSE)[c(1,5,9)],]
  
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=MSEmaxlimited,
                            method='L-BFGS-B',
                            lower=c(1,   0.01),
                            upper=c(100, 1),
                            df=df ) )
  
  #print(allFits)
  
  # pick the best fit:
  winFit <- allFits[order(allFits$value)[1],]
  
  winPar <- unlist(winFit[c('maxImpl','fracExpl')])
  
  return(winPar)
  
}

MSEmaxlimited <- function(par, df) {
  
  predictions <- maxLimited(par, df)
  
  MSE <- mean( (df$norm.impl - predictions)^2 )
  
  return( MSE )
  
}

maxLimited <- function(par, df) {
  
  #print(par)
  
  maxImpl  <- par['maxImpl']
  fracExpl <- par['fracExpl']
  
  implicit <- pmin( unlist(maxImpl / df$adaptation),
                    unlist(1-df$norm.expl)*fracExpl )
  
  #print(implicit)
  
  return(implicit)
  
}

# fractionLeft -----

getFractionLeftMSE <- function(df) {
  
  mfit <- fitFractionLeft(df)
  
  return(MSEfractionLeft(mfit, df))
  
}

fitFractionLeft <- function(df) {
  
  implFrac <- seq(0.005,0.995,length.out=100)
  implCap  <- seq(2,30,0.5)
  
  searchgrid <- expand.grid('implCap'  = implCap)
  
  searchMSE <- apply(searchgrid,
                     FUN=MSEfractionLeft,
                     MARGIN=c(1),
                     df=df)
  
  # this works for single parameters (unusual situation):
  topgrid <- data.frame('implCap'=searchgrid[order(searchMSE)[c(1,3,5)],])
  
  # get best points in the grid:
  #topgrid <- searchgrid[order(searchMSE)[c(1,5,9)],]
  
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=MSEfractionLeft,
                            method='L-BFGS-B',
                            lower=c(0.1),
                            upper=c(100),
                            df=df ) )
  
  #print(allFits)
  
  # pick the best fit:
  winFit <- allFits[order(allFits$value)[1],]
  
  winPar <- unlist(winFit[c('implCap')])
  
  return(winPar)
  
}

MSEfractionLeft <- function(par, df) {
  
  predictions <- fractionLeft(par, df)
  
  MSE <- mean( (df$norm.impl - predictions)^2 )
  
  return( MSE )
  
}

fractionLeft <- function(par, df) {
  
  implFrac <- 1.0000
  implCap  <- par['implCap']
  
  explicit  <- unlist(df$norm.expl)
  basefrac  <- unlist(implCap/df$adaptation)
  roomleft  <- (1 - explicit)
  usedfrac  <- basefrac + explicit * (implFrac - basefrac)
  
  implicit  <- roomleft * usedfrac
  
  return(implicit)
  
}

# maximum likelihood estimate -----

MLE_adaptation <- function() {
  
  df <- read.csv('data/external_data.csv', stringsAsFactors = F)
  
  df <- df[df$explicit.method=='aim.reports' & !is.na(df$explicit_sd),]
  
  df$implicit_sd <- df$implicit_sd + 10^-10
  df$explicit_sd <- df$explicit_sd + 10^-10
  
  w_i <- (1/(df$implicit_sd^2)) / ((1/(df$implicit_sd^2)) + (1/(df$explicit_sd^2)))
  w_e <- (1/(df$explicit_sd^2)) / ((1/(df$implicit_sd^2)) + (1/(df$explicit_sd^2)))
  
  a_hat <- ((w_i * df$implicit) + (w_e * df$explicit))
  
  return(data.frame('a_hat'=a_hat,
                    'adaptation'=df$adaptation,
                    'explicit'=df$explicit,
                    'w_explicit'=(w_e * df$explicit),
                    'implicit'=df$implicit,
                    'w_implicit'=(w_i * df$implicit),
                    'participant'=df$participant,
                    'rotation'=df$rotation,
                    'group'=df$group,
                    'grouplabel'=df$grouplabel,
                    'paper'=df$paper))
  
}

fitMLEmodels_old <- function() {
  
  layout(mat=matrix(c(1:6),nrow = 2, ncol = 3))
  
  df <- MLE_adaptation()
  
  
  # additive model:
  a_hat <- df$explicit + df$implicit
  plot(a_hat, df$adaptation, asp=1, main='additive + constant',
       xlim=c(-15,135),ylim=c(-15,135))
  lines(c(0,110),c(0,110),col='blue')
  
  add_RMSE <- mean(((a_hat/df$rotation) - (df$adaptation/df$rotation))^2)
  print(add_RMSE)
  
  par <- c('offset'=0)
  mdf <- data.frame( 'a_hat' = a_hat,
                     'adaptation' = df$adaptation,
                     'rotation'   = df$rotation)
  
  offset_fit <- optimx::optimx( par = par,
                                fn = a_hat_offset, 
                                lower = c(-100),
                                upper = c( 100),
                                method = 'L-BFGS-B',
                                df = mdf )
  
  #print(offset_fit)
  
  add_offset_RMSE <- offset_fit$value[1]
  print(add_offset_RMSE)
  
  # maximum likelihood model
  a_hat <- df$a_hat
  plot(a_hat, df$adaptation, asp=1, main='MLE + constant',
         xlim=c(-15,135),ylim=c(-15,135))
  lines(c(0,110),c(0,110),col='blue')
  
  MLE_RMSE <- mean(((a_hat/df$rotation) - (df$adaptation/df$rotation))^2)
  print(MLE_RMSE)
  
  par <- c('offset'=0)
  mdf <- data.frame( 'a_hat' = a_hat,
                     'adaptation' = df$adaptation,
                     'rotation'   = df$rotation)
  
  offset_fit <- optimx::optimx( par = par,
                                fn = a_hat_offset, 
                                lower = c(-100),
                                upper = c( 100),
                                method = 'L-BFGS-B',
                                df = mdf )
  
  #print(offset_fit)
  MLE_offset_RMSE <- offset_fit$value[1]
  print(MLE_offset_RMSE)
  
  
  MSE <- c(#'additive'        = add_RMSE,
           'additive+offset' = add_offset_RMSE,
           #'MLE'             = MLE_RMSE,
           'MLE+offset'      = MLE_offset_RMSE)
  
  AICs <- AICc(MSE = MSE, 
               k   = c(1,1), 
               N   = 127)
    
  print(AICs)
  print(relativeLikelihood(AICs))
  
  
  # # # # # # # # # # # # # # # # 
  # now add two overall weights:
  
  # for the additive case:
  mdf <- data.frame('adaptation' = df$adaptation,
                    'E'          = df$explicit,
                    'I'          = df$implicit)
  
  # print(str(mdf))
  
  searchgrid <- expand.grid('w_e'=seq(0.1,1.9,0.1),
                            'w_i'=seq(0.1,1.9,0.1))
  
  searchMSE <- apply(searchgrid,
                     FUN=two_weights_MSE,
                     MARGIN=c(1),
                     df=mdf)
  
  # get best points in the grid:
  topgrid <- searchgrid[order(searchMSE)[c(1:9)],]
  
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=two_weights_MSE,
                            method='L-BFGS-B',
                            lower=c(0, 0),
                            upper=c(2, 2),
                            df=mdf ) )
  
  #print(allFits)
  
  # pick the best fit:
  winFit <- allFits[order(allFits$value)[1],]
  print(winFit)
  winpar <- c('w_e'=as.numeric(winFit$w_e[1]),
              'w_i'=as.numeric(winFit$w_i[1]))
  

  a_hat <- two_weights_model(par=winpar, df=mdf)
  
  plot(a_hat, mdf$adaptation, asp=1, main='additive, 2 weights',
       xlim=c(-15,135),ylim=c(-15,135))
  lines(c(0,110),c(0,110),col='blue')
  
  # scale by rotation to get comparable MSE:
  
  aw_MSE <- mean( ((a_hat/df$rotation)-(mdf$adaptation/df$rotation))^2 ) 
  
  MSE <- c(MSE, 'additive weighted'=aw_MSE)
  
  # for the maximum likelihood case:
  # for the additive case:
  mdf <- data.frame('adaptation' = df$adaptation,
                    'E'          = df$w_explicit,
                    'I'          = df$w_implicit)
  
  searchgrid <- expand.grid('w_e'=seq(0.1,1.9,0.1),
                            'w_i'=seq(0.1,1.9,0.1))
  
  searchMSE <- apply(searchgrid,
                     FUN=two_weights_MSE,
                     MARGIN=c(1),
                     df=mdf)
  
  # get best points in the grid:
  topgrid <- searchgrid[order(searchMSE)[c(1:9)],]
  
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=two_weights_MSE,
                            method='L-BFGS-B',
                            lower=c(0, 0),
                            upper=c(2, 2),
                            df=mdf ) )
  
  #print(allFits)
  
  # pick the best fit:
  winFit <- allFits[order(allFits$value)[1],]
  print(winFit)
  winpar <- c('w_e'=as.numeric(winFit$w_e[1]),
              'w_i'=as.numeric(winFit$w_i[1]))
  
  
  a_hat <- two_weights_model(par=winpar, df=mdf)
  
  plot(a_hat, df$adaptation, asp=1, main='MLE, 2 weights',
       xlim=c(-15,135),ylim=c(-15,135))
  lines(c(0,110),c(0,110),col='blue')
  
  mw_MSE <- mean( ((a_hat/df$rotation)-(mdf$adaptation/df$rotation))^2 ) 
  
  MSE <- c(MSE, 'MLE weighted'=mw_MSE)
  
  #print(MSE)
  
  AICs <- AICc(MSE = MSE, 
               k   = c(1,1,2,2), 
               N   = 127)
  
  #print(AICs)
  print(relativeLikelihood(AICs))
  
  # # # # # # # # # # # # # # # # # # # #
  # scaled rotation plus Imp & Exp ???
  
  # for the additive case:
  mdf <- data.frame('adaptation' = df$adaptation,
                    'E'          = df$explicit,
                    'I'          = df$implicit,
                    'rotation'   = df$rotation)
  
  # print(str(mdf))
  
  searchgrid <- expand.grid('w_e'=seq(0.1,1.9,0.1),
                            'w_i'=seq(0.1,1.9,0.1),
                            'c_r'=seq(0.1,1.9,0.1))
  
  searchMSE <- apply(searchgrid,
                     FUN=scaledrotation_weights_MSE,
                     MARGIN=c(1),
                     df=mdf)
  
  # get best points in the grid:
  topgrid <- searchgrid[order(searchMSE)[c(1:9)],]
  
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=scaledrotation_weights_MSE,
                            method='L-BFGS-B',
                            lower=c(0, 0, 0),
                            upper=c(2, 2, 2),
                            df=mdf ) )
  
  #print(allFits)
  
  # pick the best fit:
  winFit <- allFits[order(allFits$value)[1],]
  print(winFit)
  winpar <- c('w_e'=as.numeric(winFit$w_e[1]),
              'w_i'=as.numeric(winFit$w_i[1]),
              'c_r'=as.numeric(winFit$c_r[1]))
  
  
  a_hat <- scaledrotation_weights_model(par=winpar, df=mdf)
  
  plot(a_hat, mdf$adaptation, asp=1, main='additive, 2 weights, scaled rot',
       xlim=c(-15,135),ylim=c(-15,135))
  lines(c(0,110),c(0,110),col='blue')
  
  # scale by rotation to get comparable MSE:
  
  aw_sr_MSE <- mean( ((a_hat/df$rotation)-(mdf$adaptation/df$rotation))^2 ) 
  
  MSE <- c(MSE, 'additive weighted, scaled'=aw_sr_MSE)
  
  # for the MLE case:
  
  mdf <- data.frame('adaptation' = df$adaptation,
                    'E'          = df$w_explicit,
                    'I'          = df$w_implicit,
                    'rotation'   = df$rotation)
  
  # print(str(mdf))
  
  searchgrid <- expand.grid('w_e'=seq(0.1,1.9,0.1),
                            'w_i'=seq(0.1,1.9,0.1),
                            'c_r'=seq(0.1,1.9,0.1))
  
  searchMSE <- apply(searchgrid,
                     FUN=scaledrotation_weights_MSE,
                     MARGIN=c(1),
                     df=mdf)
  
  # get best points in the grid:
  topgrid <- searchgrid[order(searchMSE)[c(1:9)],]
  
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=scaledrotation_weights_MSE,
                            method='L-BFGS-B',
                            lower=c(0, 0, 0),
                            upper=c(2, 2, 2),
                            df=mdf ) )
  
  #print(allFits)
  
  # pick the best fit:
  winFit <- allFits[order(allFits$value)[1],]
  print(winFit)
  winpar <- c('w_e'=as.numeric(winFit$w_e[1]),
              'w_i'=as.numeric(winFit$w_i[1]),
              'c_r'=as.numeric(winFit$c_r[1]))
  
  
  a_hat <- scaledrotation_weights_model(par=winpar, df=mdf)
  
  plot(a_hat, mdf$adaptation, asp=1, main='MLE, 2 weights, scaled rot',
       xlim=c(-15,135),ylim=c(-15,135))
  lines(c(0,110),c(0,110),col='blue')
  
  # scale by rotation to get comparable MSE:
  
  mw_sr_MSE <- mean( ((a_hat/df$rotation)-(mdf$adaptation/df$rotation))^2 ) 
  
  MSE <- c(MSE, 'MLE weighted, scaled'=mw_sr_MSE)
  
  
  #print(MSE)
  
  AICs <- AICc(MSE = MSE, 
               k   = c(1,1,2,2,3,3), 
               N   = 127)
  
  #print(AICs)
  print(relativeLikelihood(AICs))
  
  
}


fitMLEmodels <- function() {
  
  layout(mat=matrix(c(1:4),nrow = 2, ncol = 2, byrow=TRUE))
  
  df <- MLE_adaptation()
  
  
  # SCALED ADDITIVE MODEL
  par <- c('s'=1)
  mdf <- data.frame( 'E'          = df$explicit,
                     'I'          = df$implicit,
                     'adaptation' = df$adaptation,
                     'rotation'   = df$rotation)
  
  
  a_hat <- scaled_components_model(par=c('s'=1),
                                   df=mdf) / mdf$rotation
  adapt <- mdf$adaptation/mdf$rotation
  add_one_MSE <- mean( (a_hat - adapt)^2 )
  #add_scaled_MSE <- offset_fit$value[1]
  print(add_one_MSE)
  
  
  # additive model:
  plot(a_hat, adapt, asp=1, main='additive scale=1',
       xlim=c(0,2),ylim=c(0,2))
  lines(c(0,2),c(0,2),col='blue')
  
  
  
  offset_fit <- optimx::optimx( par = par,
                                fn = scaled_components_MSE, 
                                lower = c(10^-10),
                                upper = c(3),
                                method = 'L-BFGS-B',
                                df = mdf )
  
  print(offset_fit)
  
  winpar <- c('s' = as.numeric(offset_fit$s[1]))
  a_hat <- scaled_components_model(par=winpar,
                                   df=mdf) / mdf$rotation
  adapt <- mdf$adaptation/mdf$rotation
  add_scaled_MSE <- mean( (a_hat - adapt)^2 )
  #add_scaled_MSE <- offset_fit$value[1]
  print(add_scaled_MSE)
  
  
  # additive model:
  plot(a_hat, adapt, asp=1, main='additive scale=0.923',
       xlim=c(0,2),ylim=c(0,2))
  lines(c(0,2),c(0,2),col='blue')

  
  
  # maximum likelihood model
  # a_hat <- df$a_hat
  # plot(a_hat, df$adaptation, asp=1, main='MLE + constant',
  #      xlim=c(-15,135),ylim=c(-15,135))
  # lines(c(0,110),c(0,110),col='blue')
  
  par <- c('s'=2)
  mdf <- data.frame( 'E'          = df$w_explicit,
                     'I'          = df$w_implicit,
                     'adaptation' = df$adaptation,
                     'rotation'   = df$rotation)
  
  a_hat <- scaled_components_model(par=c('s'=2),
                                   df=mdf) / mdf$rotation
  adapt <- mdf$adaptation/mdf$rotation
  MLE_two_MSE <- mean( (a_hat - adapt)^2 )
  
  #MLE_scaled_MSE <- offset_fit$value[1]
  print(MLE_two_MSE)
  
  # MLE model:
  plot(a_hat, adapt, asp=1, main='MLE scale=2',
       xlim=c(0,2),ylim=c(0,2))
  lines(c(0,2),c(0,2),col='blue')
  
  
  offset_fit <- optimx::optimx( par = par,
                                fn = scaled_components_MSE, 
                                lower = c(10^-10),
                                upper = c(3),
                                method = 'L-BFGS-B',
                                df = mdf )
  
  print(offset_fit)
  
  winpar <- c('s' = as.numeric(offset_fit$s[1]))
  a_hat <- scaled_components_model(par=winpar,
                                   df=mdf) / mdf$rotation
  adapt <- mdf$adaptation/mdf$rotation
  MLE_scaled_MSE <- mean( (a_hat - adapt)^2 )
  
  #MLE_scaled_MSE <- offset_fit$value[1]
  print(MLE_scaled_MSE)
  
  # MLE model:
  plot(a_hat, adapt, asp=1, main='MLE scale=1.440',
       xlim=c(0,2),ylim=c(0,2))
  lines(c(0,2),c(0,2),col='blue')

    
  MSE <- c( 
            'additive one'    = add_one_MSE,
          #  'additive scaled' = add_scaled_MSE,
            'MLE two'         = MLE_two_MSE
          #  'MLE scaled'      = MLE_scaled_MSE
            )
  
  AICs <- AICc(MSE = MSE, 
           #    k   = c(10^-10,1,10^-10,1), 
               k   = c(10^-10,10^-10),
               N   = 127)
  
  print(AICs)
  print(relativeLikelihood(AICs))
  
}



a_hat_offset <- function(par, df) {
  
  offset <- par['offset']
  return( mean( ( ((df$a_hat + offset)/df$rotation) - (df$adaptation/df$rotation) )^2 ) )
  
}

two_weights_model <- function(par, df) {
  
  # weight parameters:
  w_e <- par['w_e']
  w_i <- par['w_i']
  
  # return predicted adaptation
  return( (w_e * df$E) + (w_i * df$I) )
  
}

two_weights_MSE <- function(par, df) {
  
  a_hat <- two_weights_model(par, df)
  
  return( mean( (df$adaptation - a_hat)^2 ) )
  
}

scaledrotation_weights_model <- function(par, df) {
  
  # weight parameters:
  w_e <- par['w_e']
  w_i <- par['w_i']
  
  # rotation scale:
  c_r <- par['c_r']

  # return predicted adaptation
  return( (c_r * df$rotation) + (w_e * df$E) + (w_i * df$I) )
  
}

scaledrotation_weights_MSE <- function(par, df) {
  
  a_hat <- scaledrotation_weights_model(par, df)
  
  return( mean( (df$adaptation - a_hat)^2 ) )
  
}

scaled_components_model <- function(par, df) {
 
  # component scale:
  s <- par['s']
  
  # return predicted adaptation
  return( (s * df$E) + (s * df$I) )
  
}

scaled_components_MSE <- function(par, df) {
  
  a_hat <- scaled_components_model(par, df)
  
  return( mean( (df$adaptation - a_hat)^2 ) )
  
}


# additivity recovery -----

simulatedAdditivity <- function(bootstraps=5000, N=24, normalize=FALSE) {
  
  # run this on the aiming group for now:
  
  #N          <- 24
  #bootstraps <- 5000
  
  set.seed(1)
  
  # implicit <- matrix( rnorm(n    = N*bootstraps,
  #                           mean = 15,
  #                           sd   = 5), nrow=bootstraps, ncol=N)
  explicit <- matrix( rnorm(n    = N*bootstraps,
                            mean = 20,
                            sd   = 5), nrow=bootstraps, ncol=N)
  
  adaptation_noise <- matrix( rnorm(n    = N*bootstraps,
                                    mean = 0,
                                    sd   = 5), nrow=bootstraps, ncol=N)
  implicit_noise <- matrix( rnorm(n    = N*bootstraps,
                                  mean = 0,
                                  sd   = 5), nrow=bootstraps, ncol=N)
  
  # # ADDITIVITY:
  # adaptation <- implicit + explicit + adaptation_noise
  
  # ADDITIVITY:
  adaptation <- 40 + adaptation_noise
  implicit <- adaptation - explicit + implicit_noise
  
  
  # normalize:
  if (normalize) {
    implicit <- implicit / adaptation
    explicit <- explicit / adaptation
  }
  
  # set up vectors to collect simulated data:
  intercept <- c()
  slope <- c()
  exp_min <- c()
  exp_max <- c()
  lo <- c()
  hi <- c()
  include1 <- c()
  
  for (bs in c(1:bootstraps)) {
    
    impl <- implicit[bs,]
    expl <- explicit[bs,]
    
    exp_min <- c(exp_min, min(expl))
    exp_max <- c(exp_max, max(expl))
    
    i_lm <- lm(impl ~ expl)
    intercept <- c(intercept, as.numeric(i_lm$coefficients[1]))
    slope <- c(slope, as.numeric(i_lm$coefficients[2]))
    CI <- confint(i_lm,parm='expl',level=0.95)
    
    lo <- c(lo, CI[1])
    hi <- c(hi, CI[2])
    if (CI[1] < -1 & CI[2] > -1) {
      include1 <- c(include1, TRUE)
    } else {
      include1 <- c(include1, FALSE)
    }
    
  }
  
  print(mean(include1))
  
  return(list('simulation'=data.frame(intercept,slope,exp_min,exp_max,lo,hi,include1),
              'data'=list('implicit'=implicit,
                          'explicit'=explicit,
                          'adaptation'=adaptation)) )
}

plotAdditivityRecovery <- function(bootstraps=5000) {
  
  sim <- simulatedAdditivity(bootstraps=bootstraps,
                            N=24)
  df <- sim[['simulation']]
  data <- sim[['data']]
  groups <- getGroups()
  
  layout(mat=matrix(c(1,2),nrow=1,ncol=2,byrow=TRUE))
  
  plot(-1000,-1000,main='simulated additivity slopes',
       xlim=c(0,3),ylim=c(-2.2,0.2),
       xlab='descriptive',ylab='slope',
       ax=F,bty='n')
  
  lines(c(0,3),c(-1,-1),col='black')
  
  slopeCI <- quantile(df$slope, probs=c(0.025, 0.5, 0.975))
  loCI    <- quantile(df$lo,    probs=c(0.025, 0.5, 0.975))
  hiCI    <- quantile(df$hi,    probs=c(0.025, 0.5, 0.975))
  
  print(slopeCI)
  print(loCI)
  print(hiCI)
  
  polygon(x=c(1/3,2/3,2/3,1/3)+0,y=rep(slopeCI[c(1,3)],each=2),border=NA,col=groups$col.tr[3])
  lines(x=c(1/3,2/3)+0,y=rep(slopeCI[c(2)],2),col=groups$col.op[3])
  
  polygon(x=c(1/3,2/3,2/3,1/3)+1,y=rep(loCI[c(1,3)],each=2),border=NA,col=groups$col.tr[1])
  lines(x=c(1/3,2/3)+1,y=rep(loCI[c(2)],2),col=groups$col.op[1])
  polygon(x=c(1/3,2/3,2/3,1/3)+2,y=rep(hiCI[c(1,3)],each=2),border=NA,col=groups$col.tr[5])
  lines(x=c(1/3,2/3)+2,y=rep(hiCI[c(2)],2),col=groups$col.op[5])
  
  axis(side=1,at=c(0.5, 1.5, 2.5),labels=c('mean 95%CI','lower 95%CI','upper 95%CI'))
  axis(side=2,at=c(-2,-1,0))
  
  # NEW PLOT
  
  plot(-1000,-1000,
       xlim=c(-5,50),ylim=c(-5,50),
       main='',xlab='',ylab='',
       bty='n',ax=F)
  
  # df <- data.frame('implicit' = data[['implicit']][1,],
  #                  'explicit' = data[['explicit']][1,])
  # 
  # linreg <- lm(implicit ~ explicit, data=df)
  # print(summary(linreg))
  # 
  # ci <- predict( linreg,
  #                newdata=data.frame(explicit=seq(10,30,.2)),
  #                interval = "confidence")
  # 
  # #print(ci)
  # 
  # X <- c(seq(10,30,.2),rev(seq(10,30,.2)))
  # Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  # polygon(x=X,y=Y,col='red')
  # 
  # print(ci[,'lwr'])
  # print(ci[,'upr'])
  # 
  # print(X)
  # print(Y)
  
  # str(df)
  for (bs in c(1:bootstraps)) {
    X <- df[bs,c('exp_min','exp_max')]
    lines(x=X,
          y=df$intercept[bs] + (X*df$slope[bs]),
          col='#9966ff0f')
  }
  
  
  axis(side=1,at=c(0,15,30,45))
  axis(side=2,at=c(0,15,30,45))
  
  #dev.off()
  
}