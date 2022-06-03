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
  
  implicit <- pmin( unlist(maxImpl / df$group_adaptation),
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
  basefrac  <- unlist(implCap/df$group_adaptation)
  roomleft  <- (1 - explicit)
  usedfrac  <- basefrac + explicit * (implFrac - basefrac)
  
  implicit  <- roomleft * usedfrac
  
  return(implicit)
  
}