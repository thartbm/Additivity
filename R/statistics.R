
adaptationFtest <- function() {
  
  # holds for every group:
  pp <- sprintf('p%03d',c(1:24))
  
  # build data frame:
  df <- NA
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    dfr <- read.csv(sprintf('data/%s-training-blocks.csv', groupname), stringsAsFactors = F)
    #adaptation <- as.numeric(dfr[which(dfr$block == 23),pp] - dfr[which(dfr$block == 4),pp])
    
    adaptation <- as.numeric( colMeans( dfr[ which( dfr$block %in% c(23,27,31) ), pp]) - 
                              colMeans( dfr[ which( dfr$block %in% c(4,7,10)   ), pp])   )
    
    
    gdf <- data.frame('participant'=sprintf('%s_p%03d',groupname,c(1:24)), 
                      'adaptation'=adaptation)
    gdf$group <- groupname
    
    if (is.data.frame(df)) {
      df <- rbind(df, gdf)
    } else {
      df <- gdf
    }
    
  }
  
  df$participant <- as.factor(df$participant)
  df$group       <- as.factor(df$group)
  
  aovmod <- afex::aov_ez(id='participant',
                         dv='adaptation',
                         data=df,
                         between='group')

  print(aovmod)
  cat('\n\n')
  print(BayesFactor::anovaBF(adaptation ~ group, whichRandom=c('participant'), data=df))
  cat('\n\n')
  print(aggregate(adaptation ~ group, data=df, FUN=mean))
}

aimingTtest <- function() {
  
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = FALSE)
  
  pp <- unlist(sprintf('p%03d',1:24))
  
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                      colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  
  print(t.test(aims, mu=0, alternative='g'))
  
  print(Reach::etaSquaredTtest(aims, mu=0))
  
}

implicitFtest <- function() {
  
  # holds for every group:
  pp <- sprintf('p%03d',c(1:24))
  
  # build data frame:
  df <- NA
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    gdf <- read.csv(sprintf('data/%s-nocursors-all.csv', groupname), stringsAsFactors = F)
    row.names(gdf) <- gdf$condition
    exclude <- as.numeric(gdf['exclude',pp] - gdf['none',pp])
    #include <- as.numeric(df['include',pp] - df['none',pp])
    
    gdf <- data.frame('participant'=sprintf('%s_p%03d',groupname,c(1:24)), 
                      'implicit'=exclude)
    gdf$group <- groupname
    
    if (is.data.frame(df)) {
      df <- rbind(df, gdf)
    } else {
      df <- gdf
    }
    
  }
  
  df$participant <- as.factor(df$participant)
  df$group       <- as.factor(df$group)
  
  aovmod <- afex::aov_ez(id='participant',
                         dv='implicit',
                         data=df,
                         between='group')
  
  print(aovmod)
  cat('\n\n')
  print(BayesFactor::anovaBF(implicit ~ group, whichRandom=c('participant'), data=df))
  cat('\n\n')
  print(aggregate(implicit ~ group, data=df, FUN=mean))
  
}

includeFtest <- function() {
  
  # holds for every group:
  pp <- sprintf('p%03d',c(1:24))
  
  # build data frame:
  df <- NA
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    gdf <- read.csv(sprintf('data/%s-nocursors-all.csv', groupname), stringsAsFactors = F)
    row.names(gdf) <- gdf$condition
    include <- as.numeric(gdf['include',pp] - gdf['none',pp])
    
    gdf <- data.frame('participant'=sprintf('%s_p%03d',groupname,c(1:24)), 
                      'include'=include)
    gdf$group <- groupname
    
    if (is.data.frame(df)) {
      df <- rbind(df, gdf)
    } else {
      df <- gdf
    }
    
  }
  
  df$participant <- as.factor(df$participant)
  df$group       <- as.factor(df$group)
  
  aovmod <- afex::aov_ez(id='participant',
                         dv='include',
                         data=df,
                         between='group',
                         type=3)
  
  print(aovmod)
  cat('\n\n')
  print(BayesFactor::anovaBF(include ~ group, whichRandom=c('participant'), data=df))
  cat('\n\n')
  print(aggregate(include ~ group, data=df, FUN=mean))
  
  
}

explicitFtest <- function() {
  
  # holds for every group:
  pp <- sprintf('p%03d',c(1:24))
  
  # build data frame:
  df <- NA
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    gdf <- read.csv(sprintf('data/%s-nocursors-all.csv', groupname), stringsAsFactors = F)
    row.names(gdf) <- gdf$condition
    exclude <- as.numeric(gdf['exclude',pp] - gdf['none',pp])
    include <- as.numeric(gdf['include',pp] - gdf['none',pp])
    
    explicit <- include - exclude
    
    gdf <- data.frame('participant'=sprintf('%s_p%03d',groupname,c(1:24)), 
                      'explicit'=explicit)
    gdf$group <- groupname
    
    if (is.data.frame(df)) {
      df <- rbind(df, gdf)
    } else {
      df <- gdf
    }
    
  }
  
  df$participant <- as.factor(df$participant)
  df$group       <- as.factor(df$group)
  
  aovmod <- afex::aov_ez(id='participant',
                       dv='explicit',
                       data=df,
                       between='group',
                       type=3)
  
  print(aovmod)
  cat('\n\n')
  print(BayesFactor::anovaBF(explicit ~ group, whichRandom=c('participant'), data=df))
  cat('\n\n')
  print(aggregate(explicit ~ group, data=df, FUN=mean))
  
  
}

explicitPostHoc <- function() {
  
  # holds for every group:
  pp <- sprintf('p%03d',c(1:24))
  
  # build data frame:
  df <- NA
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    gdf <- read.csv(sprintf('data/%s-nocursors-all.csv', groupname), stringsAsFactors = F)
    row.names(gdf) <- gdf$condition
    exclude <- as.numeric(gdf['exclude',pp] - gdf['none',pp])
    include <- as.numeric(gdf['include',pp] - gdf['none',pp])
    
    explicit <- include - exclude
    
    gdf <- data.frame('participant'=sprintf('%s_p%03d',groupname,c(1:24)), 
                      'explicit'=explicit)
    gdf$group <- groupname
    
    if (is.data.frame(df)) {
      df <- rbind(df, gdf)
    } else {
      df <- gdf
    }
    
  }
  
  df$participant <- as.factor(df$participant)
  df$group       <- as.factor(df$group)
  
  am <- afex::aov_ez(id='participant',
                     dv='explicit',
                     data=df,
                     between='group',
                     type=3)
  
  em <- emmeans::emmeans(am, ~group)
  
  cat('posthoc Tukey HSD:\n')
  
  print(pairs(em))
  
  # Bayesian? JASP -> uncorrected Bayes Factors in output, so it seems hard to do?
  # or we change the prior odds? will not matter for the result, I guess
  
  grouppairs <- list(c('aiming', 'control'), c('aiming', 'instructed'), c('control', 'instructed'))
  
  for (grouppair in grouppairs) {
    
    cat(sprintf('\n%s vs. %s\n\n', toupper(grouppair[1]), toupper(grouppair[2])))
    
    print ( BayesFactor::ttestBF(x = df$explicit[which(df$group == grouppair[1])],
                                 y = df$explicit[which(df$group == grouppair[2])])  )
    
  }
  
}

explicitRegression <- function(intercept=TRUE) {
  
  pp <- sprintf('p%03d',c(1:24))
    
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
  
  #aims <- as.numeric(df[24,pp])
  
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                      colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  
  
  df <- read.csv('data/aiming-nocursors-all.csv', stringsAsFactors = F)
  row.names(df) <- df$condition
  
  explicit <- as.numeric(df['include',pp] - df['exclude',pp])
  
  df <- data.frame('participant'=pp,
                   'aims'=aims,
                   'explicit'=explicit)
  
  if (intercept) {
    print(summary(lm(explicit ~ aims, data=df)))
  } else {
    print(summary(lm(explicit ~ aims - 1, data=df)))
  }
  
}

looseAdditivity <- function() {
  
  # first test stuff in the aiming group
  # (split by aware / unaware? shouldn't matter, right?)
  
  # aiming as estimate of explicit adaptation:
  
  expldf <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = FALSE)
  
  pp <- unlist(sprintf('p%03d',1:24))
  participant <- pp
  
  explicit <- as.numeric( colMeans( expldf[ which( expldf$block %in% c(23,27,31) ), pp]) - 
                            colMeans( expldf[ which( expldf$block %in% c(4,7,10)   ), pp])   )
  
  # exclude strategy no-cursor reaches as estimates of implicit adaptation:
  
  impldf <- read.csv('data/aiming-nocursors-all.csv', stringsAsFactors = F)
  row.names(impldf) <- impldf$condition
  implicit <- as.numeric(impldf['exclude',pp] - impldf['none',pp])
  
  # total adaptation:
  
  adaptdf <- read.csv('data/aiming-training-blocks.csv', stringsAsFactors = F)
  
  adaptation <- as.numeric( colMeans( adaptdf[ which( adaptdf$block %in% c(23,27,31) ), pp]) - 
                              colMeans( adaptdf[ which( adaptdf$block %in% c(4,7,10)   ), pp])   )
  
  
  
  df <- data.frame(participant, explicit, implicit, adaptation)
  
  LAmod <- lm(adaptation ~ explicit + implicit, data=df)
  
  print(summary(LAmod))
  
  # print(summary(lm(adaptation ~ explicit, data=df)))
  # print(summary(lm(adaptation ~ implicit, data=df)))
  
}

# mixture model -----

resetLogLikelihood <- function(par,data,fitFUN) {
  
  L <- fitFUN(par,data)$L
  
  # only positive, numeric values should be used, so
  # replace other values with a very small value:
  L[which(is.na(L) | is.infinite(L) | L<=0)] <- 1e-16
  
  return(sum(log(L)))
  
}

getExplicitData <- function() {
  
  # holds for every group:
  pp <- sprintf('p%03d',c(1:24))
  
  # build data frame:
  df <- NA
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    gdf <- read.csv(sprintf('data/%s-nocursors-all.csv', groupname), stringsAsFactors = F)
    row.names(gdf) <- gdf$condition
    exclude <- as.numeric(gdf['exclude',pp] - gdf['none',pp])
    include <- as.numeric(gdf['include',pp] - gdf['none',pp])
    
    explicit <- include - exclude
    
    gdf <- data.frame('participant'=sprintf('%s_p%03d',groupname,c(1:24)), 
                      'explicit'=explicit)
    gdf$group <- groupname
    
    if (is.data.frame(df)) {
      df <- rbind(df, gdf)
    } else {
      df <- gdf
    }
    
  }
  
  df$participant <- as.factor(df$participant)
  df$group       <- as.factor(df$group)
  
  return(df)
  
}

explicitLikelihood <- function(par, data) {
  
  # in case the parameters don't make sense: a low but possible likelihood:
  low_likelihood <- dnorm(data$explicit, mean=mean(data$explicit), sd=10*sd(data$explicit))
  
  iM <- par['iM']
  iS <- par['iS']
  eM <- par['eM']
  eS <- par['eS']
  f  <- par['f']
  
  if (iM > eM) { return( sum( log(low_likelihood) ) ) }
  
  iL <- dnorm(data$explicit, mean=iM, sd=iS)
  eL <- dnorm(data$explicit, mean=eM, sd=eS)
  
  L <- (f*iL) + ((1-f)*eL)
  
  return( sum( log( L ) ) )
  
}

evaluateModel <- function() {
  
  df <- getExplicitData() 
  
  
  iM <- median(df$explicit[which(df$group=='control')])
  eM <- median(df$explicit[which(df$group=='instructed')])
  iS <-     sd(df$explicit[which(df$group=='control')])
  eS <-     sd(df$explicit[which(df$group=='instructed')])
  
  iPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=iM, sd=iS)
  ePD <- dnorm(df$explicit[which(df$group=='aiming')], mean=eM, sd=eS)
  
  
  fit <- fitModel()
  
  winPar <- fit[['par']]
  
  # mean of control == lower peak?
  print(t.test(df$explicit[which(df$group=='control')],    mu=winPar['iM']))
  cat(sprintf('eta squared: %0.8f\n', Reach::etaSquaredTtest(df$explicit[which(df$group=='control')], mu=winPar['iM'])))
  
  print(BayesFactor::ttestBF(df$explicit[which(df$group=='control')],    mu=winPar['iM']))
  
  # mean of instructed == upper peak?
  print(t.test(df$explicit[which(df$group=='instructed')], mu=winPar['eM']))
  cat(sprintf('eta squared: %0.8f\n', Reach::etaSquaredTtest(df$explicit[which(df$group=='instructed')], mu=winPar['eM'])))

  print(BayesFactor::ttestBF(df$explicit[which(df$group=='instructed')],    mu=winPar['eM']))
  
  # is the fitted fraction the same is what we observe based on probability densities from the control and instructed groups?
  print(binom.test(x=15, n=24, p=winPar['f']))
  
  # participant classification based on the two other conditions:
  InstrCtrl <- iPD > ePD
  
  # 
  loPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=winPar['iM'], sd=winPar['iS'])
  hiPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=winPar['eM'], sd=winPar['eS'])
  Aiming    <- loPD > hiPD
  
  classifications <- data.frame(InstrCtrl, Aiming)
  print(table(InstrCtrl, Aiming))
  
}

AIClL <- function(lL, k) {
  return((2*k) - (2*lL))
}

relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}

fitModel <- function() {
  
  df <- getExplicitData() 
  #cat('loaded data\n')
  
  outputlist <- list()
  
  # first we determine the mixture model, 
  # based on the parameters of the other group:
  
  # iM <- median(df$explicit[which(df$group=='control')])
  # iS <-     sd(df$explicit[which(df$group=='control')])
  # eM <- median(df$explicit[which(df$group=='instructed')])
  # eS <-     sd(df$explicit[which(df$group=='instructed')])
  # 
  # iPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=iM, sd=iS)
  # ePD <- dnorm(df$explicit[which(df$group=='aiming')], mean=eM, sd=eS)
  # 
  # f <- mean(iPD > ePD)
  # 
  # Mpar <- c('iM'=iM,
  #           'iS'=iS,
  #           'eM'=eM,
  #           'eS'=eS,
  #           'f' =f)
  # 
  # mixL <- c('mixL'=explicitLikelihood(par=Mpar, data=df[which(df$group=='aiming'),]))
  # 
  # outputlist[['mixture']]=list('par'=Mpar,'logL'=mixL)
  
  # FULLY FITTED MODEL:
  
  # create search axes:
  iM = seq(0,15,length.out=8)
  iS = seq(0,10,length.out=8)[2:8]
  eM = seq(5,20,length.out=8)
  eS = seq(0,10,length.out=8)[2:8]
  fN = 8
  first = 0.5/(fN+1)
  f  = seq(first,(1-first),length.out = fN)
  
  # make a searchgrid data frames:
  searchgrid <- expand.grid('iM' = iM,
                            'iS' = iS,
                            'eM' = eM,
                            'eS' = eS,
                            'f'  = f   )
  #cat('made search grid\n')
  # get Likelihoods for points in search grid:
  lLs <- apply(searchgrid,FUN=explicitLikelihood,MARGIN=c(1),data=df[which(df$group=='aiming'),])
  
  #cat('did search grid\n')
  
  # get 4 best points in the grid:
  topgrid <- searchgrid[order(lLs, decreasing=TRUE)[c(1,5,9,13)],]
  control <- list( 'maximize' = TRUE )
  
  # do the actual fitting:
  invisible(capture.output(
    allTopFits <- do.call("rbind",
                          apply( topgrid,
                                 MARGIN=c(1),
                                 FUN=optimx::optimx,
                                 fn=explicitLikelihood,
                                 method=c('nlminb'),
                                 lower=c( 0.0,  0.01,  0.0,  0.01, 0.01),
                                 upper=c(20.0, 10.0,  20.0, 10.0,  0.99),
                                 control=control,
                                 data=df[which(df$group=='aiming'),])) 
  ))
  
  # # pick the best fit:
  winFit <- allTopFits[order(allTopFits$value, decreasing=TRUE)[1],]
  
  # get the parameters for the best fit: 
  winPar <- unlist(winFit[c('iM','iS','eM','eS','f')])
  
  fitL <- winFit$value
  names(fitL) <- c('fitL')
  
  #outputlist[['fitted']]=list('par'=winPar,'logL'=fitL)
  
  return( list('par'=winPar,'logL'=fitL) )
  
}

compareBiUniModal <- function() {
  
  
  bimodal <- fitModel()
  bimodal_lL <- as.numeric(bimodal$logL)
  
  
  df <- getExplicitData()
  unimodal_lL <- sum( log( dnorm(df$explicit, mean=mean(df$explicit), sd=sd(df$explicit)) ) )
  
  logL <- c('bimodal'=bimodal_lL, 'unimodal'=unimodal_lL)
  print(logL)
  
  AICs <- AIClL(logL, k=c(5,2))
  print(AICs)
  
  relL <- relativeLikelihood(AICs)
  print(relL)
  
}

# two-rate model parameter recovery simulation -----

parameterRecovery <- function(N=1000) {
  
  # we'll use the parameters for the fit to the control reaches:
  par      <- c('Ls'=0.03494189,
                'Lf'=0.1046407,
                'Rs'=0.9936162,
                'Rf'=0.8854238)
  
  # we need to also get the errors of the model
  # so we get the data:
  df       <- get2rateData(group='control')
  reaches  <- rowMeans(df[,unlist(sprintf('p%03d',c(1:24)))], na.rm=TRUE)
  
  # and the model output, to see the differences
  schedule <- df$rotation * -1
  model    <- Reach::twoRateModel(par=par,
                                  schedule=schedule)$total
  
  # so we will draw from rnorm with this sd:
  noise <- sd( reaches - model, na.rm=TRUE )
  
  # these reaches need to be set to NA:
  NA_idx <- which(is.na(schedule))
  
  Ls <- c()
  Lf <- c()
  Rs <- c()
  Rf <- c()
  
  for (loop in c(1:N)) {
    
    cat(sprintf('loop: %d\n',loop))
    
    reaches <- model + rnorm(n=264, sd=noise)
    reaches[NA_idx] <- NA
    
    rpar <- Reach::twoRateFit(schedule       = schedule,
                              reaches        = reaches,
                              checkStability = TRUE)
    
    Ls <- c(Ls, rpar['Ls'])
    Lf <- c(Lf, rpar['Lf'])
    Rs <- c(Rs, rpar['Rs'])
    Rf <- c(Rf, rpar['Rf'])
    
  }
  
  df <- data.frame(Ls, Lf, Rs, Rf)
  
  write.csv('data/tworate_parameter_recovery.csv', quote=F, row.names=F)
  
}

# linear model data recovery -----

testLinearModel <- function() {
  
  N <- 10
  
  adaptation <- 30 + rnorm(N, sd=5)
  
  explicit <- 5 + (runif(N) * 10)
  
  implicit <- adaptation - explicit
  
  plot(explicit, implicit, xlim=c(0,30),ylim=c(0,35))
  lines(c(0,30),c(30,0),col='red')
  
  print(summary(lm(implicit ~ explicit)))
  
}


runOneAdditiveSimulation <- function(freepar, fixpar) {
  
  par       <- c(freepar, fixpar)
  
  N         <- par['N']
  avg.adapt <- par['avg.adapt'] 
  sd.adapt  <- par['sd.adapt']
  avg.expl  <- par['avg.expl'] 
  sd.expl   <- par['sd.expl']
  sd.impl   <- par['sd.impl']
  
  adaptation <- avg.adapt + rnorm(N, sd=sd.adapt)
  
  explicit   <- avg.expl  + rnorm(N, sd=sd.expl)
  
  implicit <- adaptation - explicit + rnorm(N, sd=sd.impl)
  
  lin.mod <- lm(implicit ~ explicit)
  
  intercept_ci <- confint(lin.mod, parm='(Intercept)', level=0.95)
  slope_ci     <- confint(lin.mod, parm='explicit',    level=0.95)
  
  row.names(intercept_ci) <- c()
  row.names(slope_ci) <- c()
  
  ols_coef <- as.numeric(lin.mod$coef)
  
  odr_beta <- ODR_slope(X=explicit, y=implicit)
  
  return(list('intercept_ols'    = ols_coef[1],
              'slope_ols'        = ols_coef[2],
              'slope_odr'        = odr_beta,
              'intercept_ci'     = intercept_ci,
              'slope_ci'         = slope_ci))
  
}


simulateAdditivityConditions <- function(resamples = 1000) {
  
  N            <- c()
  sd.adapt     <- c()
  sd.expl      <- c()
  
  intercept_ols.lo <- c()
  intercept_ols.up <- c()
  
  slope_ols.lo     <- c()
  slope_ols.up     <- c()

  slope_odr.lo     <- c()
  slope_odr.up     <- c()
  
  sims.done    <- 0
  
  for (this.N in c(10,15,20,25,30,35)) {
    
    for (this.sd.adapt in c(1:10)) {
      
      for (this.sd.expl in c(1:10)) {
        
        slopes_ols     <- matrix(NA, nrow=1 , ncol=resamples)
        intercepts_ols <- matrix(NA, nrow=1 , ncol=resamples)
        slopes_odr     <- matrix(NA, nrow=1 , ncol=resamples)
        
        for (bs in c(1:resamples)) {
          
          fixpar <- c('N'=this.N,
                      'avg.adapt'=30,
                      'sd.adapt'=this.sd.adapt,
                      'avg.expl'=15,
                      'sd.expl'=this.sd.expl,
                      'sd.impl'=5)
          
          output <- runOneAdditiveSimulation(freepar = c(), 
                                             fixpar  = fixpar)
          
          slopes_ols[1,bs]     <- output$slope_ols
          intercepts_ols[1,bs] <- output$intercept_ols
          slopes_odr[1,bs]     <- output$slope_odr
          
        }
        
        slope_ols.ci     <- quantile(slopes_ols,probs=c(0.025,0.975))
        intercept_ols.ci <- quantile(intercepts_ols,probs=c(0.025,0.975))
        slope_odr.ci     <- quantile(slopes_odr,probs=c(0.025,0.975))
        
        N                <- c(N, this.N)
        sd.adapt         <- c(sd.adapt, this.sd.adapt)
        sd.expl          <- c(sd.expl,  this.sd.expl)
        slope_ols.lo     <- c(slope_ols.lo, slope_ols.ci[1])
        slope_ols.up     <- c(slope_ols.up, slope_ols.ci[2])
        intercept_ols.lo <- c(intercept_ols.lo, intercept_ols.ci[1])
        intercept_ols.up <- c(intercept_ols.up, intercept_ols.ci[2])
        slope_odr.lo     <- c(slope_odr.lo, slope_odr.ci[1])
        slope_odr.up     <- c(slope_odr.up, slope_odr.ci[2])
        
        sims.done <- sims.done + 1
        cat(sprintf('simulation %d / 600\n',sims.done))
        
      }
      
    }
    
  }
  
  df <- data.frame(N,
                   sd.adapt,
                   sd.expl,
                   slope_ols.lo,
                   slope_ols.up,
                   intercept_ols.lo,
                   intercept_ols.up,
                   slope_odr.lo,
                   slope_odr.up)
  
  return(df)
  
}

plotAdditivitySimulations <- function() {
  
  if (file.exists('data/strict_simulation.csv')) {
    df <- read.csv('data/strict_simulation.csv', stringsAsFactors = FALSE)
  } else {
    df <- simulateAdditivityConditions()
    write.csv(df,file='data/strict_simulation.csv', row.names = FALSE, quote=FALSE)
  }
  
  for (depvar in c('slope.up','intercept.up'))
  
  layout(mat=matrix(c(1:4), nrow = 2, ncol=2, byrow = TRUE))
  
  depvars <- c('slope_odr.up','slope_odr.lo','slope_ols.up','slope_ols.lo')
  
  expectedvalues <- c(-1,-1,-1,-1)
  
  for (depvarno in c(1,2,3,4)) {
    
    data <- df[,depvars[depvarno]]
    
    ylim <- range(data)
    ylim[1] <- min(ylim[1],-1)
    ylim[2] <- max(ylim[2],1)
    
    plot(data,main=depvars[depvarno],ylim=ylim)
    lines(c(1,600),rep(expectedvalues[depvarno],2),col='red')
    
  }
  
}

additivityNoiseSampleTradeOff <- function() {
  
  
  Ns           <- c(5, 10, 20, 25, 40, 50, 100)
  noises       <- c(1, 3, 5, 7, 9, 11, 13, 15)
  
  grid <- expand.grid('N'=Ns,'std'=noises)
  
  totalsamples <- 100000
  
  
  
  
  for (idx in c(1:dim(grid)[1])) {
    
    
    
  }
  
  sims <- simulatedAdditivity(bootstraps=1000, N=24, normalize=FALSE, std=std)
  
  
}


# ODR -----

# based on: https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca

# there is a closed solution apparently, but it's not different from the one used below
# which *should* also have one solution only, and is faster to write

ODR_slope <- function(X,y=NULL,bootstraps=NA) {

  if (is.na(bootstraps)) {

    if (is.null(y) & (length(dim(X)) > 1)) {
      y <- X[,dim(X)[2]]
      X <- X[,c(1:(dim(X)[2]-1))]
    } else if(is.null(y)) {
      cat('provide a y variable\n')
    }

    v <- prcomp(cbind(X,y))$rotation
    beta <- -v[-ncol(v),ncol(v)] / v[ncol(v),ncol(v)]
    return(beta)

  } else {
    resamples <- sample(c(1:length(y)),
                        size=(length(y)*bootstraps),
                        replace = TRUE)
    if (is.null(dim(X))) {
      strict_data <- array(NA, dim=c(bootstraps,length(y),2))
      strict_data[,,2] <- y[resamples]
      strict_data[,,1] <- X[resamples]
    } else {
      print(dim(X))
      strict_data <- array(NA, dim=c(bootstraps,length(y),dim(X)[2]+1))
      print(dim(strict_data))
      strict_data[,,(dim(X)[2]+1)] <- y[resamples]
      strict_data[,,c(1:(dim(X)[2]))] <- X[,resamples]
    }

    return( quantile( unlist(apply(strict_data, MARGIN=c(1), FUN=ODR_slope)), probs=c(0.025,0.975) ) )
  }

}

# XY trend/ksmooth -----

getTrendCI <- function(x, y, bootstraps = 1000, kernel, bandwidth, x.points) {
  
  idx <- sample(c(1:length(x)),size=length(x)*bootstraps,replace=TRUE)

  mat <- array(data=NA, dim=c(bootstraps,length(x),2))
  mat[,,1] <- x[idx]
  mat[,,2] <- y[idx]
  
  op <- rbind( apply(mat,
                     MARGIN=c(1),
                     FUN=getKsmoothY,
                     kernel=kernel,
                     bandwidth=bandwidth,
                     x.points=x.points) )
  
  CI <- apply(op,
              MARGIN=c(1),
              quantile,
              probs=c(0.025,0.975),
              na.rm=TRUE)
  
  return(CI)
  
}

getKsmoothY <- function(m,kernel,bandwidth,x.points) {
  
  # should we check for data within bounds?
  
  ks <- ksmooth(x=m[,1],y=m[,2],
                kernel=kernel,
                bandwidth=bandwidth,
                x.points=x.points)
  
  return(ks$y)
  
}

