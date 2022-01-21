
# support functions -----

getGroups <- function() {
  
  
  group <- c('control', 'instructed', 'aiming', 'aims')
  label <- c('control', 'instructed', 'aiming', 'aims')
  col.op <- c('#ff9329ff', '#e51636ff', '#7f00d8ff', '#cc00ccff')
  col.tr <- c('#ff93292f', '#e516362f', '#7f00d82f', '#cc00cc2f')
  
  return(data.frame(group, label, col.op, col.tr))
  
  
}


plotBlocks <- function(textsize=0.8) {
  
  
  # aligned: 32       1  - 32
  # no-cursor: 8      33 - 40
  # top-up: 16        41 - 56
  # no-cursor: 8      57 - 64
  # top-up: 16        65 - 80
  # no-cursor: 8      81 - 88
  
  # 32 + (3*8) + (2*16) = 88
  
  text(17,-7.5,'32',adj=c(.5,.5), cex=textsize)
  text(37,-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(49,-7.5,'16',adj=c(.5,.5), cex=textsize)
  text(61,-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(73,-7.5,'16',adj=c(.5,.5), cex=textsize)
  text(85,-7.5,'8',adj=c(.5,.5), cex=textsize)
  
  # rotated: 96       89  - 184
  # no-cursor: 16     185 - 200
  # top-up: 16        201 - 216
  # no-cursor: 16     217 - 232
  # top-up: 16        233 - 248
  # no-cursor: 16     249 - 264
  
  # 96 + (5*16) = 176
  
  # 264 trials in total
  
  text(89+(96/2),-7.5,'96',adj=c(.5,.5), cex=textsize)
  text(185+(8/2),-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(193+(8/2),-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(201+(16/2),-7.5,'16',adj=c(.5,.5), cex=textsize)
  text(217+(8/2),-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(225+(8/2),-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(233+(16/2),-7.5,'16',adj=c(.5,.5), cex=textsize)
  text(249+(8/2),-7.5,'8',adj=c(.5,.5), cex=textsize)
  text(257+(8/2),-7.5,'8',adj=c(.5,.5), cex=textsize)
  
  
  lines(c(1,32), c(0,0))
  lines(c(41,56), c(0,0))
  lines(c(65,80), c(0,0))
  lines(c(89,184), c(30,30))
  lines(c(201,216), c(30,30))
  lines(c(233,248), c(30,30))
  
  
  rect(xleft=33,ybottom=0,xright=40,ytop=30,col='#BBBBBB77', border=NA)
  rect(xleft=57,ybottom=0,xright=64,ytop=30,col='#BBBBBB77', border=NA)
  rect(xleft=81,ybottom=0,xright=88,ytop=30,col='#BBBBBB77', border=NA)
  rect(xleft=185,ybottom=0,xright=192,ytop=30,col='#99999977', border=NA)
  rect(xleft=193,ybottom=0,xright=200,ytop=30,col='#DDDDDD77', border=NA)
  rect(xleft=217,ybottom=0,xright=224,ytop=30,col='#DDDDDD77', border=NA)
  rect(xleft=225,ybottom=0,xright=232,ytop=30,col='#99999977', border=NA)
  rect(xleft=249,ybottom=0,xright=256,ytop=30,col='#99999977', border=NA)
  rect(xleft=257,ybottom=0,xright=264,ytop=30,col='#DDDDDD77', border=NA)
  
  
}


get2rateData <- function(group='aiming') {
  
  rdf <- read.csv(sprintf('data/%s-training-trials.csv', group), stringsAsFactors = FALSE)
  
  trial <- c(1:264)
  #distortion <- c(rep(0,88),rep(30,176))
  
  tr_df <- data.frame(trial)
  
  df <- merge(tr_df, rdf, by='trial', all.x=TRUE)
  
  return(df)
  
}


# figures -----

fig3_Learning <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig3_learning.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig3_learning.pdf', width=8, height=4)
  }
  
  textsize <- 0.8
  
  # we will plot all data by trial
  # and illustrate the paradigm at the same time
  
  # groups:
  # non-instructed: orange
  # instructed: red
  # aiming: purple & pink
  
  # no-cursors: gray & blue?
  
  # plot conditions (no cursors & rotations)
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
  
  title(xlab='time: trials per block', line = 0.5)
  title(ylab='reach/aim deviation [°]', line = 2.5)
  
  plotBlocks(textsize = textsize)
  
  groups <- getGroups()
  
  # plot learning data:
  
  blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
  
  groupnames <- c()
  groupcols <- c()
  
  for (group in c('control', 'instructed', 'aiming')) {
    
    df <- read.csv(sprintf('data/%s-training-trials.csv', group), stringsAsFactors = F)
    
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    groupnames <- c(groupnames, as.character(groups$label[which(groups$group == group)]))
    groupcols <- c(groupcols, col.op)
    
    for (block in blocks) {
      
      CI.lo <- df$CI.lo[which(df$trial %in% block)]
      CI.hi <- df$CI.hi[which(df$trial %in% block)]
      average <- df$average[which(df$trial %in% block)]
      
      polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
      lines(block, average, col=col.op)
      
    }
    
  }
  
  
  # plot aiming
  
  
  group <- 'aims'
  
  df <- read.csv('data/aiming-aim-trials.csv', stringsAsFactors = F)
  
  col.op <- as.character(groups$col.op[which(groups$group == group)])
  col.tr <- as.character(groups$col.tr[which(groups$group == group)])
  
  groupnames <- c(groupnames, as.character(groups$label[which(groups$group == group)]))
  groupcols <- c(groupcols, col.op)
  
  for (block in blocks) {
    
    CI.lo <- df$CI.lo[which(df$trial %in% block)]
    CI.hi <- df$CI.hi[which(df$trial %in% block)]
    average <- df$average[which(df$trial %in% block)]
    
    polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
    lines(block, average, col=col.op)
    
  }
  
  legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
  
  
  axis(side=2, at=c(0,15,30))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}


fig4_Additivity <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig4_additivity.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_additivity.pdf', width=8, height=4)
  }
  
  textsize <- 0.8
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,22), ylim=c(-10,40), ax=F, bty='n')
  title(ylab='reach deviation [°]', line=2.5)
  
  lines(x=c(0,21),y=c(0, 0 ),col="#999999", lw=1, lty=1)
  lines(x=c(0,21),y=c(30,30),col="#999999", lw=1, lty=1)
  
  grouplabels <- c()
  groupcols <- c()
  
  pp <- sprintf('p%03d',c(1:24))
  
  groups <- getGroups()
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    group <- groupnames[groupno]
    
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
    groupcols <- c(groupcols, col.op)
    
    
    df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
    row.names(df) <- df$condition
    
    exclude <- as.numeric(df['exclude',pp] - df['none',pp])
    include <- as.numeric(df['include',pp] - df['none',pp])
    
    # EXCLUDE:
    CI <- Reach::getConfidenceInterval(exclude, method='b')
    avg <- mean(exclude)
    xoffset <- (groupno*2) + 6
    polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
    # replace with lines?
    points(x=rep(xoffset+1, length(exclude)), exclude, pch=16, col=col.tr)
    
    #text(x=(groupno*8)-6.5,y=-1,labels='exclude', adj=c(0,.5), srt=-45, cex=textsize)
    
    # INCLUDE:
    CI <- Reach::getConfidenceInterval(include, method='b')
    avg <- mean(include)
    xoffset <- (groupno*2) + 13
    polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
    # replace with lines?
    points(x=rep(xoffset+1, length(include)), include, pch=16, col=col.tr)
    
    
    #text(x=(groupno*8)-4.5,y=-1,labels='include', adj=c(0,.5), srt=-45, cex=textsize)
    
    # ADAPTATION:
    # change in reach direction: block 23 - block 9:
    dfr <- read.csv(sprintf('data/%s-training-blocks.csv', group), stringsAsFactors = F)
    #adaptation <- as.numeric(dfr[which(dfr$block == 23),pp] - dfr[which(dfr$block == 4),pp])
    adaptation <- as.numeric( colMeans( dfr[ which( dfr$block %in% c(23,27,31) ), pp]) - 
                                colMeans( dfr[ which( dfr$block %in% c(4,7,10)   ), pp])   )
    
    CI <- Reach::getConfidenceInterval(adaptation, method='b')
    avg <- mean(adaptation)
    xoffset <- (groupno*2) - 1
    polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
    # replace with lines?
    points(x=rep(xoffset+1, length(adaptation)), adaptation, pch=16, col=col.tr)
    
    #text(x=(groupno*8)-1.5,y=-1,labels='adaptation', adj=c(0,.5), srt=-45, cex=textsize)
    
  }
  
  text(x = 10.5, y=-2, labels='exclude',    cex=textsize)
  text(x =  3.5, y=-2, labels='adaptation', cex=textsize)
  text(x = 17.5, y=-2, labels='include',    cex=textsize)
  
  legend(0,15,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
  
  axis(side=2, at=c(0,15,30))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

fig4_altAdditivity <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig4_alt_additivity.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_alt_additivity.pdf', width=8, height=4)
  }
  
  textsize <- 0.8
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,25), ylim=c(-10,40), ax=F, bty='n')
  title(ylab='reach deviation [°]', line=2.5)
  
  lines(x=c(0,24),y=c(0, 0 ),col="#999999", lw=1, lty=1)
  lines(x=c(0,24),y=c(30,30),col="#999999", lw=1, lty=1)
  
  grouplabels <- c()
  groupcols <- c()
  
  pp <- sprintf('p%03d',c(1:24))
  
  groups <- getGroups()
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    group <- groupnames[groupno]
    
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
    groupcols <- c(groupcols, col.op)
    
    
    df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
    row.names(df) <- df$condition
    
    exclude <- as.numeric(df['exclude',pp] - df['non',pp])
    include <- as.numeric(df['include',pp] - df['non',pp])
    
    # EXCLUDE:
    CI <- Reach::getConfidenceInterval(exclude, method='b')
    avg <- mean(exclude)
    xoffset <- (groupno*8)-7
    polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
    # replace with lines?
    points(x=rep(xoffset+1, length(exclude)), exclude, pch=16, col=col.tr)
    
    text(x=xoffset+0.5,y=-1,labels='exclude', adj=c(0,.5), srt=-45, cex=textsize)
    
    # INCLUDE:
    CI <- Reach::getConfidenceInterval(include, method='b')
    avg <- mean(include)
    polygon(x=c(-.5,.5,.5,-.5)+xoffset+2, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(-.5,.5)+xoffset+2, y=rep(avg,2), col=col.op)
    # replace with lines?
    points(x=rep(xoffset+3, length(include)), include, pch=16, col=col.tr)
    
    
    text(x=xoffset+2.5,y=-1,labels='include', adj=c(0,.5), srt=-45, cex=textsize)
    
    # ADAPTATION:
    # change in reach direction: block 23 - block 9:
    dfr <- read.csv(sprintf('data/%s-training-blocks.csv', group), stringsAsFactors = F)
    #adaptation <- as.numeric(dfr[which(dfr$block == 23),pp] - dfr[which(dfr$block == 4),pp])
    adaptation <- as.numeric( colMeans( dfr[ which( dfr$block %in% c(23,27,31) ), pp]) - 
                                colMeans( dfr[ which( dfr$block %in% c(4,7,10)   ), pp])   )
    
    CI <- Reach::getConfidenceInterval(adaptation, method='b')
    avg <- mean(adaptation)
    polygon(x=c(-.5,.5,.5,-.5)+xoffset+5, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(-.5,.5)+xoffset+5, y=rep(avg,2), col=col.op)
    # replace with lines?
    points(x=rep(xoffset+6, length(adaptation)), adaptation, pch=16, col=col.tr)
    
    text(x=xoffset+4.5,y=-1,labels='adaptation', adj=c(0,.5), srt=-45, cex=textsize)
    
  }
  
  legend(-0.5,30,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
  
  axis(side=2, at=c(0,15,30))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

fig5_Explicit <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig5_explicit.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig5_explicit.pdf', width=8, height=4)
  }
  
  layout(matrix(c(2,1), nrow=1, ncol=2), widths=c(1,2), heights=c(1))
  par(mar=c(4,3.75,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,26), ylim=c(-10,30), ax=F, bty='n')
  title(ylab='explicit learning [°]', line = 2.5)
  
  grouplabels <- c()
  groupcols <- c()
  
  pp <- sprintf('p%03d',c(1:24))
  
  groups <- getGroups()
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  for (groupno in c(1:length(groupnames))) {
    
    group <- groupnames[groupno]
    
    df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
    row.names(df) <- df$condition
    
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
    groupcols <- c(groupcols, col.op)
    
    explicit <- as.numeric(df['include',pp] - df['exclude',pp])
    CI <- Reach::getConfidenceInterval(explicit, method='b')
    avg <- mean(explicit)
    polygon(x=c(0,1,1,0)+(groupno*4)-2, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(0,1)+(groupno*4)-2, y=rep(avg,2), col=col.op)
    
    points(x=rep((groupno*4), length(explicit)), explicit, pch=16, col=col.tr)
    
    dX <- density(explicit, n=81, from=-10, to=30, bw=2.5)$y
    dX <- (dX / sum(dX)) * 150
    dY <- seq(-10,30,.5)
    
    
    polygon(x=c(0,dX,0)+18, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=col.tr)
    
    lines(dX+18, dY, col=col.op)
    
  }
  
  data <- data.frame('explicit'=seq(-10,30,.5))
  bimodal <- fitModel()
  iM <- bimodal[['par']]['iM']
  iS <- bimodal[['par']]['iS']
  eM <- bimodal[['par']]['eM']
  eS <- bimodal[['par']]['eS']
  f  <- bimodal[['par']]['f']
  iL <- dnorm(data$explicit, mean=iM, sd=iS)
  eL <- dnorm(data$explicit, mean=eM, sd=eS)
  prob_dens <- (f*iL) + ((1-f)*eL)
  
  dX <- (prob_dens / sum(prob_dens)) * 150
  
  lines(dX+18, dY, col="#001388FF", lty=2)

  axis(side=2, at=c(0,10,20))
  
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-5,25), ylim=c(-10,30), ax=F, bty='n')
  
  title(xlab='aiming [°]', line=2.5)
  title(ylab='explicit learning [°]', line=2.5)
  
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
  
  #aims <- as.numeric(df[24,pp])
  
  #aims <- as.numeric(df[which(df$block==23),pp] - df[which(df$block==4),pp])
  
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                        colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  
  at <- range(aims)
  
  at <- c(-2,20)
  
  points(aims,explicit,pch=16,col=col.tr)
  lines(at,at,col='#666666',lty=1)
  
  #print(cor.test(aims,explicit))
  
  A2R <- lm(explicit ~ aims)
  
  coef <- A2R$coefficients
  lines(at, coef[1]+(at*coef[2]), col=col.op)
  
  
  ci <- predict( A2R,
                 newdata=data.frame(aims=seq(-2,20,.2)),
                 interval = "confidence")
  
  X <- c(seq(-2,20,.2),rev(seq(-2,20,.2)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  
  axis(side=1, at=c(0,10,20))
  axis(side=2, at=c(0,10,20))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}


fig6_splitAiming <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig6_splitAiming.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig6_splitAiming.pdf', width=8, height=4)
  }
  
  textsize <- 0.8
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
  
  title(xlab='time: trials per block', line = 0.5)
  title(ylab='aim deviation [°]', line = 2.5)
  
  plotBlocks(textsize=textsize)
  
  
  # group the participants in 2 sets:
  df <- getExplicitData() 
  
  iM <- median(df$explicit[which(df$group=='control')])
  eM <- median(df$explicit[which(df$group=='instructed')])
  iS <-     sd(df$explicit[which(df$group=='control')])
  eS <-     sd(df$explicit[which(df$group=='instructed')])
  
  iPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=iM, sd=iS)
  ePD <- dnorm(df$explicit[which(df$group=='aiming')], mean=eM, sd=eS)
  
  participant <- sprintf('p%03d',c(1:24))
  strategy <- ePD > iPD
  split_aim <- data.frame(participant,strategy)
  
  groups <- getGroups()
  groupnames <- c()
  groupcols <- c()
  df <- read.csv('data/aiming-aim-trials.csv', stringsAsFactors = F)
  
  for (strategy in c(FALSE, TRUE)) {
    
    sdf <- df[,as.character(split_aim$participant[which(split_aim$strategy == strategy)])]
    
    #print(str(sdf))
    
    if (strategy) {group <- 'aiming'; groupname <- 'aware aimers (N=9)'} else {group <- 'aims'; groupname <- 'unaware aimers (N=15)'}
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    groupnames <- c(groupnames, groupname)
    groupcols <- c(groupcols, col.op)
    
    blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
    
    for (block in blocks) {
      
      block <- unlist(block)
      trial_idx <- which(df$trial %in% block)
      bdf <- sdf[trial_idx,]
      
      CI <- apply(bdf, MARGIN=c(1), Reach::getConfidenceInterval, method='b')
      CI.lo <- as.numeric(CI[1,])
      CI.hi <- as.numeric(CI[2,])
      average <- rowMeans(bdf, na.rm=TRUE)
      
      polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
      lines(block, average, col=col.op)
      
    }
    
  }
  
  # we fit the two-rate model to mean reach deviations in both sub-groups
  # is the fast process equal to aiming responses?
  
  df <- get2rateData(group='aiming')
  schedule <- df$rotation * -1
  
  
  for (strategy in c(FALSE, TRUE)) {
    
    if (strategy) {group <- 'aiming'; groupname <- 'aware aimers'} else {group <- 'aims'; groupname <- 'unaware aimers'}
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    sdf <- df[,as.character(split_aim$participant[which(split_aim$strategy == strategy)])]
    reaches <- rowMeans(sdf, na.rm = TRUE)
    
    par <- Reach::twoRateFit(schedule       = schedule,
                             reaches        = reaches,
                             checkStability = TRUE)
    
    fit <- Reach::twoRateModel(par=par, schedule=schedule)
    
    lines(fit$total, col=col.op, lty=3)
    lines(fit$slow,  col=col.op, lty=2)
    lines(fit$fast,  col=col.op, lty=1)
    
    
  }
  
  
  legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
  
  axis(side=2, at=c(0,15,30))
  
  
 
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
   
}

fig7_slowControl <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig7_implicitControl.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig7_implicitControl.pdf', width=8, height=4)
  }
  
  textsize <- 0.8
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
  
  title(xlab='time: trials per block', line = 0.5)
  title(ylab='reach deviation [°]', line = 2.5)
  
  plotBlocks(textsize=textsize)
  
  groups <- getGroups()
  groupnames <- c()
  groupcols <- c()

  df <- read.csv('data/control.csv', stringsAsFactors = F)
  
  col.op <- as.character(groups$col.op[which(groups$group == 'control')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'control')])
  
  baseline <- aggregate(reachdeviation_deg ~ participant, data=df[which(df$cursor == FALSE & df$strategy == 'none'),], FUN=mean, na.rm=TRUE)
  
  blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
  
  exclude <- df[which(df$trial %in% unlist(blocks) & df$strategy == 'exclude'),]
  
  for (participant in baseline$participant) {
    idx <- which(exclude$participant == participant)
    exclude$reachdeviation_deg[idx] <- exclude$reachdeviation_deg[idx] - baseline$reachdeviation_deg[which(baseline$participant == participant)]
  }
  
  for (block in blocks) {
    
    block <- unlist(block)
    
    CI.lo <- c()
    CI.hi <- c()
    average <- c()
    
    for (trial in unlist(block)) {
      reachdevs <- exclude$reachdeviation_deg[which(exclude$trial == trial)]
      CI <- Reach::getConfidenceInterval(reachdevs, method='b')
      CI.lo <- c(CI.lo, unlist(CI[1]))
      CI.hi <- c(CI.hi, unlist(CI[2]))
      average <- c(average, mean(reachdevs, na.rm=TRUE))
    }
    

    polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
    lines(block, average, col=col.op)
  
  }  
    
  # we fit the two-rate model to mean reach deviations in both sub-groups
  # is the fast process equal to aiming responses?
  
  df <- get2rateData(group='control')
  schedule <- df$rotation * -1
  
  reaches <- rowMeans(df[,sprintf('p%03d',c(1:24))], na.rm = TRUE)
  
  par <- Reach::twoRateFit(schedule       = schedule,
                           reaches        = reaches,
                           checkStability = TRUE)
  
  fit <- Reach::twoRateModel(par=par, schedule=schedule)

  lines(fit$total, col=col.op, lty=3)
  lines(fit$slow,  col=col.op, lty=2)
  lines(fit$fast,  col=col.op, lty=1)

  
  #legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
  
  axis(side=2, at=c(0,15,30))
  
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

fig8_fastInstructed <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig8_fastInstructed.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig8_fastInstructed.pdf', width=8, height=4)
  }
  
  textsize <- 0.8
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
  
  title(xlab='time: trials per block', line = 0.5)
  title(ylab='reach deviation [°]', line = 2.5)
  
  plotBlocks(textsize=textsize)
  
  
  groups <- getGroups()
  groupnames <- c()
  groupcols <- c()
  
  df <- read.csv('data/instructed.csv', stringsAsFactors = F)
  
  col.op <- as.character(groups$col.op[which(groups$group == 'instructed')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'instructed')])
  
  baseline <- aggregate(reachdeviation_deg ~ participant, data=df[which(df$cursor == FALSE & df$strategy == 'exclude'),], FUN=mean, na.rm=TRUE)
  
  blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
  
  exclude <- df[which(df$trial %in% unlist(blocks) & df$strategy == 'include'),]
  
  for (participant in baseline$participant) {
    idx <- which(exclude$participant == participant)
    exclude$reachdeviation_deg[idx] <- exclude$reachdeviation_deg[idx] - baseline$reachdeviation_deg[which(baseline$participant == participant)]
  }
  
  for (block in blocks) {
    
    block <- unlist(block)
    
    CI.lo <- c()
    CI.hi <- c()
    average <- c()
    
    for (trial in unlist(block)) {
      reachdevs <- exclude$reachdeviation_deg[which(exclude$trial == trial)]
      CI <- Reach::getConfidenceInterval(reachdevs, method='b')
      CI.lo <- c(CI.lo, unlist(CI[1]))
      CI.hi <- c(CI.hi, unlist(CI[2]))
      average <- c(average, mean(reachdevs, na.rm=TRUE))
    }
    
    
    polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
    lines(block, average, col=col.op)
    
  }  
  
  # we fit the two-rate model to mean reach deviations in both sub-groups
  # is the fast process equal to aiming responses?
  
  df <- get2rateData(group='instructed')
  schedule <- df$rotation * -1
  
  reaches <- rowMeans(df[,sprintf('p%03d',c(1:24))], na.rm = TRUE)
  
  par <- Reach::twoRateFit(schedule       = schedule,
                           reaches        = reaches,
                           checkStability = TRUE)
  
  fit <- Reach::twoRateModel(par=par, schedule=schedule)
  
  lines(fit$total, col=col.op, lty=3)
  lines(fit$slow,  col=col.op, lty=2)
  lines(fit$fast,  col=col.op, lty=1)
  
  
  #legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
  
  axis(side=2, at=c(0,15,30))
  
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}