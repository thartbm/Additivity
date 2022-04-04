
# support functions -----

getGroups <- function() {
  
  
  group <- c('control', 'instructed', 'aiming', 'aims', 'step')
  label <- c('control', 'instructed', 'aiming', 'aims', 'step')
  col.op <- c('#ff9329ff', '#e51636ff', '#7f00d8ff', '#cc00ccff', '#00ffffff')
  col.tr <- c('#ff93292f', '#e516362f', '#7f00d82f', '#cc00cc2f', '#00ffff2f')
  
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
  
  bnh <- -6
  
  text(17,bnh,'32',adj=c(.5,.5), cex=textsize)
  text(37,bnh,'8',adj=c(.5,.5), cex=textsize)
  text(49,bnh,'16',adj=c(.5,.5), cex=textsize)
  text(61,bnh,'8',adj=c(.5,.5), cex=textsize)
  text(73,bnh,'16',adj=c(.5,.5), cex=textsize)
  text(85,bnh,'8',adj=c(.5,.5), cex=textsize)
  
  # rotated: 96       89  - 184
  # no-cursor: 16     185 - 200
  # top-up: 16        201 - 216
  # no-cursor: 16     217 - 232
  # top-up: 16        233 - 248
  # no-cursor: 16     249 - 264
  
  # 96 + (5*16) = 176
  
  # 264 trials in total
  
  text(89+(96/2),bnh,'96',adj=c(.5,.5), cex=textsize)
  text(185+(8/2),bnh,'8',adj=c(.5,.5), cex=textsize)
  text(193+(8/2),bnh,'8',adj=c(.5,.5), cex=textsize)
  text(201+(16/2),bnh,'16',adj=c(.5,.5), cex=textsize)
  text(217+(8/2),bnh,'8',adj=c(.5,.5), cex=textsize)
  text(225+(8/2),bnh,'8',adj=c(.5,.5), cex=textsize)
  text(233+(16/2),bnh,'16',adj=c(.5,.5), cex=textsize)
  text(249+(8/2),bnh,'8',adj=c(.5,.5), cex=textsize)
  text(257+(8/2),bnh,'8',adj=c(.5,.5), cex=textsize)
  
  
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

getAdditivityData <- function() {
  
  pp <- sprintf('p%03d',c(1:24))
  
  groupnames <- c('control', 'instructed', 'aiming')
  
  
  group       <- c()
  participant <- c()
  include     <- c()
  exclude     <- c()
  adaptation  <- c()
  aiming      <- c()
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    df <- read.csv(sprintf('data/%s-nocursors-all.csv', groupname), stringsAsFactors = F)
    row.names(df) <- df$condition
    
    excl <- as.numeric(df['exclude',pp] - df['none',pp])
    incl <- as.numeric(df['include',pp] - df['none',pp])
    
    dfr <- read.csv(sprintf('data/%s-training-blocks.csv', groupname), stringsAsFactors = F)
    adapt <- as.numeric( colMeans( dfr[ which( dfr$block %in% c(23,27,31) ), pp]) - 
                           colMeans( dfr[ which( dfr$block %in% c(4,7,10)   ), pp])   )
    
    
    group       <- c(group,       rep(groupname, 24))
    participant <- c(participant, sprintf('%s_p%03d',groupname,c(1:24)))
    include     <- c(include,     incl)
    exclude     <- c(exclude,     excl)
    adaptation  <- c(adaptation,  adapt)
    
    if (groupname == 'aiming') {
      aim_df <- read.csv('data/aiming-aim-blocks.csv')
      pp <- unlist(sprintf('p%03d',1:24))
      aims <- as.numeric( colMeans( aim_df[ which( aim_df$block %in% c(23,27,31) ), pp]) - 
                            colMeans( aim_df[ which( aim_df$block %in% c(4,7,10)   ), pp])   )
      aiming <- c(aiming, aims)
    } else {
      aiming <- c(aiming, rep(NA, 24))
    }
    
    
  }
  
  adf <- data.frame(participant, group, include, exclude, adaptation, aiming)
  
  
  return(adf)
  
}


getExtraData <- function() {
  
  datasets <- list()
  
  stepwise <- read.csv('data/extra/Modchalingam_unpublished.csv', stringsAsFactors = FALSE)
  stepwise$explicit <- stepwise$include - stepwise$exclude
  stepwise$group <- sprintf('%s_%d', stepwise$group, abs(stepwise$rotation_angle))
  stepwise$rotation <- abs(stepwise$rotation_angle)
  
  datasets[['stepwise']] <- list('paper'='Modchalingam et al. (unpublished)',
                                 'data'=stepwise,
                                 'labels' = list('abrupt_60'='abrupt (60°, N=36)',
                                                 'ramped_60'='ramped (60°, N=33)',
                                                 'stepwise_15'='stepwise (15°, N=37)',
                                                 'stepwise_30'='stepwise (30°)',
                                                 'stepwise_45'='stepwise (45°)',
                                                 'stepwise_60'='stepwise (60°)'))
  
  df <- read.csv('data/extra/Neville_and_Cressman_2018.csv', stringsAsFactors = FALSE)
  df$group <- sprintf('%s_%d',c('control','instructed')[df$instruction],df$rotation)
  df$explicit <- df$include - df$exclude
  
  datasets[['neville']] <- list('paper'='Neville & Cressman (2018)',
                                'data'=df,
                                'labels' = list('control_20'='control (20°, N=11)',
                                                'control_40'='control (40°, N=10)',
                                                'control_60'='control (60°, N=10)',
                                                'instructed_20'='instructed (20°, N=11)',
                                                'instructed_40'='instructed (40°, N=10)',
                                                'instructed_60'='instructed (60°, N=10)'))
  
  
  df <- read.csv('data/extra/Schween_etal_2018_2019.csv', stringsAsFactors = FALSE)
  names(df)[which(names(df) == 'experiment')] <- 'group'
  r.idx <- which(df$workspace == 'right')
  for (dv in c('implicit','explicit','adaptation')) {
    df[r.idx,dv] <- df[r.idx,dv] * -1
  }
  
  df <- aggregate(cbind(implicit, adaptation, explicit) ~ participant + group, data=df, FUN=mean)
  df$rotation <- 45
  
  datasets[['schween']] <- list('paper'='Schween et al. (2018; 2019)',
                                'data'=df,
                                'labels' = list('one'='2018: exp. 1, (45°, N=21)',
                                                'two'='2019: exp. 3, (45°, N=20)'))
  
  df <- read.csv('data/extra/Maresch_2020.csv', stringsAsFactors = FALSE)
  df$explicit <- df$aimreport
  df$rotation <- 60
  
  datasets[['maresch2020']] <- list('paper'='Maresch et al. (2020)',
                                    'data'=df,
                                    'labels' = list('CR'='continuous report (60°, N=12)',
                                                    'IR_E'='exclusion (60°, N=17)',
                                                    'IR_I'='inclusion (60°, N=12)',
                                                    'IR_EI'='exclusion & inclusion (60°, N=11)'))
  

  df <- read.csv('data/extra/Modchalingam_2019.csv', stringsAsFactors=FALSE)
  df$explicit <- df$include - df$exclude
  df$rotation <- c('control30'=30,
                   'control60'=60,
                   'instructed30'=30,
                   'instructed60'=60)[df$group]
  
  datasets[['modchalingam2019']] <- list('paper'='Modchalingam et al. (2019)',
                                         'data'=df,
                                         'labels' = list('control30'='control (30°, N=20)',
                                                         'control60'='control (60°, N=20)',
                                                         'instructed30'='instructed (30°, N=21)',
                                                         'instructed60'='instructed (60°, N=24)'))
  
  return(datasets)
  
}

getAdditivitySlopes <- function(implicit,explicit,adaptation) {
  
  # library(scales)
  # scales::viridis_pal(begin= 0.1, end=1.0)(4)
  colors.op <- c("#482576FF", "#2A788EFF", "#43BF71FF", "#FDE725FF")
  colors.tr <- c("#4825764F", "#2A788E4F", "#43BF714F", "#FDE7254F")
  
  output <- list()
  
  output[['colors']] <- list('op'=list(c(colors.op,"#999999FF")), 'tr'=list(c(colors.tr,"#9999994F")))
  
  output[['strict']] <- list()
  output[['loose']]  <- list()
  
  # fit line where strict additivity should give slope = -1
  e2i      <- lm(implicit ~ explicit)
  coef     <- e2i$coefficients
  slope    <- coef[2]
  slope_ci <- confint(e2i,parm='explicit',level=0.95)
  
  output$strict[['intercept']] <- coef[1]
  output$strict[['slope']]     <- slope
  output$strict[['slope_ci']]  <- slope_ci
  
  op <- "#999999FF"
  tr <- "#9999994F"
  
  if ((slope_ci[1] < -1) & (-1 < slope_ci[2]) & (slope_ci[2] < 0)) {
    op <- colors.op[1]
    tr <- colors.tr[1]
  }
  if ((-1 < slope_ci[1]) & (slope_ci[2] < 0)) {
    op <- colors.op[2]
    tr <- colors.tr[2]
  }
  if ((-1 < slope_ci[1]) & (0 < slope_ci[2])) {
    op <- colors.op[3]
    tr <- colors.tr[3]
  }
  if (0 < slope_ci[1]) {
    op <- colors.op[4]
    tr <- colors.tr[4]
  }
  
  output$strict$colors <- list('op'=op, 'tr'=tr)
  
  # loose additivity allows free weights on implicit and explicit in each fit
  # if loose additivity works, the regression of adaptation over the predictions should be 1
  
  # get predictions from weighted, and added implicit and explicit on adaptation:
  EIadd <- lm(adaptation ~ implicit + explicit + 0)
  
  # see how well predictions line up with data:
  predictions <- predict(EIadd)
  p2a <- lm(adaptation ~ predictions)
  pcoef <- p2a$coefficients
  
  # get slope:
  slope_ci <- confint(p2a,parm='predictions',level=0.95)
  slope    <- pcoef[2]
  
  output$loose[['slope']]    <- slope
  output$loose[['slope_ci']] <- slope_ci
  
  op <- "#999999FF"
  tr <- "#9999994F"
  
  if ((slope_ci[2] > 1) & (1 > slope_ci[1]) & (slope_ci[1] > 0)) {
    op <- colors.op[1]
    tr <- colors.tr[1]
  }
  if ((1 > slope_ci[2]) & (slope_ci[1] > 0)) {
    op <- colors.op[2]
    tr <- colors.tr[2]
  }
  if ((1 > slope_ci[2]) & (0 > slope_ci[1])) {
    op <- colors.op[3]
    tr <- colors.tr[3]
  }
  if (0 > slope_ci[2]) {
    op <- colors.op[4]
    tr <- colors.tr[4]
  }
  
  output$loose$colors <- list('op'=op, 'tr'=tr)
  
  
  return(output)
  
}

mixCol <- function(a='#41ffc9', b='#1d7791', balance=c(1,1)) {
  
  a <- col2rgb(a,alpha=TRUE)/255
  b <- col2rgb(b,alpha=TRUE)/255
  
  w <- balance / sum(balance)
  
  R <- (a[1]*w[1]) + (b[1]*w[2])
  G <- (a[2]*w[1]) + (b[2]*w[2])
  B <- (a[3]*w[1]) + (b[3]*w[2])
  A <- (a[4]*w[1]) + (b[4]*w[2])
  
  return(rgb(red=R, green=G, blue=B, alpha=A))
  
}

satCol <- function(col='#41ffc9', sat.mult=1.25) {
  
  inter <- rgb2hsv(col2rgb(col))
  
  sat.mult  <- max(0, sat.mult) 
  inter[2,] <- min(1, inter[2,] * sat.mult)
  
  return(hsv(inter[1,], inter[2,], inter[3,]))
  
}


# figures -----

# library(magick)
# library(grImport2)
# library(rsvg)

# fig1_methods <- function(target='inline') {
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig1_methods.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig1_methods.pdf', width=8, height=3)
#   }
#   
#   # fig1a <- magick::image_read_svg('doc/fig_1a.svg')
#   # fig1b <- magick::image_read_svg('doc/fig_1b.svg')
#   # fig1c <- magick::image_read_svg('doc/fig_1c.svg')
#   
#   #rsvg::rsvg_svg('doc/fig_1a.svg', 'doc/fig_1a_cairo.svg')
#   Fig1a <- grImport2::readPicture('doc/fig_1a_cairo.svg')
#   # cat('fig1a read\n')
#   
#   #rsvg::rsvg_svg('doc/fig_1b.svg', 'doc/fig_1b_cairo.svg')
#   Fig1b <- grImport2::readPicture('doc/fig_1b_cairo.svg')
#   # cat('fig1b read\n')
#   
#   #rsvg::rsvg_svg('doc/fig_1c.svg', 'doc/fig_1c_cairo.svg')
#   Fig1c <- grImport2::readPicture('doc/fig_1c_cairo.svg')
#   # cat('fig1c read\n')
#   
#   
#   # magick imports them as bitmaps!
#   #fig1a <- magick::image_read_pdf('doc/fig_1a.pdf')
#   #fig1b <- magick::image_read_pdf('doc/fig_1b.pdf')
#   #fig1c <- magick::image_read_pdf('doc/fig_1c.pdf')
#   
#   #fig1a_asp <- magick::image_info(fig1a)[['width']] / magick::image_info(fig1a)[['height']]
#   #fig1b_asp <- magick::image_info(fig1b)[['width']] / magick::image_info(fig1b)[['height']]
#   #fig1c_asp <- magick::image_info(fig1c)[['width']] / magick::image_info(fig1c)[['height']]
#   
#   layout(mat=matrix(c(1,2,3), nrow=1, ncol=3), widths=c(1,1,1))
#   
#   par(mar=c(0.5,0.5,2,0.5))
#   
#   plot(-1000, -1000, xlim=c(0,1), ylim=c(0,1), bty='n', ax=F, asp=1)
#   grImport2::grid.picture(Fig1a, 0, 0, 1, 1, just=c(1,1))
#   title(main='A', line=0.25, font.main=1, cex.main=1.75, adj=0)
#   
#   #plot(fig1b, asp=1)
#   plot(-1000, -1000, xlim=c(0,1), ylim=c(0,1), bty='n', ax=F, asp=1)
#   grImport2::grid.picture(Fig1b, 0, 0, 1, 1, just=c(1,1))
#   title(main='B', line=0.25, font.main=1, cex.main=1.75, adj=0)
#   
#   plot(-1000, -1000, xlim=c(0,1), ylim=c(0,1), bty='n', ax=F, asp=1)
#   grImport2::grid.picture(Fig1c, 0, 0, 1, 1, just=c(1,1))
#   title(main='C', line=0.25, font.main=1, cex.main=1.75, adj=0)
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

fig2_Learning <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig2_learning.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig2_learning.pdf', width=8, height=3)
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


fig3_Additivity <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig3_additivity.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig3_additivity.pdf', width=8, height=3)
  }
  
  
  textsize <- 0.8
  par(mar=c(0,3.5,0,0.1))
  
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
  
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                        colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  groupno <- 3
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  CI <- Reach::getConfidenceInterval(aims, method='b')
  avg <- mean(aims)
  xoffset <- (groupno*2) - 1
  polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
  lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
  # replace with lines?
  points(x=rep(xoffset+1, length(aims)), aims, pch=16, col=col.tr)
  
  grouplabels <- c(grouplabels, 'aims')
  groupcols <- c(groupcols, col.op)
  
  
  text(x = 10.5, y=-2, labels='exclude',    cex=textsize)
  text(x =  3.5, y=-2, labels='adaptation', cex=textsize)
  text(x = 17.5, y=-2, labels='include',    cex=textsize)
  
  legend(0,15,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
  
  axis(side=2, at=c(0,15,30))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

# fig4_altAdditivity <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/Fig4_alt_additivity.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig4_alt_additivity.pdf', width=8, height=3)
#   }
#   
#   textsize <- 0.8
#   par(mar=c(2,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,25), ylim=c(-10,40), ax=F, bty='n')
#   title(ylab='reach deviation [°]', line=2.5)
#   
#   lines(x=c(0,24),y=c(0, 0 ),col="#999999", lw=1, lty=1)
#   lines(x=c(0,24),y=c(30,30),col="#999999", lw=1, lty=1)
#   
#   grouplabels <- c()
#   groupcols <- c()
#   
#   pp <- sprintf('p%03d',c(1:24))
#   
#   groups <- getGroups()
#   
#   groupnames <- c('control', 'instructed', 'aiming')
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     group <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
#     groupcols <- c(groupcols, col.op)
#     
#     
#     df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
#     row.names(df) <- df$condition
#     
#     exclude <- as.numeric(df['exclude',pp] - df['non',pp])
#     include <- as.numeric(df['include',pp] - df['non',pp])
#     
#     # EXCLUDE:
#     CI <- Reach::getConfidenceInterval(exclude, method='b')
#     avg <- mean(exclude)
#     xoffset <- (groupno*8)-7
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(exclude)), exclude, pch=16, col=col.tr)
#     
#     text(x=xoffset+0.5,y=-1,labels='exclude', adj=c(0,.5), srt=-45, cex=textsize)
#     
#     # INCLUDE:
#     CI <- Reach::getConfidenceInterval(include, method='b')
#     avg <- mean(include)
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset+2, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset+2, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+3, length(include)), include, pch=16, col=col.tr)
#     
#     
#     text(x=xoffset+2.5,y=-1,labels='include', adj=c(0,.5), srt=-45, cex=textsize)
#     
#     # ADAPTATION:
#     # change in reach direction: block 23 - block 9:
#     dfr <- read.csv(sprintf('data/%s-training-blocks.csv', group), stringsAsFactors = F)
#     #adaptation <- as.numeric(dfr[which(dfr$block == 23),pp] - dfr[which(dfr$block == 4),pp])
#     adaptation <- as.numeric( colMeans( dfr[ which( dfr$block %in% c(23,27,31) ), pp]) - 
#                                 colMeans( dfr[ which( dfr$block %in% c(4,7,10)   ), pp])   )
#     
#     CI <- Reach::getConfidenceInterval(adaptation, method='b')
#     avg <- mean(adaptation)
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset+5, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset+5, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+6, length(adaptation)), adaptation, pch=16, col=col.tr)
#     
#     text(x=xoffset+4.5,y=-1,labels='adaptation', adj=c(0,.5), srt=-45, cex=textsize)
#     
#   }
#   
#   legend(-0.5,30,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

fig4_Explicit <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig4_explicit.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_explicit.pdf', width=8, height=3)
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


fig5_Additivity <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig5_additivity.svg', width=8, height=8/3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig5_additivity.pdf', width=8, height=8/3)
  }
  
  
  textsize <- 0.8
  par(mar=c(3.5,3.5,3.5,3.5))
  
  layout(mat=matrix(c(1,2), nrow=1, ncol=2, byrow = T))
  
  
  grouplabels <- c()
  groupcols <- c()
  
  adf <- getAdditivityData()
  
  total <- mean(adf$adaptation)
  at <- c(-10,5+total) 
  
  groups <- getGroups()
  groupnames <- c('control', 'instructed', 'aiming')
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-15,45), ylim=c(-5,35), ax=F, bty='n', asp=1)
  lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
  lines(x=c(0,40),y=c(0,0),col='#999999', lw=1, lty=2)
  lines(x=c(0,0),y=c(0,40),col='#999999', lw=1, lty=2)
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    col.op <- as.character(groups$col.op[which(groups$group == groupname)])
    col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
    
    grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == groupname)]))
    groupcols <- c(groupcols, col.op)
    
    idx <- which(adf$group == groupname)
    excl <- adf[idx,'exclude']
    incl <- adf[idx,'include']
    adapt <- adf[idx,'adaptation']
    
    expl <- incl - excl

    at <- range(expl)
    
    e2i <- lm(excl ~ expl)
    
    
    cat(sprintf('%s:\n',toupper(groupname)))
    #print(summary(I2A))
    
    coef <- e2i$coefficients
    lines(at, coef[1]+(at*coef[2]), col=col.op)
    
    
    ci <- predict( e2i,
                   newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=col.tr,border=NA)
    
    points(expl, excl, pch=16, col=col.tr)
    
    print(confint(e2i,parm='expl',level=0.95))
    print(confint(e2i,parm='(Intercept)',level=0.95))
    
  }
  
  #print(mean(adf$adaptation))
  
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  
  grouplabels <- c(grouplabels, 'aims')
  groupcols <- c(groupcols, col.op)
  
  idx <- which(adf$group == 'aiming')
  expl <- adf$aiming[idx]
  impl <- adf$exclude[idx]
  adapt <- adf$adaptation[idx]
  
  at <- range(expl)
  
  e2i <- lm(impl ~ expl)
  
  
  cat(sprintf('%s:\n',toupper('aims')))
  print(confint(e2i,parm='expl',level=0.95))
  print(confint(e2i,parm='(Intercept)',level=0.95))
  #print(summary(I2A))
  
  coef <- e2i$coefficients
  lines(at, coef[1]+(at*coef[2]), col=col.op)
  
  
  ci <- predict( e2i,
                 newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  points(expl, impl, pch=16, col=col.tr)
  
  
  title(main='strict additivity')
  
  title(ylab='implicit measure [°]', line=2.5)
  axis(side=2, at=c(0,15,30))
  
  title(xlab='explicit measure [°]', line=2.5)
  axis(side=1, at=c(-15,15,45))
  
  legend(20,40,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
  
  # # # # # # # # # # # 
  # LOOSE ADDITIVITY
  # 
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,45), ylim=c(10,40), ax=F, bty='n', asp=1)
  #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
  lines(x=c(10,40),y=c(10,40),col="#999999", lw=1, lty=1)
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    col.op <- as.character(groups$col.op[which(groups$group == groupname)])
    col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
    
    idx <- which(adf$group == groupname)
    
    incl  <- adf$include[idx]
    impl  <- adf$exclude[idx]
    expl  <- incl - excl
    adapt <- adf$adaptation[idx]
    
    
    
    
    EIadd <- lm(adapt ~ impl + expl + 0)
    
    cat(sprintf('%s:\n',toupper(groupname)))
    #print(summary(EIadd))
    
    coef <- EIadd$coefficients
    
    #print(coef)
    
    # plot actual adaptation over predicted values:
    
    predictions <- predict(EIadd)
    at <- range(predictions)
    p2a <- lm(adapt ~ predictions)
    #print(summary(p2a))
    pcoef <- p2a$coefficients
    print(confint(p2a,parm='predictions',level=0.95))
    lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
    
    ci <- predict( p2a,
                   newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=col.tr,border=NA)
    
    points(predictions, adapt, pch=16, col=col.tr)
    
    
  }
  
  
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  
  idx <- which(adf$group == 'aiming')
  expl <- adf$aiming[idx]
  impl <- adf$exclude[idx]
  adapt <- adf$adaptation[idx]
  
  
  EIadd <- lm(adapt ~ impl + expl + 0)
  
  cat(sprintf('%s:\n',toupper('aims')))
  #print(summary(EIadd))
  
  coef <- EIadd$coefficients
  
  #print(coef)
  
  # plot actual adaptation over predicted values:
  
  predictions <- predict(EIadd)
  at <- range(predictions)
  p2a <- lm(adapt ~ predictions)
  #print(summary(p2a))
  pcoef <- p2a$coefficients
  print(confint(p2a,parm='predictions',level=0.95))
  lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
  
  ci <- predict( p2a,
                 newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  points(predictions, adapt, pch=16, col=col.tr)
  
  
  
  title(main='loose additivity')
  
  title(ylab='adaptation [°]', line=2.5)
  axis(side=2, at=c(10,25,40))
  
  title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=2.5)
  axis(side=1, at=c(0,15,30,45))
  
  
  # # # # # # # # # # # # # # # # # # # # # # 
  # CROSS_GROUP - LOOSE ADDITIVITY
  # 
  
  # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(5,45), ylim=c(5,45), ax=F, bty='n', asp=1)
  # lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
  # 
  # for (groupno in c(1:length(groupnames))) {
  #   
  #   groupname <- groupnames[groupno]
  #   
  #   col.op <- as.character(groups$col.op[which(groups$group == groupname)])
  #   col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
  #   
  #   idx <- which(adf$group != groupname)
  #   incl  <- adf$include[idx]
  #   impl  <- adf$exclude[idx]
  #   expl  <- incl - excl
  #   adapt <- adf$adaptation[idx]
  #   
  #   
  #   
  #   
  #   EIadd <- lm(adapt ~ impl + expl + 0)
  #   
  #   # cat(sprintf('%s:\n',toupper(groupname)))
  #   # print(summary(EIadd))
  #   
  #   coef <- EIadd$coefficients
  #   
  #   #print(coef)
  #   
  #   # plot actual adaptation over predicted values:
  #   
  #   idx <- which(adf$group == groupname)
  #   incl  <- adf$include[idx]
  #   impl  <- adf$exclude[idx]
  #   expl  <- incl - excl
  #   adapt <- adf$adaptation[idx]
  #   
  #   
  #   predictions <- predict(EIadd, newdata=data.frame(impl, expl, adapt))
  #   at <- range(predictions)
  #   p2a <- lm(adapt ~ predictions)
  #   #sprint(summary(p2a))
  #   pcoef <- p2a$coefficients
  #   
  #   lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
  #   
  #   ci <- predict( p2a,
  #                  newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
  #                  interval = "confidence")
  #   
  #   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  #   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  #   polygon(x=X,y=Y,col=col.tr,border=NA)
  #   
  #   points(predictions, adapt, pch=16, col=col.tr)
  #   
  #   
  # }
  # 
  # 
  # title(main='cross-group predictions')
  # 
  # title(ylab='adaptation [°]', line=2.5)
  # axis(side=2, at=c(5,25,45))
  # 
  # title(xlab=expression(paste(beta[i] %.% Implicit + beta[e] %.% Explicit)), line=2.5)
  # axis(side=1, at=c(5,25,45))
  
  

  
  blues.s <- c('#41ffc9ff',
               mixCol(a='#41ffc9', b='#1d7791', balance=c(2,1)),
               mixCol(a='#41ffc9', b='#1d7791', balance=c(1,2)),
               '#1d7791ff')
  blues.t <- c('#41ffc92f',
               mixCol(a='#41ffc92f', b='#1d77912f', balance=c(2,1)),
               mixCol(a='#41ffc92f', b='#1d77912f', balance=c(1,2)),
               '#1d77912f')
  
  stepwise <- getStepwiseData()
  stepwise$explicit <- stepwise$include - stepwise$exclude
  
  # # # # # # # # # # # # # # # # # # # # # # 
  # STEPWISE - STRICT ADDITIVITY
  # 
  

  # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-15,60), ylim=c(-15,60), ax=F, bty='n', asp=1)
  # 
  # rotations <- unique(stepwise$rotation_angle)
  # for (rot_no in c(1:length(rotations))) {
  #   rot <- rotations[rot_no]
  #   total <- mean(stepwise$adaptation[which(stepwise$rotation_angle == rot)])
  #   at <- c(-10,10+total) 
  #   lines(x=at,y=(at*-1)+total,col=blues.s[rot_no], lw=1, lty=2)
  # }
  # lines(x=c(0,0,60),y=c(60,0,0),col='#999999', lw=1, lty=2)
  # #lines(x=c(0,0),y=c(0,60),col='#999999', lw=1, lty=2)
  # 
  # 
  # for (rot_no in c(1:length(rotations))) {
  #   
  #   rot <- rotations[rot_no]
  #   
  #   col.op <- blues.s[rot_no]
  #   col.tr <- blues.t[rot_no]
  #   
  #   idx <- which(stepwise$rotation_angle == rot)
  #   excl <- stepwise[idx,'exclude']
  #   incl <- stepwise[idx,'include']
  #   expl <- stepwise[idx,'explicit']
  #   adapt <- stepwise[idx,'adaptation']
  #   
  # 
  #   at <- range(expl)
  #   e2i <- lm(excl ~ expl)
  #   
  #   coef <- e2i$coefficients
  #   lines(at, coef[1]+(at*coef[2]), col=col.op)
  #   
  #   ci <- predict( e2i,
  #                  newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
  #                  interval = "confidence")
  #   
  #   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  #   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  #   polygon(x=X,y=Y,col=col.tr,border=NA)
  #   
  #   points(expl, excl, pch=16, col=col.tr)
  #   cat(sprintf('\nSTEPWISE %d deg\n',rot))
  #   print(confint(e2i,parm='expl',level=0.95))
  #   
  # }
  # 
  # 
  # 
  # title(main='multiple rotations')
  # 
  # title(ylab='implicit measure [°]', line=2.5)
  # #axis(side=1, at=c(0,30,60))
  # #axis(side=1, at=c(-15,0,15,30,45,60), labels = c('','0','','30','','60'))
  # axis(side=1, at=c(-15,22.5,60))
  # 
  # title(xlab='explicit measure [°]', line=2.5)
  # #axis(side=2, at=c(0,30,60))
  # #axis(side=2, at=c(-15,0,15,30,45,60), labels = c('','0','','30','','60'))
  # axis(side=2, at=c(-15,22.5,60))
  # 
  # legend(30,
  #        60,
  #        legend=sprintf('%d° rotation',c(15,30,45,60)),
  #        col=blues.s, 
  #        bty='n', 
  #        lty=1, 
  #        cex=textsize)
  
  
  
  # # # # # # # # # # # # # # # # # # # # # # 
  # STEPWISE - STRICT ADDITIVITY
  # 
  
  
  # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,60), ylim=c(0,60), ax=F, bty='n', asp=1)
  # 
  # lines(c(0,60),c(0,60),col='#999999',lty=1)
  # for (rot in c(15,30,45,60)) {
  #   lines(c(0,rot,rot),c(rot,rot,0),col='#999999',lty=2)
  # }
  # 
  # impl  <- stepwise$exclude / -stepwise$rotation_angle*60
  # expl  <- stepwise$explicit / -stepwise$rotation_angle*60
  # adapt <- stepwise$adaptation / -stepwise$rotation_angle*60
  # 
  # rota  <- -stepwise$rotation_angle
  # 
  # #impl  <- stepwise$exclude
  # #expl  <- stepwise$explicit
  # #adapt <- stepwise$adaptation
  # 
  # print(range(adapt))
  # EIadd <- lm(adapt ~ impl + expl + 0)
  # 
  # col.op <- mixCol(a='#41ffc9ff', b='#1d7791ff', balance=c(1,1))
  # col.tr <- mixCol(a='#41ffc92f', b='#1d77912f', balance=c(1,1))
  # 
  # predictions <- predict(EIadd)
  # at <- range(predictions)
  # p2a <- lm(adapt ~ predictions)
  # print(summary(p2a))
  # pcoef <- p2a$coefficients
  # print(confint(p2a,parm='predictions',level=0.95))
  # lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
  # 
  # ci <- predict( p2a,
  #                newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
  #                interval = "confidence")
  # 
  # X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  # Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  # polygon(x=X,y=Y,col=col.tr,border=NA)
  # 
  # 
  # for (rotno in c(1:4)) {
  #   rot <- c(15,30,45,60)[rotno]
  #   idx <- which(rota == rot)
  #   points(predictions[idx], adapt[idx], pch=16, col=blues.t[rotno])
  # }
  # 
  # 
  # 
  # title(main='loose additivity')
  # 
  # title(ylab='adaptation [norm]', line=2.5)
  # axis(side=2, at=c(0,30,60))
  # 
  # title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=2.5)
  # axis(side=1, at=c(0,30,60))
  
  
  
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  

}

fig6_splitAiming <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig6_splitAiming.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig6_splitAiming.pdf', width=8, height=3)
  }
  
  textsize <- 0.8
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
  
  title(xlab='time: trials per block', line = 0.5)
  title(ylab='deviation [°]', line = 2.5)
  
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
  split_aim$ppno <- c(1:24)
  
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
  
  
  # now we plot the exlude reach deviations for both sub-groups:
  
  df <- read.csv('data/aiming.csv', stringsAsFactors = F)
  
  for (strategy in c(FALSE, TRUE)) {
    
    sdf <- df[which(df$participant %in% split_aim$ppno[which(split_aim$strategy == strategy)]),]
    
    if (strategy) {group <- 'aiming'; groupname <- 'aware aimers'} else {group <- 'aims'; groupname <- 'unaware aimers'}
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    baseline <- aggregate(reachdeviation_deg ~ participant, data=sdf[which(sdf$cursor == FALSE & sdf$strategy == 'none'),], FUN=mean, na.rm=TRUE)
  
    blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
  
    exclude <- sdf[which(sdf$trial %in% unlist(blocks) & sdf$strategy == 'exclude'),]
  
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

figA_slowControl <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/FigA_implicitControl.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/FigA_implicitControl.pdf', width=8, height=3)
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

figB_fastInstructed <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/FigB_fastInstructed.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/FigB_fastInstructed.pdf', width=8, height=3)
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

# letter figures -----

let1_Learning_PointEstimates <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/letter_Fig1_learning.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/letter_Fig1_learning.pdf', width=8, height=3)
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
  
  layout(mat=matrix(c(1,2),nrow=1,ncol=2),widths=c(2,1))
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-7,40), ax=F, bty='n')
  text(0,35,'A: adaptation and re-aiming', font.main=1, cex=1.35, adj=0)
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
  
  
  par(mar=c(2,0,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,22), ylim=c(-7,40), ax=F, bty='n')
  #title(ylab='reach deviation [°]', line=2.5)
  text(0,35,'B: point estimates', font.main=1, cex=1.35, adj=0)
  
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
  
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                        colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  groupno <- 3
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  CI <- Reach::getConfidenceInterval(aims, method='b')
  avg <- mean(aims)
  xoffset <- (groupno*2) - 1
  polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col='#FFFFFF00', border=col.tr)
  lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
  # replace with lines?
  points(x=rep(xoffset+1, length(aims)), aims, pch=1, col=col.tr)
  
  grouplabels <- c(grouplabels, 'aims')
  groupcols <- c(groupcols, col.op)
  
  
  text(x = 10.5, y=-6, labels='exclude',    cex=textsize)
  text(x =  3.5, y=-6, labels='adaptation', cex=textsize)
  text(x = 17.5, y=-6, labels='include',    cex=textsize)
  
  #legend(0,15,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
  
  #axis(side=2, at=c(0,15,30))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

let2_PointEstimateAdditivity <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/letter_Fig2_pointestimates.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/letter_Fig2_pointestimates.pdf', width=8, height=3)
  }
  
  textsize <- 1.5
  
  
  layout(matrix(c(1,2,3), nrow=1, ncol=3))
  par(mar=c(4.5,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-7.5,27.5), ylim=c(-5,30), ax=F, bty='n',asp=1)
  text(-7.5,30,'A: explicit measures', font.main=1, cex=1.35*1.5, adj=0)
  title(xlab='aims [°]', line=2.5, cex.lab=textsize)
  title(ylab='difference score [°]', line=2.5, cex.lab=textsize)
  
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
  pp <- sprintf('p%03d',c(1:24))
  
  adf <- getAdditivityData()
  
  idx <- which(adf$group == 'aiming')
  explicit <- adf[idx,'include'] - adf[idx,'exclude']
  
  groups <- getGroups()
  col.op <- as.character(groups$col.op[which(groups$group == 'aiming')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aiming')])
    
  
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                        colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  
  at <- range(aims)
  
  at <- c(-2,20)
  
  points(aims,explicit,pch=16,col=col.tr)
  lines(at,at,col='#666666',lty=2)
  
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
  
  
  axis(side=1, at=c(0,10,20), cex.axis=textsize)
  axis(side=2, at=c(0,10,20), cex.axis=textsize)
  
  # # # # # # # # # 3 # # # # # #
  # STRICT ADDITIVITY
  
  grouplabels <- c()
  groupcols <- c()
  
  adf <- getAdditivityData()
  
  total <- mean(adf$adaptation)
  at <- c(-10,10+total) 
  
  groups <- getGroups()
  groupnames <- c('control', 'instructed', 'aiming')
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-10,45), ylim=c(-10,45), ax=F, bty='n', asp=1)
  text(-10,45,'B: strict additivity', font.main=1, cex=1.35*1.5, adj=0)
  lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
  lines(x=c(0,40),y=c(0,0),col='#999999', lw=1, lty=2)
  lines(x=c(0,0),y=c(0,40),col='#999999', lw=1, lty=2)
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    col.op <- as.character(groups$col.op[which(groups$group == groupname)])
    col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
    
    grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == groupname)]))
    groupcols <- c(groupcols, col.op)
    
    idx <- which(adf$group == groupname)
    excl <- adf[idx,'exclude']
    incl <- adf[idx,'include']
    adapt <- adf[idx,'adaptation']
    
    expl <- incl - excl
    
    at <- range(expl)
    
    e2i <- lm(excl ~ expl)
    
    
    cat(sprintf('%s:\n',toupper(groupname)))
    #print(summary(I2A))
    
    coef <- e2i$coefficients
    lines(at, coef[1]+(at*coef[2]), col=col.op)
    
    
    ci <- predict( e2i,
                   newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=col.tr,border=NA)
    
    points(expl, excl, pch=16, col=col.tr)
    
    print(confint(e2i,parm='expl',level=0.95))
    print(confint(e2i,parm='(Intercept)',level=0.95))
    
  }
  
  #print(mean(adf$adaptation))
  
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  
  grouplabels <- c(grouplabels, 'aims')
  groupcols <- c(groupcols, col.op)
  
  idx <- which(adf$group == 'aiming')
  expl <- adf$aiming[idx]
  impl <- adf$exclude[idx]
  adapt <- adf$adaptation[idx]
  
  at <- range(expl)
  
  e2i <- lm(impl ~ expl)
  
  
  cat(sprintf('%s:\n',toupper('aims')))
  print(confint(e2i,parm='expl',level=0.95))
  print(confint(e2i,parm='(Intercept)',level=0.95))
  #print(summary(I2A))
  
  coef <- e2i$coefficients
  lines(at, coef[1]+(at*coef[2]), col=col.op)
  
  
  ci <- predict( e2i,
                 newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  points(expl, impl, pch=16, col=col.tr)
  
  
  #title(main='strict additivity')
  
  title(ylab='implicit measure [°]', line=2.5, cex.lab=textsize)
  axis(side=2, at=c(-10,15,40), cex.axis=textsize)
  
  title(xlab='explicit measure [°]', line=2.5, cex.lab=textsize)
  axis(side=1, at=c(-10,15,40), cex.axis=textsize)
  
  legend(15,40,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=0.8*1.5)
  
  # # # # # # # # # # # 
  # LOOSE ADDITIVITY
  # 
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,45), ylim=c(0,45), ax=F, bty='n', asp=1)
  text(0,45,'C: loose additivity', font.main=1, cex=1.35*1.5, adj=0)
  #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
  lines(x=c(0,40),y=c(0,40),col="#999999", lw=1, lty=1)
  
  for (groupno in c(1:length(groupnames))) {
    
    groupname <- groupnames[groupno]
    
    col.op <- as.character(groups$col.op[which(groups$group == groupname)])
    col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
    
    idx <- which(adf$group == groupname)
    
    incl  <- adf$include[idx]
    impl  <- adf$exclude[idx]
    expl  <- incl - excl
    adapt <- adf$adaptation[idx]
    
    
    
    
    EIadd <- lm(adapt ~ impl + expl + 0)
    
    cat(sprintf('%s:\n',toupper(groupname)))
    #print(summary(EIadd))
    
    coef <- EIadd$coefficients
    
    #print(coef)
    
    # plot actual adaptation over predicted values:
    
    predictions <- predict(EIadd)
    at <- range(predictions)
    p2a <- lm(adapt ~ predictions)
    #print(summary(p2a))
    pcoef <- p2a$coefficients
    print(confint(p2a,parm='predictions',level=0.95))
    lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
    
    ci <- predict( p2a,
                   newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=col.tr,border=NA)
    
    points(predictions, adapt, pch=16, col=col.tr)
    
    
  }
  
  
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  
  idx <- which(adf$group == 'aiming')
  expl <- adf$aiming[idx]
  impl <- adf$exclude[idx]
  adapt <- adf$adaptation[idx]
  
  
  EIadd <- lm(adapt ~ impl + expl + 0)
  
  cat(sprintf('%s:\n',toupper('aims')))
  #print(summary(EIadd))
  
  coef <- EIadd$coefficients
  
  #print(coef)
  
  # plot actual adaptation over predicted values:
  
  predictions <- predict(EIadd)
  at <- range(predictions)
  p2a <- lm(adapt ~ predictions)
  #print(summary(p2a))
  pcoef <- p2a$coefficients
  print(confint(p2a,parm='predictions',level=0.95))
  lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
  
  ci <- predict( p2a,
                 newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  points(predictions, adapt, pch=16, col=col.tr)
  
  
  
  #title(main='loose additivity')
  
  title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
  axis(side=2, at=c(0,20,40), cex.axis=textsize)
  
  title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
  axis(side=1, at=c(0,15,30,45), cex.axis=textsize)
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}



# let3_OtherData <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/letter_Fig3_otherdata.svg', width=8, height=6, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/letter_Fig3_otherdata.pdf', width=8, height=6)
#   }
#   
#   # more saturated colors:
#   #cols.op <- c('#00ffc1ff', '#008061ff', '#00c4ffff', '#006280ff')
#   #cols.tr <- c('#00ffc12f', '#0080612f', '#00c4ff2f', '#0062802f')
#   
#   # more handpicked colors:
#   #cols.op <- c('#10e7b2ff', '#0b9e7aff', '#38b5dbff', '#267b95ff')
#   #cols.tr <- c('#10e7b22f', '#0b9e7a2f', '#38b5db2f', '#267b952f')
#   
#   # 00cc85
#   cols.op <- c('#40ffbcff', '#00b374ff', '#40b5ffff', '#006eb3ff')
#   cols.tr <- c('#40ffbc2f', '#00b3742f', '#40b5ff2f', '#006eb32f')
#   
#   
#   groups <- c('control30','control60','instructed30','instructed60')
#   
#   textsize <- 1.5
#   
#   layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow=TRUE))
#   par(mar=c(4.5,4,0,0.1))
#   
#   
#   # # # # # # # # # 3 # # # # # #
#   # STRICT ADDITIVITY MODCHALINGAM ET AL 2019
#   
#   #df <- read.csv('data/modchalingam2019.csv', stringsAsFactors=FALSE)
#   #df$explicit <- df$include - df$exclude
#   
#   
#   
#   # grouplabels <- c()
#   # groupcols <- c()
#   # 
#   # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-10,70), ylim=c(-10,70), ax=F, bty='n', asp=1)
#   # text(-10,70,'A: strict additivity', font.main=1, cex=1.35*1.5, adj=0)
#   # 
#   # lines(x=c(0,60),y=c(0,0),col='#999999', lw=1, lty=1)
#   # lines(x=c(0,0),y=c(0,60),col='#999999', lw=1, lty=1)
#   # 
#   # for (groupno in c(1:length(groups))) {
#   #   
#   #   col.op <- cols.op[groupno]
#   #   col.tr <- cols.tr[groupno]
#   #   
#   #   groupname <- groups[groupno]
#   #   gdf <- df[which(df$group == groupname),]
#   #   
#   #   total <- mean(gdf$adaptation)
#   #   at <- c(-5,5+total) 
#   #   lines(x=at,y=(at*-1)+total,col=col.op, lw=1, lty=2)
#   #   
#   #   grouplabels <- c(grouplabels, groupname)
#   #   groupcols <- c(groupcols, col.op)
#   #   
#   #   excl <- gdf$exclude
#   #   incl <- gdf$include
#   #   adapt <- gdf$adaptation
#   #   expl <- gdf$explicit
#   #   
#   #   at <- range(expl)
#   #   
#   #   e2i <- lm(excl ~ expl)
#   #   
#   #   
#   #   cat(sprintf('%s:\n',toupper(groupname)))
#   #   #print(summary(I2A))
#   #   
#   #   coef <- e2i$coefficients
#   #   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   #   
#   #   
#   #   ci <- predict( e2i,
#   #                  newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#   #                  interval = "confidence")
#   #   
#   #   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#   #   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   #   polygon(x=X,y=Y,col=col.tr,border=NA)
#   #   
#   #   points(expl, excl, pch=16, col=col.tr)
#   #   
#   #   print(confint(e2i,parm='expl',level=0.95))
#   #   print(confint(e2i,parm='(Intercept)',level=0.95))
#   #   
#   # }
#   # 
#   # 
#   # title(ylab='implicit measure [°]', line=2.5, cex.lab=textsize)
#   # axis(side=2, at=c(-10,30,70), cex.axis=textsize)
#   # 
#   # title(xlab='explicit measure [°]', line=2.5, cex.lab=textsize)
#   # axis(side=1, at=c(-10,30,70), cex.axis=textsize)
#   # 
#   # legend(25,65,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=0.8*1.5)
#   
#   # # # # # # # # # # # # 
#   # # LOOSE ADDITIVITY MODCHALINGAM ET AL 2019
#   # # 
#   # 
#   # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,70), ylim=c(0,70), ax=F, bty='n', asp=1)
#   # text(0,70,'A: Modchalingam et al.', font.main=1, cex=1.35*1.4, adj=0)
#   # #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   # lines(x=c(0,65),y=c(0,65),col="#999999", lw=1, lty=1)
#   # 
#   # 
#   # for (groupno in c(1:length(groups))) {
#   #   
#   #   col.op <- cols.op[groupno]
#   #   col.tr <- cols.tr[groupno]
#   #   
#   #   groupname <- groups[groupno]
#   #   gdf <- df[which(df$group == groupname),]
#   #   
#   #   incl  <- gdf$include
#   #   impl  <- gdf$exclude
#   #   expl  <- gdf$explicit
#   #   adapt <- gdf$adaptation
#   # 
#   #   EIadd <- lm(adapt ~ impl + expl + 0)
#   # 
#   #   cat(sprintf('%s:\n',toupper(groupname)))
#   #   #print(summary(EIadd))
#   # 
#   #   coef <- EIadd$coefficients
#   # 
#   #   #print(coef)
#   # 
#   #   # plot actual adaptation over predicted values:
#   # 
#   #   predictions <- predict(EIadd)
#   #   at <- range(predictions)
#   #   p2a <- lm(adapt ~ predictions)
#   #   #print(summary(p2a))
#   #   pcoef <- p2a$coefficients
#   #   print(confint(p2a,parm='predictions',level=0.95))
#   #   lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#   # 
#   #   ci <- predict( p2a,
#   #                  newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#   #                  interval = "confidence")
#   # 
#   #   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#   #   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   #   polygon(x=X,y=Y,col=col.tr,border=NA)
#   # 
#   #   points(predictions, adapt, pch=16, col=col.tr)
#   # 
#   # 
#   # }
#   # 
#   # 
#   # 
#   # title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   # axis(side=2, at=c(0,30,60), cex.axis=textsize)
#   # 
#   # title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   # axis(side=1, at=c(0,30,60), cex.axis=textsize)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   # # # # # # # # # # # 
#   # LOOSE ADDITIVITY MODCHALINGAM ET AL UNPUBLISHED
#   # 
#   
#   stepwise <- read.csv('data/extra/Modchalingam_unpublished.csv', stringsAsFactors = FALSE)
#   groups <- unique(stepwise$group)
#   print(groups)
#   stepwise$explicit <- stepwise$include - stepwise$exclude
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-5,90), ylim=c(-5,90), ax=F, bty='n', asp=1)
#   text(-5,90,'A: Modchalingam et al. (1)', font.main=1, cex=1.35*1.4, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,75),y=c(0,75),col="#999999", lw=1, lty=1)
#   
#   
#   # colors <- list('stepwise'=list('op'=c('#0fde0bff','#3bde38ff','#66de64ff','#92de90ff'),
#   #                                'tr'=c('#0fde0b2f','#3bde382f','#66de642f','#92de902f')),
#   #                'abrupt'  =list('op'=c('#0fbfb3ff'),
#   #                                'tr'=c('#0fbfb32f')),
#   #                'ramped'  =list('op'=c('#e6cc0bff'),
#   #                                'tr'=c('#e6cc0b2f')))
#   colors <- list('stepwise'=list('op'=c('#0fde0bff','#34c532ff','#4fab4dff','#60925eff'),
#                                  'tr'=c('#0fde0b2f','#34c5322f','#4fab4d2f','#60925e2f')),
#                  'abrupt'  =list('op'=c('#0fbfb3ff'),
#                                  'tr'=c('#0fbfb32f')),
#                  'ramped'  =list('op'=c('#e6cc0bff'),
#                                  'tr'=c('#e6cc0b2f')))
#   
#   
#   for (groupno in c(1:length(groups))) {
#     
#     #col.op <- cols.op[groupno]
#     #col.tr <- cols.tr[groupno]
#     
#     groupname <- groups[groupno]
#     gdf <- stepwise[which(stepwise$group == groupname),]
#     #print(dim(gdf))
#     rotations <- sort(unique(gdf$rotation_angle))
#     
#     col <- colors[[groupname]]
#     
#     for (rotno in c(1:length(rotations))) {
#       
#       rotation <- rotations[rotno]
#       
#       col.op <- col[['op']][rotno]
#       col.tr <- col[['tr']][rotno]
#       
#       rdf <- gdf[which(gdf$rotation_angle == rotation),]
#       
#       incl  <- rdf$include
#       impl  <- rdf$exclude
#       expl  <- rdf$explicit
#       adapt <- rdf$adaptation
#     
#       EIadd <- lm(adapt ~ impl + expl + 0)
#     
#       cat(sprintf('%s %d:\n',toupper(groupname),abs(rotation)))
#       #print(summary(EIadd))
#     
#       coef <- EIadd$coefficients
#     
#       #print(coef)
#     
#       # plot actual adaptation over predicted values:
#     
#       predictions <- predict(EIadd)
#       at <- range(predictions)
#       p2a <- lm(adapt ~ predictions)
#       #print(summary(p2a))
#       pcoef <- p2a$coefficients
#       print(confint(p2a,parm='predictions',level=0.95))
#       
#       lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#     
#       ci <- predict( p2a,
#                      newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#                      interval = "confidence")
#     
#       X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#       Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#       polygon(x=X,y=Y,col=col.tr,border=NA)
#     
#       points(predictions, adapt, pch=16, col=col.tr)
#     
#     }
#     
#   }
#   
#   
#   
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,40,80), cex.axis=textsize)
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,40,80), cex.axis=textsize)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   # # # # # # # # # # # 
#   # LOOSE ADDITIVITY NEVILLE & CRESSMAN 2018
#   # 
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-5,90), ylim=c(-5,90), ax=F, bty='n', asp=1)
#   text(-5,90,'B: Neville & Cressman', font.main=1, cex=1.35*1.4, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,75),y=c(0,75),col="#999999", lw=1, lty=1)
#   
#   # red_seq <- c( mixCol('#e51636ff', '#ffffffff', balance = c(1, 1.0)),
#   #               mixCol('#e51636ff', '#ffffffff', balance = c(1, 0.5)),
#   #               '#e51636ff' )
#   # 
#   # blue_mix <- lapply(red_seq, mixCol, '#80bcff', balance=c(1,0.33333))
#   # 
#   # #cols.op <- c("#F28B9BFF", "#EE6479FF", "#e51636ff", "#D697B4FF", "#D37A9AFF", "#CC3F68FF")
#   # #cols.tr <- c("#F28B9B2F", "#EE64792F", "#e516362f", "#D697B42F", "#D37A9A2F", "#CC3F682F")
#   # 
#   # # more purple? 
#   # cols.op <- c("#F28B9BFF", "#EE6479FF", "#e51636ff", "#C1A0C6FF", "#BF8AB2FF", "#BA5D8CFF")
#   # cols.tr <- c("#F28B9B2F", "#EE64792F", "#e516362f", "#C1A0C62F", "#BF8AB22F", "#BA5D8C2F")
#   
#   
#   #main_seq <- c("#001388FF", mixCol("#001388FF", "#e516362f", balance=c(1,1)), "#e516362f")
#   #satCol(main_seq, sat.mult = 1.25)
#   #satCol(main_seq, sat.mult = 0.25)
# 
#   cols.op <- c("#6C7088ff", "#735B6Eff", "#E5B6BDff", "#001388ff", "#73005Bff", "#E50023ff")
#   cols.tr <- c("#6C70882f", "#735B6E2f", "#E5B6BD2f", "#0013882f", "#73005B2f", "#E500232f")
#   
#   
#   df <- read.csv('data/extra/Neville_and_Cressman_2018.csv', stringsAsFactors = FALSE)
#   df$group <- sprintf('%s_%d',c('control','instructed')[df$instruction],df$rotation)
#   df$explicit <- df$include - df$exclude
#   
#   groups <- c('control_20',
#               'control_40',
#               'control_60',
#               'instructed_20',
#               'instructed_40',
#               'instructed_60')
#   
#   for (groupno in c(1:length(groups))) {
#     
#     col.op <- cols.op[groupno]
#     col.tr <- cols.tr[groupno]
#      
#     groupname <- groups[groupno]
#     gdf <- df[which(df$group == groupname),]
#     
#     incl  <- gdf$include
#     impl  <- gdf$exclude
#     expl  <- gdf$explicit
#     adapt <- gdf$adaptation
#     
#     EIadd <- lm(adapt ~ impl + expl + 0)
# 
#     cat(sprintf('%s:\n',toupper(groupname)))
#     #print(summary(EIadd))
# 
#     coef <- EIadd$coefficients
# 
#     #print(coef)
# 
#     # plot actual adaptation over predicted values:
# 
#     predictions <- predict(EIadd)
#     at <- range(predictions)
#     p2a <- lm(adapt ~ predictions)
#     #print(summary(p2a))
#     pcoef <- p2a$coefficients
#     print(confint(p2a,parm='predictions',level=0.95))
#     lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
# 
#     ci <- predict( p2a,
#                    newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
# 
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
# 
#     points(predictions, adapt, pch=16, col=col.tr)
# 
#   }
#   
#   
#   
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,40,80), cex.axis=textsize)
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,40,80), cex.axis=textsize)
#   
#   
#   # incl  <- df$include
#   # impl  <- df$exclude
#   # expl  <- df$explicit
#   # adapt <- df$adaptation
#   # instr <- df$instruction
#   # rotat <- df$rotation
#   # 
#   # EIadd <- lm(adapt ~ impl + expl + 0)
#   # 
#   # predictions <- predict(EIadd)
#   # at <- range(predictions)
#   # p2a <- lm(adapt ~ predictions)
#   
#   
#   
#   
#   
#   # # # # # # # # # # # # # # # # # # # # #
#   # LOOSE ADDITIVITY SCHWEEN ET AL. 2018; 2019
#   # 
#   
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-5,90), ylim=c(-5,90), ax=F, bty='n', asp=1)
#   text(-5,90,'C: Schween et al.', font.main=1, cex=1.35*1.4, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,75),y=c(0,75),col="#999999", lw=1, lty=1)
#   
#   df <- read.csv('data/extra/Schween_etal_2018_2019.csv', stringsAsFactors = FALSE)
#   
#   r.idx <- which(df$workspace == 'right')
#   for (dv in c('implicit','explicit','adaptation')) {
#     df[r.idx,dv] <- df[r.idx,dv] * -1
#   }
#   
#   
#             # orange         light blue        york red      blue
#   cols.op <- c("#ffa63aff", "#e11beaff", "#E50023ff", "#346adeff")
#   cols.tr <- c("#ffa63a2f", "#e11bea2f", "#E500232f", "#346ade2f")
#   
#   # 34d5de
#   # cols.tr <- c("#ffdb002f", "#ff80c22f", "#E500232f", "#b900de2f")
#   # cols.tr <- c("#ffdb002f", "#c061ff2f", "#E500232f", "#346ade2f")
#   
#   col_idx <- 0
#   
#   for (exp in c('one','two')) {
#     
#     col_idx <- col_idx + 1
#     
#     col.op <- cols.op[col_idx]
#     col.tr <- cols.tr[col_idx]
#     
#     subdf <- df[which(df$experiment == exp),]
#     
#     impl  <- subdf$implicit
#     expl  <- subdf$explicit
#     adapt <- subdf$adaptation
#     
#     EIadd <- lm(adapt ~ impl + expl + 0)
#     
#     cat(sprintf('%s:\n',toupper(exp)))
#     #print(summary(EIadd))
#     
#     coef <- EIadd$coefficients
#     
#     #print(coef)
#     
#     # plot actual adaptation over predicted values:
#     
#     predictions <- predict(EIadd)
#     at <- range(predictions)
#     p2a <- lm(adapt ~ predictions)
#     #print(summary(p2a))
#     pcoef <- p2a$coefficients
#     print(confint(p2a,parm='predictions',level=0.95))
#     lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#     
#     ci <- predict( p2a,
#                    newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
#     
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
#     
#     points(predictions, adapt, pch=16, col=col.tr)
#     
#   }
#   
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,40,80), cex.axis=textsize)
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,40,80), cex.axis=textsize)
#   
#   
# 
#   
#   
#   # # # # # # # # # # #
#   # LOOSE ADDITIVITY MARESCH ET AL 2020
#   #
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-5,90), ylim=c(-5,90), ax=F, bty='n', asp=1)
#   text(-5,90,'D: Maresch et al.', font.main=1, cex=1.35*1.4, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,75),y=c(0,75),col="#999999", lw=1, lty=1)
#   
#   df <- read.csv('data/extra/Maresch_2020.csv', stringsAsFactors = FALSE)
#   
#   groups <- unique(df$group)
#   
#   for (groupno in c(1:length(groups))) {
#     
#     col.op <- cols.op[groupno]
#     col.tr <- cols.tr[groupno]
#     
#     groupname <- groups[groupno]
#     gdf <- df[which(df$group == groupname),]
#     
#     incl  <- gdf$include
#     impl  <- gdf$exclude
#     expl  <- gdf$aimreport
#     adapt <- gdf$adaptation
#     
#     EIadd <- lm(adapt ~ impl + expl + 0)
#     
#     cat(sprintf('%s:\n',toupper(groupname)))
#     #print(summary(EIadd))
#     
#     coef <- EIadd$coefficients
#     
#     #print(coef)
#     
#     # plot actual adaptation over predicted values:
#     
#     predictions <- predict(EIadd)
#     at <- range(predictions)
#     p2a <- lm(adapt ~ predictions)
#     #print(summary(p2a))
#     pcoef <- p2a$coefficients
#     print(confint(p2a,parm='predictions',level=0.95))
#     lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#     
#     ci <- predict( p2a,
#                    newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
#     
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
#     
#     points(predictions, adapt, pch=16, col=col.tr)
#     
#   }
#   
#   
#   
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,40,80), cex.axis=textsize)
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,40,80), cex.axis=textsize)
#   
#   
#   
# 
#   
#   # # # # # # # # # # #
#   # LOOSE ADDITIVITY MODCHALINGAM ET AL 2019
#   #
#   
#   cols.op <- c('#40ffbcff', '#00b374ff', '#40b5ffff', '#006eb3ff')
#   cols.tr <- c('#40ffbc2f', '#00b3742f', '#40b5ff2f', '#006eb32f')
#   
#   groups <- c('control30','control60','instructed30','instructed60')
#   
#   df <- read.csv('data/extra/Modchalingam_2019.csv', stringsAsFactors=FALSE)
#   df$explicit <- df$include - df$exclude
#   
#   grouplabels <- c()
#   groupcols <- c()
# 
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,70), ylim=c(0,70), ax=F, bty='n', asp=1)
#   text(0,70,'E: Modchalingam et al. (2)', font.main=1, cex=1.35*1.4, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,65),y=c(0,65),col="#999999", lw=1, lty=1)
# 
# 
#   for (groupno in c(1:length(groups))) {
# 
#     col.op <- cols.op[groupno]
#     col.tr <- cols.tr[groupno]
# 
#     groupname <- groups[groupno]
#     gdf <- df[which(df$group == groupname),]
# 
#     incl  <- gdf$include
#     impl  <- gdf$exclude
#     expl  <- gdf$explicit
#     adapt <- gdf$adaptation
# 
#     EIadd <- lm(adapt ~ impl + expl + 0)
# 
#     cat(sprintf('%s:\n',toupper(groupname)))
#     #print(summary(EIadd))
# 
#     coef <- EIadd$coefficients
# 
#     #print(coef)
# 
#     # plot actual adaptation over predicted values:
# 
#     predictions <- predict(EIadd)
#     at <- range(predictions)
#     p2a <- lm(adapt ~ predictions)
#     #print(summary(p2a))
#     pcoef <- p2a$coefficients
#     print(confint(p2a,parm='predictions',level=0.95))
#     lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
# 
#     ci <- predict( p2a,
#                    newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
# 
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
# 
#     points(predictions, adapt, pch=16, col=col.tr)
# 
# 
#   }
# 
# 
# 
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,30,60), cex.axis=textsize)
# 
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,30,60), cex.axis=textsize)
#   
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }


let3_Awareness_TwoRate <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/letter_Fig4_tworate.svg', width=8, height=3, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/letter_Fig4_tworate.pdf', width=8, height=3)
  }
  
  textsize <- 0.8
  
  layout(mat=matrix(c(1,2),nrow=1,ncol=2),widths=c(1,2))
  
  par(mar=c(2,4,0,0.1))
  
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,26), ylim=c(-7,40), ax=F, bty='n')
  text(0,35,'A: awareness', font.main=1, cex=1.35, adj=0)
  title(ylab='difference score [°]', line = 2.5)
  
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
    
    text(0.4+(groupno*4)-2,-8.5, group, srt=90, adj=c(0,0.5), cex=0.8)
    
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
  
  text(16, iM, 'unaware', srt=90, cex=0.8)
  text(16, eM, 'aware', srt=90, cex=0.8)
  
  dataM <- data.frame('explicit'=c(iM, eM))
  iLM <- dnorm(dataM$explicit, mean=iM, sd=iS)
  eLM <- dnorm(dataM$explicit, mean=eM, sd=eS)
  prob_dens_M <- (f*iLM) + ((1-f)*eLM)
  
  lines(c(18,18+((prob_dens_M[1]/sum(prob_dens))*150)),c(iM,iM), col="#001388FF", lty=1)
  lines(c(18,18+((prob_dens_M[2]/sum(prob_dens))*150)),c(eM,eM), col="#001388FF", lty=1)
  
  axis(side=2, at=c(0,15,30))
  
  
  
  
  
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-7,40), ax=F, bty='n')
  text(0,35,'B: state-space model', font.main=1, cex=1.35, adj=0)
  title(xlab='time: trials per block', line = 0.5)
  title(ylab='deviation [°]', line = 2.5)
  
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
  split_aim$ppno <- c(1:24)
  
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
  
  
  # now we plot the exlude reach deviations for both sub-groups:
  
  df <- read.csv('data/aiming.csv', stringsAsFactors = F)
  
  for (strategy in c(FALSE, TRUE)) {
    
    sdf <- df[which(df$participant %in% split_aim$ppno[which(split_aim$strategy == strategy)]),]
    
    if (strategy) {group <- 'aiming'; groupname <- 'aware aimers'} else {group <- 'aims'; groupname <- 'unaware aimers'}
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    baseline <- aggregate(reachdeviation_deg ~ participant, data=sdf[which(sdf$cursor == FALSE & sdf$strategy == 'none'),], FUN=mean, na.rm=TRUE)
    
    blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
    
    exclude <- sdf[which(sdf$trial %in% unlist(blocks) & sdf$strategy == 'exclude'),]
    
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
  
  groupnames <- c(groupnames, 'model output', 'slow', 'fast / aims', 'no-cursor blocks')
  groupcols  <- c(groupcols,  '#000000', '#000000', '#000000', '#BBBBBB')  
  lty        <- c(1,1,3,2,1,0)
  lwd        <- c(1,1,1,1,1,0)
  pch        <- c(NA,NA,NA,NA,NA,15)
  
  legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=lty, lwd=lwd, pch=pch, cex=textsize, pt.cex=1.5)
  
  axis(side=2, at=c(0,15,30))
  
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
  
  
}

let4_ExtraData <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/letter_Fig3_extradata.svg', width=7, height=7, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/letter_Fig3_extradata.pdf', width=7, height=7)
  }
  
  textsize <- 1.5
  
  lend <- 1
  paper.cex <- 0.9
  group.cex <- 0.65
  
  #layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow=TRUE))
  par(mar=c(4.5,0,0,0.1))
  
  #  3 or 6 - Modchalingam unpublished
  #  6      - Neville & Cressman
  #  2      - Schween et al., X/Y
  #  4      - Maresch et al 2020
  #  4      - Modchalingam et al 2019
  
  # 22 total + 5 headers = 27 ?
  
  datasets <- getExtraData()
  row <- 31
  
  plot(-1000,-1000,
       main='',xlab='slope',ylab='',
       xlim=c(-2.9,2.9),ylim=c(-1,row),
       bty='n',ax=F)
  
  for (xpos in c(-2.5,2.5)) {
    lines(c(xpos,xpos),c(0.5,row-.5),lty=1,col='#000000')
  }
  for (xpos in c(-1.5,1.5)) {
    lines(c(xpos,xpos),c(0.5,row-.5),lty=3,col='#999999')
  }
  
  
  axis(side=1, at=c(-2.5,-1.5),labels=c('-1','0'))
  text(-2.6,row-.5,'strict additivity',srt=90,adj=1)
  axis(side=1, at=c(1.5,2.5),labels=c('0','1'))
  text(2.6,row-.5,'loose additivity',srt=90,adj=1)
  
  # for combined estimates, we need to collect normalized data
  # we will normalize both by the rotation and by average adaptation
  adaptation.rotnorm <- c()
  adaptation.avgnorm <- c()
  explicit.rotnorm   <- c()
  explicit.avgnorm   <- c()
  implicit.rotnorm   <- c()
  implicit.avgnorm   <- c()
  
  for (dataset in datasets) {
    
    paper  <- dataset[['paper']]
    df     <- dataset[['data']]
    labels <- dataset[['labels']]
    
    if (!('implicit' %in% names(df))) {
      df$implicit <- df$exclude
    }
    
    row    <- row-1
    text(0,row,paper,cex=paper.cex,font=2)
    
    groups <- unique(df$group)
    
    for (group in groups) {
      
      row <- row-1
      label <- labels[group]
      text(0,row,label,cex=group.cex)
      
      # select data for the group:
      gdf <- df[which(df$group == group),]
      
      #cat(sprintf('%s N=%d\n',label,dim(gdf)[1]))
      
      # extract relevant columns:
      implicit   <- gdf$implicit
      explicit   <- gdf$explicit
      adaptation <- gdf$adaptation
      
      slopes <- getAdditivitySlopes(implicit = implicit,
                                    explicit = explicit,
                                    adaptation = adaptation)
      
      for (assumption in c('strict','loose')) {
        model <- slopes[[assumption]]
        offset <- c('strict'=-1.5, 'loose'=1.5)[assumption]
        # plot the slope and it's confidence interval:
        lines(x=model$slope_ci+offset, y=rep(row,2), lw=6, col=model$colors$tr, lend=lend)
        points(x=model$slope+offset,y=row,pch=1,col=model$colors$op)
      }
      
      # # plot the slope of the line and it's confidence interval:
      # lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
      # points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
      
      if (!(label %in% c('stepwise (30°)','stepwise (45°)','stepwise (60°)'))) {
        adaptation.rotnorm <- c(adaptation.rotnorm, adaptation / gdf$rotation)
        adaptation.avgnorm <- c(adaptation.avgnorm, adaptation / mean(adaptation))
        explicit.rotnorm   <- c(explicit.rotnorm,   explicit   / gdf$rotation)
        explicit.avgnorm   <- c(explicit.avgnorm,   explicit   / mean(adaptation))
        implicit.rotnorm   <- c(implicit.rotnorm,   implicit   / gdf$rotation)
        implicit.avgnorm   <- c(implicit.avgnorm,   implicit   / mean(adaptation))
      }
      
    }
    
  }
  
  # altogether there are 342 participants!
  row    <- row-1
  text(0,row,'combined',cex=paper.cex,font=2)
  
  row    <- row-1
  text(0,row,'rotation-normalized (N=346)',cex=group.cex)
  
  slopes <- getAdditivitySlopes(implicit = implicit.rotnorm,
                                explicit = explicit.rotnorm,
                                adaptation = adaptation.rotnorm)
  
  # plot the slope and it's confidence interval:
  lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
  points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
  
  # plot the slope of the line and it's confidence interval:
  lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
  points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
  
  
  row    <- row-1
  
  text(0,row,'adaptation-normalized (N=346)',cex=group.cex)
  slopes <- getAdditivitySlopes(implicit = implicit.avgnorm,
                                explicit = explicit.avgnorm,
                                adaptation = adaptation.avgnorm)
  
  # plot the slope and it's confidence interval:
  lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
  points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
  
  # plot the slope of the line and it's confidence interval:
  lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
  points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
  
  #legend(-2.5, y=0, 
  #       legend=c('supports additivity', 'supports combining', 'supports zero-sum', 'supports subtractivity', 'unclear'),
  #       col = slopes$colors.tr, lwd=c(6,6,6,6,6),
  #       bty='n', cex=0.9, ncol=5)
  
  colors.tr <- unlist(slopes$colors$tr)
  colors.op <- unlist(slopes$colors$op)
  
  print(colors.tr)
  
  legend(x=-2.4, y=0, 
         legend=c('additive', 'combine', 'zero-sum', 'subtractive', 'unclear'),
         col = colors.tr, lwd=c(6,6,6,6,6),
         bty='n', cex=0.75, ncol=5)
         
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}


testColors <- function() {
  
  #col.op <- c("#344088ff", "#732C64ff", "#E5576Dff", "#001388ff", "#73005Bff", "#E50023ff")
  #col.tr <- c("#3440882f", "#732C642f", "#E5576D2f", "#0013882f", "#73005B2f", "#E500232f")
  
  
  col.op <- c("#6C7088ff", "#735B6Eff", "#E5B6BDff", "#001388ff", "#73005Bff", "#E50023ff")
  col.tr <- c("#6C70882f", "#735B6E2f", "#E5B6BD2f", "#0013882f", "#73005B2f", "#E500232f")
  
  
  plot(-1000,-1000,xlim=c(0,1+length(col.op)),ylim=c(0.5,1.5),bty='n',ax=F,
       main='',xlab='',ylab='')
  
  for (cn in c(1:length(col.op))) {
    
    points(c(1:length(col.op)),rep(0.75,length(col.op)),col=col.op, pch=16, cex=3)
    points(c(1:length(col.op)),rep(1.25,length(col.op)),col=col.tr, pch=16, cex=3)
    
  }
  
}
