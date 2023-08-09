
library(rlang)

# support functions -----

getGroups <- function() {
  
  
  group <- c('control', 'instructed', 'aiming', 'aims', 'step')
  label <- c('control', 'instructed', 'aiming', 'aims', 'step')
  col.op <- c('#ff9329ff', '#e51636ff', '#7f00d8ff', '#cc00ccff', '#00ffffff')
  col.tr <- c('#ff93292f', '#e516362f', '#7f00d82f', '#cc00cc2f', '#00ffff2f')
  
  return(data.frame(group, label, col.op, col.tr))
  
  
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


# getExtraData <- function() {
#   
#   datasets <- list()
#   
#   df <- read.csv('data/extra/Bond_and_Taylor2015.csv', stringsAsFactors = FALSE)
#   
#   datasets[['bond2015']] <- list('paper'='Bond & Taylor (2015)',
#                                  'data'=df,
#                                  'labels' = list('rotating' = 'exp. 1, rotating landmarks, 45° (N=10)',
#                                                  'fixed' = 'exp. 1, fixed landmarks, 45° (N=10)',
#                                                  'fifteen'= 'exp. 3, 15° (N=10)',
#                                                  'thirty' = 'exp. 3, 30° (N=10)',
#                                                  'sixty'  = 'exp. 3, 60° (N=10)',
#                                                  'ninety' = 'exp. 3, 90° (N=10)'))
#   
#   
#   df <- read.csv('data/extra/Taylor_cs_45.csv', stringsAsFactors = FALSE)
#   
#   datasets[['taylor_cs']] <- list('paper'='Taylor c.s. (2014, 2016, unpublished)',
#                                   'data'=df,
#                                   'labels' = list('Taylor2014' = 'Taylor et al. (2014), 45° (N=15)',
#                                                   'Brudner2016' = 'Brudner et al. (2016), 45° (N=10)',
#                                                   'unpublished' = 'unpublished, 45° (N=10)'))
#   
#   
#   df <- read.csv('data/extra/Neville_and_Cressman_2018.csv', stringsAsFactors = FALSE)
#   df$group <- sprintf('%s_%d',c('control','instructed')[df$instruction],df$rotation)
#   df$explicit <- df$include - df$exclude
#   df$implicit <- df$exclude
#   
#   datasets[['neville']] <- list('paper'='Neville & Cressman (2018)',
#                                 'data'=df,
#                                 'labels' = list('control_20'='control 20° (N=11)',
#                                                 'control_40'='control 40° (N=10)',
#                                                 'control_60'='control 60° (N=10)',
#                                                 'instructed_20'='instructed 20° (N=11)',
#                                                 'instructed_40'='instructed 40° (N=10)',
#                                                 'instructed_60'='instructed 60° (N=10)'))
#   
#   
#   df <- read.csv('data/extra/Schween_etal_2018_2019.csv', stringsAsFactors = FALSE)
#   names(df)[which(names(df) == 'experiment')] <- 'group'
#   r.idx <- which(df$workspace == 'right')
#   for (dv in c('implicit','explicit','adaptation')) {
#    df[r.idx,dv] <- df[r.idx,dv] * -1
#   }
#   
#   df <- aggregate(cbind(implicit, adaptation, explicit) ~ participant + group, data=df, FUN=mean)
#   df$rotation <- 45
#   
#   datasets[['schween']] <- list('paper'='Schween et al. (2018; 2019)',
#                                 'data'=df,
#                                 'labels' = list('one'='2018: exp. 1, 45° (N=21)',
#                                                 'two'='2019: exp. 3, 45° (N=20)'))
# 
#   df <- read.csv('data/extra/Modchalingam_2019.csv', stringsAsFactors=FALSE)
#   df$explicit <- df$include - df$exclude
#   df$implicit <- df$exclude
#   df$rotation <- c('control30'=30,
#                    'control60'=60,
#                    'instructed30'=30,
#                    'instructed60'=60)[df$group]
#   
#   datasets[['modchalingam2019']] <- list('paper'='Modchalingam et al. (2019)',
#                                          'data'=df,
#                                          'labels' = list('control30'='control 30° (N=20)',
#                                                          'control60'='control 60° (N=20)',
#                                                          'instructed30'='instructed 30° (N=21)',
#                                                          'instructed60'='instructed 60° (N=24)'))
#   
#   df <- read.csv('data/extra/Maresch_2020.csv', stringsAsFactors = FALSE)
#   df$explicit <- df$aimreport
#   df$rotation <- 60
#   df$implicit <- df$exclude
#   
#   datasets[['maresch2020']] <- list('paper'='Maresch et al. (2020)',
#                                     'data'=df,
#                                     'labels' = list('CR'='continuous report, 60°, (N=12)',
#                                                     'IR_E'='exclusion, 60°, (N=17)',
#                                                     'IR_I'='inclusion, 60° (N=12)',
#                                                     'IR_EI'='exclusion & inclusion, 60° (N=11)'))
#   
#   
#   df <- read.csv('data/extra/Decarie_and_Cressman_2022.csv', stringsAsFactors = FALSE)
#   df$rotation <- 30
#   
#   datasets[['decarie_and_cressman']] <- list('paper' = 'Decarie & Cressman (2022)',
#                                              'data' = df,
#                                              'labels' = list('PTWF' = 'feedback, 30° (N=24)',
#                                                              'PTNF' = 'no feedback, 30° (N=24)',
#                                                              'CTRL' = 'control, 30° (N=24)'))
#   
#   stepwise <- read.csv('data/extra/Modchalingam_unpublished.csv', stringsAsFactors = FALSE)
#   stepwise$explicit <- stepwise$include - stepwise$exclude
#   #stepwise$group <- sprintf('%s_%d', stepwise$group, abs(stepwise$rotation_angle))
#   stepwise$rotation <- abs(stepwise$rotation_angle)
#   stepwise$implicit <- stepwise$exclude
#   stepwise$participant <- stepwise$ppt
#   
#   datasets[['stepwise']] <- list('paper'='Modchalingam et al. (unpublished)',
#                                  'data'=stepwise,
#                                  'labels' = list('abrupt_60'='abrupt, 60°, (N=36)',
#                                                  'ramped_60'='ramped, 60° (N=33)',
#                                                  'stepwise_15'='stepwise, 15° (N=37)',
#                                                  'stepwise_30'='stepwise, 30°',
#                                                  'stepwise_45'='stepwise, 45°',
#                                                  'stepwise_60'='stepwise, 60°'))
#   
#   return(datasets)
#   
# }
# 
# bindExtraData <- function() {
#   
#   datasets <- getAllExtraData()
#   
#   alldata <- NA
#   
#   for (dataset in datasets) {
#     
#     subdf <- dataset[['data']]
#     
#     # print(dataset[['paper']])
#     # print(str(subdf))
#     
#     subdf <- subdf[,c('group','participant','rotation','adaptation','explicit','implicit')]
#     
#     subdf$paper <- dataset[['paper']]
#     subdf$grouplabel <- dataset[['labels']][subdf$group]
#     
#     if (is.data.frame(alldata)) {
#       alldata <- rbind(alldata, subdf)
#     } else {
#       alldata <- subdf
#     }
#     
#   }
#   
#   for (colname in names(alldata)) {
#     alldata[,colname] <- unlist(alldata[,colname])
#   }
#   
#   return(alldata)
#   
# }

getAdditivitySlopes <- function(implicit,explicit,adaptation) {
  
  # library(scales)
  # scales::viridis_pal(begin= 0.1, end=1.0)(4)
  colors.op <- c("#482576FF", "#2A788EFF", "#43BF71FF", "#FDE725FF")
  colors.tr <- c("#4825763F", "#2A788E3F", "#43BF713F", "#FDE7253F")
  
  output <- list()
  
  output[['colors']] <- list('op'=list(c(colors.op,"#999999FF")), 'tr'=list(c(colors.tr,"#99999927")))
  
  output[['strict']] <- list()
  output[['loose']]  <- list()
  
  # fit line where strict additivity should give slope = -1
  e2i      <- lm(implicit ~ explicit)
  coef     <- e2i$coefficients
  slope    <- coef[2]
  slope_ci <- confint(e2i,parm='explicit',level=0.95)
  
  # we use orthogonal distance regression instead, to stay away from slope=0 as much as possible
  
  #slope    <- ODR_slope(X=explicit, y=implicit, bootstraps=NA)
  #slope_ci <- ODR_slope(X=explicit, y=implicit, bootstraps=1000)

  #output$strict[['intercept']] <- coef[1]
  output$strict[['slope']]     <- slope
  output$strict[['slope_ci']]  <- slope_ci
  
  op <- "#999999FF"
  tr <- "#99999927"
  
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
  tr <- "#9999992F"
  
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

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  #invisible(t.col)
  return(t.col)
}

setColorAlpha <- function(col, alpha = 34) {
  
  # store the names:
  colornames <- names(col)
  
  # print(colornames)
  
  # get RGB values for named color
  rgb.val <- t(col2rgb(col))
  
  # add alpha column:
  # rgb.val <- rbind(rgb.val, rep(alpha, dim(rgb.val)[1]))
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val,
               alpha = alpha,
               max   = 255)
  
  names(t.col) <- colornames
  
  return(t.col)
  
}


# old figures -----

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

# fig2_Learning <- function(target='inline') {
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig2_learning.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig2_learning.pdf', width=8, height=3)
#   }
#   
#   textsize <- 0.8
#   
#   # we will plot all data by trial
#   # and illustrate the paradigm at the same time
#   
#   # groups:
#   # non-instructed: orange
#   # instructed: red
#   # aiming: purple & pink
#   
#   # no-cursors: gray & blue?
#   
#   # plot conditions (no cursors & rotations)
#   
#   par(mar=c(2,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
#   
#   title(xlab='time: trials per block', line = 0.5)
#   title(ylab='reach/aim deviation [°]', line = 2.5)
#   
#   plotBlocks(textsize = textsize)
#   
#   groups <- getGroups()
#   
#   # plot learning data:
#   
#   blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
#   
#   groupnames <- c()
#   groupcols <- c()
#   
#   for (group in c('control', 'instructed', 'aiming')) {
#     
#     df <- read.csv(sprintf('data/%s-training-trials.csv', group), stringsAsFactors = F)
#     
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     groupnames <- c(groupnames, as.character(groups$label[which(groups$group == group)]))
#     groupcols <- c(groupcols, col.op)
#     
#     for (block in blocks) {
#       
#       CI.lo <- df$CI.lo[which(df$trial %in% block)]
#       CI.hi <- df$CI.hi[which(df$trial %in% block)]
#       average <- df$average[which(df$trial %in% block)]
#       
#       polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#       lines(block, average, col=col.op)
#       
#     }
#     
#   }
#   
#   
#   # plot aiming
#   
#   
#   group <- 'aims'
#   
#   df <- read.csv('data/aiming-aim-trials.csv', stringsAsFactors = F)
#   
#   col.op <- as.character(groups$col.op[which(groups$group == group)])
#   col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#   
#   groupnames <- c(groupnames, as.character(groups$label[which(groups$group == group)]))
#   groupcols <- c(groupcols, col.op)
#   
#   for (block in blocks) {
#     
#     CI.lo <- df$CI.lo[which(df$trial %in% block)]
#     CI.hi <- df$CI.hi[which(df$trial %in% block)]
#     average <- df$average[which(df$trial %in% block)]
#     
#     polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#     lines(block, average, col=col.op)
#     
#   }
#   
#   legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   
#   axis(side=2, at=c(0,15,30))
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }


# fig3_Additivity <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/Fig3_additivity.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig3_additivity.pdf', width=8, height=3)
#   }
#   
#   
#   textsize <- 0.8
#   par(mar=c(0,3.5,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,22), ylim=c(-10,40), ax=F, bty='n')
#   title(ylab='reach deviation [°]', line=2.5)
#   
#   lines(x=c(0,21),y=c(0, 0 ),col="#999999", lw=1, lty=1)
#   lines(x=c(0,21),y=c(30,30),col="#999999", lw=1, lty=1)
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
#     exclude <- as.numeric(df['exclude',pp] - df['none',pp])
#     include <- as.numeric(df['include',pp] - df['none',pp])
#     
#     # EXCLUDE:
#     CI <- Reach::getConfidenceInterval(exclude, method='b')
#     avg <- mean(exclude)
#     xoffset <- (groupno*2) + 6
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(exclude)), exclude, pch=16, col=col.tr)
#     
#     #text(x=(groupno*8)-6.5,y=-1,labels='exclude', adj=c(0,.5), srt=-45, cex=textsize)
#     
#     # INCLUDE:
#     CI <- Reach::getConfidenceInterval(include, method='b')
#     avg <- mean(include)
#     xoffset <- (groupno*2) + 13
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(include)), include, pch=16, col=col.tr)
#     
#     
#     #text(x=(groupno*8)-4.5,y=-1,labels='include', adj=c(0,.5), srt=-45, cex=textsize)
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
#     xoffset <- (groupno*2) - 1
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(adaptation)), adaptation, pch=16, col=col.tr)
#     
#     #text(x=(groupno*8)-1.5,y=-1,labels='adaptation', adj=c(0,.5), srt=-45, cex=textsize)
#     
#   }
#   
#   df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
#   aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
#                         colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
#   groupno <- 3
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   CI <- Reach::getConfidenceInterval(aims, method='b')
#   avg <- mean(aims)
#   xoffset <- (groupno*2) - 1
#   polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#   lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#   # replace with lines?
#   points(x=rep(xoffset+1, length(aims)), aims, pch=16, col=col.tr)
#   
#   grouplabels <- c(grouplabels, 'aims')
#   groupcols <- c(groupcols, col.op)
#   
#   
#   text(x = 10.5, y=-2, labels='exclude',    cex=textsize)
#   text(x =  3.5, y=-2, labels='adaptation', cex=textsize)
#   text(x = 17.5, y=-2, labels='include',    cex=textsize)
#   
#   legend(0,15,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

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

# fig4_Explicit <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/Fig4_explicit.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig4_explicit.pdf', width=8, height=3)
#   }
#   
#   layout(matrix(c(2,1), nrow=1, ncol=2), widths=c(1,2), heights=c(1))
#   par(mar=c(4,3.75,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,26), ylim=c(-10,30), ax=F, bty='n')
#   title(ylab='explicit learning [°]', line = 2.5)
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
#     df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
#     row.names(df) <- df$condition
#     
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
#     groupcols <- c(groupcols, col.op)
#     
#     explicit <- as.numeric(df['include',pp] - df['exclude',pp])
#     CI <- Reach::getConfidenceInterval(explicit, method='b')
#     avg <- mean(explicit)
#     polygon(x=c(0,1,1,0)+(groupno*4)-2, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(0,1)+(groupno*4)-2, y=rep(avg,2), col=col.op)
#     
#     points(x=rep((groupno*4), length(explicit)), explicit, pch=16, col=col.tr)
#     
#     dX <- density(explicit, n=81, from=-10, to=30, bw=2.5)$y
#     dX <- (dX / sum(dX)) * 150
#     dY <- seq(-10,30,.5)
#     
#     
#     polygon(x=c(0,dX,0)+18, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=col.tr)
#     
#     lines(dX+18, dY, col=col.op)
#     
#   }
#   
#   data <- data.frame('explicit'=seq(-10,30,.5))
#   bimodal <- fitModel()
#   iM <- bimodal[['par']]['iM']
#   iS <- bimodal[['par']]['iS']
#   eM <- bimodal[['par']]['eM']
#   eS <- bimodal[['par']]['eS']
#   f  <- bimodal[['par']]['f']
#   iL <- dnorm(data$explicit, mean=iM, sd=iS)
#   eL <- dnorm(data$explicit, mean=eM, sd=eS)
#   prob_dens <- (f*iL) + ((1-f)*eL)
#   
#   dX <- (prob_dens / sum(prob_dens)) * 150
#   
#   lines(dX+18, dY, col="#001388FF", lty=2)
# 
#   axis(side=2, at=c(0,10,20))
#   
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-5,25), ylim=c(-10,30), ax=F, bty='n')
#   
#   title(xlab='aiming [°]', line=2.5)
#   title(ylab='explicit learning [°]', line=2.5)
#   
#   df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
#   
#   #aims <- as.numeric(df[24,pp])
#   
#   #aims <- as.numeric(df[which(df$block==23),pp] - df[which(df$block==4),pp])
#   
#   aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
#                         colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
#   
#   at <- range(aims)
#   
#   at <- c(-2,20)
#   
#   points(aims,explicit,pch=16,col=col.tr)
#   lines(at,at,col='#666666',lty=1)
#   
#   #print(cor.test(aims,explicit))
#   
#   A2R <- lm(explicit ~ aims)
#   
#   coef <- A2R$coefficients
#   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   
#   
#   ci <- predict( A2R,
#                  newdata=data.frame(aims=seq(-2,20,.2)),
#                  interval = "confidence")
#   
#   X <- c(seq(-2,20,.2),rev(seq(-2,20,.2)))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   
#   axis(side=1, at=c(0,10,20))
#   axis(side=2, at=c(0,10,20))
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }


# fig5_Additivity <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/Fig5_additivity.svg', width=8, height=8/3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig5_additivity.pdf', width=8, height=8/3)
#   }
#   
#   
#   textsize <- 0.8
#   par(mar=c(3.5,3.5,3.5,3.5))
#   
#   layout(mat=matrix(c(1,2), nrow=1, ncol=2, byrow = T))
#   
#   
#   grouplabels <- c()
#   groupcols <- c()
#   
#   adf <- getAdditivityData()
#   
#   total <- mean(adf$adaptation)
#   at <- c(-10,5+total) 
#   
#   groups <- getGroups()
#   groupnames <- c('control', 'instructed', 'aiming')
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-15,45), ylim=c(-5,35), ax=F, bty='n', asp=1)
#   lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
#   lines(x=c(0,40),y=c(0,0),col='#999999', lw=1, lty=2)
#   lines(x=c(0,0),y=c(0,40),col='#999999', lw=1, lty=2)
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     groupname <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#     
#     grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == groupname)]))
#     groupcols <- c(groupcols, col.op)
#     
#     idx <- which(adf$group == groupname)
#     excl <- adf[idx,'exclude']
#     incl <- adf[idx,'include']
#     adapt <- adf[idx,'adaptation']
#     
#     expl <- incl - excl
# 
#     at <- range(expl)
#     
#     e2i <- lm(excl ~ expl)
#     
#     
#     cat(sprintf('%s:\n',toupper(groupname)))
#     #print(summary(I2A))
#     
#     coef <- e2i$coefficients
#     lines(at, coef[1]+(at*coef[2]), col=col.op)
#     
#     
#     ci <- predict( e2i,
#                    newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
#     
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
#     
#     points(expl, excl, pch=16, col=col.tr)
#     
#     print(confint(e2i,parm='expl',level=0.95))
#     print(confint(e2i,parm='(Intercept)',level=0.95))
#     
#   }
#   
#   #print(mean(adf$adaptation))
#   
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   
#   grouplabels <- c(grouplabels, 'aims')
#   groupcols <- c(groupcols, col.op)
#   
#   idx <- which(adf$group == 'aiming')
#   expl <- adf$aiming[idx]
#   impl <- adf$exclude[idx]
#   adapt <- adf$adaptation[idx]
#   
#   at <- range(expl)
#   
#   e2i <- lm(impl ~ expl)
#   
#   
#   cat(sprintf('%s:\n',toupper('aims')))
#   print(confint(e2i,parm='expl',level=0.95))
#   print(confint(e2i,parm='(Intercept)',level=0.95))
#   #print(summary(I2A))
#   
#   coef <- e2i$coefficients
#   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   
#   
#   ci <- predict( e2i,
#                  newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#                  interval = "confidence")
#   
#   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   points(expl, impl, pch=16, col=col.tr)
#   
#   
#   title(main='strict additivity')
#   
#   title(ylab='implicit measure [°]', line=2.5)
#   axis(side=2, at=c(0,15,30))
#   
#   title(xlab='explicit measure [°]', line=2.5)
#   axis(side=1, at=c(-15,15,45))
#   
#   legend(20,40,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   # # # # # # # # # # # 
#   # LOOSE ADDITIVITY
#   # 
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,45), ylim=c(10,40), ax=F, bty='n', asp=1)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(10,40),y=c(10,40),col="#999999", lw=1, lty=1)
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     groupname <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#     
#     idx <- which(adf$group == groupname)
#     
#     incl  <- adf$include[idx]
#     impl  <- adf$exclude[idx]
#     expl  <- incl - excl
#     adapt <- adf$adaptation[idx]
#     
#     
#     
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
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   
#   idx <- which(adf$group == 'aiming')
#   expl <- adf$aiming[idx]
#   impl <- adf$exclude[idx]
#   adapt <- adf$adaptation[idx]
#   
#   
#   EIadd <- lm(adapt ~ impl + expl + 0)
#   
#   cat(sprintf('%s:\n',toupper('aims')))
#   #print(summary(EIadd))
#   
#   coef <- EIadd$coefficients
#   
#   #print(coef)
#   
#   # plot actual adaptation over predicted values:
#   
#   predictions <- predict(EIadd)
#   at <- range(predictions)
#   p2a <- lm(adapt ~ predictions)
#   #print(summary(p2a))
#   pcoef <- p2a$coefficients
#   print(confint(p2a,parm='predictions',level=0.95))
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
#   
#   title(main='loose additivity')
#   
#   title(ylab='adaptation [°]', line=2.5)
#   axis(side=2, at=c(10,25,40))
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=2.5)
#   axis(side=1, at=c(0,15,30,45))
#   
#   
#   # # # # # # # # # # # # # # # # # # # # # # 
#   # CROSS_GROUP - LOOSE ADDITIVITY
#   # 
#   
#   # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(5,45), ylim=c(5,45), ax=F, bty='n', asp=1)
#   # lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   # 
#   # for (groupno in c(1:length(groupnames))) {
#   #   
#   #   groupname <- groupnames[groupno]
#   #   
#   #   col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#   #   col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#   #   
#   #   idx <- which(adf$group != groupname)
#   #   incl  <- adf$include[idx]
#   #   impl  <- adf$exclude[idx]
#   #   expl  <- incl - excl
#   #   adapt <- adf$adaptation[idx]
#   #   
#   #   
#   #   
#   #   
#   #   EIadd <- lm(adapt ~ impl + expl + 0)
#   #   
#   #   # cat(sprintf('%s:\n',toupper(groupname)))
#   #   # print(summary(EIadd))
#   #   
#   #   coef <- EIadd$coefficients
#   #   
#   #   #print(coef)
#   #   
#   #   # plot actual adaptation over predicted values:
#   #   
#   #   idx <- which(adf$group == groupname)
#   #   incl  <- adf$include[idx]
#   #   impl  <- adf$exclude[idx]
#   #   expl  <- incl - excl
#   #   adapt <- adf$adaptation[idx]
#   #   
#   #   
#   #   predictions <- predict(EIadd, newdata=data.frame(impl, expl, adapt))
#   #   at <- range(predictions)
#   #   p2a <- lm(adapt ~ predictions)
#   #   #sprint(summary(p2a))
#   #   pcoef <- p2a$coefficients
#   #   
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
#   # title(main='cross-group predictions')
#   # 
#   # title(ylab='adaptation [°]', line=2.5)
#   # axis(side=2, at=c(5,25,45))
#   # 
#   # title(xlab=expression(paste(beta[i] %.% Implicit + beta[e] %.% Explicit)), line=2.5)
#   # axis(side=1, at=c(5,25,45))
#   
#   
# 
#   
#   blues.s <- c('#41ffc9ff',
#                mixCol(a='#41ffc9', b='#1d7791', balance=c(2,1)),
#                mixCol(a='#41ffc9', b='#1d7791', balance=c(1,2)),
#                '#1d7791ff')
#   blues.t <- c('#41ffc92f',
#                mixCol(a='#41ffc92f', b='#1d77912f', balance=c(2,1)),
#                mixCol(a='#41ffc92f', b='#1d77912f', balance=c(1,2)),
#                '#1d77912f')
#   
#   stepwise <- getStepwiseData()
#   stepwise$explicit <- stepwise$include - stepwise$exclude
#   
#   # # # # # # # # # # # # # # # # # # # # # # 
#   # STEPWISE - STRICT ADDITIVITY
#   # 
#   
# 
#   # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-15,60), ylim=c(-15,60), ax=F, bty='n', asp=1)
#   # 
#   # rotations <- unique(stepwise$rotation_angle)
#   # for (rot_no in c(1:length(rotations))) {
#   #   rot <- rotations[rot_no]
#   #   total <- mean(stepwise$adaptation[which(stepwise$rotation_angle == rot)])
#   #   at <- c(-10,10+total) 
#   #   lines(x=at,y=(at*-1)+total,col=blues.s[rot_no], lw=1, lty=2)
#   # }
#   # lines(x=c(0,0,60),y=c(60,0,0),col='#999999', lw=1, lty=2)
#   # #lines(x=c(0,0),y=c(0,60),col='#999999', lw=1, lty=2)
#   # 
#   # 
#   # for (rot_no in c(1:length(rotations))) {
#   #   
#   #   rot <- rotations[rot_no]
#   #   
#   #   col.op <- blues.s[rot_no]
#   #   col.tr <- blues.t[rot_no]
#   #   
#   #   idx <- which(stepwise$rotation_angle == rot)
#   #   excl <- stepwise[idx,'exclude']
#   #   incl <- stepwise[idx,'include']
#   #   expl <- stepwise[idx,'explicit']
#   #   adapt <- stepwise[idx,'adaptation']
#   #   
#   # 
#   #   at <- range(expl)
#   #   e2i <- lm(excl ~ expl)
#   #   
#   #   coef <- e2i$coefficients
#   #   lines(at, coef[1]+(at*coef[2]), col=col.op)
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
#   #   cat(sprintf('\nSTEPWISE %d deg\n',rot))
#   #   print(confint(e2i,parm='expl',level=0.95))
#   #   
#   # }
#   # 
#   # 
#   # 
#   # title(main='multiple rotations')
#   # 
#   # title(ylab='implicit measure [°]', line=2.5)
#   # #axis(side=1, at=c(0,30,60))
#   # #axis(side=1, at=c(-15,0,15,30,45,60), labels = c('','0','','30','','60'))
#   # axis(side=1, at=c(-15,22.5,60))
#   # 
#   # title(xlab='explicit measure [°]', line=2.5)
#   # #axis(side=2, at=c(0,30,60))
#   # #axis(side=2, at=c(-15,0,15,30,45,60), labels = c('','0','','30','','60'))
#   # axis(side=2, at=c(-15,22.5,60))
#   # 
#   # legend(30,
#   #        60,
#   #        legend=sprintf('%d° rotation',c(15,30,45,60)),
#   #        col=blues.s, 
#   #        bty='n', 
#   #        lty=1, 
#   #        cex=textsize)
#   
#   
#   
#   # # # # # # # # # # # # # # # # # # # # # # 
#   # STEPWISE - STRICT ADDITIVITY
#   # 
#   
#   
#   # plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,60), ylim=c(0,60), ax=F, bty='n', asp=1)
#   # 
#   # lines(c(0,60),c(0,60),col='#999999',lty=1)
#   # for (rot in c(15,30,45,60)) {
#   #   lines(c(0,rot,rot),c(rot,rot,0),col='#999999',lty=2)
#   # }
#   # 
#   # impl  <- stepwise$exclude / -stepwise$rotation_angle*60
#   # expl  <- stepwise$explicit / -stepwise$rotation_angle*60
#   # adapt <- stepwise$adaptation / -stepwise$rotation_angle*60
#   # 
#   # rota  <- -stepwise$rotation_angle
#   # 
#   # #impl  <- stepwise$exclude
#   # #expl  <- stepwise$explicit
#   # #adapt <- stepwise$adaptation
#   # 
#   # print(range(adapt))
#   # EIadd <- lm(adapt ~ impl + expl + 0)
#   # 
#   # col.op <- mixCol(a='#41ffc9ff', b='#1d7791ff', balance=c(1,1))
#   # col.tr <- mixCol(a='#41ffc92f', b='#1d77912f', balance=c(1,1))
#   # 
#   # predictions <- predict(EIadd)
#   # at <- range(predictions)
#   # p2a <- lm(adapt ~ predictions)
#   # print(summary(p2a))
#   # pcoef <- p2a$coefficients
#   # print(confint(p2a,parm='predictions',level=0.95))
#   # lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#   # 
#   # ci <- predict( p2a,
#   #                newdata=data.frame(predictions=seq(at[1],at[2],length.out=40)),
#   #                interval = "confidence")
#   # 
#   # X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#   # Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   # polygon(x=X,y=Y,col=col.tr,border=NA)
#   # 
#   # 
#   # for (rotno in c(1:4)) {
#   #   rot <- c(15,30,45,60)[rotno]
#   #   idx <- which(rota == rot)
#   #   points(predictions[idx], adapt[idx], pch=16, col=blues.t[rotno])
#   # }
#   # 
#   # 
#   # 
#   # title(main='loose additivity')
#   # 
#   # title(ylab='adaptation [norm]', line=2.5)
#   # axis(side=2, at=c(0,30,60))
#   # 
#   # title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=2.5)
#   # axis(side=1, at=c(0,30,60))
#   
#   
#   
#   
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# 
# }

# fig6_splitAiming <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/Fig6_splitAiming.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig6_splitAiming.pdf', width=8, height=3)
#   }
#   
#   textsize <- 0.8
#   
#   par(mar=c(2,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
#   
#   title(xlab='time: trials per block', line = 0.5)
#   title(ylab='deviation [°]', line = 2.5)
#   
#   plotBlocks(textsize=textsize)
#   
#   
#   # group the participants in 2 sets:
#   df <- getExplicitData() 
#   
#   iM <- median(df$explicit[which(df$group=='control')])
#   eM <- median(df$explicit[which(df$group=='instructed')])
#   iS <-     sd(df$explicit[which(df$group=='control')])
#   eS <-     sd(df$explicit[which(df$group=='instructed')])
#   
#   iPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=iM, sd=iS)
#   ePD <- dnorm(df$explicit[which(df$group=='aiming')], mean=eM, sd=eS)
#   
#   participant <- sprintf('p%03d',c(1:24))
#   strategy <- ePD > iPD
#   split_aim <- data.frame(participant,strategy)
#   split_aim$ppno <- c(1:24)
#   
#   groups <- getGroups()
#   groupnames <- c()
#   groupcols <- c()
#   df <- read.csv('data/aiming-aim-trials.csv', stringsAsFactors = F)
#   
#   for (strategy in c(FALSE, TRUE)) {
#     
#     sdf <- df[,as.character(split_aim$participant[which(split_aim$strategy == strategy)])]
#     
#     #print(str(sdf))
#     
#     if (strategy) {group <- 'aiming'; groupname <- 'aware aimers (N=9)'} else {group <- 'aims'; groupname <- 'unaware aimers (N=15)'}
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     groupnames <- c(groupnames, groupname)
#     groupcols <- c(groupcols, col.op)
#     
#     blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
#     
#     for (block in blocks) {
#       
#       block <- unlist(block)
#       trial_idx <- which(df$trial %in% block)
#       bdf <- sdf[trial_idx,]
#       
#       CI <- apply(bdf, MARGIN=c(1), Reach::getConfidenceInterval, method='b')
#       CI.lo <- as.numeric(CI[1,])
#       CI.hi <- as.numeric(CI[2,])
#       average <- rowMeans(bdf, na.rm=TRUE)
#       
#       polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#       lines(block, average, col=col.op)
#       
#     }
#     
#   }
#   
#   
#   # now we plot the exlude reach deviations for both sub-groups:
#   
#   df <- read.csv('data/aiming.csv', stringsAsFactors = F)
#   
#   for (strategy in c(FALSE, TRUE)) {
#     
#     sdf <- df[which(df$participant %in% split_aim$ppno[which(split_aim$strategy == strategy)]),]
#     
#     if (strategy) {group <- 'aiming'; groupname <- 'aware aimers'} else {group <- 'aims'; groupname <- 'unaware aimers'}
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     baseline <- aggregate(reachdeviation_deg ~ participant, data=sdf[which(sdf$cursor == FALSE & sdf$strategy == 'none'),], FUN=mean, na.rm=TRUE)
#   
#     blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
#   
#     exclude <- sdf[which(sdf$trial %in% unlist(blocks) & sdf$strategy == 'exclude'),]
#   
#     for (participant in baseline$participant) {
#       idx <- which(exclude$participant == participant)
#       exclude$reachdeviation_deg[idx] <- exclude$reachdeviation_deg[idx] - baseline$reachdeviation_deg[which(baseline$participant == participant)]
#     }
#   
#     for (block in blocks) {
#     
#       block <- unlist(block)
#     
#       CI.lo <- c()
#       CI.hi <- c()
#       average <- c()
#     
#       for (trial in unlist(block)) {
#         reachdevs <- exclude$reachdeviation_deg[which(exclude$trial == trial)]
#         CI <- Reach::getConfidenceInterval(reachdevs, method='b')
#         CI.lo <- c(CI.lo, unlist(CI[1]))
#         CI.hi <- c(CI.hi, unlist(CI[2]))
#         average <- c(average, mean(reachdevs, na.rm=TRUE))
#       }
#     
#       polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#       lines(block, average, col=col.op)
#     
#     }  
#   
#   }
#   
#   # we fit the two-rate model to mean reach deviations in both sub-groups
#   # is the fast process equal to aiming responses?
#   
#   df <- get2rateData(group='aiming')
#   schedule <- df$rotation * -1
#   
#   
#   for (strategy in c(FALSE, TRUE)) {
#     
#     if (strategy) {group <- 'aiming'; groupname <- 'aware aimers'} else {group <- 'aims'; groupname <- 'unaware aimers'}
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     sdf <- df[,as.character(split_aim$participant[which(split_aim$strategy == strategy)])]
#     reaches <- rowMeans(sdf, na.rm = TRUE)
#     
#     par <- Reach::twoRateFit(schedule       = schedule,
#                              reaches        = reaches,
#                              checkStability = TRUE)
#     
#     fit <- Reach::twoRateModel(par=par, schedule=schedule)
#     
#     lines(fit$total, col=col.op, lty=3)
#     lines(fit$slow,  col=col.op, lty=2)
#     lines(fit$fast,  col=col.op, lty=1)
#     
#     
#   }
#   
#   
#   legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   
#  
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#    
# }

# figA_slowControl <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/FigA_implicitControl.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/FigA_implicitControl.pdf', width=8, height=3)
#   }
#   
#   textsize <- 0.8
#   
#   par(mar=c(2,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
#   
#   title(xlab='time: trials per block', line = 0.5)
#   title(ylab='reach deviation [°]', line = 2.5)
#   
#   plotBlocks(textsize=textsize)
#   
#   groups <- getGroups()
#   groupnames <- c()
#   groupcols <- c()
# 
#   df <- read.csv('data/control.csv', stringsAsFactors = F)
#   
#   col.op <- as.character(groups$col.op[which(groups$group == 'control')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'control')])
#   
#   baseline <- aggregate(reachdeviation_deg ~ participant, data=df[which(df$cursor == FALSE & df$strategy == 'none'),], FUN=mean, na.rm=TRUE)
#   
#   blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
#   
#   exclude <- df[which(df$trial %in% unlist(blocks) & df$strategy == 'exclude'),]
#   
#   for (participant in baseline$participant) {
#     idx <- which(exclude$participant == participant)
#     exclude$reachdeviation_deg[idx] <- exclude$reachdeviation_deg[idx] - baseline$reachdeviation_deg[which(baseline$participant == participant)]
#   }
#   
#   for (block in blocks) {
#     
#     block <- unlist(block)
#     
#     CI.lo <- c()
#     CI.hi <- c()
#     average <- c()
#     
#     for (trial in unlist(block)) {
#       reachdevs <- exclude$reachdeviation_deg[which(exclude$trial == trial)]
#       CI <- Reach::getConfidenceInterval(reachdevs, method='b')
#       CI.lo <- c(CI.lo, unlist(CI[1]))
#       CI.hi <- c(CI.hi, unlist(CI[2]))
#       average <- c(average, mean(reachdevs, na.rm=TRUE))
#     }
#     
# 
#     polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#     lines(block, average, col=col.op)
#   
#   }  
#     
#   # we fit the two-rate model to mean reach deviations in both sub-groups
#   # is the fast process equal to aiming responses?
#   
#   df <- get2rateData(group='control')
#   schedule <- df$rotation * -1
#   
#   reaches <- rowMeans(df[,sprintf('p%03d',c(1:24))], na.rm = TRUE)
#   
#   par <- Reach::twoRateFit(schedule       = schedule,
#                            reaches        = reaches,
#                            checkStability = TRUE)
#   
#   fit <- Reach::twoRateModel(par=par, schedule=schedule)
# 
#   lines(fit$total, col=col.op, lty=3)
#   lines(fit$slow,  col=col.op, lty=2)
#   lines(fit$fast,  col=col.op, lty=1)
# 
#   
#   #legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

# figB_fastInstructed <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/FigB_fastInstructed.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/FigB_fastInstructed.pdf', width=8, height=3)
#   }
#   
#   textsize <- 0.8
#   
#   par(mar=c(2,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-8,38), ax=F, bty='n')
#   
#   title(xlab='time: trials per block', line = 0.5)
#   title(ylab='reach deviation [°]', line = 2.5)
#   
#   plotBlocks(textsize=textsize)
#   
#   
#   groups <- getGroups()
#   groupnames <- c()
#   groupcols <- c()
#   
#   df <- read.csv('data/instructed.csv', stringsAsFactors = F)
#   
#   col.op <- as.character(groups$col.op[which(groups$group == 'instructed')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'instructed')])
#   
#   baseline <- aggregate(reachdeviation_deg ~ participant, data=df[which(df$cursor == FALSE & df$strategy == 'exclude'),], FUN=mean, na.rm=TRUE)
#   
#   blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
#   
#   exclude <- df[which(df$trial %in% unlist(blocks) & df$strategy == 'include'),]
#   
#   for (participant in baseline$participant) {
#     idx <- which(exclude$participant == participant)
#     exclude$reachdeviation_deg[idx] <- exclude$reachdeviation_deg[idx] - baseline$reachdeviation_deg[which(baseline$participant == participant)]
#   }
#   
#   for (block in blocks) {
#     
#     block <- unlist(block)
#     
#     CI.lo <- c()
#     CI.hi <- c()
#     average <- c()
#     
#     for (trial in unlist(block)) {
#       reachdevs <- exclude$reachdeviation_deg[which(exclude$trial == trial)]
#       CI <- Reach::getConfidenceInterval(reachdevs, method='b')
#       CI.lo <- c(CI.lo, unlist(CI[1]))
#       CI.hi <- c(CI.hi, unlist(CI[2]))
#       average <- c(average, mean(reachdevs, na.rm=TRUE))
#     }
#     
#     
#     polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#     lines(block, average, col=col.op)
#     
#   }  
#   
#   # we fit the two-rate model to mean reach deviations in both sub-groups
#   # is the fast process equal to aiming responses?
#   
#   df <- get2rateData(group='instructed')
#   schedule <- df$rotation * -1
#   
#   reaches <- rowMeans(df[,sprintf('p%03d',c(1:24))], na.rm = TRUE)
#   
#   par <- Reach::twoRateFit(schedule       = schedule,
#                            reaches        = reaches,
#                            checkStability = TRUE)
#   
#   fit <- Reach::twoRateModel(par=par, schedule=schedule)
#   
#   lines(fit$total, col=col.op, lty=3)
#   lines(fit$slow,  col=col.op, lty=2)
#   lines(fit$fast,  col=col.op, lty=1)
#   
#   
#   #legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

# previous figures -----

# fig1_Additivity <- function(target='inline') {
# 
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig1_additivity.svg', width=8, height=5, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig1_additivity.pdf', width=8, height=5)
#   }
#   
#   textsize <- 1.35
#   
#   layout(mat=matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE))
#   
#   groups <- getGroups()
#   
#   par(mar=c(3.5, 3.5, 0, 0.1))
#   
#   process <- c('implicit',  'explicit',  'adaptation')
#   col.op  <- c('#0000ffff', '#e51636ff', '#7f00d8ff' )
#   col.tr  <- c('#0000ff2f', '#e516362f', '#7f00d82f' )
#   cols <- data.frame(process, col.op, col.tr)
#   
#   # # # # # # # # # # # # # # # # # 
#   # A: rotation=45, adaptation=40, implicit=20, explicit=20
#   
#   plot(-1000, -1000,
#        main='', xlab='', ylab='', 
#        xlim=c(0,1), ylim=c(0,1), 
#        ax=F, bty='n', asp=1)
#   text(0,0.95,'A: additivity', font.main=1, cex=1.35*1.5, adj=0)
#   
#   angles_deg <- c('rotation'   = 45,
#                   'adaptation' = 40,
#                   'implicit'   = 20,
#                   'explicit'   = 20,
#                   'target'     = 15)
#   
#   drawAdditivitySchematic(angles_deg = angles_deg,
#                           cols = cols,
#                           addLabels = 1)
#   
#   lab_ang <- (angles_deg['target']/180)*pi
#   text(x=cos(lab_ang)-0.15,
#        y=sin(lab_ang)-0.1,
#        labels='visual target',
#        srt=angles_deg['target'])
#   
#   best_deg <- sum(angles_deg[c('target','rotation')])
#   best_rad <- (best_deg/180)*pi
#   text(x=cos(best_rad)-0.15,
#        y=sin(best_rad)-0.17,
#        labels='full compensation',
#        srt=best_deg,
#        col='gray')
#   
# 
#   # # # # # # # # # # # # # # # # # 
#   # B: rotation=45, adaptation=40, implicit=30, explicit=10
#   
#   plot(-1000, -1000,
#        main='', xlab='', ylab='', 
#        xlim=c(0,1), ylim=c(0,1), 
#        ax=F, bty='n', asp=1)
#   text(0,0.95,'B: less explicit', font.main=1, cex=1.35*1.5, adj=0)
#   #title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
#   #title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
# 
#   angles_deg <- c('rotation'   = 45,
#                   'adaptation' = 40,
#                   'implicit'   = 30,
#                   'explicit'   = 10,
#                   'target'     = 15)
#   
#   drawAdditivitySchematic(angles_deg = angles_deg,
#                           cols = cols,
#                           addLabels = 2)
#   
#   # # # # # # # # # # # # # # # # # 
#   # C: rotation=45, adaptation=40, implicit=10, explicit=30
#   
#   plot(-1000, -1000,
#        main='', xlab='', ylab='', 
#        xlim=c(0,1), ylim=c(0,1), 
#        ax=F, bty='n', asp=1)
#   text(0,0.95,'C: more explicit', font.main=1, cex=1.35*1.5, adj=0)
#   #title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
#   #title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
#   
#   angles_deg <- c('rotation'   = 45,
#                   'adaptation' = 40,
#                   'implicit'   = 10,
#                   'explicit'   = 30,
#                   'target'     = 15)
#   
#   drawAdditivitySchematic(angles_deg = angles_deg,
#                           cols = cols,
#                           addLabels = 2)
#   # # # # # # # # # # # # # # # # # 
#   # D: diagonal (slope:-1, intercept:24) explicit=c(4,12,20), implicit=c(20,12,4)
#   
#   plot(-1000, -1000,
#        main='', xlab='', ylab='', 
#        xlim=c(-5,50), ylim=c(-5,50), 
#        ax=F, bty='n', asp=1)
#   text(-5,47.25,'D: linearity', font.main=1, cex=1.35*1.5, adj=0)
#   title(xlab='explicit [°]', line = 2, cex.lab=textsize)
#   title(ylab='implicit [°]', line = 2, cex.lab=textsize)
#   
#   # standard deviation of noise in simulations:
#   std = 7
#   set.seed(1)
#   sims <- simulatedAdditivity(bootstraps=1000, N=24, normalize=FALSE, std=std)
#   df <- sims[['simulation']]
#   data <- sims[['data']]
#   
#   lines(x=c(0,0,45),y=c(45,0,0),col='gray')
#   
#   lines(x=c(0,10,10),
#         y=c(30,30,0),
#         col='gray',
#         lty=2)
#   lines(x=c(0,20,20),
#         y=c(20,20,0),
#         col='gray',
#         lty=2)
#   lines(x=c(0,30,30),
#         y=c(10,10,0),
#         col='gray',
#         lty=2)
#   
#   points(x=c(0,40),
#          y=c(40,0),
#          col=cols$col.op[which(cols$process %in% c('implicit','explicit'))],
#          pch=16,cex=1.5)
#   
#   lines(x=c(-2,42),y=c(42,-2),col='black')
#   
#   points(x=c(20,10,30),
#          y=c(20,30,10),
#          col=cols$col.op[which(cols$process == 'adaptation')],
#          pch=16,cex=1.5)
#   
#   text(x=c(20,10,30)+3,
#        y=c(20,30,10)+3,
#        col=cols$col.op[which(cols$process == 'adaptation')],
#        labels=c('A','B','C'),
#        cex=1.5)
#   
#   axis(side=1,at=c(0,10,20,30,40))
#   axis(side=2,at=c(0,10,20,30,40))
#   
#   
#   # # # # # # # # # # # # # # # # # 
#   # E: many different total adaptations, with slopes in 95% CI
#   
#   plot(-1000, -1000,
#        main='', xlab='', ylab='', 
#        xlim=c(-0.1,1.25), ylim=c(-0.1,1.25), 
#        ax=F, bty='n', asp=1)
#   text(-0.1,1.1825,'E: simulations', font.main=1, cex=1.35*1.5, adj=0)
#   # title(xlab='explicit [°]', line = 2, cex.lab=textsize)
#   # title(ylab='implicit [°]', line = 2, cex.lab=textsize)
#   
#   text(-0.1,1,
#        'generative model:',
#        cex=1.1,
#        adj=0)
#   stdstr <- sprintf('=%0.1f°)',std)
#   text(0,0.8,
#        expression(paste(epsilon, ' = N(', mu, '=0°, ', sigma, '=7°)') ),
#        cex=1.1,
#        adj=0)
#   
#   text(0,0.65,
#        expression(paste(A[p], ' = 40° + ', epsilon['a,p']) ),
#        cex=1.1,
#        adj=0)
#   text(0,0.5,
#        expression(paste(E[p], ' = 20° + ', epsilon['e,p']) ),
#        cex=1.1,
#        adj=0)
#   text(0,0.35,
#        expression(paste(I[p], ' = ', A[p], ' - ', E[p], ' + ', epsilon['i,p']) ),
#        cex=1.1,
#        adj=0)
#   
#   text(-0.1,0.15,
#        'slope recovery:',
#        cex=1.1,
#        adj=0)
#   
#   text(0,-0.05,
#        expression(paste(hat(I)[p], ' = ', beta[0], ' + ', beta[1], E[p]) ),
#        cex=1.2,
#        adj=0)
#   
#   # # # # # # # # # # # # # # # # # 
#   # F: many different total adaptations, with regression 95% CI
#   
#   plot(-1000, -1000,
#        main='', xlab='', ylab='', 
#        xlim=c(-5,50), ylim=c(-5,50), 
#        ax=F, bty='n', asp=1)
#   # text(-5,47.25,'E: simulations', font.main=1, cex=1.35*1.5, adj=0)
#   title(xlab='explicit [°]', line = 2, cex.lab=textsize)
#   title(ylab='implicit [°]', line = 2, cex.lab=textsize)
#   
#   lines(x=c(0,0,45),y=c(45,0,0),col='gray')
#   lines(x=c(-2,42),y=c(42,-2),col='black')
#   #points(x=c(0,40),y=c(40,0),col='black',pch=1,cex=1.5)
#   
#   
#   for (bs in c(1:dim(df)[1])) {
#     X <- df[bs,c('exp_min','exp_max')]
#     if (df$include1[bs]) {col='#6666ff0f'} else {col='#ff66662f'}
#     lines(x=X,
#           y=df$intercept[bs] + (X*df$slope[bs]),
#           col=col)
#   }
#   
#   lines(x=c(5,35),
#         y=c(35,5),
#         lty=3,
#         col='black')
#   
#   axis(side=1,at=c(0,45))
#   axis(side=2,at=c(0,45))
#   
#   
#   # # # # # # # # # # # # # # # # # # 
#   # # F: implicit & explicit normalized by adaptation
#   # 
#   # plot(-1000, -1000,
#   #      main='', xlab='', ylab='', 
#   #      xlim=c(-0.1,1.25), ylim=c(-0.1,1.25), 
#   #      ax=F, bty='n', asp=1)
#   # text(-0.1,1.1825,'F: normalization', font.main=1, cex=1.35*1.5, adj=0)
#   # title(xlab='explicit/adaptation', line = 2, cex.lab=textsize)
#   # title(ylab='implicit/adaptation', line = 2, cex.lab=textsize)
#   # 
#   # lines(x=c(0,0,1.1),y=c(1.1,0,0),col='gray')
#   # lines(x=c(-0.05,1.05),y=c(1.05,-0.05),col='black')
#   # points(x=c(0,1),y=c(1,0),col='black',pch=1,cex=1.5)
#   # 
#   # 
#   # points(x=data[['explicit']][1,]/40,
#   #        y=data[['implicit']][1,]/40,
#   #        pch=1, cex=1.5,
#   #        col='gray')
#   # points(x=data[['explicit']][1,]/data[['adaptation']][1,],
#   #        y=data[['implicit']][1,]/data[['adaptation']][1,],
#   #        pch=1, cex=1.5,
#   #        col='blue')
#   # 
#   # axis(side=1,at=c(0,1))
#   # axis(side=2,at=c(0,1))
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }

# fig3_Learning_PointEstimates <- function(target='inline') {
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig3_learning.svg', width=8, height=6, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig3_learning.pdf', width=8, height=6)
#   }
#   
#   textsize <- 1.35
#   
#   # we will plot all data by trial
#   # and illustrate the paradigm at the same time
#   
#   # groups:
#   # non-instructed: orange
#   # instructed: red
#   # aiming: purple & pink
#   
#   # no-cursors: gray & blue?
#   
#   # plot conditions (no cursors & rotations)
#   
#   layout(mat=matrix(c(1,1,2,3,4,5),nrow=2,ncol=3,byrow=TRUE))
#   
#   par(mar=c(2,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-7,40), ax=F, bty='n')
#   text(0,35,'A: adaptation and re-aiming', font.main=1, cex=1.35*1.5, adj=0)
#   title(xlab='time: trials per block', line = 0.5,cex.lab=textsize)
#   title(ylab='reach/aim deviation [°]', line = 2.5,cex.lab=textsize)
#   
#   plotBlocks(textsize = textsize)
#   
#   groups <- getGroups()
#   
#   # plot learning data:
#   
#   blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
#   
#   groupnames <- c()
#   groupcols <- c()
#   
#   for (group in c('control', 'instructed', 'aiming')) {
#     
#     df <- read.csv(sprintf('data/%s-training-trials.csv', group), stringsAsFactors = F)
#     
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     groupnames <- c(groupnames, as.character(groups$label[which(groups$group == group)]))
#     groupcols <- c(groupcols, col.op)
#     
#     for (block in blocks) {
#       
#       CI.lo <- df$CI.lo[which(df$trial %in% block)]
#       CI.hi <- df$CI.hi[which(df$trial %in% block)]
#       average <- df$average[which(df$trial %in% block)]
#       
#       polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#       lines(block, average, col=col.op)
#       
#     }
#     
#   }
#   
#   
#   # plot aiming
#   
#   
#   group <- 'aims'
#   
#   df <- read.csv('data/aiming-aim-trials.csv', stringsAsFactors = F)
#   
#   col.op <- as.character(groups$col.op[which(groups$group == group)])
#   col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#   
#   groupnames <- c(groupnames, as.character(groups$label[which(groups$group == group)]))
#   groupcols  <- c(groupcols, col.op)
#   
#   for (block in blocks) {
#     
#     CI.lo <- df$CI.lo[which(df$trial %in% block)]
#     CI.hi <- df$CI.hi[which(df$trial %in% block)]
#     average <- df$average[which(df$trial %in% block)]
#     
#     polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
#     lines(block, average, col=col.op)
#     
#   }
#   
#   legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   
#   axis(side=2, at=c(0,15,30),cex.axis=textsize)
#   
#   
#   par(mar=c(2,0,0,0.1))
#   
#   
#   # # # # # # # # # 
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,25), ylim=c(-7,40), ax=F, bty='n')
#   #title(ylab='reach deviation [°]', line=2.5)
#   text(0,35,'B: average measures', font.main=1, cex=1.35*1.5, adj=0)
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
#     exclude <- as.numeric(df['exclude',pp] - df['none',pp])
#     include <- as.numeric(df['include',pp] - df['none',pp])
#     
#     # EXCLUDE:
#     CI <- Reach::getConfidenceInterval(exclude, method='b')
#     avg <- mean(exclude)
#     xoffset <- (groupno*2) + 6
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(exclude)), exclude, pch=16, col=col.tr)
#     
#     #text(x=(groupno*8)-6.5,y=-1,labels='exclude', adj=c(0,.5), srt=-45, cex=textsize)
#     
#     # INCLUDE:
#     CI <- Reach::getConfidenceInterval(include, method='b')
#     avg <- mean(include)
#     xoffset <- (groupno*2) + 13
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(include)), include, pch=16, col=col.tr)
#     
#     
#     #text(x=(groupno*8)-4.5,y=-1,labels='include', adj=c(0,.5), srt=-45, cex=textsize)
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
#     xoffset <- (groupno*2) - 1
#     polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#     # replace with lines?
#     points(x=rep(xoffset+1, length(adaptation)), adaptation, pch=16, col=col.tr)
#     
#     #text(x=(groupno*8)-1.5,y=-1,labels='adaptation', adj=c(0,.5), srt=-45, cex=textsize)
#     print(xoffset)
#   }
#   
#   df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
#   aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
#                         colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
#   groupno <- 3
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   CI <- Reach::getConfidenceInterval(aims, method='b')
#   avg <- mean(aims)
#   #xoffset <- (groupno*2) - 1
#   xoffset <- 22
#   polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col='#FFFFFF00', border=col.tr)
#   lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
#   # replace with lines?
#   points(x=rep(xoffset+1, length(aims)), aims, pch=1, col=col.tr)
#   
#   grouplabels <- c(grouplabels, 'aims')
#   groupcols <- c(groupcols, col.op)
#   
#   
#   # text(x = 10.5, y=-6, labels='exclude',    cex=textsize)
#   # text(x =  3.5, y=-6, labels='adaptation', cex=textsize)
#   # text(x = 17.5, y=-6, labels='include',    cex=textsize)
#   # text(x = 22.5, y=-6, labels='aims',       cex=textsize)
#   
#   text( x = c(3.5, 10.5, 17.5, 22.5), 
#         y = rep(-5, 4),
#         labels = c('adaptation', 'exclude', 'include', 'aims'),
#         adj=c(0.5,1),
#         cex=textsize
#         )
#   
#   #legend(0,15,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
#   
#   #axis(side=2, at=c(0,15,30))
#   
#   
#   # # # # # # # # # # # # # # 
#   # SECOND ROW OF PLOT
#   
#   #textsize <- 1.5
#   
#   
#   par(mar=c(4.5,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-7.5,27.5), ylim=c(-5,30), ax=F, bty='n',asp=1)
#   text(-7.5,30,'C: explicit measures', font.main=1, cex=1.35*1.5, adj=0)
#   title(xlab='aims [°]', line=2.5, cex.lab=textsize)
#   title(ylab='difference score [°]', line=2.5, cex.lab=textsize)
#   
#   df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
#   pp <- sprintf('p%03d',c(1:24))
#   
#   adf <- getAdditivityData()
#   
#   idx <- which(adf$group == 'aiming')
#   explicit <- adf[idx,'include'] - adf[idx,'exclude']
#   
#   groups <- getGroups()
#   col.op <- as.character(groups$col.op[which(groups$group == 'aiming')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aiming')])
#   
#   
#   aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
#                         colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
#   
#   at <- range(aims)
#   
#   at <- c(-2,20)
#   
#   points(aims,explicit,pch=16,col=col.tr)
#   lines(at,at,col='#666666',lty=2)
#   
#   #print(cor.test(aims,explicit))
#   
#   A2R <- lm(explicit ~ aims)
#   
#   coef <- A2R$coefficients
#   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   
#   
#   ci <- predict( A2R,
#                  newdata=data.frame(aims=seq(-2,20,.2)),
#                  interval = "confidence")
#   
#   X <- c(seq(-2,20,.2),rev(seq(-2,20,.2)))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   
#   axis(side=1, at=c(0,10,20), cex.axis=textsize)
#   axis(side=2, at=c(0,10,20), cex.axis=textsize)
#   
#   # # # # # # # # # 3 # # # # # #
#   # STRICT ADDITIVITY
#   
#   grouplabels <- c()
#   groupcols <- c()
#   
#   adf <- getAdditivityData()
#   
#   total <- mean(adf$adaptation)
#   at <- c(-10,10+total) 
#   
#   groups <- getGroups()
#   groupnames <- c('control', 'instructed', 'aiming')
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-10,45), ylim=c(-10,45), ax=F, bty='n', asp=1)
#   text(-10,45,'D: strict additivity', font.main=1, cex=1.35*1.5, adj=0)
#   lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
#   lines(x=c(0,40),y=c(0,0),col='#999999', lw=1, lty=2)
#   lines(x=c(0,0),y=c(0,40),col='#999999', lw=1, lty=2)
#   
#   points(c(0,total),c(total,0),col='#000000')
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     groupname <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#     
#     grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == groupname)]))
#     groupcols <- c(groupcols, col.op)
#     
#     idx <- which(adf$group == groupname)
#     excl <- adf[idx,'exclude']
#     incl <- adf[idx,'include']
#     adapt <- adf[idx,'adaptation']
#     
#     expl <- incl - excl
#     
#     # correct for individual variation in total adaptation:
#     err <- mean(adapt) - adapt
#     excl <- excl - err
#     
#     at <- range(expl)
#     
#     #e2i <- lm(excl ~ e_p_n)
#     e2i <- lm(excl ~ expl)
#     
#     
#     cat(sprintf('%s:\n',toupper(groupname)))
#     #print(summary(e2i))
#     
#     coef <- e2i$coefficients
#     lines(at, coef[1]+(at*coef[2]), col=col.op)
#     
#     
#     ci <- predict( e2i,
#                    newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
#     
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
#     
#     points(expl, excl, pch=16, col=col.tr)
#     
#     print(confint(e2i,parm='expl',level=0.95))
#     print(confint(e2i,parm='(Intercept)',level=0.95))
#     
#   }
#   
#   #print(mean(adf$adaptation))
#   
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   
#   grouplabels <- c(grouplabels, 'aims')
#   groupcols <- c(groupcols, col.op)
#   
#   idx <- which(adf$group == 'aiming')
#   expl <- adf$aiming[idx]
#   impl <- adf$exclude[idx]
#   adapt <- adf$adaptation[idx]
#   
#   at <- range(expl)
#   
#   e2i <- lm(impl ~ expl)
#   
#   
#   cat(sprintf('%s:\n',toupper('aims')))
#   print(confint(e2i,parm='expl',level=0.95))
#   print(confint(e2i,parm='(Intercept)',level=0.95))
#   #print(summary(I2A))
#   
#   coef <- e2i$coefficients
#   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   
#   
#   ci <- predict( e2i,
#                  newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#                  interval = "confidence")
#   
#   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   points(expl, impl, pch=16, col=col.tr)
#   
#   
#   #title(main='strict additivity')
#   
#   title(ylab='implicit measure [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(-10,15,40), cex.axis=textsize)
#   
#   title(xlab='explicit measure [°]', line=2.5, cex.lab=textsize)
#   axis(side=1, at=c(-10,15,40), cex.axis=textsize)
#   
#   legend(1,45,
#          legend=c(grouplabels[1:2],'aiming (difference)','aiming (report)'),
#          col=groupcols, 
#          bty='n', lty=1, cex=0.8*1.5)
#   
#   # # # # # # # # # # # 
#   # LOOSE ADDITIVITY
#   # 
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,45), ylim=c(0,45), ax=F, bty='n', asp=1)
#   text(0,45,'E: loose additivity', font.main=1, cex=1.35*1.5, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,40),y=c(0,40),col="#999999", lw=1, lty=1)
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     groupname <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#     
#     idx <- which(adf$group == groupname)
#     
#     incl  <- adf$include[idx]
#     impl  <- adf$exclude[idx]
#     expl  <- incl - excl
#     adapt <- adf$adaptation[idx]
#     
#     
#     
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
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   
#   idx <- which(adf$group == 'aiming')
#   expl <- adf$aiming[idx]
#   impl <- adf$exclude[idx]
#   adapt <- adf$adaptation[idx]
#   
#   
#   EIadd <- lm(adapt ~ impl + expl + 0)
#   
#   cat(sprintf('%s:\n',toupper('aims')))
#   #print(summary(EIadd))
#   
#   coef <- EIadd$coefficients
#   
#   #print(coef)
#   
#   # plot actual adaptation over predicted values:
#   
#   predictions <- predict(EIadd)
#   at <- range(predictions)
#   p2a <- lm(adapt ~ predictions)
#   #print(summary(p2a))
#   pcoef <- p2a$coefficients
#   print(confint(p2a,parm='predictions',level=0.95))
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
#   legend(12,15,
#          legend=c(grouplabels[1:2],'aiming (Inc-Exc)','aiming (reports)'),
#          col=groupcols, 
#          bty='n', lty=1, cex=0.8*1.5)
#   
#   #title(main='loose additivity')
#   
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,20,40), cex.axis=textsize)
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,15,30,45), cex.axis=textsize)
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }

# old_let2_PointEstimateAdditivity <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/letter_Fig2_pointestimates.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/letter_Fig2_pointestimates.pdf', width=8, height=3)
#   }
#   
#   textsize <- 1.5
#   
#   
#   layout(matrix(c(1,2,3), nrow=1, ncol=3))
#   par(mar=c(4.5,4,0,0.1))
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-7.5,27.5), ylim=c(-5,30), ax=F, bty='n',asp=1)
#   text(-7.5,30,'A: explicit measures', font.main=1, cex=1.35*1.5, adj=0)
#   title(xlab='aims [°]', line=2.5, cex.lab=textsize)
#   title(ylab='difference score [°]', line=2.5, cex.lab=textsize)
#   
#   df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
#   pp <- sprintf('p%03d',c(1:24))
#   
#   adf <- getAdditivityData()
#   
#   idx <- which(adf$group == 'aiming')
#   explicit <- adf[idx,'include'] - adf[idx,'exclude']
#   
#   groups <- getGroups()
#   col.op <- as.character(groups$col.op[which(groups$group == 'aiming')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aiming')])
#     
#   
#   aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
#                         colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
#   
#   at <- range(aims)
#   
#   at <- c(-2,20)
#   
#   points(aims,explicit,pch=16,col=col.tr)
#   lines(at,at,col='#666666',lty=2)
#   
#   #print(cor.test(aims,explicit))
#   
#   A2R <- lm(explicit ~ aims)
#   
#   coef <- A2R$coefficients
#   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   
#   
#   ci <- predict( A2R,
#                  newdata=data.frame(aims=seq(-2,20,.2)),
#                  interval = "confidence")
#   
#   X <- c(seq(-2,20,.2),rev(seq(-2,20,.2)))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   
#   axis(side=1, at=c(0,10,20), cex.axis=textsize)
#   axis(side=2, at=c(0,10,20), cex.axis=textsize)
#   
#   # # # # # # # # # 3 # # # # # #
#   # STRICT ADDITIVITY
#   
#   grouplabels <- c()
#   groupcols <- c()
#   
#   adf <- getAdditivityData()
#   
#   total <- mean(adf$adaptation)
#   at <- c(-10,10+total) 
#   
#   groups <- getGroups()
#   groupnames <- c('control', 'instructed', 'aiming')
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-10,45), ylim=c(-10,45), ax=F, bty='n', asp=1)
#   text(-10,45,'B: strict additivity', font.main=1, cex=1.35*1.5, adj=0)
#   lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
#   lines(x=c(0,40),y=c(0,0),col='#999999', lw=1, lty=2)
#   lines(x=c(0,0),y=c(0,40),col='#999999', lw=1, lty=2)
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     groupname <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#     
#     grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == groupname)]))
#     groupcols <- c(groupcols, col.op)
#     
#     idx <- which(adf$group == groupname)
#     excl <- adf[idx,'exclude']
#     incl <- adf[idx,'include']
#     adapt <- adf[idx,'adaptation']
#     
#     expl <- incl - excl
#     
#     at <- range(expl)
#     
#     e2i <- lm(excl ~ expl)
#     
#     
#     cat(sprintf('%s:\n',toupper(groupname)))
#     #print(summary(I2A))
#     
#     coef <- e2i$coefficients
#     lines(at, coef[1]+(at*coef[2]), col=col.op)
#     
#     
#     ci <- predict( e2i,
#                    newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#                    interval = "confidence")
#     
#     X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#     Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#     polygon(x=X,y=Y,col=col.tr,border=NA)
#     
#     points(expl, excl, pch=16, col=col.tr)
#     
#     print(confint(e2i,parm='expl',level=0.95))
#     print(confint(e2i,parm='(Intercept)',level=0.95))
#     
#   }
#   
#   #print(mean(adf$adaptation))
#   
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   
#   grouplabels <- c(grouplabels, 'aims')
#   groupcols <- c(groupcols, col.op)
#   
#   idx <- which(adf$group == 'aiming')
#   expl <- adf$aiming[idx]
#   impl <- adf$exclude[idx]
#   adapt <- adf$adaptation[idx]
#   
#   at <- range(expl)
#   
#   e2i <- lm(impl ~ expl)
#   
#   
#   cat(sprintf('%s:\n',toupper('aims')))
#   print(confint(e2i,parm='expl',level=0.95))
#   print(confint(e2i,parm='(Intercept)',level=0.95))
#   #print(summary(I2A))
#   
#   coef <- e2i$coefficients
#   lines(at, coef[1]+(at*coef[2]), col=col.op)
#   
#   
#   ci <- predict( e2i,
#                  newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
#                  interval = "confidence")
#   
#   X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   points(expl, impl, pch=16, col=col.tr)
#   
#   
#   #title(main='strict additivity')
#   
#   title(ylab='implicit measure [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(-10,15,40), cex.axis=textsize)
#   
#   title(xlab='explicit measure [°]', line=2.5, cex.lab=textsize)
#   axis(side=1, at=c(-10,15,40), cex.axis=textsize)
#   
#   legend(15,40,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=0.8*1.5)
#   
#   # # # # # # # # # # # 
#   # LOOSE ADDITIVITY
#   # 
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,45), ylim=c(0,45), ax=F, bty='n', asp=1)
#   text(0,45,'C: loose additivity', font.main=1, cex=1.35*1.5, adj=0)
#   #lines(x=c(5,45),y=c(5,45),col="#999999", lw=1, lty=1)
#   lines(x=c(0,40),y=c(0,40),col="#999999", lw=1, lty=1)
#   
#   for (groupno in c(1:length(groupnames))) {
#     
#     groupname <- groupnames[groupno]
#     
#     col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#     
#     idx <- which(adf$group == groupname)
#     
#     incl  <- adf$include[idx]
#     impl  <- adf$exclude[idx]
#     expl  <- incl - excl
#     adapt <- adf$adaptation[idx]
#     
#     
#     
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
#   col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
#   col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
#   
#   idx <- which(adf$group == 'aiming')
#   expl <- adf$aiming[idx]
#   impl <- adf$exclude[idx]
#   adapt <- adf$adaptation[idx]
#   
#   
#   EIadd <- lm(adapt ~ impl + expl + 0)
#   
#   cat(sprintf('%s:\n',toupper('aims')))
#   #print(summary(EIadd))
#   
#   coef <- EIadd$coefficients
#   
#   #print(coef)
#   
#   # plot actual adaptation over predicted values:
#   
#   predictions <- predict(EIadd)
#   at <- range(predictions)
#   p2a <- lm(adapt ~ predictions)
#   #print(summary(p2a))
#   pcoef <- p2a$coefficients
#   print(confint(p2a,parm='predictions',level=0.95))
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
#   
#   #title(main='loose additivity')
#   
#   title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
#   axis(side=2, at=c(0,20,40), cex.axis=textsize)
#   
#   title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
#   axis(side=1, at=c(0,15,30,45), cex.axis=textsize)
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }



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


# fig4_Awareness_TwoRate <- function(target='inline') {
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig4_tworate.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig4_tworate.pdf', width=8, height=3)
#   }
#   
#   textsize <- 0.8
#   
#   layout(mat=matrix(c(1,2),nrow=1,ncol=2),widths=c(1,2))
#   
#   par(mar=c(2,4,0,0.1))
#   
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,26), ylim=c(-7,40), ax=F, bty='n')
#   text(0,35,'A: awareness', font.main=1, cex=1.35, adj=0)
#   title(ylab='difference score [°]', line = 2.5)
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
#     df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
#     row.names(df) <- df$condition
#     
#     col.op <- as.character(groups$col.op[which(groups$group == group)])
#     col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
#     groupcols <- c(groupcols, col.op)
#     
#     explicit <- as.numeric(df['include',pp] - df['exclude',pp])
#     CI <- Reach::getConfidenceInterval(explicit, method='b')
#     avg <- mean(explicit)
#     polygon(x=c(0,1,1,0)+(groupno*4)-2, y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
#     lines(x=c(0,1)+(groupno*4)-2, y=rep(avg,2), col=col.op)
#     
#     text(0.4+(groupno*4)-2,-8.5, group, srt=90, adj=c(0,0.5), cex=0.8)
#     
#     points(x=rep((groupno*4), length(explicit)), explicit, pch=16, col=col.tr)
#     
#     dX <- density(explicit, n=81, from=-10, to=30, bw=2.5)$y
#     dX <- (dX / sum(dX)) * 150
#     dY <- seq(-10,30,.5)
#     
#     
#     polygon(x=c(0,dX,0)+18, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=col.tr)
#     
#     lines(dX+18, dY, col=col.op)
#     
#   }
#   
#   data <- data.frame('explicit'=seq(-10,30,.5))
#   bimodal <- fitModel()
#   iM <- bimodal[['par']]['iM']
#   iS <- bimodal[['par']]['iS']
#   eM <- bimodal[['par']]['eM']
#   eS <- bimodal[['par']]['eS']
#   f  <- bimodal[['par']]['f']
#   iL <- dnorm(data$explicit, mean=iM, sd=iS)
#   eL <- dnorm(data$explicit, mean=eM, sd=eS)
#   prob_dens <- (f*iL) + ((1-f)*eL)
#   
#   dX <- (prob_dens / sum(prob_dens)) * 150
#   
#   #lines(dX+18, dY, col="#001388FF", lty=2)
#   lines(dX+18, dY, col="#000000FF", lty=2)
#   
#   text(16, iM, 'unaware', srt=90, cex=0.8)
#   text(16, eM, 'aware', srt=90, cex=0.8)
#   
#   dataM <- data.frame('explicit'=c(iM, eM))
#   iLM <- dnorm(dataM$explicit, mean=iM, sd=iS)
#   eLM <- dnorm(dataM$explicit, mean=eM, sd=eS)
#   prob_dens_M <- (f*iLM) + ((1-f)*eLM)
#   
#   # lines(c(18,18+((prob_dens_M[1]/sum(prob_dens))*150)),c(iM,iM), col="#001388FF", lty=1)
#   # lines(c(18,18+((prob_dens_M[2]/sum(prob_dens))*150)),c(eM,eM), col="#001388FF", lty=1)
#   lines(c(18,18+((prob_dens_M[1]/sum(prob_dens))*150)),c(iM,iM), col="#000000FF", lty=1)
#   lines(c(18,18+((prob_dens_M[2]/sum(prob_dens))*150)),c(eM,eM), col="#000000FF", lty=1)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   
#   
#   
#   
#   
#   
#   plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-7,40), ax=F, bty='n')
#   text(0,35,'B: state-space model', font.main=1, cex=1.35, adj=0)
#   title(xlab='time: trials per block', line = 0.5)
#   title(ylab='deviation [°]', line = 2.5)
#   
#   plotBlocks(textsize=textsize)
#   
#   
#   # group the participants in 2 sets:
#   df <- getExplicitData() 
#   
#   iM <- median(df$explicit[which(df$group=='control')])
#   eM <- median(df$explicit[which(df$group=='instructed')])
#   iS <-     sd(df$explicit[which(df$group=='control')])
#   eS <-     sd(df$explicit[which(df$group=='instructed')])
#   
#   iPD <- dnorm(df$explicit[which(df$group=='aiming')], mean=iM, sd=iS)
#   ePD <- dnorm(df$explicit[which(df$group=='aiming')], mean=eM, sd=eS)
#   
#   participant <- sprintf('p%03d',c(1:24))
#   strategy <- ePD > iPD
#   split_aim <- data.frame(participant,strategy)
#   split_aim$ppno <- c(1:24)
#   
#   groups <- getGroups()
#   groupnames <- c()
#   groupcols <- c()
#   df <- read.csv('data/aiming-aim-trials.csv', stringsAsFactors = F)
#   
#   for (strategy in c(FALSE, TRUE)) {
#     
#     sdf <- df[,as.character(split_aim$participant[which(split_aim$strategy == strategy)])]
#     
#     #print(str(sdf))
#     
#     if (strategy) {
#       group <- 'aiming'
#       groupname <- 'aware aimers (N=9)'
#       col.op.expl <- as.character(groups$col.op[which(groups$group == 'aiming')])
#       col.op.impl <- as.character(groups$col.op[which(groups$group == 'aims')])
#     } else {
#       group <- 'aims'
#       groupname <- 'unaware aimers (N=15)'
#       col.op.expl <- '#0047AB'
#       col.op.impl <- '#00d0d0'
#       
#       # azure blue: #007FFF
#       # turquoise:  #00FFEF
#       # cyan:       #00B7EB # implicit
#       # cobalt:     #0047AB # explicit
#       # 
#       # 
#     }
#     #col.op <- as.character(groups$col.op[which(groups$group == group)])
# 
#     # not using transparent colors right now:
#     #col.tr <- as.character(groups$col.tr[which(groups$group == group)])
#     
#     groupnames <- c(groupnames, groupname)
#     groupcols <- c(groupcols, col.op.expl)
#     
#     blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
#     
#     for (block in blocks) {
#       
#       block <- unlist(block)
#       trial_idx <- which(df$trial %in% block)
#       bdf <- sdf[trial_idx,]
#       
#       CI <- apply(bdf, MARGIN=c(1), Reach::getConfidenceInterval, method='b')
#       CI.lo <- as.numeric(CI[1,])
#       CI.hi <- as.numeric(CI[2,])
#       average <- rowMeans(bdf, na.rm=TRUE)
#       
#       polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=t_col(col.op.expl, percent = 80), border=NA)
#       lines(block, average, col=col.op.expl)
#       
#     }
#     
#   }
#   
#   
#   # now we plot the exlude reach deviations for both sub-groups:
#   
#   df <- read.csv('data/aiming.csv', stringsAsFactors = F)
#   
#   for (strategy in c(FALSE, TRUE)) {
#     
#     sdf <- df[which(df$participant %in% split_aim$ppno[which(split_aim$strategy == strategy)]),]
#     
#     if (strategy) {
#       group <- 'aiming'
#       groupname <- 'aware aimers (N=9)'
#       col.op.expl <- as.character(groups$col.op[which(groups$group == 'aiming')])
#       col.op.impl <- as.character(groups$col.op[which(groups$group == 'aims')])
#     } else {
#       group <- 'aims'
#       groupname <- 'unaware aimers (N=15)'
#       col.op.expl <- '#0047AB'
#       col.op.impl <- '#00d0d0'
#     }
#     
#     baseline <- aggregate(reachdeviation_deg ~ participant, data=sdf[which(sdf$cursor == FALSE & sdf$strategy == 'none'),], FUN=mean, na.rm=TRUE)
#     
#     blocks <- list(seq(185,192), seq(193,200), seq(217,224), seq(225,232), seq(249,256), seq(257,264))  
#     
#     exclude <- sdf[which(sdf$trial %in% unlist(blocks) & sdf$strategy == 'exclude'),]
#     
#     for (participant in baseline$participant) {
#       idx <- which(exclude$participant == participant)
#       exclude$reachdeviation_deg[idx] <- exclude$reachdeviation_deg[idx] - baseline$reachdeviation_deg[which(baseline$participant == participant)]
#     }
#     
#     for (block in blocks) {
#       
#       block <- unlist(block)
#       
#       CI.lo <- c()
#       CI.hi <- c()
#       average <- c()
#       
#       for (trial in unlist(block)) {
#         reachdevs <- exclude$reachdeviation_deg[which(exclude$trial == trial)]
#         CI <- Reach::getConfidenceInterval(reachdevs, method='b')
#         CI.lo <- c(CI.lo, unlist(CI[1]))
#         CI.hi <- c(CI.hi, unlist(CI[2]))
#         average <- c(average, mean(reachdevs, na.rm=TRUE))
#       }
#       
#       polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=t_col(col.op.impl,percent = 80), border=NA)
#       lines(block, average, col=col.op.impl)
#       
#     }  
#     
#   }
#   
#   # we fit the two-rate model to mean reach deviations in both sub-groups
#   # is the fast process equal to aiming responses?
#   
#   df <- get2rateData(group='aiming')
#   schedule <- df$rotation * -1
#   
#   
#   for (strategy in c(FALSE, TRUE)) {
#     
#     if (strategy) {
#       group <- 'aiming'
#       groupname <- 'aware aimers (N=9)'
#       col.op <- as.character(groups$col.op[which(groups$group == 'aiming')])
#       #col.op.impl <- as.character(groups$col.op[which(groups$group == 'aims')])
#     } else {
#       group <- 'aims'
#       groupname <- 'unaware aimers (N=15)'
#       col.op <- '#0047AB'
#       #col.op.impl <- '#00d0d0'
#     }
#     
#     sdf <- df[,as.character(split_aim$participant[which(split_aim$strategy == strategy)])]
#     reaches <- rowMeans(sdf, na.rm = TRUE)
#     
#     par <- Reach::twoRateFit(schedule       = schedule,
#                              reaches        = reaches,
#                              checkStability = TRUE)
#     
#     fit <- Reach::twoRateModel(par=par, schedule=schedule)
#     
#     lines(fit$total, col=col.op, lty=3)
#     lines(fit$slow,  col=col.op, lty=2)
#     lines(fit$fast,  col=col.op, lty=1)
#     
#     
#   }
#   
#   groupnames <- c(groupnames, 'model output', 'slow', 'fast / aims', 'no-cursor blocks')
#   groupcols  <- c(groupcols,  '#000000', '#000000', '#000000', '#BBBBBB')  
#   lty        <- c(1,1,3,2,1,0)
#   lwd        <- c(1,1,1,1,1,0)
#   pch        <- c(NA,NA,NA,NA,NA,15)
#   
#   legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=lty, lwd=lwd, pch=pch, cex=textsize, pt.cex=1.5)
#   
#   axis(side=2, at=c(0,15,30))
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
#   
# }

# fig5_ExtraData <- function(target='inline') {
#   
#   if (target == 'svg') {
#     svglite::svglite(file='doc/Fig5_extradata.svg', width=4, height=6, fix_text_size = FALSE)
#   }
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig5_extradata.pdf', width=4, height=6)
#   }
#   
#   textsize <- 0.8
#   
#   lend <- 1
#   paper.cex <- 0.65
#   group.cex <- 0.5
#   
#   #layout(matrix(c(1,2,3,1,4,5), nrow=2, ncol=3, byrow=TRUE), width=c(3,2,2))
#   par(mar=c(3,0,0,0.1))
#   
#   #datasets <- getExtraData()
#   datasets <- getAllExtraData(methods=c('PDP.difference'))
#   datasets <- getAllExtraData(methods=c('aim.reports'))
#   ndatasets <- length(names(datasets))
#   ngroups   <- 0
#   for (dataset in datasets) {
#     ngroups <- ngroups + length(dataset[['labels']])
#   }
#   
#   print(ndatasets + ngroups)
#   
#   row <- ndatasets + ngroups + 2
#   
#   plot(-1000,-1000,
#        main='',xlab='',ylab='',
#        xlim=c(-2.9,2.9),ylim=c(-1,row),
#        bty='n',ax=F)
#   
#   title(xlab='slope',line=2,cex.lab=textsize)
#   
#   for (xpos in c(-2.5,2.5)) {
#     lines(c(xpos,xpos),c(0.5,row-.5),lty=1,col='#000000')
#   }
#   for (xpos in c(-1.5,1.5)) {
#     lines(c(xpos,xpos),c(0.5,row-.5),lty=3,col='#999999')
#   }
#   
#   
#   axis(side=1, at=c(-2.5,-1.5),labels=c('-1','0'), cex.axis=textsize)
#   text(-2.65,.5,'strict additivity',srt=90,adj=c(0,0.5), cex=textsize)
#   axis(side=1, at=c(1.5,2.5),labels=c('0','1'), cex.axis=textsize)
#   text(2.65,.5,'loose additivity',srt=90,adj=c(0,0.5), cex=textsize)
#   
#   # for combined estimates, we need to collect normalized data
#   # we will normalize both by the rotation and by average adaptation
#   adaptation.rotnorm <- c()
#   adaptation.avgnorm <- c()
#   explicit.rotnorm   <- c()
#   explicit.avgnorm   <- c()
#   implicit.rotnorm   <- c()
#   implicit.avgnorm   <- c()
#   
#   for (dataset in datasets) {
#     
#     paper  <- dataset[['paper']]
#     df     <- dataset[['data']]
#     labels <- dataset[['labels']]
#     
#     if (!('implicit' %in% names(df))) {
#       df$implicit <- df$exclude
#     }
#     
#     row    <- row-1
#     text(0,row,paper,cex=paper.cex,font=2)
#     
#     groups <- unique(df$group)
#     
#     for (group in groups) {
#       
#       row <- row-1
#       label <- labels[group]
#       text(0,row,label,cex=group.cex)
#       
#       # select data for the group:
#       gdf <- df[which(df$group == group),]
#       
#       #cat(sprintf('%s N=%d\n',label,dim(gdf)[1]))
#       
#       # extract relevant columns:
#       implicit   <- gdf$implicit
#       explicit   <- gdf$explicit
#       adaptation <- gdf$adaptation
#       
#       slopes <- getAdditivitySlopes(implicit = implicit,
#                                     explicit = explicit,
#                                     adaptation = adaptation)
#       
#       #print(slopes)
#       
#       for (assumption in c('strict','loose')) {
#         model <- slopes[[assumption]]
#         offset <- c('strict'=-1.5, 'loose'=1.5)[assumption]
#         # plot the slope and it's confidence interval:
#         lines(x=model$slope_ci+offset, y=rep(row,2), lw=6, col=model$colors$tr, lend=lend)
#         points(x=model$slope+offset,y=row,pch=1,col=model$colors$op)
#       }
#       
#       # # plot the slope of the line and it's confidence interval:
#       # lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
#       # points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
#       
#       # print(label)
#       if (!(label %in% c('stepwise, 30°','stepwise, 45°','stepwise, 60°'))) {
#         adaptation.rotnorm <- c(adaptation.rotnorm, adaptation / gdf$rotation)
#         adaptation.avgnorm <- c(adaptation.avgnorm, adaptation / mean(adaptation))
#         explicit.rotnorm   <- c(explicit.rotnorm,   explicit   / gdf$rotation)
#         explicit.avgnorm   <- c(explicit.avgnorm,   explicit   / mean(adaptation))
#         implicit.rotnorm   <- c(implicit.rotnorm,   implicit   / gdf$rotation)
#         implicit.avgnorm   <- c(implicit.avgnorm,   implicit   / mean(adaptation))
#       }
#       
#     }
#     
#   }
#   
#   totalN <- length(adaptation.rotnorm)
#   print(totalN)
#   
#   row    <- row-1
#   text(0,row,sprintf('all data (N=%d)',totalN),cex=paper.cex,font=2)
#   
#   row    <- row-1
#   text(0,row,sprintf('rotation-normalized'),cex=group.cex)
#   
#   slopes <- getAdditivitySlopes(implicit = implicit.rotnorm,
#                                 explicit = explicit.rotnorm,
#                                 adaptation = adaptation.rotnorm)
#   
#   # plot the slope and it's confidence interval:
#   lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
#   points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
#   
#   # plot the slope of the line and it's confidence interval:
#   lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
#   points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
#   
#   
#   row    <- row-1
#   
#   text(0,row,sprintf('adaptation-normalized'),cex=group.cex)
#   slopes <- getAdditivitySlopes(implicit = implicit.avgnorm,
#                                 explicit = explicit.avgnorm,
#                                 adaptation = adaptation.avgnorm)
#   
#   # plot the slope and it's confidence interval:
#   lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
#   points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
#   
#   # plot the slope of the line and it's confidence interval:
#   lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
#   points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
#   
#   #legend(-2.5, y=0, 
#   #       legend=c('supports additivity', 'supports combining', 'supports zero-sum', 'supports subtractivity', 'unclear'),
#   #       col = slopes$colors.tr, lwd=c(6,6,6,6,6),
#   #       bty='n', cex=0.9, ncol=5)
#   
#   colors.tr <- unlist(slopes$colors$tr)
#   colors.op <- unlist(slopes$colors$op)
#   
#   #print(colors.tr)
#   
#   legend(x=-2.4, y=0, 
#          legend=c('additive', 'combine', 'zero-slope', 'subtractive', 'unclear'),
#          col = colors.tr, lwd=c(5,5,5,5,5),
#          bty='n', cex=group.cex, ncol=5, seg.len=1)
#   
#   
#   # par(mar=c(3.5,3.5,0.1,0.1))
#   # 
#   # slopes <- getAdditivitySlopes(implicit = implicit.rotnorm,
#   #                               explicit = explicit.rotnorm,
#   #                               adaptation = adaptation.rotnorm)
#   # 
#   # print(range(explicit.rotnorm))
#   # print(range(implicit.rotnorm))
#   # print(range(adaptation.rotnorm))
#   # 
#   # #print(slopes)
#   # 
#   # plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#   #      main='',xlab='',ylab='',
#   #      bty='n',ax=F,asp=1)
#   # lines(c(0,1),c(1,0),col='#999999')
#   # points(explicit.rotnorm, implicit.rotnorm, col=unlist(slopes$colors$tr)[2],pch=16,cex=0.8)
#   # 
#   # # idx <- which(explicit.rotnorm > 0.1 & explicit.rotnorm < 1 & implicit.rotnorm > 0 & implicit.rotnorm < 1)
#   # # print(idx)
#   # # print(length(idx))
#   # # improt <- implicit.rotnorm
#   # # exprot <- explicit.rotnorm
#   # # rotmodel <- lm(improt ~ exprot)
#   # # library(segmented)
#   # # print(segmented.lm(rotmodel))
#   # 
#   # exp <- seq(0,1,0.01)
#   # 
#   # imp_per <- 0.3333 + (exp * 0.45)
#   # 
#   # imp <- imp_per * (1-exp)
#   # 
#   # lines(exp,imp,col='red')
#   # 
#   # title(main='rotation normalized', line=-2)
#   # axis(side=1, at=c(0,1))
#   # title(xlab='explicit', line=2)
#   # axis(side=2, at=c(0,1))
#   # title(ylab='implicit', line=2)
#   # 
#   # 
#   # 
#   # plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#   #      main='',xlab='',ylab='',
#   #      bty='n',ax=F,asp=1)
#   # lines(c(0,1),c(1,0),col='#999999')
#   # points(explicit.avgnorm, implicit.avgnorm, col=unlist(slopes$colors$tr)[2],pch=16,cex=0.8)
#   # 
#   # title(main='adaptation normalized', line=-2)
#   # axis(side=1, at=c(0,1))
#   # title(xlab='explicit', line=2)
#   # axis(side=2, at=c(0,1))
#   # title(ylab='implicit', line=2)
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

# fig6_Relations <- function(target='inline', methods=c('aim.reports')) {
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig6_relations.svg', width=8, height=3, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig6_relations.pdf', width=8, height=3)
#   }
#   
#   layout(mat=matrix(c(1,2,3),nrow=1,ncol=3))
#   
#   par(mar=c(3.1,3.1,0.1,0.1))
#   
#   df <- bindExtraData(methods=methods)
#   pal <- scales::viridis_pal(alpha=0.4, begin=0, end=1)(256)    # 1) choose colors
#   df$col <- pal[(((df$rotation-min(df$rotation))/diff(range(df$rotation)))*255)+1]              # 2) interpolate numbers
#   
#   norm.var <- unlist(df$adaptation)
#   df$norm.expl <- df$explicit/norm.var
#   df$norm.impl <- df$implicit/norm.var
#   
#   
#   plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#        main='',xlab='',ylab='',
#        bty='n',ax=F,asp=1)
#   
#   title(xlab='explicit/adaptation',line=2)
#   title(ylab='implicit/adaptation',line=2)
#   
#   text(-0.2,1.25,'A: rotation size', font.main=1, cex=1.35*1.5, adj=0)
#   
#   lines(c(0,0,1.1),c(1.1,0,0),col='#999999',lw=1,lty=1)
#   lines(c(-0.1,1.1),c(1.1,-0.1),col='#000000',lw=1,lty=1)
#   points(c(0,1),c(1,0),col='#000000')
#   
#   points(df$norm.expl, df$norm.impl, pch=1, col=df$col, cex=1)
#   
#   X <- seq(-0.1,1.1,0.01)
#   
#   # trendCI <- getTrendCI(x = df$norm.explicit,
#   #                       y = df$norm.implicit,
#   #                       bootstraps = 1000,
#   #                       kernel='normal',
#   #                       bandwidth = 0.25,
#   #                       x.points=X)
#   # 
#   # polygon(x=c(X,rev(X)), y=c(trendCI[1,],rev(trendCI[2,])),col='#FF000027',border=NA)
#   # 
#   # trends <- ksmooth(df$norm.explicit, df$norm.implicit,
#   #                   kernel="normal",
#   #                   bandwidth=0.25,
#   #                   x.points=X)
#   # 
#   # lines(trends$x, trends$y, col='red')
#   
#   # trend lines when N>100
#   for (rotation in c(30,45,60)) {
#     
#     subdf <- df[which(df$rotation == rotation),]
#     col.tr <- subdf$col[1]
#     col.op <- t_col(col.tr, percent = 0)
#     # print(which(is.na(subdf$norm.impl)))
#     trendCI <- getTrendCI(x = subdf$norm.expl,
#                           y = subdf$norm.impl,
#                           bootstraps = 1000,
#                           kernel='normal',
#                           bandwidth = 0.25,
#                           x.points=X)
#     # print('got trend')
#     polygon(x=c(X,rev(X)), y=c(trendCI[1,],rev(trendCI[2,])),col=col.tr,border=NA)
#     
#     trends <- ksmooth(subdf$norm.expl, subdf$norm.impl,
#                       kernel="normal",
#                       bandwidth=0.25,
#                       x.points=X)
#     
#     lines(trends$x, trends$y, col=col.op)
#     
#     if (rotation == 60) {
#       col60.op <- col.op
#       col60.tr <- col.tr
#     }
#     
#   }
#   
#   # # add 2 models for 60 degree data:
#   # df60 <- df[which(df$rotation == 60),]
#   # 
#   # # linear model for 60 degree data:
#   # a_fit <- fitAdditivity(df60)
#   # newdata <- data.frame('norm.expl'=seq(-0.1,1.1,0.01))
#   # a_pred <- predict.lm(a_fit, newdata=newdata)
#   # lines(newdata$norm.expl, a_pred, col='blue', lw=1, lty=3)
#   # 
#   # # capped proportional model
#   # l_fit <- fitMaxlimited(df60)
#   # newdata$group_adaptation <- mean(df60$group_adaptation)
#   # l_pred <- maxLimited(par=l_fit, df=newdata)
#   # lines(newdata$norm.expl, l_pred, col='blue', lw=1, lty=2)
#   
#   
#   axis(side=1,at=c(0,1))
#   axis(side=2,at=c(0,1))
#   
#   leg <- aggregate(rotation ~ col, data=df, FUN=mean)
#   leg <- leg[order(leg$rotation),]
#   rotcounts <- table(df$rotation)
#   
#   legend(x=0.7,y=1.2,
#          legend=sprintf('%d° (N=%d)',leg$rotation,as.numeric(rotcounts[sprintf('%d',leg$rotation)])),
#          pch=16,col=unlist(lapply(leg$col,FUN=t_col, percent=0)),cex=1.0,bty='n')
#   #title='rotation [°]',
#   
#   # legend(x=0.02, y=0.25,
#   #       legend=c('regression', 'capped fraction'),
#   #       col=c('blue','blue'),lty=c(3,2),
#   #       cex=1.0,bty='n')
#   
#   
#   # print(unlist(lapply(leg$col,FUN=t_col, percent=0)))
#   
#   #col.op <- t_col(col.tr, percent = 0)
#   
#   # # # # # # # # # # # # # # # # #
#   # APPLY MODELS TO 60° DATA ONLY?
#   
#   plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#        main='',xlab='',ylab='',
#        bty='n',ax=F,asp=1)
# 
#   title(xlab='explicit/adaptation',line=2)
#   title(ylab='implicit/adaptation',line=2)
# 
#   text(-0.2,1.25,'B: capped fraction', font.main=1, cex=1.35*1.5, adj=0)
# 
#   lines(c(0,0,1.1),c(1.1,0,0),col='#999999',lw=1,lty=1)
#   lines(c(-0.1,1.1),c(1.1,-0.1),col='#000000',lw=1,lty=1)
#   points(c(0,1),c(1,0),col='#000000')
# 
#   df60 <- df[which(df$rotation == 60),]
# 
#   points(df60$norm.expl, df60$norm.impl, pch=1, col='#66666660', cex=1)
# 
#   trendCI <- getTrendCI(x = subdf$norm.expl,
#                         y = subdf$norm.impl,
#                         bootstraps = 1000,
#                         kernel='normal',
#                         bandwidth = 0.25,
#                         x.points=X)
# 
#   polygon(x=c(X,rev(X)), y=c(trendCI[1,],rev(trendCI[2,])),col='#66666638',border=NA)
# 
# 
#   compareModels(df60)
# 
#   # first the two LM-based models:
#   a_fit <- fitAdditivity(df60)
#   #p_fit <- fitPolynomial(df60) # not plotted!
# 
#   newdata <- data.frame('norm.expl'=seq(-0.1,1.1,0.01))
# 
#   a_pred <- predict.lm(a_fit, newdata=newdata)
#   lines(newdata$norm.expl, a_pred, col='orange', lw=2)
# 
#   #p_pred <- predict.lm(p_fit, newdata=newdata)
#   #lines(newdata$norm.expl, p_pred, col='purple', lw=2)
# 
#   # this is the first one I came up with:
#   l_fit <- fitMaxlimited(df60)
#   newdata$adaptation <- mean(df60$adaptation)
#   l_pred <- maxLimited(par=l_fit, df=newdata)
#   lines(newdata$norm.expl, l_pred, col='blue', lw=2)
# 
#   # # and a second one:
#   # f_fit <- fitFractionLeft(df60)
#   # f_pred <- fractionLeft(par=f_fit, df=newdata)
#   # lines(newdata$norm.expl, f_pred, col='purple', lw=2)
# 
#   legend(x=0.3, y=1.2,
#          legend=c('regression', 'capped fraction model'),
#          col=c('orange','blue'),
#          cex=1.0,pch=16,bty='n')
#   
#   axis(side=1,at=c(0,1))
#   axis(side=2,at=c(0,1))
# 
#   
#   
#   # # # # # # # # # # # # # # # #
#   # MAXIMUM LIKELIHOOD ESTIMATES
#   
#   groups <- getGroups()
#   
#   cols.op <- c()
#   
#   plot(-1000,-1000,xlim=c(-0.4,2.4),ylim=c(-0.4,2.4),
#        main='',xlab='',ylab='',
#        bty='n',ax=F,asp=1)
#   
#   title(xlab='predicted adaptation/rotation',line=2)
#   title(ylab='measured adaptation/rotation',line=2)
#   
#   text(-0.4,2.5,'C: max. likelihood', font.main=1, cex=1.35*1.5, adj=0)
#   lines(c(0,2),c(0,2),col='#999999',lw=1,lty=1)
#   
#   MLdf <- MLE_adaptation(methods=c('PDP.difference'))
#   
#   # additive model:
#   a_hat <- MLdf$explicit + MLdf$implicit
#   
#   # par <- c('offset'=0)
#   mdf <- data.frame( 'a_hat'      = a_hat,
#                      'adaptation' = MLdf$adaptation,
#                      'rotation'   = MLdf$rotation)
#   
#   # find best offset:
#   # offset_fit <- optimx::optimx( par = par,
#   #                               fn = a_hat_offset, 
#   #                               lower = c(-100),
#   #                               upper = c( 100),
#   #                               method = 'L-BFGS-B',
#   #                               df = mdf )
#   # 
#   # add_offset_MSE <- offset_fit$value[1]
#   # print(add_offset_MSE)
#   # 
#   # print(offset_fit)
#   
#   
#   # groupname <- 'aiming'
#   # col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#   # col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#   solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
#                    rgb(136, 153, 255, 255, max = 255))
#   transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
#                    rgb(136, 153, 255, 47,  max = 255))
#   col.op <- solidcolors[1]
#   col.tr <- transcolors[1]
#   cols.op <- c(cols.op, col.op)
#   
#   # predictions <- (a_hat+offset_fit$offset[1])/MLdf$rotation
#   predictions <- a_hat/MLdf$rotation
#   adapt <- MLdf$adaptation/MLdf$rotation
#   points(predictions, adapt, col=col.tr)
#   
#   
#   at <- range(predictions)
#   add.p2a <- lm(adapt ~ predictions)
#   pcoef <- add.p2a$coefficients
#   lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#   
#   predict.points <- seq(at[1],at[2],length.out=40)
#   
#   ci <- predict( add.p2a,
#                  newdata=data.frame(predictions=predict.points),
#                  interval = "confidence")
#   
#   X <- c(predict.points,rev(predict.points))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   # print(summary(add.p2a))
#   print(pcoef)
#   print(confint(add.p2a,'predictions',level=0.95))
#   
#   # maximum likelihood model
#   # a_hat <- MLdf$a_hat
#   a_hat <- (2 * MLdf$w_explicit) + (2 * MLdf$w_implicit)
#   
#   # par <- c('offset'=0)
#   mdf <- data.frame( 'a_hat'      = a_hat,
#                      'adaptation' = MLdf$adaptation,
#                      'rotation'   = MLdf$rotation)
#   
#   # offset_fit <- optimx::optimx( par = par,
#   #                               fn = a_hat_offset, 
#   #                               lower = c(-100),
#   #                               upper = c( 100),
#   #                               method = 'L-BFGS-B',
#   #                               df = mdf )
#   # 
#   # MLE_offset_MSE <- offset_fit$value[1]
#   # print(MLE_offset_MSE)
#   
#   # groupname <- 'instructed'
#   # col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#   # col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#   col.op <- solidcolors[2]
#   col.tr <- transcolors[2]
#   cols.op <- c(cols.op, col.op)
#   
#   adapt <- MLdf$adaptation/MLdf$rotation
#   predictions <- a_hat/MLdf$rotation
#   points(predictions, adapt, col=col.tr)
#   
#   
#   # MSE <- c('additive'=add_offset_MSE,
#   #          'MLE'=MLE_offset_MSE)
#   # 
#   # AICs <- AICc(MSE = MSE,            # MSE goodness of fit
#   #              k   = c(1,1),         # number of parameters (offset only)
#   #              N   = dim(MLdf)[1])   # N observations (127 participants)
#   # 
#   # print(relativeLikelihood(AICs))
#   
#   # # # # #
#   
#   at <- range(predictions)
#   mle.p2a <- lm(adapt ~ predictions)
#   pcoef <- mle.p2a$coefficients
#   lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#   
#   predict.points <- seq(at[1],at[2],length.out=40)
#   
#   ci <- predict( mle.p2a,
#                  newdata=data.frame(predictions=predict.points),
#                  interval = "confidence")
#   
#   X <- c(predict.points,rev(predict.points))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   # print(anova(add.p2a, mle.p2a))
#   
#   print(pcoef)
#   print(confint(mle.p2a,'predictions',level=0.95))
#   
#   
#   # # # # # 
#   
#   legend(x=-0.4,y=2.4,
#          legend=c(
#                   expression(paste(hat(A)[k], ' = ', E[k], ' + ', I[k], ' (additive)') ),
#                   expression(paste(hat(A)[k], ' = ', w['e,k'], E[k], ' + ', w['i,k'], I[k], ' (MLE)'))
#                   ),
#          col=cols.op,cex=1.0,bty='n',pch=16)
#   
#   axis(side=1,at=c(0,1,2))
#   axis(side=2,at=c(0,1,2))
#   
#   
#   # # # # # # # # # # # # # #
#   # COMPARE EXPLICIT METHODS
#   
#   
#   # plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#   #      main='',xlab='',ylab='',
#   #      bty='n',ax=F,asp=1)
#   # 
#   # title(xlab='explicit/adaptation',line=2)
#   # title(ylab='implicit/adaptation',line=2)
#   # 
#   # text(-0.2,1.25,'C: explicit measure', font.main=1, cex=1.35*1.5, adj=0)
#   # 
#   # lines(c(0,0,1.1),c(1.1,0,0),col='#999999',lw=1,lty=1)
#   # lines(c(-0.1,1.1),c(1.1,-0.1),col='#000000',lw=1,lty=1)
#   # points(c(0,1),c(1,0),col='#000000')
#   # 
#   # 
#   # methods <- c("aim.reports","PDP.difference")
#   # 
#   # X <- seq(-0.1,1.1,0.01)
#   # 
#   # #groups <- getGroups()
#   # 
#   # #print(str(df))
#   # 
#   # colors <- c()
#   # Ns     <- c()
#   # 
#   # for (methodno in c(1:length(methods))) {
#   #   
#   #   subdf <- df[which(df$explicit.method == methods[methodno]),]
#   #   N <- dim(subdf)[1]
#   #   Ns <- c(Ns,N)
#   #   
#   #   groupname <- c('aims','control')[methodno]
#   #   col.op <- as.character(groups$col.op[which(groups$group == groupname)])
#   #   col.tr <- as.character(groups$col.tr[which(groups$group == groupname)])
#   #   
#   #   colors <- c(colors, col.op)
#   #   
#   #   points(subdf$norm.expl, subdf$norm.impl, pch=1, col=col.tr, cex=1)
#   #   
#   #   #print(str(subdf))
#   #   #print(subdf$norm.expl)
#   #   
#   #   trendCI <- getTrendCI(x = subdf$norm.expl,
#   #                         y = subdf$norm.impl,
#   #                         bootstraps = 1000,
#   #                         kernel='normal',
#   #                         bandwidth = 0.25,
#   #                         x.points=X)
#   #   
#   #   polygon(x=c(X,rev(X)), y=c(trendCI[1,],rev(trendCI[2,])),col=col.tr,border=NA)
#   #   
#   #   trends <- ksmooth(subdf$norm.expl, subdf$norm.impl,
#   #                     kernel="normal",
#   #                     bandwidth=0.25,
#   #                     x.points=X)
#   #   
#   #   lines(trends$x, trends$y, col=col.op)
#   #   
#   #   
#   # }
#   # 
#   # legend(x=0.3,y=1.2,
#   #        legend=sprintf('%s (N=%d)',c('aiming / reports', 'PDP difference'),Ns),
#   #        pch=16,col=colors,cex=1.0,bty='n')
#   # 
#   # 
#   # axis(side=1,at=c(0,1))
#   # axis(side=2,at=c(0,1))
#   
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }

# plotting helpers -----

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

addRegression <- function(x, y, col, pch=16, alpha=34, cex=1) {
  
  idx <- intersect(which(!is.na(x)), which(!is.na(y)))
  x <- x[idx]
  y <- y[idx]
  
  if (!is.null(pch)) {
    points(x=x,y=y,pch=pch,col=setColorAlpha(col,alpha=alpha), cex=cex)
  }
  
  at <- range(x)
  yx <- lm(y ~ x)
  
  coef <- yx$coefficients
  lines(at, coef[1]+(at*coef[2]), col=col)
  
  ci <- predict( yx,
                 newdata=data.frame(x=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=setColorAlpha(col,alpha=alpha),border=NA)
  
}





extraDataScatters <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/extradata_scatters.svg', width=5, height=7, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/extradata_scatters.pdf', width=5, height=7)
  }
  
  textsize <- 1.5
  
  lend <- 1
  paper.cex <- 0.9
  group.cex <- 0.65
  
  layout(matrix(c(1:6), nrow=3, ncol=2, byrow=TRUE))
  par(mar=c(3.5,2.5,2,0.1))
  
  #  3 or 6 - Modchalingam unpublished
  #  6      - Neville & Cressman
  #  2      - Schween et al., X/Y
  #  4      - Maresch et al 2020
  #  4      - Modchalingam et al 2019
  
  # 22 total + 5 headers = 27 ?
  
  datasets <- getExtraData()
  ndatasets <- length(names(datasets))
  ngroups   <- 0
  for (dataset in datasets) {
    ngroups <- ngroups + length(dataset[['labels']])
  }
  
  print(ndatasets + ngroups)
  
  row <- ndatasets + ngroups + 4
  

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
    #text(0,row,paper,cex=paper.cex,font=2)
    
    groups <- unique(df$group)
    
    lim <- range(c(df$adaptation, df$implicit, df$explicit))
    
    for (group in groups) {
      
      row <- row-1
      label <- labels[group]
      #text(0,row,label,cex=group.cex)
      
      # select data for the group:
      gdf <- df[which(df$group == group),]
      
      #cat(sprintf('%s N=%d\n',label,dim(gdf)[1]))
      
      # extract relevant columns:
      implicit   <- gdf$implicit
      explicit   <- gdf$explicit
      adaptation <- gdf$adaptation
      
      # slopes <- getAdditivitySlopes(implicit = implicit,
      #                               explicit = explicit,
      #                               adaptation = adaptation)
      
      # print(slopes)
      
      # for (assumption in c('strict','loose')) {
      #   model <- slopes[[assumption]]
      #   offset <- c('strict'=-1.5, 'loose'=1.5)[assumption]
      #  # plot the slope and it's confidence interval:
      #   lines(x=model$slope_ci+offset, y=rep(row,2), lw=6, col=model$colors$tr, lend=lend)
      #   points(x=model$slope+offset,y=row,pch=1,col=model$colors$op)
      # }
      
      # # plot the slope of the line and it's confidence interval:
      # lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
      # points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
      plot(-1000,-1000,xlim=lim,ylim=lim)
      points(explicit, implicit, col='blue')
      points(explicit, adaptation, col='red')
      
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
  
  totalN <- length(adaptation.rotnorm)
  
  # row    <- row-1
  # text(0,row,sprintf('all data (N=%d)',totalN),cex=paper.cex,font=2)
  # 
  # row    <- row-1
  # text(0,row,sprintf('rotation-normalized'),cex=group.cex)
  # 
  # slopes <- getAdditivitySlopes(implicit = implicit.rotnorm,
  #                               explicit = explicit.rotnorm,
  #                               adaptation = adaptation.rotnorm)
  # 
  # # plot the slope and it's confidence interval:
  # lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
  # points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
  # 
  # # plot the slope of the line and it's confidence interval:
  # lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
  # points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
  # 
  # 
  # row    <- row-1
  # 
  # text(0,row,sprintf('adaptation-normalized'),cex=group.cex)
  # slopes <- getAdditivitySlopes(implicit = implicit.avgnorm,
  #                               explicit = explicit.avgnorm,
  #                               adaptation = adaptation.avgnorm)
  
  # # plot the slope and it's confidence interval:
  # lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
  # points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
  # 
  # # plot the slope of the line and it's confidence interval:
  # lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
  # points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
  # 
  #legend(-2.5, y=0, 
  #       legend=c('supports additivity', 'supports combining', 'supports zero-sum', 'supports subtractivity', 'unclear'),
  #       col = slopes$colors.tr, lwd=c(6,6,6,6,6),
  #       bty='n', cex=0.9, ncol=5)
  
  # colors.tr <- unlist(slopes$colors$tr)
  # colors.op <- unlist(slopes$colors$op)
  # 
  # print(colors.tr)
  
  # legend(x=-2.4, y=0, 
  #        legend=c('additive', 'combine', 'zero-sum', 'subtractive', 'unclear'),
  #        col = colors.tr, lwd=c(6,6,6,6,6),
  #        bty='n', cex=0.75, ncol=5)
  
  lim <- range(c(explicit.avgnorm, implicit.avgnorm, adaptation.avgnorm))
  
  plot(-1000,-1000,xlim=lim,ylim=lim, asp=1)
  points(explicit.avgnorm, implicit.avgnorm, col='blue')
  points(explicit.avgnorm, adaptation.avgnorm, col='red')
  lines(c(0,1),c(1,0),lty=2,col='green')

  lim <- range(c(explicit.rotnorm, implicit.rotnorm, adaptation.rotnorm))
  
  plot(-1000,-1000,xlim=lim,ylim=lim, asp=1)
  points(explicit.rotnorm, implicit.rotnorm, col='blue')
  points(explicit.rotnorm, adaptation.rotnorm, col='red')
  lines(c(0,1),c(1,0),lty=2,col='green')
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

# testColors <- function() {
#   
#   #col.op <- c("#344088ff", "#732C64ff", "#E5576Dff", "#001388ff", "#73005Bff", "#E50023ff")
#   #col.tr <- c("#3440882f", "#732C642f", "#E5576D2f", "#0013882f", "#73005B2f", "#E500232f")
#   
#   
#   col.op <- c("#6C7088ff", "#735B6Eff", "#E5B6BDff", "#001388ff", "#73005Bff", "#E50023ff")
#   col.tr <- c("#6C70882f", "#735B6E2f", "#E5B6BD2f", "#0013882f", "#73005B2f", "#E500232f")
#   
#   
#   plot(-1000,-1000,xlim=c(0,1+length(col.op)),ylim=c(0.5,1.5),bty='n',ax=F,
#        main='',xlab='',ylab='')
#   
#   for (cn in c(1:length(col.op))) {
#     
#     points(c(1:length(col.op)),rep(0.75,length(col.op)),col=col.op, pch=16, cex=3)
#     points(c(1:length(col.op)),rep(1.25,length(col.op)),col=col.tr, pch=16, cex=3)
#     
#   }
#   
# }

drawAdditivitySchematic <- function(angles_deg, 
                                    cols,
                                    addLabels = FALSE) {
  
  angles_rad <- (angles_deg/180)*pi
  
  #print(angles_deg)
  
  # target:
  points(cos(angles_rad['target']),sin(angles_rad['target']),col='black')
  
  # ideal reach:
  lines(x=c(0,cos(angles_rad['target']+angles_rad['rotation'])),
        y=c(0,sin(angles_rad['target']+angles_rad['rotation'])),
        lty=2,col='gray')
  
  # explicit part:
  explicit <- seq( angles_rad['target'], 
                   angles_rad['target'] + angles_rad['explicit'],
                   length.out = 30)
  
  polygon(x = c(0,cos(explicit)) * 0.8,
          y = c(0,sin(explicit)) * 0.8,
          border = NA,
          col=cols$col.tr[which(cols$process == 'explicit')] )
  
  # implicit part:
  implicit <- seq( angles_rad['target'] + angles_rad['explicit'], 
                   angles_rad['target'] + angles_rad['explicit'] + angles_rad['implicit'],
                   length.out = 30)
  
  polygon(x = c(0,cos(implicit)) * 0.8,
          y = c(0,sin(implicit)) * 0.8,
          border = NA,
          col=cols$col.tr[which(cols$process == 'implicit')] )
  
  # full adaptation:
  adaptation <- seq( angles_rad['target'], 
                     angles_rad['target'] + angles_rad['explicit'] + angles_rad['implicit'],
                     length.out = 60)
  
  polygon( x = c(cos(adaptation)*0.8, cos(rev(adaptation))),
           y = c(sin(adaptation)*0.8, sin(rev(adaptation))),
           border = NA,
           col=cols$col.tr[which(cols$process == 'adaptation')] )
  
  
  # actual reach:
  # lines(x=c(0,cos(angles_rad['target']+angles_rad['adaptation'])),
  #       y=c(0,sin(angles_rad['target']+angles_rad['adaptation'])),
  #       lty=1,
  #       col=cols$col.op[which(cols$process == 'adaptation')])
  
  arrows(x0 = 0,
         y0 = 0,
         x1 = cos(angles_rad['target']+angles_rad['adaptation']),
         y1 = sin(angles_rad['target']+angles_rad['adaptation']),
         length=0.1, angle=15,
         col=cols$col.op[which(cols$process == 'adaptation')])

  arrows(x0 = 0,
         y0 = 0,
         x1 = cos(angles_rad['target']+angles_rad['explicit'])*0.8,
         y1 = sin(angles_rad['target']+angles_rad['explicit'])*0.8,
         length=0.1, angle=15,
         col=cols$col.op[which(cols$process == 'explicit')])
  
  
  if (addLabels) {
    # explicit label:
    exp.ang <- angles_rad['target'] + (angles_rad['explicit']/2)
    text(x = cos(exp.ang)*0.5,
         y = sin(exp.ang)*0.5,
         adj = c(0.5, 0.5),
         labels = c('explicit',sprintf('%d°',angles_deg['explicit']))[addLabels],
         col = cols$col.op[which(cols$process == 'explicit')],
         srt = (exp.ang/pi)*180)
    
    # implicit label:
    imp.ang <- angles_rad['target'] + angles_rad['explicit'] + (angles_rad['implicit']/2) 
    text(x = cos(imp.ang)*0.5,
         y = sin(imp.ang)*0.5,
         adj = c(0.5, 0.5),
         labels = c('implicit',sprintf('%d°',angles_deg['implicit']))[addLabels],
         col = cols$col.op[which(cols$process == 'implicit')],
         srt = (imp.ang/pi)*180)
    
    adt.ang <- angles_rad['target'] + (angles_rad['adaptation'] / 2)
    text(x = cos(adt.ang)*0.9,
         y = sin(adt.ang)*0.9,
         adj = c(0.5, 0.5),
         labels = c('adaptation',sprintf('%d°',angles_deg['adaptation']))[addLabels],
         col = cols$col.op[which(cols$process == 'adaptation')],
         srt = ((adt.ang/pi)*180) - 90 )
    
  }
  
}


# submission eNeuro -----

fig1_Additivity <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig1_additivity.svg', width=8, height=5, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig1_additivity.pdf', width=8, height=5)
  }
  
  textsize <- 1.35
  
  layout(mat=matrix(c(1,2,3, 4,5,6),nrow=2,ncol=3,byrow=TRUE))
  
  groups <- getGroups()
  
  par(mar=c(3.5, 3.5, 0, 0.1))
  
  process <- c('implicit',  'explicit',  'adaptation')
  col.op  <- c('#0000ffff', '#e51636ff', '#7f00d8ff' )
  col.tr  <- c('#0000ff2f', '#e516362f', '#7f00d82f' )
  cols <- data.frame(process, col.op, col.tr)
  
  # # # # # # # # # # # # # # # # # 
  # A: rotation=45, adaptation=40, implicit=20, explicit=20
  
  plot(-1000, -1000,
       main='', xlab='', ylab='', 
       xlim=c(0,1), ylim=c(0,1), 
       ax=F, bty='n', asp=1)
  text(0,0.95,'A: additivity', font.main=1, cex=1.35*1.5, adj=0)
  
  angles_deg <- c('rotation'   = 45,
                  'adaptation' = 40,
                  'implicit'   = 20,
                  'explicit'   = 20,
                  'target'     = 15)
  
  drawAdditivitySchematic(angles_deg = angles_deg,
                          cols = cols,
                          addLabels = 1)
  
  lab_ang <- (angles_deg['target']/180)*pi
  text(x=cos(lab_ang)-0.15,
       y=sin(lab_ang)-0.1,
       labels='visual target',
       srt=angles_deg['target'])
  
  best_deg <- sum(angles_deg[c('target','rotation')])
  best_rad <- (best_deg/180)*pi
  text(x=cos(best_rad)-0.15,
       y=sin(best_rad)-0.17,
       labels='full compensation',
       srt=best_deg,
       col='gray')
  
  
  # # # # # # # # # # # # # # # # # 
  # B: rotation=45, adaptation=40, implicit=30, explicit=10
  
  plot(-1000, -1000,
       main='', xlab='', ylab='', 
       xlim=c(0,1), ylim=c(0,1), 
       ax=F, bty='n', asp=1)
  text(0,0.95,'B: less explicit', font.main=1, cex=1.35*1.5, adj=0)
  #title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
  #title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
  
  angles_deg <- c('rotation'   = 45,
                  'adaptation' = 40,
                  'implicit'   = 30,
                  'explicit'   = 10,
                  'target'     = 15)
  
  drawAdditivitySchematic(angles_deg = angles_deg,
                          cols = cols,
                          addLabels = 2)
  
  # # # # # # # # # # # # # # # # # 
  # C: rotation=45, adaptation=40, implicit=10, explicit=30
  
  plot(-1000, -1000,
       main='', xlab='', ylab='', 
       xlim=c(0,1), ylim=c(0,1), 
       ax=F, bty='n', asp=1)
  text(0,0.95,'C: more explicit', font.main=1, cex=1.35*1.5, adj=0)
  #title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
  #title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
  
  angles_deg <- c('rotation'   = 45,
                  'adaptation' = 40,
                  'implicit'   = 10,
                  'explicit'   = 30,
                  'target'     = 15)
  
  drawAdditivitySchematic(angles_deg = angles_deg,
                          cols = cols,
                          addLabels = 2)
  # # # # # # # # # # # # # # # # # 
  # D: diagonal (slope:-1, intercept:24) explicit=c(4,12,20), implicit=c(20,12,4)
  
  plot(-1000, -1000,
       main='', xlab='', ylab='', 
       xlim=c(-5,50), ylim=c(-5,50), 
       ax=F, bty='n', asp=1)
  text(-5,47.25,'D: linearity', font.main=1, cex=1.35*1.5, adj=0)
  title(xlab='explicit [°]', line = 2, cex.lab=textsize)
  title(ylab='implicit [°]', line = 2, cex.lab=textsize)
  

  lines(x=c(0,0,45),y=c(45,0,0),col='gray')
  
  lines(x=c(0,10,10),
        y=c(30,30,0),
        col='gray',
        lty=2)
  lines(x=c(0,20,20),
        y=c(20,20,0),
        col='gray',
        lty=2)
  lines(x=c(0,30,30),
        y=c(10,10,0),
        col='gray',
        lty=2)
  
  points(x=c(0,40),
         y=c(40,0),
         col=cols$col.op[which(cols$process %in% c('implicit','explicit'))],
         pch=16,cex=1.5)
  
  text(3,40,'only implicit adaptation',col=cols$col.op[which(cols$process == 'implicit')],adj=0)
  text(40,3,'only explicit adaptation',col=cols$col.op[which(cols$process == 'explicit')],adj=0,srt=90)
  
  
  lines(x=c(-2,42),y=c(42,-2),col='black')
  
  points(x=c(20,10,30),
         y=c(20,30,10),
         col=cols$col.op[which(cols$process == 'adaptation')],
         pch=16,cex=1.5)
  
  text(x=c(20,10,30)+3,
       y=c(20,30,10)+3,
       col=cols$col.op[which(cols$process == 'adaptation')],
       labels=c('A','B','C'),
       cex=1.5)
  
  axis(side=1,at=c(0,10,20,30,40))
  axis(side=2,at=c(0,10,20,30,40))
  
  

  # # # # # # # # # # # # # # # # # 
  # F: many different total adaptations, with regression 95% CI
  
  plot(-1000, -1000,
       main='', xlab='', ylab='', 
       xlim=c(-5,50), ylim=c(-5,50), 
       ax=F, bty='n', asp=1)
  text(-5,47.25,'E: simulations', font.main=1, cex=1.35*1.5, adj=0)
  title(xlab='explicit [°]', line = 2, cex.lab=textsize)
  title(ylab='implicit [°]', line = 2, cex.lab=textsize)
  
  lines(x=c(0,0,45),y=c(45,0,0),col='gray')
  lines(x=c(-2,42),y=c(42,-2),col='black')
  #points(x=c(0,40),y=c(40,0),col='black',pch=1,cex=1.5)
  
  # standard deviation of noise in simulations:
  std = 7
  set.seed(1)
  sims <- simulatedAdditivity(bootstraps=1000, N=24, normalize=FALSE, std=std)
  df <- sims[['simulation']]
  data <- sims[['data']]
  
  
  for (bs in c(1:dim(df)[1])) {
    X <- df[bs,c('exp_min','exp_max')]
    if (df$include1[bs]) {col='#6666ff0f'} else {col='#ff66662f'}
    lines(x=X,
          y=df$intercept[bs] + (X*df$slope[bs]),
          col=col)
  }
  
  lines(x=c(5,35),
        y=c(35,5),
        lty=3,
        col='black')
  
  legend(15,45,c( expression('-1 in slope 95% CI'),
                  expression('-1 not in slope 95% CI') ),
         lty=1, bty='n', seg.len=1.2,
         col=c('#6666ffcc','#ff6666cc') )
    
  
  axis(side=1,at=c(0,45))
  axis(side=2,at=c(0,45))
  
  
  # # # # # # # # # # # # # # # # # 
  # E: many different total adaptations, with slopes in 95% CI
  
  # par(mar=c(0, 3.5, 0, 0.1))
  # 
  # plot(-1000, -1000,
  #      main='', xlab='', ylab='', 
  #      xlim=c(0,1), ylim=c(0,1), 
  #      ax=F, bty='n')
  # # title(xlab='explicit [°]', line = 2, cex.lab=textsize)
  # # title(ylab='implicit [°]', line = 2, cex.lab=textsize)
  # 
  # text(0,0,'F: simulations', font.main=1, cex=1.35*1.5, adj=0)
  # 
  # fontsize = 1.1
  
  # text(0,0.95,
  #      'generative model:',
  #      cex=fontsize,
  #      adj=0)
  # stdstr <- sprintf('=%0.1f°)',std)
  # text(0,0.7,
  #      expression(paste(epsilon, ' = N(', mu, '=0°, ', sigma, '=7°)') ),
  #      cex=fontsize,
  #      adj=0)
  # 
  # text(0,0.5,
  #      expression(paste(A[p], ' = 40° + ', epsilon['a,p']) ),
  #      cex=fontsize,
  #      adj=0)
  # text(0,0.3,
  #      expression(paste(E[p], ' = 20° + ', epsilon['e,p']) ),
  #      cex=fontsize,
  #      adj=0)
  # text(0,0.1,
  #      expression(paste(I[p], ' = ', A[p], ' - ', E[p], ' + ', epsilon['i,p']) ),
  #      cex=fontsize,
  #      adj=0)
  # 
  # text(0.5,.95,
  #      'slope recovery:',
  #      cex=fontsize,
  #      adj=0)
  # 
  # text(0.5,.7,
  #      expression(paste(hat(I)[p], ' = ', beta[0], ' + ', beta[1], E[p]) ),
  #      cex=fontsize,
  #      adj=0)
  
  
  # # # # # # # # # # # # # # # # # # 
  # # F: implicit & explicit normalized by adaptation
  # 
  # plot(-1000, -1000,
  #      main='', xlab='', ylab='', 
  #      xlim=c(-0.1,1.25), ylim=c(-0.1,1.25), 
  #      ax=F, bty='n', asp=1)
  # text(-0.1,1.1825,'F: normalization', font.main=1, cex=1.35*1.5, adj=0)
  # title(xlab='explicit/adaptation', line = 2, cex.lab=textsize)
  # title(ylab='implicit/adaptation', line = 2, cex.lab=textsize)
  # 
  # lines(x=c(0,0,1.1),y=c(1.1,0,0),col='gray')
  # lines(x=c(-0.05,1.05),y=c(1.05,-0.05),col='black')
  # points(x=c(0,1),y=c(1,0),col='black',pch=1,cex=1.5)
  # 
  # 
  # points(x=data[['explicit']][1,]/40,
  #        y=data[['implicit']][1,]/40,
  #        pch=1, cex=1.5,
  #        col='gray')
  # points(x=data[['explicit']][1,]/data[['adaptation']][1,],
  #        y=data[['implicit']][1,]/data[['adaptation']][1,],
  #        pch=1, cex=1.5,
  #        col='blue')
  # 
  # axis(side=1,at=c(0,1))
  # axis(side=2,at=c(0,1))
  
  par(mar=c(2., 3., 2., 3.))
  
  sim.df <- read.csv('data/additivitySlopesSimulations_3.csv', stringsAsFactors = FALSE)
  
  pm <- persp(x = unique(sim.df$N),
              y = unique(sim.df$std),
              z = matrix(sim.df$incl_1,
                         nrow=length(unique(sim.df$N))),
              xlab='\n\nsample size', ylab='\n\nnoise [sd]', zlab='\n\n\nproportion 95% CIs\nwith slope -1',
              xlim=c(0,100),ylim=c(0,15),zlim=c(0,1),
              col='#ffffffcc',
              border='#999999',
              theta=40,
              phi=30,
              # mar=c(10,10,0,2),
              ticktype = 'detailed', box=TRUE, axes=FALSE,
              
              cex.axis=0.90,
              cex.lab=0.90)
  
  # x-axis tick marks (sample size: 0-100):
  tick.start <- trans3d(c(0,25,50,75,100), 0, 0, pm)
  tick.end <- trans3d(c(0,25,50,75,100), -0.60, 0, pm)
  segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
  
  # x-axis tick labels:
  label.pos <- trans3d(c(0,25,50,75,100), -2, 0, pm)
  text(label.pos$x, label.pos$y, labels=c('0','25','50','75','100'), adj=c(0.5, NA), srt=0, cex=0.9, xpd=TRUE)
  
  # x-axis label:
  xlp <- trans3d(c(0,100), -4, 0, pm)
  ang <- ( atan2(diff(xlp$y), diff(xlp$x)) / pi) * 180
  xlp <- trans3d(50, -4, 0, pm)
  text(xlp$x, xlp$y, 'sample size', srt=ang, xpd=TRUE)
  
  # y-axis tick marks (noise: 0-15):
  tick.start <- trans3d(100, c(0,5,10,15), 0, pm)
  tick.end <- trans3d(104, c(0,5,10,15), 0, pm)
  segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

  # y-axis tick labels:
  label.pos <- trans3d(112, c(0,5,10,15), 0, pm)
  text(label.pos$x, label.pos$y, labels=c('0','5','10','15'), adj=c(0.5, NA), srt=0, cex=0.9, xpd=TRUE)

  # y-axis label:
  ylp <- trans3d(128, c(0,15), 0, pm)
  ang <- ( atan2(diff(ylp$y), diff(ylp$x)) / pi) * 180
  ylp <- trans3d(124, 7.5, 0, pm)
  text(ylp$x, ylp$y, 'noise [sd]', srt=ang, xpd=TRUE)
  
  
  # z-axis tick marks (proportion 95% CIs with slope -1):
  tick.start <- trans3d(0, 0, c(0.2,0.4,0.6,0.8,1.0), pm)
  tick.end <- trans3d(0, -.6, c(0.2,0.4,0.6,0.8,1.0), pm)
  segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

  # z-axis tick labels:
  label.pos <- trans3d(0, -2, c(0.2,0.4,0.6,0.8,1.0), pm)
  text(label.pos$x, label.pos$y, labels=c('0.2','0.4','0.6','0.8','1.0'), adj=c(0.5, NA), srt=0, cex=0.9, xpd=TRUE)
  
  # z-axis label:
  zlp <- trans3d(0, -5, c(0,1), pm)
  ang <- ( atan2(-1*diff(zlp$y), -1*diff(zlp$x)) / pi) * 180
  zlp <- trans3d(0, -5, 0.5, pm)
  text(zlp$x, zlp$y, 'proportion 95% CIs\nwith slope -1', srt=ang, xpd=TRUE)
  
  
  ex_idx <- which(sim.df$N == 24 & sim.df$std == 7)
  proportion <- sim.df$incl_1[ex_idx]
  
  ex_point <- trans3d(24, 7, proportion, pm)
  points(ex_point$x, ex_point$y, col='red', pch=1, cex=2)
  
  
  # text(-0.5,0.4,'F: simulations', font.main=1, cex=1.35*1.5, adj=0)
  
  # persp(x = unique(sim.df$N),
  #       y = unique(sim.df$std),
  #       z = matrix(sim.df$ci95span,
  #                  nrow=length(unique(sim.df$N))),
  #       xlab='\nsample size', ylab='\nstandard deviation', zlab='\n95% CI span',
  #       xlim=c(0,100),ylim=c(0,15),zlim=c(0,6),
  #       theta=40,
  #       phi=30,
  #       ticktype='detailed')
  
  
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}




# fig1_3D_simulation_results <- function() {
#   
#   
#   
#   layout(matrix(c(1,2),byro=TRUE, nrow = 1, ncol = 2))
#   
#   
#   df <- read.csv('data/additivitySlopesSimulations.csv', stringsAsFactors = FALSE)
#   
#   persp(x = unique(df$N),
#         y = unique(df$std),
#         z = matrix(df$incl_1, 
#                       nrow=length(unique(df$N))),
#         xlab='\nsample size', ylab='\nnoise parameter', zlab='\n\nproportion 95% CIs\nwith slope -1',
#         xlim=c(0,100),ylim=c(0,15),zlim=c(0,1),
#         theta=40,
#         phi=30,
#         ticktype = 'detailed')
#   
#   persp(x = unique(df$N),
#         y = unique(df$std),
#         z = matrix(df$ci95span,
#                       nrow=length(unique(df$N))),
#         xlab='\nsample size', ylab='\nstandard deviation', zlab='\n95% CI span',
#         xlim=c(0,100),ylim=c(0,15),zlim=c(0,6),
#         theta=40,
#         phi=30,
#         ticktype='detailed')
#   
#   
#   
# }


fig3_ExperimentRsults <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig3_exp_results.svg', width=8, height=8, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig3_exp_results.pdf', width=8, height=8)
  }
  
  textsize <- 1.35
  
  # we will plot all data by trial
  # and illustrate the paradigm at the same time
  
  # groups:
  # non-instructed: orange
  # instructed: red
  # aiming: purple & pink
  
  # no-cursors: gray & blue?
  
  # plot conditions (no cursors & rotations)
  
  layout(mat=matrix(c(1,1,1,2,2,3,4,5,6),nrow=3,ncol=3,byrow=TRUE))
  
  par(mar=c(2,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-7,40), ax=F, bty='n')
  text(0,35,'A: adaptation and re-aiming', font.main=1, cex=1.35*1.5, adj=0)
  title(xlab='time: trials per block', line = 0.5,cex.lab=textsize)
  title(ylab='reach/aim deviation [°]', line = 2.5,cex.lab=textsize)
  
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
  groupcols  <- c(groupcols, col.op)
  
  for (block in blocks) {
    
    CI.lo <- df$CI.lo[which(df$trial %in% block)]
    CI.hi <- df$CI.hi[which(df$trial %in% block)]
    average <- df$average[which(df$trial %in% block)]
    
    polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=col.tr, border=NA)
    lines(block, average, col=col.op)
    
  }
  
  legend(-5,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
  
  
  axis(side=2, at=c(0,15,30),cex.axis=textsize)
  
  
  par(mar=c(2,4,0,0.1))
  
  
  # # # # # # # # # 
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-1,25), ylim=c(-7,40), ax=F, bty='n')
  #title(ylab='reach deviation [°]', line=2.5)
  text(-1,-7+(diff(c(-7,40))*0.95),'B: average measures', font.main=1, cex=1.35*1.5, adj=0)
  
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
    print(xoffset)
  }
  
  df <- read.csv('data/aiming-aim-blocks.csv', stringsAsFactors = F)
  aims <- as.numeric( colMeans( df[ which( df$block %in% c(23,27,31) ), pp]) - 
                        colMeans( df[ which( df$block %in% c(4,7,10)   ), pp])   )
  groupno <- 3
  col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  CI <- Reach::getConfidenceInterval(aims, method='b')
  avg <- mean(aims)
  #xoffset <- (groupno*2) - 1
  xoffset <- 22
  polygon(x=c(-.5,.5,.5,-.5)+xoffset, y=c(rep(CI[1],2),rep(CI[2],2)), col='#FFFFFF00', border=col.tr)
  lines(x=c(-.5,.5)+xoffset, y=rep(avg,2), col=col.op)
  # replace with lines?
  points(x=rep(xoffset+1, length(aims)), aims, pch=1, col=col.tr)
  
  grouplabels <- c(grouplabels, 'aims')
  groupcols <- c(groupcols, col.op)
  
  
  # text(x = 10.5, y=-6, labels='exclude',    cex=textsize)
  # text(x =  3.5, y=-6, labels='adaptation', cex=textsize)
  # text(x = 17.5, y=-6, labels='include',    cex=textsize)
  # text(x = 22.5, y=-6, labels='aims',       cex=textsize)
  
  text( x = c(3.5, 10.5, 17.5, 22.5), 
        y = rep(-5, 4),
        labels = c('adaptation', 'exclude', 'include', 'aims'),
        adj=c(0.5,1),
        cex=textsize
  )
  
  #legend(0,15,legend=grouplabels,col=groupcols, bty='n', lty=1, cex=textsize)
  
  #axis(side=2, at=c(0,15,30))
  
  title(ylab='reach/aim deviation [°]', line = 2.5,cex.lab=textsize)
  axis(side=2, at=c(0,15,30),cex.axis=textsize)
  
  
  
  # # # # # # # # # # # # # # #'
  # AIMING STRICT ADDITIVITY
  
  grouplabels <- c()
  groupcols <- c()
  
  adf <- getAdditivityData()
  
  total <- mean(adf$adaptation[which(adf$group == 'aiming')])
  print(total)
  at <- c(-2,2+total) 
  
  groups <- getGroups()
  groupnames <- c('control', 'instructed', 'aiming')
  
  par(mar=c(4.5,4,0,0.1))
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-2,35), ylim=c(-2,35), ax=F, bty='n', asp=1)
  text(-2,-2+(diff(c(-2,35))*0.95),'C: aiming additivity', font.main=1, cex=1.35*1.5, adj=0)
  lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
  lines(x=c(0,30),y=c(0,0),col='#999999', lw=1, lty=2)
  lines(x=c(0,0),y=c(0,30),col='#999999', lw=1, lty=2)
  
  # points(c(0,total),c(total,0),col='#000000')
  
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
  axis(side=2, at=c(0,10,20,30), cex.axis=textsize)
  
  title(xlab='aiming reports [°]', line=2.5, cex.lab=textsize)
  axis(side=1, at=c(0,10,20,30), cex.axis=textsize)
  
  # legend(1,45,
  #        legend=c('aiming (difference)','aiming (report)'),
  #        col=groupcols, 
  #        bty='n', lty=1, cex=0.8*1.5)
  
  
  # # # # # # # # # # # # # # 
  # THIRD ROW OF PLOT
  
  #textsize <- 1.5
  
  
  par(mar=c(4.5,4,0,0.1))
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-7.5,27.5), ylim=c(-5,30), ax=F, bty='n',asp=1)
  text(-7.5,(-5+(diff(c(-5,30))*0.95)),'D: explicit measures', font.main=1, cex=1.35*1.5, adj=0)
  title(xlab='aims [°]', line=2.5, cex.lab=textsize)
  title(ylab='inclusion - exclusion [°]', line=2.5, cex.lab=textsize)
  
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
  print(summary(A2R))
  coef <- A2R$coefficients
  lines(at, coef[1]+(at*coef[2]), col=col.op)
  
  
  ci <- predict( A2R,
                 newdata=data.frame(aims=seq(-2,20,.2)),
                 interval = "confidence")
  
  print(confint(A2R,parm='aims',level=0.95))
  
  
  X <- c(seq(-2,20,.2),rev(seq(-2,20,.2)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  
  axis(side=1, at=c(0,10,20), cex.axis=textsize)
  axis(side=2, at=c(0,10,20), cex.axis=textsize)
  
  # # # # # # # # # 3 # # # # # #
  # PDP STRICT ADDITIVITY
  
  grouplabels <- c()
  groupcols <- c()
  
  adf <- getAdditivityData()
  
  total <- mean(adf$adaptation)
  at <- c(-3,3+total) 
  
  groups <- getGroups()
  groupnames <- c('control', 'instructed', 'aiming')
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(-6,43), ylim=c(-6,43), ax=F, bty='n', asp=1)
  text(-6,(-6+(diff(c(-6,43))*0.95)),'E: PDP additivity', font.main=1, cex=1.35*1.5, adj=0)
  lines(x=at,y=(at*-1)+total,col="#999999", lw=1, lty=1)
  lines(x=c(0,40),y=c(0,0),col='#999999', lw=1, lty=2)
  lines(x=c(0,0),y=c(0,40),col='#999999', lw=1, lty=2)
  
  # points(c(0,total),c(total,0),col='#000000')
  
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
    
    # correct for individual variation in total adaptation:
    err <- mean(adapt) - adapt
    excl <- excl - err
    
    at <- range(expl)
    
    #e2i <- lm(excl ~ e_p_n)
    e2i <- lm(excl ~ expl)
    
    
    cat(sprintf('%s:\n',toupper(groupname)))
    #print(summary(e2i))
    
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
  
  # #print(mean(adf$adaptation))
  # 
  # col.op <- as.character(groups$col.op[which(groups$group == 'aims')])
  # col.tr <- as.character(groups$col.tr[which(groups$group == 'aims')])
  # 
  # grouplabels <- c(grouplabels, 'aims')
  # groupcols <- c(groupcols, col.op)
  # 
  # idx <- which(adf$group == 'aiming')
  # expl <- adf$aiming[idx]
  # impl <- adf$exclude[idx]
  # adapt <- adf$adaptation[idx]
  # 
  # at <- range(expl)
  # 
  # e2i <- lm(impl ~ expl)
  # 
  # 
  # cat(sprintf('%s:\n',toupper('aims')))
  # print(confint(e2i,parm='expl',level=0.95))
  # print(confint(e2i,parm='(Intercept)',level=0.95))
  # #print(summary(I2A))
  # 
  # coef <- e2i$coefficients
  # lines(at, coef[1]+(at*coef[2]), col=col.op)
  # 
  # 
  # ci <- predict( e2i,
  #                newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
  #                interval = "confidence")
  # 
  # X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  # Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  # polygon(x=X,y=Y,col=col.tr,border=NA)
  # 
  # points(expl, impl, pch=16, col=col.tr)
  
  
  #title(main='strict additivity')
  
  title(ylab='implicit measure [°]', line=2.5, cex.lab=textsize)
  axis(side=2, at=c(0,20,40), cex.axis=textsize)
  
  title(xlab='include - exclude [°]', line=2.5, cex.lab=textsize)
  axis(side=1, at=c(0,20,40), cex.axis=textsize)
  
  legend(1,40,
         legend=grouplabels,
         col=groupcols, 
         bty='n', lty=1, cex=0.8*1.5)
  
  # # # # # # # # # # # 
  # LOOSE ADDITIVITY
  # 
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,45), ylim=c(0,45), ax=F, bty='n', asp=1)
  text(0,(0+(diff(c(0,45))*0.95)),'F: loose additivity', font.main=1, cex=1.35*1.5, adj=0)
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
  
  legend(12,15,
         legend=c(grouplabels[1:2],'aiming (Inc-Exc)','aiming (reports)'),
         col=groupcols, 
         bty='n', lty=1, cex=0.8*1.5)
  
  #title(main='loose additivity')
  
  title(ylab='adaptation [°]', line=2.5, cex.lab=textsize)
  axis(side=2, at=c(0,20,40), cex.axis=textsize)
  
  title(xlab=expression(paste(beta[i] %.% implicit + beta[e] %.% explicit)), line=3, cex.lab=textsize)
  axis(side=1, at=c(0,15,30,45), cex.axis=textsize)
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}



fig4_Explicit_TwoRate <- function(target='inline') {
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig4_tworate.svg', width=6, height=4.5, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig4_tworate.pdf', width=6, height=4.5)
  }
  
  textsize <- 1.0
  
  layout(mat=matrix(c(1,2,3,4,4,4),nrow=2,ncol=3,byrow = TRUE))
  
  par(mar=c(2,4,0,0.1))
  
  
  pp <- sprintf('p%03d',c(1:24))
  groups <- getGroups()
  groupnames <- c('control', 'instructed', 'aiming')
  
  grouplabels <- c()
  groupcols <- c()
  
  set.seed(93)
  
  for (groupno in c(1:length(groupnames))) {
    
    group <- groupnames[groupno]
    
    plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,22), ylim=c(-7,40), ax=F, bty='n')
    text(0,35,c('A: control', 'B: instructed', 'C: aiming')[groupno], font.main=1, cex=1.35*textsize, adj=0)
    title(ylab='difference score [°]', line = 2.5)
    
    df <- read.csv(sprintf('data/%s-nocursors-all.csv', group), stringsAsFactors = F)
    row.names(df) <- df$condition
    
    col.op <- as.character(groups$col.op[which(groups$group == group)])
    col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    grouplabels <- c(grouplabels, as.character(groups$label[which(groups$group == group)]))
    groupcols <- c(groupcols, col.op)
    
    explicit <- as.numeric(df['include',pp] - df['exclude',pp])
    CI <- Reach::getConfidenceInterval(explicit, method='b')
    avg <- mean(explicit)
    polygon(x=c(2,6,6,2), y=c(rep(CI[1],2),rep(CI[2],2)), col=col.tr, border=NA)
    lines(x=c(2,6), y=rep(avg,2), col=col.op)
    
    # text(0.4+(groupno*4)-2,-8.5, group, srt=90, adj=c(0,0.5), cex=0.8)
    
    # points(x=rep(10, length(explicit)), explicit, pch=16, col=col.tr, cex=2)
    points(x=runif(length(explicit),min=9,max=11), explicit, pch=16, col=col.tr, cex=2)
    
    dX <- density(explicit, n=81, from=-10, to=30, bw=2.5)$y
    dX <- (dX / sum(dX)) * 150
    dY <- seq(-10,30,.5)
    
    polygon(x=c(0,dX,0)+14, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=col.tr)
    
    lines(dX+14, dY, col=col.op)
    
    axis(side=2, at=c(0,15,30))
    
    
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
  
  #lines(dX+18, dY, col="#001388FF", lty=2)
  lines(dX+14, dY, col="#000000FF", lty=2)
  
  text(13, iM, 'unaware', srt=90, cex=textsize)
  text(13, eM, 'aware', srt=90, cex=textsize)
  
  dataM <- data.frame('explicit'=c(iM, eM))
  iLM <- dnorm(dataM$explicit, mean=iM, sd=iS)
  eLM <- dnorm(dataM$explicit, mean=eM, sd=eS)
  prob_dens_M <- (f*iLM) + ((1-f)*eLM)
  
  # lines(c(18,18+((prob_dens_M[1]/sum(prob_dens))*150)),c(iM,iM), col="#001388FF", lty=1)
  # lines(c(18,18+((prob_dens_M[2]/sum(prob_dens))*150)),c(eM,eM), col="#001388FF", lty=1)
  lines(c(14,14+((prob_dens_M[1]/sum(prob_dens))*150)),c(iM,iM), col="#000000FF", lty=1)
  lines(c(14,14+((prob_dens_M[2]/sum(prob_dens))*150)),c(eM,eM), col="#000000FF", lty=1)
  

  
  
  # # # # # # # # # # # # # # # 
  # TWO RATE MODEL PLOTS
  
  
  
  plot(-1000,-1000,main='',xlab='', ylab='', xlim=c(0,265), ylim=c(-7,40), ax=F, bty='n')
  text(0,35,'D: state-space model', font.main=1, cex=1.35, adj=0)
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
    
    if (strategy) {
      group <- 'aiming'
      groupname <- 'aware aimers (N=9)'
      col.op.expl <- as.character(groups$col.op[which(groups$group == 'aiming')])
      col.op.impl <- as.character(groups$col.op[which(groups$group == 'aims')])
    } else {
      group <- 'aims'
      groupname <- 'unaware aimers (N=15)'
      col.op.expl <- '#0047AB'
      col.op.impl <- '#00d0d0'
      
      # azure blue: #007FFF
      # turquoise:  #00FFEF
      # cyan:       #00B7EB # implicit
      # cobalt:     #0047AB # explicit
      # 
      # 
    }
    #col.op <- as.character(groups$col.op[which(groups$group == group)])
    
    # not using transparent colors right now:
    #col.tr <- as.character(groups$col.tr[which(groups$group == group)])
    
    groupnames <- c(groupnames, groupname)
    groupcols <- c(groupcols, col.op.expl)
    
    blocks <- list(seq(1,32), seq(41,56), seq(65,80), seq(89,184), seq(201, 216), seq(233,248))
    
    for (block in blocks) {
      
      block <- unlist(block)
      trial_idx <- which(df$trial %in% block)
      bdf <- sdf[trial_idx,]
      
      CI <- apply(bdf, MARGIN=c(1), Reach::getConfidenceInterval, method='b')
      CI.lo <- as.numeric(CI[1,])
      CI.hi <- as.numeric(CI[2,])
      average <- rowMeans(bdf, na.rm=TRUE)
      
      polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=t_col(col.op.expl, percent = 80), border=NA)
      lines(block, average, col=col.op.expl)
      
    }
    
  }
  
  
  # now we plot the exlude reach deviations for both sub-groups:
  
  df <- read.csv('data/aiming.csv', stringsAsFactors = F)
  
  for (strategy in c(FALSE, TRUE)) {
    
    sdf <- df[which(df$participant %in% split_aim$ppno[which(split_aim$strategy == strategy)]),]
    
    if (strategy) {
      group <- 'aiming'
      groupname <- 'aware aimers (N=9)'
      col.op.expl <- as.character(groups$col.op[which(groups$group == 'aiming')])
      col.op.impl <- as.character(groups$col.op[which(groups$group == 'aims')])
    } else {
      group <- 'aims'
      groupname <- 'unaware aimers (N=15)'
      col.op.expl <- '#0047AB'
      col.op.impl <- '#00d0d0'
    }
    
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
      
      polygon(x=c(block, rev(block)),y=c(CI.lo, rev(CI.hi)), col=t_col(col.op.impl,percent = 80), border=NA)
      lines(block, average, col=col.op.impl)
      
    }  
    
  }
  
  # we fit the two-rate model to mean reach deviations in both sub-groups
  # is the fast process equal to aiming responses?
  
  df <- get2rateData(group='aiming')
  schedule <- df$rotation * -1
  
  
  for (strategy in c(FALSE, TRUE)) {
    
    if (strategy) {
      group <- 'aiming'
      groupname <- 'aware aimers (N=9)'
      col.op <- as.character(groups$col.op[which(groups$group == 'aiming')])
      #col.op.impl <- as.character(groups$col.op[which(groups$group == 'aims')])
    } else {
      group <- 'aims'
      groupname <- 'unaware aimers (N=15)'
      col.op <- '#0047AB'
      #col.op.impl <- '#00d0d0'
    }
    
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

fig5_External_Aiming <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig5_extradata.svg', width=4, height=6, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig5_extradata.pdf', width=4, height=6)
  }
  
  textsize <- 0.8
  
  lend <- 1
  paper.cex <- 0.65
  group.cex <- 0.5
  
  #layout(matrix(c(1,2,3,1,4,5), nrow=2, ncol=3, byrow=TRUE), width=c(3,2,2))
  par(mar=c(3,0,0,0.1))
  
  #datasets <- getExtraData()
  datasets <- getAllExtraData(methods=c('PDP.difference'))
  datasets <- getAllExtraData(methods=c('aim.reports'))
  ndatasets <- length(names(datasets))
  ngroups   <- 0
  for (dataset in datasets) {
    ngroups <- ngroups + length(dataset[['labels']])
  }
  
  print(ndatasets + ngroups)
  
  row <- ndatasets + ngroups + 2
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(-3.2,3.2),ylim=c(-1,row),
       bty='n',ax=F)
  
  title(xlab='slope',line=2,cex.lab=textsize)
  
  for (xpos in c(-2.5,2.5)) {
    lines(c(xpos,xpos),c(0.5,row-.5),lty=1,col='#000000')
  }
  for (xpos in c(-1.5,1.5)) {
    lines(c(xpos,xpos),c(0.5,row-.5),lty=3,col='#999999')
  }
  
  
  axis(side=1, at=c(-2.5,-1.5),labels=c('-1','0'), cex.axis=textsize)
  text(-2.65,.5,'strict additivity',srt=90,adj=c(0,0.5), cex=textsize)
  axis(side=1, at=c(1.5,2.5),labels=c('0','1'), cex.axis=textsize)
  text(2.65,.5,'loose additivity',srt=90,adj=c(0,0.5), cex=textsize)
  
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
      
      # print(label)
      if (!(label %in% c('stepwise, 30°','stepwise, 45°','stepwise, 60°'))) {
        adaptation.rotnorm <- c(adaptation.rotnorm, adaptation / gdf$rotation)
        adaptation.avgnorm <- c(adaptation.avgnorm, adaptation / mean(adaptation))
        explicit.rotnorm   <- c(explicit.rotnorm,   explicit   / gdf$rotation)
        explicit.avgnorm   <- c(explicit.avgnorm,   explicit   / mean(adaptation))
        implicit.rotnorm   <- c(implicit.rotnorm,   implicit   / gdf$rotation)
        implicit.avgnorm   <- c(implicit.avgnorm,   implicit   / mean(adaptation))
      }
      
    }
    
  }
  
  totalN <- length(adaptation.rotnorm)
  print(totalN)
  
  row    <- row-1
  text(0,row,sprintf('all data (N=%d)',totalN),cex=paper.cex,font=2)
  
  row    <- row-1
  text(0,row,sprintf('rotation-normalized'),cex=group.cex)
  
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
  
  text(0,row,sprintf('adaptation-normalized'),cex=group.cex)
  slopes <- getAdditivitySlopes(implicit = implicit.avgnorm,
                                explicit = explicit.avgnorm,
                                adaptation = adaptation.avgnorm)
  
  # plot the slope and it's confidence interval:
  lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
  points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
  
  # plot the slope of the line and it's confidence interval:
  lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
  points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
  
  
  colors.tr <- unlist(slopes$colors$tr)
  colors.op <- unlist(slopes$colors$op)
  
  
  legend(x=-2.4, y=0, 
         legend=c('additive', 'combine', 'zero-slope', 'subtractive', 'unclear'),
         col = colors.tr, lwd=c(5,5,5,5,5),
         bty='n', cex=group.cex, ncol=5, seg.len=1)
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

fig6_External_PDP <- function(target='inline') {
  
  if (target == 'svg') {
    svglite::svglite(file='doc/Fig6_extra_PDP.svg', width=4, height=5, fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig6_extra_PDP.pdf', width=4, height=5)
  }
  
  textsize <- 0.8
  
  lend <- 1
  paper.cex <- 0.65
  group.cex <- 0.5
  
  #layout(matrix(c(1,2,3,1,4,5), nrow=2, ncol=3, byrow=TRUE), width=c(3,2,2))
  par(mar=c(3,0,0,0.1))
  
  #datasets <- getExtraData()
  datasets <- getAllExtraData(methods=c('PDP.difference'))
  # datasets <- getAllExtraData(methods=c('aim.reports'))
  ndatasets <- length(names(datasets))
  ngroups   <- 0
  for (dataset in datasets) {
    ngroups <- ngroups + length(dataset[['labels']])
  }
  
  print(ndatasets + ngroups)
  
  row <- ndatasets + ngroups + 4
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(-3.2,3.2),ylim=c(-1,row),
       bty='n',ax=F)
  
  title(xlab='slope',line=2,cex.lab=textsize)
  
  for (xpos in c(-2.5,2.5)) {
    lines(c(xpos,xpos),c(0.5,row-.5),lty=1,col='#000000')
  }
  for (xpos in c(-1.5,1.5)) {
    lines(c(xpos,xpos),c(0.5,row-.5),lty=3,col='#999999')
  }
  
  
  axis(side=1, at=c(-2.5,-1.5),labels=c('-1','0'), cex.axis=textsize)
  text(-2.65,.5,'strict additivity',srt=90,adj=c(0,0.5), cex=textsize)
  axis(side=1, at=c(1.5,2.5),labels=c('0','1'), cex.axis=textsize)
  text(2.65,.5,'loose additivity',srt=90,adj=c(0,0.5), cex=textsize)
  
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
      
      # print(label)
      if (!(label %in% c('stepwise, 30°','stepwise, 45°','stepwise, 60°'))) {
        adaptation.rotnorm <- c(adaptation.rotnorm, adaptation / gdf$rotation)
        adaptation.avgnorm <- c(adaptation.avgnorm, adaptation / mean(adaptation))
        explicit.rotnorm   <- c(explicit.rotnorm,   explicit   / gdf$rotation)
        explicit.avgnorm   <- c(explicit.avgnorm,   explicit   / mean(adaptation))
        implicit.rotnorm   <- c(implicit.rotnorm,   implicit   / gdf$rotation)
        implicit.avgnorm   <- c(implicit.avgnorm,   implicit   / mean(adaptation))
      }
      
    }
    
  }
  
  totalN <- length(adaptation.rotnorm)
  print(totalN)
  
  row    <- row-1
  text(0,row,sprintf('all data (N=%d)',totalN),cex=paper.cex,font=2)
  
  row    <- row-1
  text(0,row,sprintf('rotation-normalized'),cex=group.cex)
  
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
  
  text(0,row,sprintf('adaptation-normalized'),cex=group.cex)
  slopes <- getAdditivitySlopes(implicit = implicit.avgnorm,
                                explicit = explicit.avgnorm,
                                adaptation = adaptation.avgnorm)
  
  # plot the slope and it's confidence interval:
  lines(x=slopes$strict$slope_ci-1.5, y=rep(row,2), lw=6, col=slopes$strict$colors$tr, lend=lend)
  points(x=slopes$strict$slope-1.5,y=row,pch=1,col=slopes$strict$colors$op)
  
  # plot the slope of the line and it's confidence interval:
  lines(x=slopes$loose$slope_ci+1.5, y=rep(row,2), lw=6, col=slopes$loose$colors$tr, lend=lend)
  points(x=slopes$loose$slope+1.5,y=row,pch=1,col=slopes$loose$colors$op)
  
  
  colors.tr <- unlist(slopes$colors$tr)
  colors.op <- unlist(slopes$colors$op)
  
  
  legend(x=-2.4, y=0, 
         legend=c('additive', 'combine', 'zero-slope', 'subtractive', 'unclear'),
         col = colors.tr, lwd=c(5,5,5,5,5),
         bty='n', cex=group.cex, ncol=5, seg.len=1)
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}


fig7_Relations_too <- function(target='inline') {
  
  # set up files, if any:
  if (target=='svg') {
    svglite::svglite(file='doc/Fig7_relations.svg', width=8, height=4, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/Fig7_relations.pdf', width=8, height=4)
  }
  
  # set up global plot parameters:
  layout(mat=matrix(c(1:2),
                    nrow=1,ncol=2,
                    byrow = TRUE))
  
  par(mar=c(3.1,3.1,0.1,0.1))
  
  
  # get all data:
  df <- bindExtraData(methods=c('aim.reports', 'PDP.difference'))
  
  # make sure that each rotation is present for both aim and PDP subsets of data
  use_rotations <- as.numeric(names(which(apply(table(df$rotation, df$explicit.method) > 0, 1, all))))
  df <- df[which(df$rotation %in% use_rotations),]
  
  allrotations <- sort(unique(df$rotation))
  nrotations <- length(allrotations)
  
  # set up color pallet on all data
  pal.tr <- scales::viridis_pal(alpha=0.16, begin=0, end=1)(256)    # 1) choose colors
  pal.op <- scales::viridis_pal(alpha=1.0, begin=0, end=1)(256)    # 1) choose colors
  df$col.tr <- pal.tr[(((df$rotation-min(df$rotation))/diff(range(df$rotation)))*255)+1]              # 2) interpolate numbers
  df$col.op <- pal.op[(((df$rotation-min(df$rotation))/diff(range(df$rotation)))*255)+1]              # 2) interpolate numbers
  
  
  # normalize by individual adaptation
  norm.var <- unlist(df$adaptation)
  df$norm.expl <- df$explicit/norm.var
  df$norm.impl <- df$implicit/norm.var
  
  
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
                   rgb(136, 153, 255, 255, max = 255))
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(136, 153, 255, 47,  max = 255))
  
  
  # # # # # # # # # # 3 # # 
  # AIMING BASED DATA
  
  
  plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
       main='',xlab='',ylab='',
       bty='n',ax=F,asp=1)
  
  title(xlab='aiming/adaptation',line=2)
  title(ylab='exclusion/adaptation',line=2)
  
  text(-0.2,1.15,'A: aiming reports', font.main=1, cex=1.35*1.5, adj=c(0,0.5))
  
  lines(c(0,0,1.1),c(1.1,0,0),col='#999999',lw=1,lty=1)
  lines(c(-0.1,1.1),c(1.1,-0.1),col='#999999',lw=1,lty=1)
  
  # plot stuff with aiming reports
  
  aim.df <- df[which(df$explicit.method == 'aim.reports'),]
  
  points(aim.df$norm.expl, aim.df$norm.impl, pch=16, col=df$col.tr, cex=1)
  
  
  # 
  # X = seq(-0.1,1.1,0.01)  
  # 
  # trendCI <- getTrendCI(x = aim.df$norm.expl,
  #                       y = aim.df$norm.impl,
  #                       bootstraps = 1000,
  #                       kernel='normal',
  #                       bandwidth = 0.25,
  #                       x.points=X)
  # # print('got trend')
  # polygon(x=c(X,rev(X)), y=c(trendCI[1,],rev(trendCI[2,])),col=transcolors[1],border=NA)
  # 
  # trends <- ksmooth(aim.df$norm.expl, aim.df$norm.impl,
  #                   kernel="normal",
  #                   bandwidth=0.25,
  #                   x.points=X)
  # 
  # lines(trends$x, trends$y, col=solidcolors[1])
  # 
  
  
  idx <- which(aim.df$norm.expl > -0.2 & aim.df$norm.expl < 1.2 &aim.df$norm.impl > -0.2 & aim.df$norm.impl < 1.2)
  
  addRegression(x=aim.df$norm.expl[idx],
                y=aim.df$norm.impl[idx],
                col=solidcolors[1],pch=NULL,alpha=34,cex=1)
  
  rotations <- sort(unique(aim.df$rotation))
  lcols <- c()
  lNs <- c()
  for (r in rotations) {
    lcols <- c(lcols, aim.df$col.op[which(aim.df$rotation == r)[1]])
    lNs <- c(lNs, length(which(aim.df$rotation == r)))
  }
  
  legend( x = 0.6,
          y = 1.0575,
          legend = sprintf('%d° (N=%d)',rotations, lNs),
          bty='n',
          box.col = '#FFFFFFCC',
          pch=16,
          col=lcols)
  
  axis(1,at=c(0,1))
  axis(2,at=c(0,1))
  
  
  # # # # # # # # # # # # # #
  
  # get colors for aiming/PDP
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), # red
                   rgb(136, 153, 255, 255, max = 255)) # blue
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(136, 153, 255, 47,  max = 255))
  
  
  cols.op <- c()
  
  plot(-1000,-1000,xlim=c(-0.4,2.4),ylim=c(-0.4,2.4),
       main='',xlab='',ylab='',
       bty='n',ax=F,asp=1)
  
  title(xlab='predicted adaptation/rotation',line=2)
  title(ylab='measured adaptation/rotation',line=2)
  
  # text(-0.2,1.15,'E: distribution of measures', font.main=1, cex=1.35*1.5, adj=0)
  
  text(-0.4,2.3,'B: MLE model', font.main=1, cex=1.35*1.5, adj=c(0,0.5))
  lines(c(0,2),c(0,2),col='#999999',lw=1,lty=1)
  
  MLdf <- MLE_adaptation(df=aim.df)
  
  # additive model:
  a_hat <- MLdf$explicit + MLdf$implicit
  
  # par <- c('offset'=0)
  mdf <- data.frame( 'a_hat'      = a_hat,
                     'adaptation' = MLdf$adaptation,
                     'rotation'   = MLdf$rotation)
  

  
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
                   rgb(136, 153, 255, 255, max = 255))
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(136, 153, 255, 47,  max = 255))
  col.op <- solidcolors[1]
  col.tr <- transcolors[1]
  cols.op <- c(cols.op, col.op)
  
  # predictions <- (a_hat+offset_fit$offset[1])/MLdf$rotation
  predictions <- a_hat/MLdf$rotation
  idx <- which(!is.na(predictions))
  predictions <- predictions[idx]
  adapt <- MLdf$adaptation[idx]/MLdf$rotation[idx]
  points(predictions, adapt, col=col.tr)
  
  
  at <- range(predictions)
  add.p2a <- lm(adapt ~ predictions)
  pcoef <- add.p2a$coefficients
  lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
  
  predict.points <- seq(at[1],at[2],length.out=40)
  
  ci <- predict( add.p2a,
                 newdata=data.frame(predictions=predict.points),
                 interval = "confidence")
  
  X <- c(predict.points,rev(predict.points))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  # # print(summary(add.p2a))
  # print(pcoef)
  # print(confint(add.p2a,'predictions',level=0.95))
  
  # maximum likelihood model
  # a_hat <- MLdf$a_hat
  a_hat <- (2 * MLdf$w_explicit) + (2 * MLdf$w_implicit)
  
  idx <- which(!is.na(a_hat))
  
  # par <- c('offset'=0)
  mdf <- data.frame( 'a_hat'      = a_hat[idx],
                     'adaptation' = MLdf$adaptation[idx],
                     'rotation'   = MLdf$rotation[idx])
  

  col.op <- solidcolors[2]
  col.tr <- transcolors[2]
  cols.op <- c(cols.op, col.op)
  
  adapt <- MLdf$adaptation[idx]/MLdf$rotation[idx]
  predictions <- a_hat[idx]/MLdf$rotation[idx]
  points(predictions, adapt, col=col.tr)
  
  
  # MSE <- c('additive'=add_offset_MSE,
  #          'MLE'=MLE_offset_MSE)
  # 
  # AICs <- AICc(MSE = MSE,            # MSE goodness of fit
  #              k   = c(1,1),         # number of parameters (offset only)
  #              N   = dim(MLdf)[1])   # N observations (127 participants)
  # 
  # print(relativeLikelihood(AICs))
  
  # # # # #
  
  at <- range(predictions)
  mle.p2a <- lm(adapt ~ predictions)
  pcoef <- mle.p2a$coefficients
  lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
  
  predict.points <- seq(at[1],at[2],length.out=40)
  
  ci <- predict( mle.p2a,
                 newdata=data.frame(predictions=predict.points),
                 interval = "confidence")
  
  X <- c(predict.points,rev(predict.points))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=col.tr,border=NA)
  
  # print(anova(add.p2a, mle.p2a))
  
  cat('MLE predictions:\n')
  print(pcoef)
  print(confint(mle.p2a,'predictions',level=0.95))
  
  
  # # # # # 
  
  legend(x=-0.4,y=2.2,
         legend=c(
           expression(paste(hat(A)[p], ' = ', E[p], ' + ', I[p], ' (additive)') ),
           expression(paste(hat(A)[p], ' = ', w['e,p'], E[p], ' + ', w['i,p'], I[p], ' (MLE)'))
         ),
         col=cols.op,cex=1.0,bty='n',pch=16)
  
  axis(side=1,at=c(0,1,2))
  axis(side=2,at=c(0,1,2))
  
  
  
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}

# fig7_Relations_too <- function(target='inline') {
#   
#   # set up files, if any:
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig7_relations.svg', width=8, height=6, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/Fig7_relations.pdf', width=8, height=6)
#   }
#   
#   # set up global plot parameters:
#   layout(mat=matrix(c(1:6),
#                     nrow=2,ncol=3,
#                     byrow = TRUE))
#   
#   par(mar=c(3.1,3.1,0.1,0.1))
#   
#   
#   # get all data:
#   df <- bindExtraData(methods=c('aim.reports', 'PDP.difference'))
#   
#   # make sure that each rotation is present for both aim and PDP subsets of data
#   use_rotations <- as.numeric(names(which(apply(table(df$rotation, df$explicit.method) > 0, 1, all))))
#   df <- df[which(df$rotation %in% use_rotations),]
#   
#   allrotations <- sort(unique(df$rotation))
#   nrotations <- length(allrotations)
#   
#   # set up color pallet on all data
#   pal.tr <- scales::viridis_pal(alpha=0.16, begin=0, end=1)(256)    # 1) choose colors
#   pal.op <- scales::viridis_pal(alpha=1.0, begin=0, end=1)(256)    # 1) choose colors
#   df$col.tr <- pal.tr[(((df$rotation-min(df$rotation))/diff(range(df$rotation)))*255)+1]              # 2) interpolate numbers
#   df$col.op <- pal.op[(((df$rotation-min(df$rotation))/diff(range(df$rotation)))*255)+1]              # 2) interpolate numbers
#   
#   # get colors for aiming/PDP
#   solidcolors =  c(rgb(229, 22,  54,  255, max = 255), # red
#                    rgb(136, 153, 255, 255, max = 255)) # blue
#   transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
#                    rgb(136, 153, 255, 47,  max = 255))
#   
#   # normalize by individual adaptation
#   norm.var <- unlist(df$adaptation)
#   df$norm.expl <- df$explicit/norm.var
#   df$norm.impl <- df$implicit/norm.var
#   
#   
#   # # # # # # # # # # 3 # # 
#   # AIMING BASED DATA
#   
#   
#   plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#        main='',xlab='',ylab='',
#        bty='n',ax=F,asp=1)
#   
#   title(xlab='aiming/adaptation',line=2)
#   title(ylab='exclusion/adaptation',line=2)
#   
#   text(-0.2,1.15,'A: aiming reports', font.main=1, cex=1.35*1.5, adj=0)
#   
#   lines(c(0,0,1.1),c(1.1,0,0),col='#999999',lw=1,lty=1)
#   lines(c(-0.1,1.1),c(1.1,-0.1),col='#000000',lw=1,lty=1)
#   
#   # plot stuff with aiming reports
#   
#   aim.df <- df[which(df$explicit.method == 'aim.reports'),]
#   
#   points(aim.df$norm.expl, aim.df$norm.impl, pch=16, col=df$col.tr, cex=1)
#   
#   
#   # 
#   X = seq(-0.1,1.1,0.01)  
#   # 
#   # trendCI <- getTrendCI(x = aim.df$norm.expl,
#   #                       y = aim.df$norm.impl,
#   #                       bootstraps = 1000,
#   #                       kernel='normal',
#   #                       bandwidth = 0.25,
#   #                       x.points=X)
#   # # print('got trend')
#   # polygon(x=c(X,rev(X)), y=c(trendCI[1,],rev(trendCI[2,])),col=transcolors[1],border=NA)
#   # 
#   # trends <- ksmooth(aim.df$norm.expl, aim.df$norm.impl,
#   #                   kernel="normal",
#   #                   bandwidth=0.25,
#   #                   x.points=X)
#   # 
#   # lines(trends$x, trends$y, col=solidcolors[1])
#   # 
#   
#   
#   idx <- which(aim.df$norm.expl > -0.2 & aim.df$norm.expl < 1.2 &aim.df$norm.impl > -0.2 & aim.df$norm.impl < 1.2)
#   
#   addRegression(x=aim.df$norm.expl[idx],
#                 y=aim.df$norm.impl[idx],
#                 col='#999999',pch=NULL,alpha=34,cex=1)
#   
#   axis(1,at=c(0,1))
#   axis(2,at=c(0,1))
#   
#   
#   
#   plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#        main='',xlab='',ylab='',
#        bty='n',ax=F)
#   
#   title(xlab='(measure)/adaptation',line=2)
#   title(ylab='relative density',line=2)
#   
#   text(-0.2,1.15,'B: distributions', font.main=1, cex=1.35*1.5, adj=0)
#   
#   # weights for rotations:
#   rotweights <- 1/(table(aim.df$rotation)/dim(aim.df)[1])
#   aim.df$weight.rot <- rotweights[as.character(aim.df$rotation)]
#   aim.df$weight.rot <- aim.df$weight.rot / sum(aim.df$weight.rot)
#   
#   for (var_idx in c(1,2)) {
#     
#     if (var_idx == 1) {dvar <- aim.df$norm.expl}
#     if (var_idx == 2) {dvar <- aim.df$norm.impl}
#     dens <- density(dvar, n=length(X), from=min(X), to=max(X), weights=aim.df$weight.rot)
#     col.op <- solidcolors[var_idx]
#     col.tr <- transcolors[var_idx]
#     
#     dx <- dens$x
#     #dy <- (dens$y / max(dens$y)) * 0.8
#     dy <- dens$y/2
#     
#     polygon(x= c(min(dx),dx,max(dx)),
#             y= c(0,dy,0),
#             border=NA,
#             col=col.tr)
#     
#     lines(x = dx, y=dy, col=col.op)
#     
#   }
#   
#   legend(0,1,legend=c('explicit (aiming)','implicit (exlcusion)'),col=solidcolors,lty=1,cex=1.0,bty='n',)
#   
#   axis(1,c(0,1))
#   axis(2,c(0,0.5,1),labels = c('0','1','2'))
#   
#   
#   
#   # # # # # # # # # # # # # #
#   
#   cols.op <- c()
#   
#   plot(-1000,-1000,xlim=c(-0.4,2.4),ylim=c(-0.4,2.4),
#        main='',xlab='',ylab='',
#        bty='n',ax=F,asp=1)
#   
#   title(xlab='predicted adaptation/rotation',line=2)
#   title(ylab='measured adaptation/rotation',line=2)
#   
#   # text(-0.2,1.15,'E: distribution of measures', font.main=1, cex=1.35*1.5, adj=0)
#   
#   text(-0.2,2.3,'C: models', font.main=1, cex=1.35*1.5, adj=0)
#   lines(c(0,2),c(0,2),col='#999999',lw=1,lty=1)
#   
#   MLdf <- MLE_adaptation(df=aim.df)
#   
#   # additive model:
#   a_hat <- MLdf$explicit + MLdf$implicit
#   
#   # par <- c('offset'=0)
#   mdf <- data.frame( 'a_hat'      = a_hat,
#                      'adaptation' = MLdf$adaptation,
#                      'rotation'   = MLdf$rotation)
#   
#   
#   
#   solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
#                    rgb(136, 153, 255, 255, max = 255))
#   transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
#                    rgb(136, 153, 255, 47,  max = 255))
#   col.op <- solidcolors[1]
#   col.tr <- transcolors[1]
#   cols.op <- c(cols.op, col.op)
#   
#   # predictions <- (a_hat+offset_fit$offset[1])/MLdf$rotation
#   predictions <- a_hat/MLdf$rotation
#   idx <- which(!is.na(predictions))
#   predictions <- predictions[idx]
#   adapt <- MLdf$adaptation[idx]/MLdf$rotation[idx]
#   points(predictions, adapt, col=col.tr)
#   
#   
#   at <- range(predictions)
#   add.p2a <- lm(adapt ~ predictions)
#   pcoef <- add.p2a$coefficients
#   lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#   
#   predict.points <- seq(at[1],at[2],length.out=40)
#   
#   ci <- predict( add.p2a,
#                  newdata=data.frame(predictions=predict.points),
#                  interval = "confidence")
#   
#   X <- c(predict.points,rev(predict.points))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   # # print(summary(add.p2a))
#   # print(pcoef)
#   # print(confint(add.p2a,'predictions',level=0.95))
#   
#   # maximum likelihood model
#   # a_hat <- MLdf$a_hat
#   a_hat <- (2 * MLdf$w_explicit) + (2 * MLdf$w_implicit)
#   
#   idx <- which(!is.na(a_hat))
#   
#   # par <- c('offset'=0)
#   mdf <- data.frame( 'a_hat'      = a_hat[idx],
#                      'adaptation' = MLdf$adaptation[idx],
#                      'rotation'   = MLdf$rotation[idx])
#   
#   
#   col.op <- solidcolors[2]
#   col.tr <- transcolors[2]
#   cols.op <- c(cols.op, col.op)
#   
#   adapt <- MLdf$adaptation[idx]/MLdf$rotation[idx]
#   predictions <- a_hat[idx]/MLdf$rotation[idx]
#   points(predictions, adapt, col=col.tr)
#   
#   
#   # MSE <- c('additive'=add_offset_MSE,
#   #          'MLE'=MLE_offset_MSE)
#   # 
#   # AICs <- AICc(MSE = MSE,            # MSE goodness of fit
#   #              k   = c(1,1),         # number of parameters (offset only)
#   #              N   = dim(MLdf)[1])   # N observations (127 participants)
#   # 
#   # print(relativeLikelihood(AICs))
#   
#   # # # # #
#   
#   at <- range(predictions)
#   mle.p2a <- lm(adapt ~ predictions)
#   pcoef <- mle.p2a$coefficients
#   lines(at, pcoef[1]+(at*pcoef[2]), col=col.op)
#   
#   predict.points <- seq(at[1],at[2],length.out=40)
#   
#   ci <- predict( mle.p2a,
#                  newdata=data.frame(predictions=predict.points),
#                  interval = "confidence")
#   
#   X <- c(predict.points,rev(predict.points))
#   Y <- c(ci[,'lwr'],rev(ci[,'upr']))
#   polygon(x=X,y=Y,col=col.tr,border=NA)
#   
#   # print(anova(add.p2a, mle.p2a))
#   
#   print(pcoef)
#   print(confint(mle.p2a,'predictions',level=0.95))
#   
#   
#   # # # # # 
#   
#   legend(x=-0.4,y=2.2,
#          legend=c(
#            expression(paste(hat(A)[k], ' = ', E[k], ' + ', I[k], ' (additive)') ),
#            expression(paste(hat(A)[k], ' = ', w['e,k'], E[k], ' + ', w['i,k'], I[k], ' (MLE)'))
#          ),
#          col=cols.op,cex=1.0,bty='n',pch=16)
#   
#   axis(side=1,at=c(0,1,2))
#   axis(side=2,at=c(0,1,2))
#   
#   
#   
#   
#   # # # # # # # # # # 3 # # 
#   # PDP BASED DATA
#   
#   X = seq(-0.1,1.1,0.01)  
#   
#   
#   plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#        main='',xlab='',ylab='',
#        bty='n',ax=F,asp=1)
#   
#   title(xlab='PDP difference/adaptation',line=2)
#   title(ylab='exclusion/adaptation',line=2)
#   
#   text(-0.2,1.15,'D: PDP difference', font.main=1, cex=1.35*1.5, adj=0)
#   
#   lines(c(0,0,1.1),c(1.1,0,0),col='#999999',lw=1,lty=1)
#   lines(c(-0.1,1.1),c(1.1,-0.1),col='#000000',lw=1,lty=1)
#   
#   # plot stuff with PDP differences
#   
#   PDP.df <- df[which(df$explicit.method == 'PDP.difference'),]
#   
#   points(PDP.df$norm.expl, PDP.df$norm.impl, pch=16, col=df$col.tr, cex=1)
#   
#   idx <- which(PDP.df$norm.expl > -0.2 & PDP.df$norm.expl < 1.2 & PDP.df$norm.impl > -0.2 & PDP.df$norm.impl < 1.2)
#   
#   addRegression(x=PDP.df$norm.expl[idx],
#                 y=PDP.df$norm.impl[idx],
#                 col='#999999',pch=NULL,alpha=34,cex=1)
#   
#   
#   axis(1,c(0,1))
#   axis(2,c(0,1))
#   
#   
#   
#   plot(-1000,-1000,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
#        main='',xlab='',ylab='',
#        bty='n',ax=F)
#   
#   title(xlab='exclusion/adaptation',line=2)
#   title(ylab='relative density',line=2)
#   
#   text(-0.2,1.15,'E: distributions', font.main=1, cex=1.35*1.5, adj=0)
#   
#   # weights for rotations:s
#   rotweights <- 1/(table(PDP.df$rotation)/dim(PDP.df)[1])
#   PDP.df$weight.rot <- rotweights[as.character(PDP.df$rotation)]
#   PDP.df$weight.rot <- PDP.df$weight.rot / sum(PDP.df$weight.rot)
#   
#   for (var_idx in c(1,2)) {
#     
#     if (var_idx == 1) {dvar <- PDP.df$norm.expl}
#     if (var_idx == 2) {dvar <- PDP.df$norm.impl}
#     dens <- density(dvar, n=length(X), from=min(X), to=max(X), weights=PDP.df$weight.rot)
#     col.op <- solidcolors[var_idx]
#     col.tr <- transcolors[var_idx]
#     lty <- 1
#     if (var_idx == 1) {
#       lty <- 2
#       col.tr <- NA
#     }
#     
#     dx <- dens$x
#     dy <- dens$y / 2
#     
#     polygon(x= c(min(dx),dx,max(dx)),
#             y= c(0,dy,0),
#             border=NA,
#             col=col.tr)
#     
#     print(lty)
#     
#     lines(x = dx, y=dy, col=col.op, lty=lty)
#     
#   }
#   
#   axis(1,c(0,1))
#   axis(2,c(0,0.5,1),labels = c('0','1','2'))
#   
#   legend(0,1,legend=c('explicit (PDP difference)','implicit (exlcusion)'),col=solidcolors,lty=c(2,1),cex=1.0,bty='n',)
#   
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
#   all.df <- rbind(aim.df, PDP.df)
#   
#   # print(summary(lm(norm.impl ~ explicit.method * rotation, data=all.df, weights=weight.rot)))
#   # print(summary(lm(norm.expl ~ explicit.method * rotation, data=all.df, weights=weight.rot)))
#   
#   # print(aggregate(norm.impl ~ rotation + explicit.method, data=df, FUN=mean))
#   a.df <- aggregate(cbind(norm.expl, norm.impl) ~ rotation + explicit.method, data=df, FUN=mean)
#   
#   print(a.df)
#   
#   plot(-1000,-1000,xlim=c(-0.05,1.05),ylim=c(-0.2,1.7),
#        main='',xlab='',ylab='',
#        bty='n',ax=F)
#   
#   title(ylab='proportion of adaptation',line=2)
#   title(xlab='rotation',line=2)
#   
#   text(-0.05,
#        (1.15/diff(c(-0.2,1.2)))*diff(c(-0.2,1.7)),
#        'F: build-up', font.main=1, cex=1.35*1.5, adj=0)
#   
#   lines(x=c(-0.05, 1.05), y=c(1,1), lty=2, col='#999999')
#   
#   for (method.idx in c(1:2)) {
#     
#     for (rot.idx in c(1:4)) {
#       
#       rotation <- c(15,30,45,60)[rot.idx]
#       method   <- c('aim.reports','PDP.difference')[method.idx]
#       
#       impl <- a.df$norm.impl[which(a.df$explicit.method == method & a.df$rotation == rotation)]
#       expl <- a.df$norm.expl[which(a.df$explicit.method == method & a.df$rotation == rotation)]
#       
#       x <- ((method.idx-1)/2) + ((rotation-15)/112.5) + (0.1 * (method.idx-1))
#       print(x)
#       
#       polygon( x = c(-0.06,0.06,0.06,-0.06)+x,
#                y = c( 0, 0, impl,impl),
#                border=solidcolors[2],
#                col=transcolors[2])
#       polygon( x = c(-0.06,0.06,0.06,-0.06)+x,
#                y = c( impl, impl, expl+impl,expl+impl),
#                border=solidcolors[1],
#                col=transcolors[1])
#       
#     }
#     
#   }
#   
#   legend(.6,1.5,c("explicit","implicit"),pch=22,bg=transcolors,col=solidcolors,cex=1,bty='n')
#   
#   axis(1, at= c(0,15,30,45)/112.5,     c('15','30','45','60'))
#   axis(1, at=(c(0,15,30,45)/112.5)+0.6,c('15','30','45','60'))
#   text(0.2,-0.1,'aiming')
#   text(0.8,-0.1,'PDP')
#   axis(2,at=c(0,0.5,1.0,1.5))
#   
# }
