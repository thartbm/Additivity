plotLearning <- function(target='inline') {
  
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
  
  legend(0,30,legend=groupnames,col=groupcols, bty='n', lty=1, cex=textsize)
  
  
  axis(side=2, at=c(0,15,30))
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}