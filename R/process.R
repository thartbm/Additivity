
# Processing Raw Data -----

getParticipantData <- function(group, participant) {
  
  # home-target distance in pixels: 433.548
  
  #cat(sprintf('data/%s/%s_p%03d.csv\n', group, group, participant))
  
  rawparticipantdf <- read.csv(sprintf('data/%s/%s_p%03d.csv', group, group, participant), stringsAsFactors = FALSE)  
  
  # booleans need to be logical variables:
  rawparticipantdf$showcursor_bool <- as.logical(rawparticipantdf$showcursor_bool)
  rawparticipantdf$doaiming_bool <- as.logical(rawparticipantdf$doaiming_bool)
  
  # replace position columns names: all position columns must end in "_pix"
  posvarnames <- c('cursorx','cursory','targetx','targety','mousex','mousey')
  for (pvn in posvarnames) {
    names(rawparticipantdf)[which(names(rawparticipantdf) == pvn)] <- sprintf('%s_pix',pvn)
  }
  
  # data frame for trimmed reach data:
  ppdf <- NA
  
  # cumulative trials to loop through
  unique.trials <- unique(rawparticipantdf$cutrial_no)
  
  for (trialno in unique.trials) {
    
    trialdf <- rawparticipantdf[which(rawparticipantdf$cutrial_no == trialno),]
    
    if (trialdf$showcursor_bool[1]) {
      targetReached <- 433.548*0.05
      untilHold <- NA
    } else {
      targetReached <- NA
      untilHold <- list('kind'='epoch-distance',
                        'mindist'=433.548*0.5,
                        'epoch'=249,
                        'threshold'=0.01*433.5484)
    }
    
    homeHoldEpoch <- 1999
    if (trialdf$doaiming_bool[1]) {
      homeHoldEpoch <- 499
    }
    
    trimdf <- trimReach(trialdf, 
                        homeStart = 'pr0.05',
                        holdHome=  list( 'epoch' = homeHoldEpoch,
                                         'distance' = 0.03125*433.5484),
                        targetReached = targetReached,
                        untilHold = untilHold,
                        device = 'cursor',
                        homepos = c(0,0),
                        velocity = NA )
    
    
    
    # homeHoldEpoch <- 2000
    # if (trialdf$doaiming_bool[1]) {
    #   homeHoldEpoch <- 500
    # }
    
    if (trialdf$showcursor_bool[1]) {

      reachdev <- Reach::getReachAngleAt( trimdf, 
                                          location = 'pr0.33333', 
                                          posunit = 'pix', 
                                          timeunit = 'ms', 
                                          device = 'mouse', 
                                          holdvelocity = NA, 
                                          holdduration = NA)
              
    } else {

      reachdev <- Reach::getReachAngleAt( trimdf, 
                                          location = 'endpoint', 
                                          posunit = 'pix', 
                                          timeunit = 'ms', 
                                          device = 'mouse', 
                                          holdvelocity = NA, 
                                          holdduration = NA)
      
    }
    
    rt <- getReactionTime(trimdf,
                          distance=433.548*0.10)
    
    if (rt <= 0) {rt <- NA}

    reachdev <- cbind(reachdev, matrix(rt - homeHoldEpoch - 1, ncol=1, nrow=1, dimnames=list(c(),c('RT_ms'))) )
    
    infodf <- data.frame('group' = group,
                         'participant' = participant,
                         'trial' = trialno,
                         'rotation' = abs(trimdf$rotation_deg[1]),
                         'doaim' = trimdf$doaiming_bool[1],
                         'aimdev' = trimdf$aimdeviation_deg[1],
                         'cursor' = trimdf$showcursor_bool[1],
                         'strategy' = trimdf$usestrategy_cat[1])
    reachdf <- data.frame(reachdev)
    
    if (is.data.frame(ppdf)) {
      ppdf <- rbind( ppdf, cbind(infodf, reachdf) ) 
    } else {
      ppdf <- cbind(infodf, reachdf)
    }
    
    # get aiming deviation (if any...)
    
    
    # if (is.data.frame(ppdf)) {
    #   # when at least one trial is already added
    #   ppdf <- rbind(ppdf, trialdf)
    # } else {
    #   # when it is still NA
    #   ppdf <- trialdf
    # }
  }
  
  
  return(ppdf)
  
}

processData <- function() {
  
  for (group in c('control', 'instructed', 'aiming')) {
    
    cat(sprintf('%s\n',toupper(group)))
    
    groupdf <- NA
    
    participants <- c(1:24)
    # if (group == 'aiming') {
    #   participants <- c(1:23)
    # }
    
    for (participant in participants) {
      
      cat(sprintf('%d... ',participant))
      
      ppdf <- getParticipantData(group=group, participant=participant)
      
      if (is.data.frame(groupdf)) {
        groupdf <- rbind(groupdf, ppdf)
      } else {
        groupdf <- ppdf
      }
      
    }
    cat('\n')
    write.csv(groupdf, sprintf('data/%s.csv',group), quote=F, row.names=F)
    
  }
  
}

#' @title Trim a data frame describing a reach to the interesting part. 
#' @param trialdf Data frame representing the reach with variables in columns
#' (named consistent with settings below) and samples in rows.
#' @param homeStart If the participant has to get to the home position before
#' starting the out-and-back reach, this part could be trimmed. Set `homeStart`
#' to a numeric value expressing how close the `device` has to be to the home
#' position, specified in the same unit as `posunit`. The start of the trial
#' will be trimmed (not returned) up to when the device is that close to the 
#' start/home position. By default this is NA, so that this part is not 
#' trimmed. If this is a character variable, starting with "pr" and ending with
#' numbers, those numbers should indicate a proportion of the home-to-target
#' distance to use as cutoff value for trimming the first part of the reach.
#' @param targetReached If the return movement is represented in the data, this
#' may have to be trimmed as well. This parameter sets the *distance* at which 
#' the target is considered reached, and data after this point is trimmed.
#' The target position should be in columns named "targetx_[posunit]" and 
#' "targety_[posunit]". This argument is a numeric variable given in the 
#' position unit specified later on.
#' @param velocity Very slow movement is usually not diagnostic. _After_ the
#' other parts of the data are trimmed, the instantaneous velocity is used as
#' a cut-off criterion: the first part of the reach that is under the velocity
#' criterion as a fraction of the maximum velocity in the whole reach, is 
#' trimmed. And either the final part that is below the velocity criterion is
#' trimmed, or everything after the first dip below the velocity criterion, 
#' depending on the `firstMove` parameter. Set to `NA` for no velocity 
#' criterion. By default this is set conservatively to 0.05.
#' 
#' May give unexpected results if used with `untilHold`.
#' @param firstMove Only used if the `velocity` parameter is not `NA`. If set 
#' to TRUE, the first part of the trajectory up to where it dips below the 
#' velocity criterion is kept (the rest is trimmed). If FALSE, only the final
#' part of the trajectory that goes below the velocity criterion is trimmed.
#' 
#' May give unexpected results if used in combination with `untilHold`.
#' @param holdHome Not used if set to `NA` (default). Otherwise, this should
#' be a list with two named entries:
#' 
#' "distance": numeric, the maximum distance the device (see below) can be from
#' the home position to count as a hold, set in position units
#' 
#' "epoch": numeric, the amount of time the device has to be closer than the
#' distance criterion from the home position, for the hold to be completed, set
#' in time units as described below
#' 
#' May give unexpected results if used with any velocity criterion or with
#' homeStart.
#' @param untilHold Not used if set to `NA` (default). Otherwise, this should
#' be a list with four named entries:
#' 
#' "kind": character setting one of (currently) two ways to determine a hold,
#' can be one of "sample-velocity" or "epoch-distance". When it is
#' "sample-velocity", a sequence of samples spanning the hold epoch all should
#' have velocity below the threshold value. When it is "epoch-distance" the 
#' total distance moved during the epoch should be below the threshold value.
#' 
#' "mindist": numeric: minimum distance from home that the hold has to occur 
#' at, given in position units as set below
#' 
#' "threshold": numeric setting maximum velocity or distance in position and
#' time units as set below
#'   
#' "epoch": numeric duration of the hold in time units specified below
#' 
#' All data _after_ the hold is trimmed, but the hold itself is not.
#' 
#' May give unexpected results if used in combination with `firstMove` or any
#' `velocity` criterion.
#' @param device The position columns to use are given by "[device]x_[posunit]"
#' in the `trialdf`, and similar for y. Can be something like 'hand', 'cursor',
#' 'mouse', 'stylus' or 'robot'.
#' @param posunit The unit used for the x and y position data. Could be "pix"
#' or "cm", or whatever is used in the data. Default: "pix".
#' @param timeunit The unit used for the time stamps of each sample. The column
#' names is "time_[timeunit]". Default: "ms"
#' @param homepos The coordinates of the home position. Default is (0,0).
#' @return Data frame describing the reach, minus the trimmed parts.
#' @description
#' ?
#' @details
#' ?
#' @examples
#' ?
#' @export
trimReach <- function(trialdf, homeStart=NA, targetReached=NA, velocity=0.05, firstMove=FALSE, holdHome=NA, untilHold=NA, device='hand', posunit='pix', timeunit='ms', homepos=c(0,0)) {
  
  targetposition <- as.numeric( trialdf[ 1, c( sprintf('targetx_%s', posunit ), sprintf( 'targety_%s', posunit ) ) ] )
  targetposition <- targetposition - homepos
  targetdistance <- sqrt( sum( targetposition^2 ) )
  
  nsamples <- dim(trialdf)[1]
  #cat(sprintf('** start with %d samples\n',nsamples))
  # cat('-----\n')
  
  if (!is.na(homeStart)) {
    
    # we need the device position, relative to the home position
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    
    if (is.numeric(homeStart)) {
      cutoff <- homeStart
    }
    
    if (is.character((homeStart))) {
      
      # cutoff at a percentage from home to target in whatever unit is used
      if (substring(homeStart,1,2) == 'pr') {
        
        cutoff <- as.numeric(substring(homeStart, 3))
        cutoff <- cutoff * targetdistance
        
      }
      
    }
    
    # get the distance from home:
    devicedist <- sqrt(x^2 + y^2)
    
    if (devicedist[1] > cutoff) {
      
      # cat('first sample too far from home\n')
      
      # find the first sample, where device is closer to home than the cutoff:
      if (any(devicedist < cutoff)) {
        rown <- max(1, min(which(devicedist < cutoff))-1) # why the minus one?
        trialdf <- trialdf[c(rown:dim(trialdf)[1]),]
      }
      
    }
    
    # if (dim(trialdf)[1]<nsamples) {
    #   newsamples <- dim(trialdf)[1]
    #   cat(sprintf('homeStart cuts %d samples\n', nsamples-newsamples))
    # }
    
  }
  
  if (!is.na(targetReached) && is.numeric(targetReached)) {
    
    # we need the trajectroy and device position, relative to the home position
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    targetx <- trialdf[1,sprintf('targetx_%s',posunit)] - homepos[1]
    targety <- trialdf[1,sprintf('targety_%s',posunit)] - homepos[1]
    
    # distance to target for every sample:
    dist <- sqrt((x - targetx)^2 + (y - targety)^2)
    
    crit <- which(dist < targetReached)
    # we only trim if the target is actually reached:
    if (length(crit) > 0) {
      trialdf <- trialdf[c(1:crit[1]),]
    }
    
  }
  
  if (!is.na(holdHome) && is.list(holdHome) && (length(holdHome) == 2)) {
    epoch <- holdHome$epoch
    distance <- holdHome$distance
    
    # first we get the necessary variables from the data frame:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    
    # only look within distance:
    didx <- which(sqrt(x^2 + y^2) < distance & sample_time >= epoch)
    
    for (sample.idx in didx) {
      tidx <- which((sample_time[c(1:sample.idx)]-sample_time[sample.idx]) > (-epoch))
      
      if (all(sqrt(x[tidx]^2 + y[tidx]^2) < distance)) {
        # the trial now includes the hold period:
        trialdf <- trialdf[c(min(tidx):dim(trialdf)[1]),]
        break() # break out of the for-loop
      }
    }
    
  }
  
  if (!is.na(untilHold) && is.list(untilHold) && (length(untilHold) == 4)) {
    
    # here we use sample-to-sample velocity as used during the experiment
    # (so no smoothed / splined trajectory)
    
    # first we get the necessary variables from the data frame:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    
    if (untilHold$kind == 'sample-velocity') {
      # calculate instantaneous velocity:
      velocity <- c(0,sqrt(diff(x)^2 + diff(y)^2) / diff(sample_time))
      print(velocity)
      # which samples are below the velocity criterion:
      belowcriterion <- which(velocity < untilHold$threshold)
      
      # this might be helpful for non-averaged hold criterion algorithms:
      bc_runs <- rle(belowcriterion) # no idea how to continue...
      
      # STUFF: NOT COMPLETE!
      cat('sample-velocity method of determining a hold is not complete:\nnot trimming\n')
      
    }
    
    if (untilHold$kind == 'epoch-distance') {
      
      # only look beyond mindist:
      didx <- which(sqrt(x^2 + y^2) > untilHold$mindist)
      
      for (sample.idx in didx) {
        tidx <- which((sample_time[c(1:sample.idx)]-sample_time[sample.idx]) > (-untilHold$epoch))
        # print(sum(sqrt(diff(x[tidx])^2 + diff(y[tidx])^2)))
        if (sum(sqrt(diff(x[tidx])^2 + diff(y[tidx])^2)) < untilHold$threshold) {
          # the trial now includes the hold period:
          trialdf <- trialdf[c(1:max(tidx)),]
          break() # break out of the for-loop
        }
      }
      
    }
    
  }
  
  if (!is.na(velocity)) {
    
    # here we use a spline smoothed velocity signal:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    SplVel <- getSplinedVelocity(x=x, y=y, t=sample_time)
    velocity <- SplVel$velocity
    spline_time <- SplVel$time
    
    # determine numeric velocity threshold
    
    # see which samples are below this threshold
    if (!is.na(velocity)) {
      if (is.logical(firstMove) && firstMove) {
        
        # trim after first dip below threshold (after first going above it)
        
      } else {
        
        # trim after last dip below threshold (after first going above it) 
        
      }
      cat('velocity trimming is not completed\nnot trimming\n')
    }
    
  }
  
  return(trialdf)
  
}

getReactionTime <- function(trialdf, distance) {
  
  X <- trialdf$cursorx_pix
  Y <- trialdf$cursory_pix
  t <- trialdf$time_ms
  t <- t - t[1]
  
  idx <- which(sqrt(X^2 + Y^2) > distance)[1]
  return(t[idx])
  
}

# Secondary Processing -----


baseline <- function(reachvector,blidx) {
  
  return(reachvector - mean(reachvector[blidx], na.rm=TRUE))
  
}

removeOutliers <- function(reachvector,sds=3) {
  
  mu <- mean(reachvector, na.rm=TRUE)
  sigma <- sd(reachvector, na.rm=TRUE)
  
  reachvector[ which(abs(reachvector-mu) > (sigma*sds)) ] <- NA
  
  return(reachvector)
  
}

getTrainingReaches <- function(df, depvar='reachdeviation_deg') {
  
  # retain only the lines with training reaches:
  df <- df[which(df$cursor == TRUE),]
  
  # get the number of participants
  participants <- unique(df$participant)
  N <- length(participants)
  
  # -- trial-by-trial data --
  
  # put reaches in matrix:
  reachmat <- matrix(data=df[,depvar], ncol=N, nrow=(dim(df)[1]/N), byrow=F)
  
  preNAs <- length(which(is.na(reachmat)))
  
  # remove exessively large reach deviations
  reachmat[abs(reachmat) > 60] <- NA
  
  post60NAs <- length(which(is.na(reachmat)))
  
  # remove outliers:
  reachmat <- t(apply(reachmat, FUN=removeOutliers, MARGIN=c(1), sds=3))
  
  postSDSNAs <- length(which(is.na(reachmat)))
  
  cat(sprintf('pre NAs: %d - 60 NAs: %d - sd NAs: %d - (length: %d)\n', preNAs, post60NAs, postSDSNAs, length(reachmat)))
  
  NAs <- matrix(is.na(reachmat), nrow=dim(reachmat)[1], ncol=dim(reachmat)[2])
  pNAs <- colSums(NAs)
  cat(sprintf('max trials removed: %d - participants with no removals: %d\n', max(pNAs), length(which(pNAs == 0))))
  
  # baseline correction:
  reachmat <- apply(reachmat, FUN=baseline, MARGIN=c(2), blidx=c(17:32))
  
  # get descriptive statistics:
  average <- apply(reachmat, FUN=mean, MARGIN=c(1), na.rm=TRUE)
  
  # get 95% confidence intervals:
  CI <- apply(reachmat, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
  
  # make data frame:
  reachdevs <- data.frame(reachmat)
  names(reachdevs) <- sprintf('p%03d',participants)
  
  # add descriptives:
  reachdevs$average <- average
  reachdevs$CI.lo <- CI[1,]
  reachdevs$CI.hi <- CI[2,]
  
  # get columns with other info
  infocols <- df[which(df$participant == participants[1]),c('trial','rotation')]
  
  # -- blocked data --
  
  # get a block column (this will end up in the info-columns):
  df$block <- floor((df$trial-1) / 8) + 1
  Nblocks <- length(unique(df$block))
  
  # use reachmat matrix to get reachmat_b?
  reacharr <- array(data=c(reachmat), dim=c(8,Nblocks,N))
  reachmat_b <- apply(reacharr, FUN=mean, MARGIN=c(2,3), na.rm=TRUE)
  
  # get descriptive statistics:
  average_b <- apply(reachmat_b, FUN=mean, MARGIN=c(1), na.rm=TRUE)
  
  # get 95% confidence intervals:
  CI_b <- apply(reachmat_b, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
  
  # make data frame:
  reachdevs_b <- data.frame(reachmat_b)
  names(reachdevs_b) <- sprintf('p%03d',participants)
  
  # add descriptives:
  reachdevs_b$average <- average_b
  reachdevs_b$CI.lo <- CI_b[1,]
  reachdevs_b$CI.hi <- CI_b[2,]
  
  # get columns with other info
  infocols_b <- df[which(df$participant == participants[1] & df$trial %in% seq(1,264,8)),c('block','rotation')]
  
  # return everything in PyVMEC-style data frame
  return(list('trials' = cbind(infocols,   reachdevs),
              'blocks' = cbind(infocols_b, reachdevs_b)))
  
}


getReactionTimes <- function(df, depvar='reachdeviation_deg') {
  
  # retain only the lines with training reaches:
  df <- df[which(df$cursor == TRUE),]
  
  # get the number of participants
  participants <- unique(df$participant)
  N <- length(participants)
  
  # -- trial-by-trial data --
  
  # put reaches in matrix:
  reachmat <- matrix(data=df[,depvar], ncol=N, nrow=(dim(df)[1]/N), byrow=F)
  
  # remove exessivelylarge reach deviations
  reachmat[abs(reachmat) > 60] <- NA
  
  # remove outliers:
  reachmat <- t(apply(reachmat, FUN=removeOutliers, MARGIN=c(1), sds=3))
  
  # baseline correction:
  reachmat <- apply(reachmat, FUN=baseline, MARGIN=c(2), blidx=c(17:32))
  
  # get descriptive statistics:
  average <- apply(reachmat, FUN=mean, MARGIN=c(1), na.rm=TRUE)
  
  # get 95% confidence intervals:
  CI <- apply(reachmat, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
  
  # make data frame:
  reachdevs <- data.frame(reachmat)
  names(reachdevs) <- sprintf('p%03d',participants)
  
  # add descriptives:
  reachdevs$average <- average
  reachdevs$CI.lo <- CI[1,]
  reachdevs$CI.hi <- CI[2,]
  
  # get columns with other info
  infocols <- df[which(df$participant == participants[1]),c('trial','rotation')]
  
  # -- blocked data --
  
  # # get a block column (this will end up in the info-columns):
  # df$block <- floor((df$trial-1) / 8) + 1
  # Nblocks <- length(unique(df$block))
  # 
  # # use reachmat matrix to get reachmat_b?
  # reacharr <- array(data=c(reachmat), dim=c(8,Nblocks,N))
  # reachmat_b <- apply(reacharr, FUN=mean, MARGIN=c(2,3), na.rm=TRUE)
  # 
  # # get descriptive statistics:
  # average_b <- apply(reachmat_b, FUN=mean, MARGIN=c(1), na.rm=TRUE)
  # 
  # # get 95% confidence intervals:
  # CI_b <- apply(reachmat_b, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
  # 
  # # make data frame:
  # reachdevs_b <- data.frame(reachmat_b)
  # names(reachdevs_b) <- sprintf('p%03d',participants)
  # 
  # # add descriptives:
  # reachdevs_b$average <- average_b
  # reachdevs_b$CI.lo <- CI_b[1,]
  # reachdevs_b$CI.hi <- CI_b[2,]
  # 
  # # get columns with other info
  # infocols_b <- df[which(df$participant == participants[1] & df$trial %in% seq(1,264,8)),c('block','rotation')]
  
  # return everything in PyVMEC-style data frame
  # return(list('trials' = cbind(infocols,   reachdevs),
  #             'blocks' = cbind(infocols_b, reachdevs_b)))
  
  return(list('trials' = cbind(infocols,   reachdevs)))
  
}

saveTrainingdata <- function() {
  
  # PyVMEC style output:
  
  for (group in c('control','instructed','aiming')) {
    
    df <- read.csv(sprintf('data/%s.csv',group), stringsAsFactors = FALSE)
    
    rdf <- getTrainingReaches(df)
    
    write.csv(rdf[['trials']], sprintf('data/%s-training-trials.csv',group), quote=FALSE, row.names=FALSE)
    write.csv(rdf[['blocks']], sprintf('data/%s-training-blocks.csv',group), quote=FALSE, row.names=FALSE)
    
  }
  
  df <- read.csv('data/aiming.csv', stringsAsFactors = FALSE)
  
  adf <- getTrainingReaches(df, depvar='aimdev')
  
  write.csv(adf[['trials']], 'data/aiming-aim-trials.csv', quote=FALSE, row.names=FALSE)
  write.csv(adf[['blocks']], 'data/aiming-aim-blocks.csv', quote=FALSE, row.names=FALSE)
  
  
}

saveNoCursorData <- function() {
  
  for (group in c('control','instructed','aiming')) {
    
    # print(group)
    
    df <- read.csv(sprintf('data/%s.csv',group), stringsAsFactors = FALSE)
    
    ndf <- getNoCursors(df)
    
    #print(ndf)
    
    write.csv(ndf[['all']], sprintf('data/%s-nocursors-all.csv',group), quote=FALSE, row.names=FALSE)
    write.csv(ndf[['blocks']], sprintf('data/%s-nocursors-blocks.csv',group), quote=FALSE, row.names=FALSE)
    
  }
  
}

getNoCursors <- function(df,FUN=median) {
  
  df <- df[which(df$cursor == FALSE),]
  
  # get a block column... for blocked output
  df$block <- floor((df$trial-1) / 8) + 1
  # to counter counterbalancing, we line stuff up by "superblock" though:
  df$superblock <- floor((df$block) / 2) + 1
  
  # first the overall no-cursor stuff:
  
  # get the number of participants
  participants <- unique(df$participant)
  N <- length(participants)
  
  # overall data:
  avg_long <- aggregate(reachdeviation_deg ~ strategy + participant, data=df, FUN=FUN, na.rm=TRUE)
  
  # matrix
  nc_mat <- matrix(data=avg_long$reachdeviation_deg, ncol=N, nrow=3, byrow=FALSE)
  
  # data frame
  avg_wide <- data.frame(nc_mat)
  colnames(avg_wide) <- sprintf('p%03d',participants)
  
  # get descriptive statistics:
  average <- apply(nc_mat, FUN=mean, MARGIN=c(1), na.rm=TRUE)
  
  # get 95% confidence intervals:
  CI <- apply(nc_mat, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
  
  # add descriptives:
  avg_wide$average <- average
  avg_wide$CI.lo <- CI[1,]
  avg_wide$CI.hi <- CI[2,]
  
  # add first column with condition names:
  avg_wide <- cbind( data.frame( 'condition'=avg_long$strategy[which(avg_long$participant == participants[1])]), avg_wide )
  
  
  
  # now split by block...
  
  # blocked data:
  avg_long_b <- aggregate(reachdeviation_deg ~ strategy + superblock + participant, data=df, FUN=FUN, na.rm=TRUE)
  
  # convert super-block number to iteration:
  lookup <- data.frame('superblock'=c(3,5,6,13,15,17), 'iteration'=c(1,2,3,1,2,3))
  avg_long_b$iteration <- lookup$iteration[match(avg_long_b$superblock, lookup$superblock)]
  
  # get medians per block:
  avg_long_b <- aggregate(reachdeviation_deg ~ participant + iteration + strategy, data=avg_long_b, FUN=FUN, na.rm=TRUE)
  
  # matrix (keep for descriptives):
  nc_mat_b <- matrix(data=avg_long_b$reachdeviation_deg, ncol=N, nrow=3*3, byrow=TRUE)
  
  # start making data frame:
  avg_wide_b <- data.frame(nc_mat_b)
  
  colnames(avg_wide_b) <- sprintf('p%03d', participants)
  
  # descriptives:
  average_b <- apply(nc_mat_b, FUN=mean, MARGIN=c(1), na.rm=TRUE)
  
  # get 95% confidence intervals:
  CI_b <- apply(nc_mat_b, FUN=Reach::getConfidenceInterval, MARGIN=c(1))
  
  # add descriptives:
  avg_wide_b$average <- average_b
  avg_wide_b$CI.lo <- CI_b[1,]
  avg_wide_b$CI.hi <- CI_b[2,]
  
  # info columns
  infocols <- data.frame( 'condition' = rep(unique(avg_long_b$strategy[which(avg_long_b$participant == participants[1])]), each=3),
                          'iteration' = rep(c(1,2,3), 3) )
  
  avg_wide_b <- cbind(infocols, avg_wide_b)
  
  return(list('all'    = avg_wide,
              'blocks' = avg_wide_b))
  
}

# Modchalingam -----

getStepwiseData <- function() {

  all_adaptation <- read.csv('data/Modchalingam/shanaa_adaptation.csv', stringsAsFactors = F)
  all_nocursor   <- read.csv('data/Modchalingam/shanaa_nocursor.csv', stringsAsFactors = F)
  
  stepwise <- NA
  
  for (group in c('abrupt','ramped','stepwise')) {
    
    participants <- switch(group,
                           'abrupt'=sprintf('%03d_longAbruptExp',c(1:37)),
                           'ramped'=sprintf('%03d_gradualExp',c(1:33)),
                           'stepwise'=sprintf('%d_stepwiseExp',c(1:38)))
    
    adaptation <- all_adaptation[which(all_adaptation$ppt %in% participants),]
    nocursor   <- all_nocursor[  which(all_nocursor$ppt   %in% participants),]
    
    # list of 9 trials just before each no-cursor block:
    if (group == 'stepwise') {
      precede_nc <- c(37:45, 76:84, 139:147, 178:186, 241:249, 280:288, 343:351, 382:390)
    } else {
      precede_nc <- c(343:351, 382:390)
      nocursor <- nocursor[which(nocursor$trial_num_cont > 351),]
    }
    
    #print(str(adaptation))
    #print(str(nocursor))
    
    # select trials:
    adaptation <- adaptation[which(adaptation$trial_num_cont %in% precede_nc),]
    # average per target, participant and rotation:
    adaptation <- aggregate(angular_dev ~ rotation_angle + targetangle_deg + ppt, data=adaptation, FUN=mean)
    # average per participant and rotation:
    adaptation <- aggregate(angular_dev ~ rotation_angle + ppt, data=adaptation, FUN=mean)
    # rename variable:
    names(adaptation)[which(names(adaptation) == 'angular_dev')] <- 'adaptation'
    
    #print(str(adaptation))
    
    # same for exclude:
    exclude <- nocursor[which(nocursor$strat_use == 0),]
    exclude <- aggregate(angular_dev ~ rotation_angle + targetangle_deg + ppt, data=exclude, FUN=mean)
    exclude <- aggregate(angular_dev ~ rotation_angle + ppt, data=exclude, FUN=mean)
    names(exclude)[which(names(exclude) == 'angular_dev')] <- 'exclude'
    
    #print(str(exclude))
    
    # and for include:
    include <- nocursor[which(nocursor$strat_use == 1),]
    include <- aggregate(angular_dev ~ rotation_angle + targetangle_deg + ppt, data=include, FUN=mean)
    include <- aggregate(angular_dev ~ rotation_angle + ppt, data=include, FUN=mean)
    names(include)[which(names(include) == 'angular_dev')] <- 'include'
    
    #print(str(include))
    
    # merge into 1 data frame:
    group_df <- merge(adaptation, exclude)
    group_df <- merge(group_df, include)
    
    #print(group_df)
    
    group_df$group <- group
    
    if (is.data.frame(stepwise)) {
      stepwise <- rbind(stepwise, group_df)
    } else {
      stepwise <- group_df
    }
    
  }
  
  return(stepwise)
  
  # adaptation <- read.csv('data/Modchalingam/shanaa_adaptation.csv', stringsAsFactors = F)
  # nocursor   <- read.csv('data/Modchalingam/shanaa_nocursor.csv', stringsAsFactors = F)
  # 
  # # list of 9 trials just before each no-cursor block:
  # precede_nc <- c(37:45, 76:84, 139:147, 178:186, 241:249, 280:288, 343:351, 382:390)
  # 
  # # select trials:
  # adaptation <- adaptation[which(adaptation$trial_num_cont %in% precede_nc),]
  # # average per target, participant and rotation:
  # adaptation <- aggregate(angular_dev ~ rotation_angle + targetangle_deg + ppt, data=adaptation, FUN=mean)
  # # average per participant and rotation:
  # adaptation <- aggregate(angular_dev ~ rotation_angle + ppt, data=adaptation, FUN=mean)
  # # rename variable:
  # names(adaptation)[which(names(adaptation) == 'angular_dev')] <- 'adaptation'
  # 
  # # same for exclude:
  # exclude <- nocursor[which(nocursor$strat_use == 0),]
  # exclude <- aggregate(angular_dev ~ rotation_angle + targetangle_deg + ppt, data=exclude, FUN=mean)
  # exclude <- aggregate(angular_dev ~ rotation_angle + ppt, data=exclude, FUN=mean)
  # names(exclude)[which(names(exclude) == 'angular_dev')] <- 'exclude'
  # 
  # # and for include:
  # include <- nocursor[which(nocursor$strat_use == 1),]
  # include <- aggregate(angular_dev ~ rotation_angle + targetangle_deg + ppt, data=include, FUN=mean)
  # include <- aggregate(angular_dev ~ rotation_angle + ppt, data=include, FUN=mean)
  # names(include)[which(names(include) == 'angular_dev')] <- 'include'
  # 
  # # merge into 1 data frame:
  # stepwise <- merge(adaptation, exclude)
  # stepwise <- merge(stepwise, include)
  # 
  # return(stepwise)

}

# Neville & Cressman -----

# data in the csv files we got has to be processed in some way to get it into the format we'd like
# participants have unique numbers across all 6 groups, which saves a lot of trouble

# we want the blocks right after training (I guess... so T1, the average across targets)
# we want the actual values, so variable 1

# getNevilleCressmanData <- function() {
#   
#   group <- c() # same rotation + instruction (6 groups)
#   participant <- c()
#   implicit <- c()
#   explicit <- c()
#   include <- c()
#   exclude <- c()
#   adaptation <- c()
#   
#   # I think the groups are people who got instructed (explicit) or not (implicit)
#   
#   dataframes <- list()
#   
#   for (depvar in c('Explicit', 'Implicit')) {
#     
#     # Variable: 1=absolute (degrees?)
#     # Variable: 2=% of adaptation? 
#     # Strategy: 1=non-strategy, 2=strategy (1=control, 2=instructed)
#     
#     df <- read.csv(sprintf('data/Neville and Cressman/%s.csv', depvar), stringsAsFactors = FALSE)
#     #df$reachdev    <- (df$PDP3_Avg_T1	+ df$PDP5_Avg_T1 + df$PDP7_Avg_T1)/3
#     df$reachdev    <- df$PDP7_Avg_T1
#     
#     df$rotation    <- df$Rotation * 20
#     df$instruction <- c('control','instructed')[df$Strategy]
#     
#     df$group       <- sprintf('%s%d',c('control','instructed')[df$Strategy], df$Rotation * 20)
#     
#     df_proportion  <- df[which(df$Variable == 2),]
#     
#     df             <- df[which(df$Variable == 1),]
#     
#     df$adaptation  <- df$reachdev / df_proportion$reachdev
#     
#     df$participant <- df$Subject
#     
#     df <- df[,c('participant','reachdev','rotation','instruction','group','adaptation')]
#     
#     dataframes[[depvar]] <- df
#     
#   }
#   
#   
#   group <- c() # same rotation + instruction (6 groups)
#   participant <- c()
#   implicit <- c()
#   explicit <- c()
#   include <- c()
#   exclude <- c()
#   adaptation_e <- c()
#   adaptation_i <- c()
#   
#   df <- dataframes[['Implicit']][,c('participant','group','rotation','instruction')]
#   df$exclude <- dataframes[['Implicit']]$reachdev
#   df$include <- dataframes[['Implicit']]$reachdev + dataframes[['Explicit']]$reachdev
#   
#   df$adaptation_i <- dataframes[['Implicit']]$adaptation
#   df$adaptation_e <- dataframes[['Explicit']]$adaptation
#   
#   plot(df$adaptation_i, df$adaptation_e)
#   
#   return(df)
# 
# }

# getNevilleCressmanData <- function() {
#   
#   
#   dfs <- list()
#   
#   for (depvar in c('Explicit', 'Implicit')) {
#     
#     # Variable: 1=absolute (degrees?)
#     # Variable: 2=% of adaptation? 
#     # Strategy: 1=non-strategy, 2=strategy (1=control, 2=instructed)
#     
#     df <- read.csv(sprintf('data/Neville and Cressman/%s.csv', depvar), stringsAsFactors = FALSE)
#     dfs[[depvar]] <- df
#     
#   }
#   
#   group <- c() # same rotation + instruction (6 groups)
#   rotation <- c()
#   instruction <- c()
#   participant <- c()
#   explicit <- c()
#   exclude <- c()
#   include <- c()
#   adaptation_imp <- c()
#   adaptation_exp <- c()
#   
#   # I think the groups are people who got instructed (explicit) or not (implicit)
#   participants <- sort(intersect(unique(dfs[['Implicit']]$Subject),unique(dfs[['Explicit']]$Subject)))
#   
#   #print(participants)
#   #print(length(participants))
#   
#   for (ppno in participants) {
#     
#     print(ppno)
#     
#     idx_abs <- which(dfs[['Implicit']]$Subject == ppno & dfs[['Implicit']]$Variable == 1)
#     excl <- dfs[['Implicit']]$PDP7_Avg_T1[idx_abs]
#     
#     idx_prp <- which(dfs[['Implicit']]$Subject == ppno & dfs[['Implicit']]$Variable == 2)
#     i_adapt <- excl / dfs[['Implicit']]$PDP7_Avg_T1[idx_prp]
#     
#     print(c(excl,i_adapt))
#     
#     idx_abs <- which(dfs[['Explicit']]$Subject == ppno & dfs[['Explicit']]$Variable == 1)
#     expl <- dfs[['Explicit']]$PDP7_Avg_T1[idx_abs]
#     
#     idx_prp <- which(dfs[['Explicit']]$Subject == ppno & dfs[['Explicit']]$Variable == 2)
#     e_adapt <- expl / dfs[['Explicit']]$PDP7_Avg_T1[idx_prp]
#     
#     print(c(expl,e_adapt))
#     
#     rotat <- dfs[['Explicit']]$Rotation[idx_abs] * 20
#     instr <- c('control','instructed')[dfs[['Explicit']]$Strategy[idx_abs]]
#     
#     grp   <- sprintf('%s%d',instr, rotat)
#     
#     print(grp)
#     
#     participant <- c(participant, ppno)
#     rotation    <- c(rotation, rotat)
#     instruction <- c(instruction, instr)
#     group       <- c(group, grp)
#     
#     explicit    <- c(explicit, expl)
#     exclude     <- c(exclude, excl)
#     include     <- c(include, expl+excl)
#     
#     adaptation_imp <- c(adaptation_imp, i_adapt)
#     adaptation_exp <- c(adaptation_exp, e_adapt)
#     
#   }
#   
#   return(data.frame(participant,
#                     rotation,
#                     instruction,
#                     group,
#                     explicit,
#                     exclude,
#                     include,
#                     adaptation_imp,
#                     adaptation_exp))
#   
# }

# Schween et al -----

collectSchweenData <- function() {
  
  experiment  <- c()
  participant <- c()
  #group       <- c()
  workspace   <- c()
  target      <- c()
  implicit    <- c()
  adaptation  <- c()
  explicit    <- c()
  
  targets <- seq(180,0,-22.5)
  
  for (exp in c(1,2)) {
    
    expname <- c('B5_nostrat','A1_hands')[exp]
    
    #groups <- read.csv(sprintf('data/Schween/%s_groups_neu.csv', expname), stringsAsFactors = F)
    #colnames(groups) <- c('group')
    #targets         <- read.csv(sprintf('data/Schween/%s_targetAngles_neu.csv', expname), stringsAsFactors = F)
    reachdeviations <- read.csv(sprintf('data/Schween/%s_postTests_neu.csv', expname), stringsAsFactors = F)
    
    
    ppnos <- c(1:(20+(2-exp)))
    
    for (ppno in ppnos) {
      
      #groupno <- groups$group[ppno]
      
      for (ws in c('left', 'right')) {
        
        target_idx <- c(1:9) + list('left'=10, 'right'=20 )[[ws]]
        
        for (target_id in target_idx) {
          
          for (depvar in c('imp','tot','exp')) {
            
            colname <- sprintf('post_%s_%d',depvar,target_id)
            
            depval <- reachdeviations[ppno,colname]
            
            if (depvar == 'imp') {
              implicit <- c(implicit, depval)
            }
            if (depvar == 'tot') {
              adaptation <- c(adaptation, depval)
            }
            if (depvar == 'exp') {
              explicit <- c(explicit, depval)
            }
            
          }
          
          experiment  <- c(experiment, c('one','two')[exp])
          participant <- c(participant, ppno)
          #group       <- c(group, groupno)
          workspace   <- c(workspace, ws)
          target      <- c(target, targets[target_id %% 10])
          
        }
        
      }
      
    }
    
  }
  
  return( data.frame(experiment,
                     participant,
                     #group,
                     workspace,
                     target,
                     implicit,
                     adaptation,
                     explicit) )
  
}


summarizeSchweenData <- function() {
  
  df2 <- collectSchweenData()
  
  experiment  <- c()
  participant <- c()
  #group       <- c()
  workspace   <- c()
  implicit    <- c()
  explicit    <- c()
  adaptation  <- c()
  
  
  for (exp in unique(df2$experiment)) {
    
    imp_targets <- list( 'one'=list('left'=c(112.5, 135.0), 'right'=c(67.5, 45.0)),
                         'two'=list('left'=c( 90,   112.5), 'right'=c(90,   67.5)) )[[exp]]
  
    df <- df2[which(df2$experiment == exp),]
    participants <- unique(df$participant)
    
    for (ppno in participants) {
      
      for (ws in c('left','right')) {
        
        idx <- which(df$participant == ppno & df$workspace == ws)
        
        data <- data.frame( 'x' = df$target[idx],
                            'y' = df$implicit[idx] )
        data <- data[which(!is.nan(data$y)),]
        
        experiment  <- c(experiment, exp)
        participant <- c(participant, ppno)
        #group       <- c(group, df$group[idx[1]])
        workspace   <- c(workspace, df$workspace[idx[1]])
        
        # fitting a sine to individual participants' data is not possible:
        
        # par = c('phase'  = 90/pi,
        #         'mult'   = 15,
        #         'offset' = 0)
        # fit <- optim(par    = par,
        #              fn     = sineMSE,
        #              method = "L-BFGS-B",
        #              lower  = c(0.1,     0.1, -45),
        #              upper  = c(180/pi, 90,    45),
        #              data   = data)
        # print(fit)
        # 
        # plot(data$x, data$y)
        # dat <- data.frame('x' = seq(0,180,0.5))
        # lines(dat$x, sinePredict(par=fit$par, data=dat))
        
        # instead, we will use the target with the average largest value
        
        implicit    <- c(implicit, mean( df$implicit[intersect(idx, which(df$target %in% imp_targets[[ws]]))], na.rm=TRUE)  )
        
        explicit    <- c(explicit, mean(df$explicit[idx], na.rm=TRUE))
        adaptation  <- c(adaptation, df$adaptation[intersect(idx, which(df$target == 90))])
        
        
      }
      
    }
    
  }
  
  df <- data.frame( experiment,
                    participant,
                    #group,
                    workspace,
                    implicit,
                    explicit,
                    adaptation )
  
  write.csv(df, 'data/Schween_etal_2018_2019.csv', quote=F, row.names=F)
  
}

sineMSE <- function(par, data) {
  
  errors <- sinePredict(par, data) - data$y
  
  return( mean(errors^2, na.rm=TRUE) )
  
}

sinePredict <- function(par, data) {

  phase  <- par['phase']
  mult   <- par['mult']
  offset <- par['offset']
  
  return( ((mult * sin(data$x / phase)) + offset) ) 
  
}

# Werner and Schrueder 2022 ------

processWernerData <- function() {
  
  df <- read.csv('data/Werner 2022/S1_Dataset.csv', skip=2)
  
  group       <- c()
  participant <- c()
  episode     <- c()
  phase       <- c()
  reachdev    <- c()
  
  phases <- c(rep('Baseline', 5),
              rep('Adaptation',25),
              rep('Exclusion',2),
              rep('Refresh',2),
              rep('Inclusion',2),
              rep('Refresh',2),
              rep('Transfer',2),
              rep('Refresh',2),
              rep('Washout',5))
  
  for (ppno in c(1:dim(df)[1])) {

    group       <- c(group, rep(df$Group[ppno], 47))
    participant <- c(participant, rep(ppno, 47))
    episode     <- c(episode, c(1:47))
    phase       <- c(phase, phases)
    reachdev    <- c(reachdev, as.numeric(df[ppno,2:48]))
    
  }
  
  df <- data.frame(group,
                   participant,
                   episode,
                   phase,
                   reachdev)
  
  return(df)
  
}

summarizeWerner <- function() {
  
  df <- processWernerData()
  episodes <- c(4,5,29,30,31,32,35,36)
  df <- df[which(df$episode %in% episodes),]
  
  participants <- unique(df$participant)
  
  participant <- c()
  group       <- c()
  adaptation  <- c()
  exclude     <- c()
  include     <- c()
  
  for (ppno in participants) {
    
    pdf <- df[which(df$participant == ppno),]
    apdf <- aggregate(reachdev ~ phase, data=pdf, FUN=mean, na.rm=TRUE)
    
    baseline <- apdf$reachdev[which(apdf$phase == 'Baseline')]
    
    participant <- c(participant, ppno)
    group       <- c(group,       pdf$group[1])
    adaptation  <- c(adaptation, (apdf$reachdev[which(apdf$phase == 'Adaptation')] - baseline) * -1)
    exclude     <- c(exclude,    (apdf$reachdev[which(apdf$phase == 'Exclusion')]            ) * -1)
    include     <- c(include,    (apdf$reachdev[which(apdf$phase == 'Inclusion')]            ) * -1)
    
  }
  
  df <- data.frame( participant,
                    group,
                    adaptation,
                    exclude,
                    include      )
  
  return(df)
  
}

# Maresch et al 2020 -----

processMareschData <- function() {
  
  # the last 3 * 16 trials are the same for the 3 groups: a major PDP block
  end_episodes <- list('inclusion'  = 860+c(1:16),
                       'adaptation' = 876+c(9:16),
                       'exclusion'  = 892+c(1:16) )
  # but we probably want some more adaptation trials where there was no decay: last 8?
  
  # groups are in the order they appear in the paper (I think)
  # 1 = CR, they have what looks like 2 blocks in between training, but the first is just training with reports that are used for analysis
  # 2 = IR-E
  # 3 = IR-I
  # 4 = IR-EI
  
  # the "episodes" in the episode table look like they can be used to find more of the excl/incl and report trials
  
  # 31/32/33/34/35 are the 4-trial report blocks
  # 41/42/43/44/45 are the 4-trial exclusion blocks
  # 51/52/53/54/55 are the 4-trial inclusion blocks
  
  # we'll not use the exclusion/inclusion blocks troughout training, only the ones at the end
  # we'll have to use the 3X report blocks though, as those are the ones available for all groups
  
  # episode 1 (trials 1-80) is baseline, but only for with-cursor reaching
  # there are no no-cursor or reporting baseline blocks
  
  # episodes 101 - 500 are all the rotated training trials
  # the CR group has 4 fewer training trials in the first bout, for some reason
  
  subject_table  <- read.csv('data/Maresch2020/SubjectTable.csv',
                             stringsAsFactors=FALSE) 
  
  episode_table  <- read.csv('data/Maresch2020/EpisodeTable.csv', 
                             stringsAsFactors=FALSE)
  
  behavior_table <- read.csv('data/Maresch2020/BehaviorTable.csv',
                             stringsAsFactors=FALSE)
  
  report_table   <- read.csv('data/Maresch2020/ReportTable2.csv',
                             stringsAsFactors=FALSE)
  
  group       <- c()
  participant <- c()
  adaptation  <- c()
  exclude     <- c()
  include     <- c()
  aimreport   <- c()
  
  for (groupno in c(1:4)) {
    
    # who are the participants in this group?
    
    ppnos <- subject_table$subjectID_ST[which(subject_table$groupID == groupno)]
    
    groupname <- c('CR',
                   'IR_E',
                   'IR_I',
                   'IR_EI')[groupno]
    
    #print(groupname)
    #print(ppnos)
    
    for (ppno in ppnos) {
      
      group       <- c(group, groupname)
      participant <- c(participant, ppno)
      
      for (end_episode in names(end_episodes)) {
        
        trialnos <- episode_table$TrialNums_EP[which(episode_table$episode %in% end_episodes[[end_episode]] & episode_table$groupID_EP == groupno)]
        
        avg <- mean( behavior_table$subjectBeh[which(behavior_table$subjectID == ppno &
                                                     behavior_table$trialNum  %in% trialnos)], na.rm=TRUE)
        
        if (end_episode == 'inclusion')  { include    <- c(include,    avg) }
        if (end_episode == 'adaptation') { adaptation <- c(adaptation, avg) }
        if (end_episode == 'exclusion')  { exclude    <- c(exclude,    avg) }
        
      }
      
      aimreport <- c(aimreport, mean( report_table$subjectReports[which(report_table$subjectID == ppno)], na.rm=TRUE) )
      
    }
    
  }
  
  adaptation <- adaptation * -1
  include    <- include    * -1
  exclude    <- exclude    * -1
  aimreport  <- aimreport  * -1
  
  df <- data.frame( group,
                    participant, 
                    adaptation, 
                    include, 
                    exclude,
                    aimreport  )

  return(df)

}
