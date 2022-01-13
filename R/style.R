
getGroups <- function() {
  
  
  group <- c('control', 'instructed', 'aiming', 'aims')
  label <- c('control', 'instructed', 'aiming', 'aims')
  col.op <- c('#ff9329ff', '#e51636ff', '#7f00d8ff', '#cc00ccff')
  col.tr <- c('#ff93292f', '#e516362f', '#7f00d82f', '#cc00cc2f')
  
  return(data.frame(group, label, col.op, col.tr))
  
  
}
