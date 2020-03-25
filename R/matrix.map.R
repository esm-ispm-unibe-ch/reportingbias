##### Function to create "contrast design matrix" X for threshold analysis  #####
#####           to map basic parameters to the network structure            #####

##  the only argument is x, object of class netmeta           ##


matrix.map <- function(x) {
  
  # number of treatment
  K <- x$n
  
  # treatment names
  if (x$reference.group == "")
    trts <- colnames(x$A.matrix)
  else
    trts <- c(x$reference.group,
              colnames(x$A.matrix)[colnames(x$A.matrix) != x$reference.group])
  
  
  studies.pre <- data.frame(studlab = x$studlab,
                            treat1 = factor(x$treat1, levels = trts), treat2 = factor(x$treat2, levels = trts),
                            narms = x$narms[match(x$studlab, x$studies)],
                            stringsAsFactors = F)

  studies <- studies.pre[order(studies.pre$studlab), ]
  
  
  sel <- studies.pre$treat2 == x$reference.group

  
  studies$treat1[sel] <- studies.pre$treat2[sel]
  studies$treat2[sel] <- studies.pre$treat1[sel]
  
  # all (observed) comparisons
  comparison <- data.frame(cbind(studies$treat1, studies$treat2))
  
  # unique (observed) comparisons
  comparison <- unique(comparison[order(comparison[,1],comparison[,2]), ])
  
  # buils design matrix X
  X <- matrix(0, nrow = nrow(comparison), ncol = K-1)
  for (i in 1:nrow(X)) {
    if (comparison[i, 1] > 1) X[i, comparison[i, 1] - 1]  <- -1
    if (comparison[i, 2] > 1) X[i, comparison[i, 2] - 1]  <- 1
  }
  
  
  return(X)

  }