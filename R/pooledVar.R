pooledVar <- function(nma, data) {
  nma$data$var <- nma$seTE.adj^2
  
  #narms <- count(data, id, wt=n())
  #multiarm <- which(nma$narms>2)
  nma$data$var[which(nma$n.arms>2)] <- rep(sapply(which(nma$narms>2),
                                                  function(i){
                                                    x <- nma$data[nma$data$studlab==i, c("studlab","n1","n2","treat1","treat2","var")]
                                                    num <- (x$n1+x$n2-1)*x$var
                                                    den <- (nma$narms[i]-1)*(sum(x$n1+x$n2)/(nma$narms[i]-1)-nma$narms[i])
                                                    sum(num)/den
                                                  }), times = nma$narms[which(nma$narms>2)])
  data <- cbind(data, varStudies=rep(nma$data[!duplicated(nma$data$studlab), "var"], times = nma$narms))
  return(data)
}
