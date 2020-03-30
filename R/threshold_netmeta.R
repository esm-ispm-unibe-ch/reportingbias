#####   function to run threshold analysis                    #####
####    arguments:  nma, object of class netmeta
####                nma.type (default "random"), character giving type (random of fixed effects)
####                        used in original nma analysis
####                opt.max (default TRUE), optimal decision
####                        is the max treat. effect (e.g. for
####                        beneficial outcomes) or the minimum
####                        (e.g. for harmful outcomes)
####                decision (default "decision"), decision rule
####                        based on MCID; if "change" then use
####                        maximum efficacy rule but only changes
####                        the decision when alternative treatment
####                        becomes more effective than base case by mcid or more
####                mcid (default 0 i.e. max efficacy decision rule),
####                        minimally clinical important difference for the decision (if "decision")
####                        or for changing the decision (if "change")


threshold_netmeta <- function(nma, nma.type="random", opt.max=T, decision="decision", mcid=0) {

  K <- nma$n  # Number of treatments

  X <- matrix.map(nma)  ## check you get same design matrix as above

  if (nma.type=="random") {
    # create basic parameters based on chosen reference
    basicparam<-nma$TE.random[-1, 1] #the basic parameters all vs treat 1
    covariance<-nma$Cov.random[1:(K-1), 1:(K-1)] # the variance-covariance between the basic parameters
  }
  else {
    # create basic parameters based on chosen reference
    basicparam<-nma$TE.fixed[-1, 1] #the basic parameters all vs treat 1
    covariance<-nma$Cov.fixed[1:(K-1), 1:(K-1)] # the variance-covariance between the basic parameters
  }

  lik.cov<-recon_vcov(covariance, X = X) #reconstruct the variance

  thresh <- nma_thresh(mean.dk = basicparam,
                       lhood = lik.cov,
                       post = covariance,
                       nmatype = "fixed", X = X,
                       trt.code =  nma$trts, opt.max = opt.max,
                       mcid = mcid, mcid.type = decision)

  return(thresh)
}
