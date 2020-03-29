#####   function to produce threshold analysis forest plot                    #####
####    arguments:  thresh, object of clss thresh
####                        produced by nma_thresh
####                nma, object of class netmeta
####                nma.type (default "random"), character giving type (random of fixed effects)
####                        used in original nma analysis and also specified in threshold analysis
####                xlab, character giving the label for the x-axis
####                xlim, Numeric vector (length 2) of lower and upper limits for the x-axis, default -2 to 2


threshplot <-  function(thresh, nma, nma.type="random", xlab="", xlim = c(-2, 2)) {

  # design matrix of basic parameters
  X <- thresh$call$X
  
        # Get indices of contrasts in likelihood in full contrast vector
        d.a <- d.b <- vector(length = nrow(X))
        for (i in 1:nrow(X)) {
          d.a[i] <- ifelse(any(X[i,] == -1), which(X[i,] == -1),0) + 1
          d.b[i] <- ifelse(any(X[i,] == 1),which(X[i,] == 1),0) + 1
        }
        
 # data points with confidence intervals
  if (nma.type=="random") {
    contr.mean <- diag(nma$TE.random[d.b, d.a])
    CI2.5  <- diag(nma$lower.random[d.b, d.a])
    CI97.5 <- diag(nma$upper.random[d.b, d.a])
  }
  else {
    contr.mean <- diag(nma$TE.fixed[d.b, d.a])
    CI2.5  <- diag(nma$lower.fixed[d.b, d.a])
    CI97.5 <- diag(nma$upper.fixed[d.b, d.a])
  }
        
        # Label the contrasts and display using a forest plot, along with 95% confidence intervals
        plotdat <- data.frame(lab = paste0(nma$trts[d.b], " vs. ", nma$trts[d.a]), 
                              contr.mean, CI2.5, CI97.5)

        # produce forest plot
        fp <- thresh_forest(
          thresh = thresh,     # Threshold object produced by nma_thresh
          y = contr.mean,      # Column of data points
          CI.lo = CI2.5, CI.hi = CI97.5,  # Columns of lower and upper limits of CIs
          label = lab,       # Column of labels
          data = plotdat,      # Data frame containing above data
          # -- Plotting options below here --
          label.title = "Contrast", 
          xlab = xlab,
          CI.title = "95% Confidence Interval",
          xlim = xlim, 
          refline = 0, 
          digits = 2, cutoff = 30,
          II.title = expression("Invariant Interval and "*tilde(k)*"*"),
          II.cols = rgb(0.72, 0.8, 0.93))
        
}