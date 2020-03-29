rm(list=ls())

library(devtools)
install_github("esm-ispm-unibe-ch/reportingbias")
library(reportingbias)
library(RCurl)
library(netmeta)

# download and store dataset
data <- read.csv(text=getURL("https://raw.githubusercontent.com/esm-ispm-unibe-ch/reportingbias/master/diabetes.csv"))


# pairwise format
data.pw <- pairwise(t, r, n, data = data, studlab = id, sm="OR")

# run netmeta
nma <- netmeta(TE, seTE, treat1, treat2, studlab=studlab, data = data.pw)

# threshold analysis
thresh <- threshold_netmeta(nma, opt.max = F, mcid = 0.1, decision = "change")
threshplot(thresh, nma, xlab = "Log OR")
