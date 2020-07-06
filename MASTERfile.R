rm(list=ls())

library(devtools)
install_github("esm-ispm-unibe-ch/reportingbias")
library(reportingbias)
library(nmathresh)
library(netmeta)
library(nmadb)

# download and store dataset
data <- diabetes


# pairwise format
data.pw <- pairwise(t, r, n, data = data, studlab = id, sm="OR")

# run netmeta
nma1 <- netmeta(TE, seTE, treat1, treat2, studlab=studlab, data = data.pw)

# threshold analysis
thresh1 <- threshold_netmeta(nma1, opt.max = F, mcid = 0.05)
threshplot(thresh1, nma1, xlab = "Log OR", xlim = c(-4,4))

thresh1b <- threshold_netmeta(nma1, opt.max = F, decision = "change", mcid = 0.05)
threshplot(thresh1b, nma1, xlab = "Log OR", xlim = c(-4,4))




# example(s) with network from database

nma2 <- runnetmeta(482001)
summary(nma2)

thresh2 <- threshold_netmeta(nma2, opt.max = F, mcid = 0.1)
threshplot(thresh2, nma2, xlab = "Log RR", xlim = c(-4,4))

thresh2b <- threshold_netmeta(nma2, opt.max = F, decision="change", mcid = 0.1)
threshplot(thresh2b, nma2)

# save funnel plots in pdf file
pdf("funnel plots.pdf")

fp1 <- nmafunnel(nma2)

dev.off()
