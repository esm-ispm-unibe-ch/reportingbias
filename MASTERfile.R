rm(list=ls())

library(devtools)
install_github("esm-ispm-unibe-ch/reportingbias")
library(reportingbias)
library(nmathresh)
library(netmeta)

# download and store dataset
data <- data("diabetes")


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
library(nmadb)
NMADB <- getNMADB()
binaryIDs = NMADB[NMADB$Verified=="True" & NMADB$Type.of.Outcome.=="Binary" & NMADB$Format!="iv",]$Record.ID
continuousIDs = NMADB[NMADB$Verified=="True" & NMADB$Type.of.Outcome.=="Continuous" & NMADB$Format!="iv",]$Record.ID

NMADB[NMADB$Record.ID==474842, c("Primary.Outcome", "Harmful.Beneficial.Outcome")]

nma2 <- runnetmeta(474842)
summary(nma2)

thresh2 <- threshold_netmeta(nma2, opt.max = F, mcid = 0.1)
threshplot(thresh2, nma2, xlab = "Log OR")

thresh2b <- threshold_netmeta(nma2, opt.max = F, decision="change", mcid = 0.1)
threshplot(thresh2b, nma2, xlab = "Log OR")
