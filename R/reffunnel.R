reffunnel <- function (nma, small.values = "good", ref) 
{
  wo <- nma$data$treat1 > nma$data$treat2
  if (any(wo)) {
    nma$data$TE[wo] <- -nma$data$TE[wo]
    ttreat1 <- nma$data$treat1
    nma$data$treat1[wo] <- nma$data$treat2[wo]
    nma$data$treat2[wo] <- ttreat1[wo]
    if (!is.null(nma$data$n1) & !is.null(nma$data$n2)) {
      tn1 <- nma$data$n1
      nma$data$n1[wo] <- nma$data$n2[wo]
      nma$data$n2[wo] <- tn1[wo]
    }
    if (!is.null(nma$data$event1) & !is.null(nma$data$event2)) {
      tevent1 <- nma$data$event1
      nma$data$event1[wo] <- nma$data$event2[wo]
      nma$data$event2[wo] <- tevent1[wo]
    }
  }
  if (nma$sm == "OR" | nma$sm == "RR") {
    ma <- metabin(event1, n1, event2, n2, subset = nma$treat1 == ref | nma$treat2 == ref, data = nma$data)
    funnel(ma, contour = c(0.9, 0.95, 0.99), col.contour = c("darkred", "red", "lightcoral"), 
           cex = 1.5, col = "darkblue", bg = "blue", cex.lab = 1.2, 
           xlab = paste(ma$sm, "effect reported as", ref, "over other treatment"))
    legend("bottomleft", c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
           fill = c("darkred", "red", "lightcoral"), cex=0.8)
    mtext(paste(ref, "vs other treatments"), cex = 2)
    mb <- metabias(ma, method.bias = "score")
  }
  else {
    ma <- metagen(nma$TE, nma$seTE, subset = nma$treat1 == ref || nma$treat2 == ref, sm = sm)
    funnel(ma, contour = c(0.9, 0.95, 0.99), col.contour = c("darkred", "red", "lightcoral"), 
           cex = 1.5, col = "darkblue", bg = "blue", cex.lab = 1.2, 
           xlab = paste(ma$sm, "effect reported as", ref, "- other treatment"))
    legend("bottomleft", c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), 
           fill = c("darkred", "red", "lightcoral"))
    mtext(paste(ref, "vs other treatments"), cex = 2)
    mb <- metabias(ma)
  }
small.2nd <- "Small studies favour other treatment"
small.1st <- "Small studies favour reference"
tests <- data.frame(bias = round(mb$estimate["bias"], digits = 2), 
                    p.value = round(mb$p.value,digits = 2), 
                    interpretation = ifelse(small.values == "good", 
                                            ifelse(mb$estimate["bias"] > 0, small.2nd, small.1st), 
                                            ifelse(mb$estimate["bias"] > 0, small.1st, small.2nd)))
res <- list(tests = tests, test.method = mb$method, effect.measure = ma$sm)
print(tests)
}
