# R-code for the paper: ----------------------------------------------------
# A probabilistic procedure for estimating an optimal echo-integration threshold
# using the Expectation-Maximisation algorithm.
# López-Serrano A., Villalobos H., Nevárez-Martínez M.O.
# Aquatic Living Resources
# DOI: 10.1051/alr/2017048


# Load Packages -----------------------------------------------------------
  library(echogram); library(mmand); library(mixtools)
  
# Load RData --------------------------------------------------------------
  load("LopezSerrano_et_al_data.RData")

# Create additional functions----------------------------------------------
# Hector Villalobos <hvillalo@ipn.mx>

# Plot mixmodel, modified from plot.mixEM()
plot.mix <- function(mix.object, breaks=NULL, ylim=NULL, xlab=NULL, ylab=NULL, 
                     col=NULL, main=NULL, ...) {
  mo <- mix.object
  k <- ncol(mo$posterior)
  x <- sort(mo$x)
  if ( !missing(breaks) ) {
    a <- hist(x, plot = FALSE, breaks=breaks)
  } else {
    a <- hist(x, plot = FALSE)
    breaks <- a$breaks
  }  
  maxy <- max(max(a$density), 0.3989 * mo$lambda/mo$sigma)
  if ( missing(main) )
    main <- "Density Curves"
  if ( missing(xlab) )
    xlab <- "Data"
  if ( missing(ylab) )
    ylab <- "PDF"
  if ( missing(col) )
    col <- 2:(k + 1)
  if ( missing(ylim) )
    ylim <- c(0, max(a$density))
  
  hist(x, breaks=breaks, prob=TRUE, border="grey", ylab=ylab, xlab=xlab, 
       main=main, ylim=ylim, ...)
  
  arbmean <- TRUE
  arbvar <- TRUE
  
  for (i in 1:k) {
    lines(x, mo$lambda[i] * dnorm(x, mean = mo$mu[i * arbmean + (1 - arbmean)], 
              sd = mo$sigma[i * arbvar + (1 - arbvar)]),  col = col[i], lwd = 2)
  }
}


# Optimal Global Threshold for two groups
ogt <- function(mix.object, lim=NULL) {
  mo <- mix.object
  x <- sort(mo$x)
  arbmean <- TRUE
  arbvar <- TRUE
  
  for (i in 1:2){
    pdf <- mo$lambda[i] * dnorm(x, mean=mo$mu[i*arbmean+(1-arbmean)], sd=mo$sigma[i*arbvar+(1-arbvar)]) 
    assign(paste("pdf", i, sep=""), pdf)
  }

  if ( !missing(lim) ){
    li <- lim[1]
    ls <- lim[2]
    xx <- x[x > li & x < ls]
    pdf1 <- pdf1[x > li & x < ls]
    pdf2 <- pdf2[x > li & x < ls]
    x <- xx
  }  
  Thr <- x[which.min(abs(pdf1-pdf2))]
  Thr
}


# Extract sea bottom echoes
bot <- function(echogram, bot.width = NA) {
  echo <- echogram
  if ( class(echo) != "echogram" ) 
    stop ("need object of class 'echogram'")
  ans <- echo
  me <- mask.echogram(echo, bott.off=0, mask=FALSE)
  me <- ifelse(is.na(me), 1, NA)
  ans$Sv <- ans$Sv * me
  if (missing(bot.width))
    bot.width <- 5
  ans <- mask.echogram(ans, bott.off=-bot.width, mask=TRUE)
  ans
}


# Restore sea bottom echoes
restore.bot <- function(echogram, echo.bot){
  ans <- echogram
  m1 <- ans$Sv
  freq <- attr(m1, "frequency")
  
  M <- array(NA, dim=c(dim(m1), 2))
  M[ , , 1] <- m1
  M[ , , 2] <- echo.bot$Sv
  
  sum2 <- function(x){
    ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
  }
  MM <- apply(M, 1:2, sum2)
  ans$Sv <- MM
  attr(ans$Sv, "frequency") <- freq
  ans
}


# Apply threshold to echogram
threshold.echogram <- function(echogram, Svmin, Svmax){
  ans <- echogram
  ans$Sv[ans$Sv < Svmin] <- NA
  ans$Sv[ans$Sv > Svmax] <- NA
  ans
}


# Figure 1 ----------------------------------------------------------------
  # copy echogram
    eco.L04s <- eco.L04
  # echogram averaging
    eco.L04s$Sv <- meanFilter(eco.L04$Sv, shapeKernel(c(5, 5)))
  
  # location of 100 m depth line
    h100 <- 1637
  
  # define area of interest
  aoi <- data.frame(x=c(200, 600, 600, 200), y=c(2251, 2251, 2629, 2629))
  
  # Plot Figure 1a
  dev.new()
  dev.new()
  echogram(eco.L04, cex.axis=1.1, cex.lab=1.1, cex.main=1.1, colbar = FALSE)
    abline(h=h100); polygon(aoi)
  
   attach(SvData)
     points(SvData[trawlNo == 4 & category == "schools", c('x', 'y')], pch=3)
     points(SvData[trawlNo == 4 & category == "other", c('x', 'y')], pch=4)
   detach(SvData)
  
  # Plot Figure 1b   
  dev.new()
    echogram(eco.L04s, cex.axis=1.1, cex.lab=1.1, cex.main=1.1, 
             main="38 kHz, 5x5 averaged")
  


# Figure 2 ----------------------------------------------------------------
  # breaks for histograms
  brks <- seq(-134, -30, 2)
    
  # raw Sv. The three echograms below can be plotted as above
  eco.F2a <- mask.echogram(eco.L04, bott.off = 2) # excluding sea bottom echoes
  eco.F2b <- trim.echogram(eco.F2a, 100) # the fist 100 m
  eco.F2c <- trim.echogram(eco.F2b, ping.ini=200, ping.end=600, depth.max=40) # aoi
  
  # extracting matrices of Sv Data 
  F2a <- as.vector(eco.F2a$Sv)
  F2b <- as.vector(eco.F2b$Sv)
  F2c <- as.vector(eco.F2c$Sv)

  # histograms (a); (b): (c)
  dev.new()
    hist(F2a, breaks=brks, freq = FALSE)
  dev.new()
    hist(F2b, breaks=brks, freq = FALSE)
  dev.new()
    hist(F2c, breaks=brks, freq = FALSE)
    
  # Finding Optimal Global Threshold for F2c with Em algorithm 
  # (if the figure is not satisfactory, run this code a second time)
  pdf.F2c <- normalmixEM(F2c[!is.na(F2c)], mu=c(-75, -45))
  summary(pdf.F2c)
  thr1 <- ogt(pdf.F2c, lim=c(-60, -50))
  cat("Optimal Global Threshold:", thr1)
  dev.new()
    plot.mix(pdf.F2c, breaks=brks)
    abline(v=thr1)
  
      
  # smooth Sv
  eco.F2d <- mask.echogram(eco.L04s, bott.off = 2)
  eco.F2e <- trim.echogram(eco.F2d, 100) 
  eco.F2f <- trim.echogram(eco.F2e, ping.ini = 200, ping.end=600, depth.max=40)
  
  F2d <- as.vector(eco.F2d$Sv)
  F2e <- as.vector(eco.F2e$Sv)
  F2f <- as.vector(eco.F2f$Sv)
  
  # histograms (d); (e): (f)
  dev.new()
    hist(F2d, breaks=brks, freq = FALSE)
  dev.new()
    hist(F2e, breaks=brks, freq = FALSE)
  dev.new()
    hist(F2f, breaks=brks, freq = FALSE)
 
  # Optimal Global Threshold for F2f with Em algorithm    
  pdf.F2f <- normalmixEM(F2f[!is.na(F2f)], k=2)
  summary(pdf.F2f)
  thr2 <- ogt(pdf.F2f)
  cat("Optimal Global Threshold:", thr2)
  dev.new()
    plot.mix(pdf.F2f, breaks=brks)
    abline(v=thr2)
    

# Figure 3 ----------------------------------------------------------------
  # Fig. 3a
  schools <- SvData$Sv038[SvData$category == "schools"]
  other <- SvData$Sv038[SvData$category == "other"]
    
  brk <- seq(-100, -20, 2)
  dev.new()
  hist(schools, breaks = brk, prob=TRUE, ylim=c(0, 0.07), col="grey90", 
       border="grey50", cex.axis=1.2,, cex.lab=1.2, main = "", ylab="PDF",
       xlab=expression(paste(S[v], "  (dB re 1 ", m^{-1}, ")")),   las=1)
    
  par(new=TRUE)
  hist(other, breaks = brk, prob=TRUE, ylim=c(0, 0.07), border="black",
         main = "", xlab="", ylab="", xaxt="n", yaxt="n")

  
  # Fig. 3b
  # Optimal Global Threshold for sampled raw Sv data
  mixmdl <- normalmixEM(SvData$Sv038, k=2)
  summary(mixmdl)
  thr <- ogt(mixmdl)
  cat("Optimal Global Threshold:", thr) 
  
  # posterior probabilities
  pProb <- data.frame(x=mixmdl$x, schools=mixmdl$posterior[, 2], other=mixmdl$posterior[, 1])
  pProb <- pProb[order(pProb$x), ]
  
  dev.new()
  plot.mix(mixmdl, brk, xlab=expression(paste(S[v], "  (dB re 1 ", m^{-1}, ")")), 
           cex.axis=1.2, main="", ylim=c(0, 0.03), cex.lab=1.2)
  abline(v=thr)
  
    
  #Fig. 3c
  dev.new()
  plot(pProb$x, pProb$schools, type="n", ylab="Posterior probability", cex.axis=1.2, 
         cex.lab=1.2, xlab=expression(paste(S[v], "  (dB re 1 ", m^{-1}, ")")) )
    
    abline(h=seq(0, 1, 0.05), v=c(thr, pProb$x[which.min(abs(pProb$schools-0.99))], 
                                  pProb$x[which.min(abs(pProb$other-0.99))]), lty=3, col="grey")
    abline(v=c(thr, pProb$x[which.min(abs(pProb$schools-0.95))], 
               pProb$x[which.min(abs(pProb$other-0.95))]), lty=3, col="grey")
    abline(v=c(thr, pProb$x[which.min(abs(pProb$schools-0.90))], 
               pProb$x[which.min(abs(pProb$other-0.90))]), lty=3, col="grey")
    lines(pProb$x, pProb$schools, col="green", lwd=2)
    lines(pProb$x, pProb$other, col="red", lwd=2)


# Figure 4. ---------------------------------------------------------------
  # Figs. 4a and 4b 
  bte <- bot(eco.L04, bot.width = 10)
  eco <- mask.echogram(eco.L04)
    
  eco.L04t2 <- threshold.echogram(eco, Svmin = thr2, Svmax=0)
    eco.L04t2 <- restore.bot(eco.L04t2, bte)
  eco.L04t <- threshold.echogram(eco, Svmin = thr, Svmax=0)
    eco.L04t <- restore.bot(eco.L04t, bte)
  
  dev.new()
    echogram(eco.L04t2, main="Trawl No. 4", colbar=F, cex.axis=1.2)
  dev.new()  
    echogram(eco.L04t, main="", colbar=TRUE, cex.axis=1.2)

  # Figs. 4c and 4d 
  eco.L14t <- threshold.echogram(eco.L14, Svmin = thr, Svmax=0)
  
  dev.new()
    echogram(eco.L14, main="Trawl No. 14", colbar=F, cex.axis=1.2)
  dev.new()  
    echogram(eco.L14t, main="", colbar=TRUE, cex.axis=1.2)  

  # Figs. 4e and 4f 
  bte <- bot(eco.L17, bot.width = 10)
  eco <- mask.echogram(eco.L17)
    
  eco.L17t <- threshold.echogram(eco, Svmin = thr, Svmax=0)
    eco.L17t <- restore.bot(eco.L17t, bte)
    
  dev.new()
    echogram(eco.L17, main="Trawl No. 17", colbar=F, cex.axis=1.2)
  dev.new()  
    echogram(eco.L17t, main="", colbar=TRUE, cex.axis=1.2)
    
    
        
  