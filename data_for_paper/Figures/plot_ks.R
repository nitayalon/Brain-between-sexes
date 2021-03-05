windowsFonts(A = windowsFont("Arial Black"))
plot(ks_range, non_extrem__survival, 
     type = 'l', col = '#A9A9A9', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', 
     cex.lab= 1.5, cex.axis = 1.5, 
     lty = 5,
     family="A",
     font=2,
     ps = 24,
     frame.plot = FALSE)
lines(ks_range, extrem__survival, 
      type = 'l', col = '#A9A9A9', lwd = 2.5, lty = 3)
lines(seq(1/400,1,1/400) * sqrt(400), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
# lines(seq(1/289,1,1/289) * sqrt(289), ks_distribution, type = 'l', col = 'black', lwd = 2.5)

