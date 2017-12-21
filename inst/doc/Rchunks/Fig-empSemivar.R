  layout(matrix(c(1,2), nrow = 2, byrow = TRUE))
  par(mar = c(5,5,5,3))
  plot(empsvgm.out$mndist/1000, empsvgm.out$svgm, 
       ylim = c(0,max(empsvgm.out$svgm)),
       xlim = c(0, max(empsvgm.out$mndist/1000)),
       pch = 19, cex = empsvgm.out$npair/250,
       xlab = "Linear Network Distance (kilometers)",
       ylab = "Semivariogram", cex.axis = 1.5, cex.lab = 2)
   d4fit = 1:85000
   lines(d4fit,sigmapLexp*(1-ExpMod(d4fit, alphaLexp/1000)), lwd = 4,
      col = '#e41a1c', lty  = 2)
   lines(d4fit,psilExpLinWLSnug*(1-ExpMod(d4fit,rangExpLinWLSnug/1000)) + nuggExpLinWLSnug, lwd = 4,
      col = '#e41a1c', lty  = 1)
   lines(d4fit,psilSphLinWLS*(1-SphMod(d4fit,rangSphLinWLS/1000)), lwd = 4,
      col = '#4daf4a', lty  = 2)
   lines(d4fit,psilSphLinWLSnug*(1-SphMod(d4fit,rangSphLinWLSnug/1000)) + nuggSphLinWLSnug, lwd = 4,
      col = '#4daf4a', lty  = 1)
   lines(d4fit,psilGauLinWLS*(1-GauMod(d4fit,rangGauLinWLS/1000)), lwd = 4,
      col = '#377eb8', lty  = 2)
   lines(d4fit,psilGauLinWLSnug*(1-GauMod(d4fit,rangGauLinWLSnug/1000)) + nuggGauLinWLSnug, lwd = 4,
      col = '#377eb8', lty  = 1)
   lines(d4fit,psilCauLinWLS*(1-CauMod(d4fit,rangCauLinWLS/1000)), lwd = 4,
      col = '#984ea3', lty  = 2)
   lines(d4fit,psilCauLinWLSnug*(1-CauMod(d4fit,rangCauLinWLSnug/1000)) + nuggCauLinWLSnug, lwd = 4,
      col = '#984ea3', lty  = 1)
   lines(d4fit,psilHolLinWLS*(1-HolMod(d4fit,rangHolLinWLS/1000)), lwd = 4,
      col = '#ff7f00', lty  = 2)
   lines(d4fit,psilHolLinWLSnug*(1-HolMod(d4fit,rangHolLinWLSnug/1000)) + nuggHolLinWLSnug, lwd = 4,
      col = '#ff7f00', lty  = 1)
   legend(30,3, legend = c('Exponential','Spherical','Gaussian','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 1.5,
	col = c('#e41a1c','#4daf4a','#377eb8','#984ea3','#ff7f00'))
  mtext('(a)', adj = -.10, padj = -.6, cex = 3)


  par(mar = c(5,5,5,3))
  plot(empsvgmE.out$mndist/1000, empsvgmE.out$svgm, 
       ylim = c(0,max(empsvgmE.out$svgm)),
       xlim = c(0, max(empsvgmE.out$mndist/1000)),
       pch = 19, cex = empsvgmE.out$npair/300,
       xlab = "Euclidean Distance (kilometers)",
       ylab = "Semivariogram", cex.axis = 1.5, cex.lab = 2)
   d4fit = 1:85000
   lines(d4fit,psilExpEucWLSnug*(1-ExpMod(d4fit,rangExpEucWLSnug/1000)) + nuggExpEucWLSnug, lwd = 4,
      col = '#e41a1c', lty  = 1)
   lines(d4fit,psilSphEucWLSnug*(1-SphMod(d4fit,rangSphEucWLSnug/1000)) + nuggSphEucWLSnug, lwd = 4,
      col = '#4daf4a', lty  = 1)
   lines(d4fit,psilGauEucWLSnug*(1-GauMod(d4fit,rangGauEucWLSnug/1000)) + nuggGauEucWLSnug, lwd = 4,
      col = '#377eb8', lty  = 1)
   lines(d4fit,psilCauEucWLSnug*(1-CauMod(d4fit,rangCauEucWLSnug/1000)) + nuggCauEucWLSnug, lwd = 4,
      col = '#984ea3', lty  = 1)
   lines(d4fit,psilHolEucWLSnug*(1-HolMod(d4fit,rangHolEucWLSnug/1000)) + nuggHolEucWLSnug, lwd = 4,
      col = '#ff7f00', lty  = 1)
   lines(d4fit,psilExpEucREMLnug*(1-ExpMod(d4fit,rangExpEucREMLnug/1000)) + nuggExpEucREMLnug, lwd = 4,
      col = '#e41a1c', lty  = 2)
   lines(d4fit,psilSphEucREMLnug*(1-SphMod(d4fit,rangSphEucREMLnug/1000)) + nuggSphEucREMLnug, lwd = 4,
      col = '#4daf4a', lty  = 2)
   lines(d4fit,psilGauEucREMLnug*(1-GauMod(d4fit,rangGauEucREMLnug/1000)) + nuggGauEucREMLnug, lwd = 4,
      col = '#377eb8', lty  = 2)
   lines(d4fit,psilCauEucREMLnug*(1-CauMod(d4fit,rangCauEucREMLnug/1000)) + nuggCauEucREMLnug, lwd = 4,
      col = '#984ea3', lty  = 2)
   lines(d4fit,psilHolEucREMLnug*(1-HolMod(d4fit,rangHolEucREMLnug/1000)) + nuggHolEucREMLnug, lwd = 4,
      col = '#ff7f00', lty  = 2)
   legend(20,3, legend = c('Exponential','Spherical','Gaussian','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 1.5,
	col = c('#e41a1c','#4daf4a','#377eb8','#984ea3','#ff7f00'))
mtext('(b)', adj = -.10, padj = -.6, cex = 3)

