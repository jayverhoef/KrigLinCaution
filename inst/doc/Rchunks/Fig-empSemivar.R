  layout(matrix(c(1,2), nrow = 2, byrow = TRUE))
  par(mar = c(5,5,5,3))
  plot(empsvgm.out$mndist, empsvgm.out$svgm, 
       ylim = c(0,max(empsvgm.out$svgm)),
       xlim = c(0, max(empsvgm.out$mndist)),
       pch = 19, cex = empsvgm.out$npair/250,
       xlab = "Linear Network Distance",
       ylab = "Semivariogram", cex.axis = 1.5, cex.lab = 2)
   d4fit = 1:85000
   lines(d4fit,sigmapLexp*(1-ExpMod(d4fit, alphaLexp)), lwd = 4,
      col = '#e41a1c', lty  = 2)
   lines(d4fit,psilExpLinWLSnug*(1-ExpMod(d4fit,rangExpLinWLSnug)) + nuggExpLinWLSnug, lwd = 4,
      col = '#e41a1c', lty  = 1)
   lines(d4fit,psilSphLinWLS*(1-SphMod(d4fit,rangSphLinWLS)), lwd = 4,
      col = '#4daf4a', lty  = 2)
   lines(d4fit,psilSphLinWLSnug*(1-SphMod(d4fit,rangSphLinWLSnug)) + nuggSphLinWLSnug, lwd = 4,
      col = '#4daf4a', lty  = 1)
   lines(d4fit,psilGauLinWLS*(1-GauMod(d4fit,rangGauLinWLS)), lwd = 4,
      col = '#377eb8', lty  = 2)
   lines(d4fit,psilGauLinWLSnug*(1-GauMod(d4fit,rangGauLinWLSnug)) + nuggGauLinWLSnug, lwd = 4,
      col = '#377eb8', lty  = 1)
   lines(d4fit,psilCauLinWLS*(1-CauMod(d4fit,rangCauLinWLS)), lwd = 4,
      col = '#984ea3', lty  = 2)
   lines(d4fit,psilCauLinWLSnug*(1-CauMod(d4fit,rangCauLinWLSnug)) + nuggCauLinWLSnug, lwd = 4,
      col = '#984ea3', lty  = 1)
   lines(d4fit,psilHolLinWLS*(1-HolMod(d4fit,rangHolLinWLS)), lwd = 4,
      col = '#ff7f00', lty  = 2)
   lines(d4fit,psilHolLinWLSnug*(1-HolMod(d4fit,rangHolLinWLSnug)) + nuggHolLinWLSnug, lwd = 4,
      col = '#ff7f00', lty  = 1)
   legend(30000,3, legend = c('Exponential','Spherical','Gaussian','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 1.5,
	col = c('#e41a1c','#4daf4a','#377eb8','#984ea3','#ff7f00'))
  mtext('(a)', adj = -.10, padj = -.6, cex = 3)

  par(mar = c(5,5,5,3))
  plot(empsvgmE.out$mndist, empsvgmE.out$svgm, 
       ylim = c(0,max(empsvgmE.out$svgm)),
       xlim = c(0, max(empsvgmE.out$mndist)),
       pch = 19, cex = empsvgmE.out$npair/300,
       xlab = "Euclidean Distance",
       ylab = "Semivariogram", cex.axis = 1.5, cex.lab = 2)
   d4fit = 1:85000
   lines(d4fit,psilExpEucWLSnug*(1-ExpMod(d4fit,rangExpEucWLSnug)) + nuggExpEucWLSnug, lwd = 4,
      col = '#e41a1c', lty  = 1)
   lines(d4fit,psilSphEucWLSnug*(1-SphMod(d4fit,rangSphEucWLSnug)) + nuggSphEucWLSnug, lwd = 4,
      col = '#4daf4a', lty  = 1)
   lines(d4fit,psilGauEucWLSnug*(1-GauMod(d4fit,rangGauEucWLSnug)) + nuggGauEucWLSnug, lwd = 4,
      col = '#377eb8', lty  = 1)
   lines(d4fit,psilCauEucWLSnug*(1-CauMod(d4fit,rangCauEucWLSnug)) + nuggCauEucWLSnug, lwd = 4,
      col = '#984ea3', lty  = 1)
   lines(d4fit,psilHolEucWLSnug*(1-HolMod(d4fit,rangHolEucWLSnug)) + nuggHolEucWLSnug, lwd = 4,
      col = '#ff7f00', lty  = 1)
   lines(d4fit,psilExpEucREMLnug*(1-ExpMod(d4fit,rangExpEucREMLnug)) + nuggExpEucREMLnug, lwd = 4,
      col = '#e41a1c', lty  = 2)
   lines(d4fit,psilSphEucREMLnug*(1-SphMod(d4fit,rangSphEucREMLnug)) + nuggSphEucREMLnug, lwd = 4,
      col = '#4daf4a', lty  = 2)
   lines(d4fit,psilGauEucREMLnug*(1-GauMod(d4fit,rangGauEucREMLnug)) + nuggGauEucREMLnug, lwd = 4,
      col = '#377eb8', lty  = 2)
   lines(d4fit,psilCauEucREMLnug*(1-CauMod(d4fit,rangCauEucREMLnug)) + nuggCauEucREMLnug, lwd = 4,
      col = '#984ea3', lty  = 2)
   lines(d4fit,psilHolEucREMLnug*(1-HolMod(d4fit,rangHolEucREMLnug)) + nuggHolEucREMLnug, lwd = 4,
      col = '#ff7f00', lty  = 2)
   legend(20000,3, legend = c('Exponential','Spherical','Gaussian','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 1.5,
	col = c('#e41a1c','#4daf4a','#377eb8','#984ea3','#ff7f00'))
mtext('(b)', adj = -.10, padj = -.6, cex = 3)

