
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.1.1-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/kotzeb0912)](https://cran.r-project.org/package=kotzeb0912) [![packageversion](https://img.shields.io/badge/Package%20version-1.0-orange.svg?style=flat-square)](commits/master)

[![Last-changedate](https://img.shields.io/badge/last%20change-2017--05--09-yellowgreen.svg)](/commits/master)

# KrigLinCaution
## An R package in support of publication, "Kriging Models for Linear Networks and non-Euclidean Distances: Cautions, Solutions, and a Comment on Ladle et al. (2016)." 

#### Jay M. Ver Hoef<sup>a</sup>

#### <sup>a</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center 

As a scientific work, and in keeping with common scientific practicies, I kindly request that you cite my research project and applicable publications if you use my work(s) or data in your publications or presentations. Additionally, I strongly encourage and welcome collaboration to promote use of these data in the proper context and scope.  The publication is currently submitted:

#### Ver Hoef, Jay. M. Kriging Models for Linear Networks and non-Euclidean Distances: Cautions, Solutions, and a Comment on Ladle et al. (2016). In preparation for *Methods in Ecology and Evolution*.


Executive Summary
-----------------

 1. There are now many examples where ecological researchers used non-Euclidean distance metrics in geostatistical models that were designed for Euclidean distance, such as those used for kriging.  This can lead to problems where predictions have negative variance estimates.  Technically, this occurs because the spatial covariance matrix, which depends on the geostatistical models, is not guaranteed to be positive definite when non-Euclidean distance metrics are used.
  2. I give a quick review of kriging and illustrate the problem with several fabricated examples, including locations on a circle, locations on a linear dichotomous network like streams, and locations on a linear trail or road network. I re-examine the linear network distance models from Ladle et al. (2016) and show that they are not guaranteed to have a positive definite covariance matrix.
  3.  I introduce the reduced rank method, also called predictive process models, fixed-rank kriging, and spatial basis functions, for creating valid spatial covariance matrices with non-Euclidean distance metrics.  It has an additional advantage of fast computation for large data sets.
  4. I re-analyze the data of Ladle et al. (2016), showing that their fitted models, which used linear network distance in a geostatistical model without any nugget effect, had poor predictive performance compared to a model using Euclidean distance with a nugget effect, and it also had improper coverage for the prediction intervals. The reduced rank approach using linear network distances had the best predictive performance and had proper coverage for the prediction intervals.

Installation
------------

Installation of this R data package is done through the `devtools::install_github()` function or by downloading the [source package from the latest release](https://github.com/jayverhoef/KrigLinCaution).

```
library("devtools")
install_github("jayverhoef/KrigLinCaution")
```

Run R Scripts
-------------

The knitr document used to create the manuscript can be found here on your computer system:

```
system.file("doc/KrigLinCaution.Rnw", package = "KrigLinCaution")
```

which contains all of the R code embedded in the Latex manuscript.  Stripping out the R code with the "purl" command yields a pure R script, which can be found here:

```
system.file("doc/KrigLinCaution.R", package = "KrigLinCaution")
```

To run the whole script from within R use:

```
source(system.file("doc/KrigLinCaution.R", package = "KrigLinCaution"))
```

A pure Latex document can be found here:

```
system.file("doc/KrigLinCaution.R", package = "KrigLinCaution")
```

-------------
##### Disclaimer

<sub>This repository is a scientific product and is not official communication of the Alaska Fisheries Science Center, the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All AFSC Marine Mammal Laboratory (AFSC-MML) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. AFSC-MML has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.</sub>
