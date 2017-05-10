# exponential model
  acor.exp = function(Dis,alpha) {exp(-Dis/alpha)}
# Gaussian model
  acor.gau = function(Dis,alpha) {exp(-(Dis/alpha)^2)}
# spherical model
  acor.sph = function(Dis,alpha) {
		(1 - 1.5*Dis/alpha + .5*(Dis/alpha)^3)*(Dis < alpha)}
# Cauchy
  acor.cau = function(Dis,alpha) {(1/(1 + (Dis/alpha)^2))}
# hole effect
	acor.hol = function(Dis,alpha) {
		mat = sin(Dis/alpha)/(Dis/alpha)
    diag(mat) = rep(1, times = length(Dis[,1]))
		mat
}


