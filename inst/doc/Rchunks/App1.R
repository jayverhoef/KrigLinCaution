linDmat = rbind(
  c(0,1,2,2),
  c(1,0,1,1),
  c(2,1,0,2),
  c(2,1,2,0))
Sig = GauMod(linDmat,3) + diag(rep(0.01, times = 4))
Sig
eigen(Sig)$values
Q = eigen(Sig)$vectors
Q
w = Q[,4]
t(w) %*% Sig %*% w
