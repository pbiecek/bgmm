soft.e.step <- function(X, model.params) {
  # densities for all components
#  fik  <- t(apply(X, 1, get.all.densities, model.params, model.params$k))
  lfik <- matrix(0, nrow(X), model.params$k)
  for (i in 1:model.params$k) {
      if (model.params$d > 1) {
        ss = svd(model.params$cvar[i,,])
        matc = t(ss$u) %*% diag(ss$d^(-1/2)) %*% ss$v
        tx = apply(X, 1, get("-"), model.params$mu[i,,drop=F])
        lfik[,i] <-  -colSums((matc %*% tx)^2)/2 - sum(log(2*pi*ss$d))/2
      } else {
#        tx = apply(X, 1, get("-"), model.params$mu[i,,drop=F])
#        lfik[,i] <-  -(tx^2)/(2*model.params$cvar[i,,]) - log(2*pi*model.params$cvar[i,,])/2
        lfik[,i] <-   dnorm(X, model.params$mu[i,,drop=F], sqrt(model.params$cvar[i,,]), log=T)
      }
  }  
        
  if (is.null(model.params$P)) { #unsupervised
    p.ik <- matrix(1,model.params$n,model.params$k)
  } else { #soft
    p.ik <- rbind(model.params$P, matrix(1/model.params$k,model.params$n-model.params$m,model.params$k))
  }
#  repeat.rows(rep(-log(model.params$k), model.params$k), model.params$n-model.params$m))

  fik = exp(lfik)
  tij = t(t(p.ik) * model.params$pi) * fik
  n.ltij = log(rowSums(tij))
  log.likelihood = sum(n.ltij)
  tij = tij/exp(n.ltij)
  # normalisation step
  
  list(tij =  tij, log.likelihood=log.likelihood )
}
