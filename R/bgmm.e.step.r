bgmm.e.step <- function(X, model.params) {
  # densities for all components
  lfik <- matrix(0, nrow(X), model.params$k)
  for (i in 1:model.params$k) {
      if (model.params$d > 1) {
        ss = svd(model.params$cvar[i,,])
        matc = t(ss$u) %*% diag(ss$d^(-1/2)) %*% ss$v
        tx = apply(X, 1, get("-"), model.params$mu[i,,drop=F])
        lfik[,i] <-  -colSums((matc %*% tx)^2)/2 - sum(log(2*pi*ss$d))/2
      } else {
        lfik[,i] <-   dnorm(X, model.params$mu[i,,drop=F], sqrt(model.params$cvar[i,,]), log=T)
      }
  }
  # prior distribution 
  b.pi <- rbind(model.params$B, repeat.rows(model.params$pi, model.params$n-model.params$m))
  # normalisation step
  fb.ik = exp(lfik) * b.pi
 list(tij =  t(apply(fb.ik, 1, normalize)), log.likelihood=sum(log(rowSums(fb.ik))) )
}

