plot.mModel <- function(x, ...) {
#  if (x$d > 2)
#    stop("PLOT SUPPORTS ONLY 1D and 2D data")

   map.class = NULL
   if (!is.null(x$class))
       map.class = x$class
   if (!is.null(x$B))
       map.class = apply(x$B, 1, which.max)
   if (!is.null(x$P))
       map.class = apply(x$P, 1, which.max)
   if ("unsupervisedModel" %in% class(x)) {
       map.class = NULL
   }

  if (x$d == 1)
    plot.1d(x$X, x$knowns, map.class, x, ...)
  if (x$d > 1)
    plot.2d(x$X, x$knowns, map.class, x, ...)
}


plot.2d <-function(x, knowns, map.class, bgmm2d, ...) {
  if (bgmm2d$d > 2) {
      tmp     <- prcomp(rbind(x, knowns))
      bgmm2d$mu<- data.frame(bgmm2d$mu)
      colnames(bgmm2d$mu) = colnames(x)
      x       <- predict(tmp, x)[,1:2]
      knowns  <- predict(tmp, knowns)[,1:2]
      bgmm2d$mu<- predict(tmp, bgmm2d$mu)[,1:2]
      B       <- tmp$rotation[,1:2]
      cvar = array(0,dim=c(bgmm2d$k,2,2))
      for (i in 1:(bgmm2d$k))
          cvar[i,,] <- t(B) %*% bgmm2d$cvar[1,,] %*% B
      bgmm2d$cvar <- cvar
   }

   Xk = rbind(x, knowns)

   plot(Xk, type="n", ...)
   #
   # plot unknowns
   if (!is.null(x))
     points(x, pch=19, col=rgb(0,0,0,0.5), cex=0.5)
   #
   # plot knowns
   if (!is.null(map.class)) {
#     labs <- unique(map.class)
#     for (ii in seq_along(labs)) {
#       points(knowns[map.class == labs[ii],], col=ii+1, pch=ii+1, lwd=2)
#     }
     points(knowns, col=map.class+1, pch=map.class+1, lwd=2)
   }
   #
   # plot densities
   for (i in 1:bgmm2d$k) {
       car::ellipse(center = bgmm2d$mu[i, ], shape = bgmm2d$cvar[i, , ], radius = 1, col=i+1, lwd=2, lty=2)
       car::ellipse(center = bgmm2d$mu[i, ], shape = bgmm2d$cvar[i, , ], radius = 2, col=i+1, lwd=1, lty=2)
       }
}


plot.1d <-function(x, knowns, map.class, bgmm1d, npoints=1000, addHlines=TRUE, ...) {
   Xk = rbind(x, knowns)

   densx = seq(min(Xk), max(Xk), length.out=npoints)
   densy = matrix(0, bgmm1d$k, npoints)
   for (i in 1:bgmm1d$k)
      densy[i, ] = dnorm(densx, bgmm1d$mu[i,1], sqrt(bgmm1d$cvar[i,1,1]))
   #
   # plot densities
   matplot(densx, t(densy), col=1+(1:bgmm1d$k), lwd=2, type="l", ylab="density", ...)
   if (addHlines) for (i in 1:bgmm1d$k)
                     abline(v=bgmm1d$mu[i,1], lty=2)
   #
   # plot unknowns
   rug(Xk)
   #
   # plot knowns
   if (!is.null(map.class))
     for (i in 1:length(unique(map.class)))
        if (sum(map.class==i)>0) rug(knowns[map.class==i,], lwd=4, col=1+i, side=3)
}
