
library(cmfrec)

tiling_sim <-  function(noise_level,seed=1){


  nrow_Z =100
  ncol_Z=20

  max_class=10
  max_iter_como=10
  set.seed(seed)
  x <-runif(nrow_Z )
  y <-runif(nrow_Z )
  X = cbind(x,y)
  max_iter_cEBMF=10
  library(flashier)

  library(keras)

  library(tensorflow)

  #problem fro set.seed(1)
  f <- matrix(NA, nrow = 3, ncol =ncol_Z )
  for ( i in 1:ncol (f)){

    t1<- sample (c(0,1), size=1)
    t2<- sample (c(0,1), size=1)

    f[1,i] <- t1*rnorm(n=1)
    f[2,i] <- t2*rnorm(n=1)

    f[3,i] <- t2*rnorm(n=1)

  }
  L <- matrix(NA, ncol=3, nrow=length(x))

  factor  <- c()

  for (i in 1:length(x)){

    if ( (x[i] <.33 & y[i] <.33 )|(x[i] >.33 & y[i] >.33 &  x[i] <.66 & y[i] <.66) | (x[i] >.66 & y[i] >.66 )){
      L[i,] <- c(1,0,0)
      factor=c(factor,1)
    }else{
      if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
        L[i,] <- c(0,1,0)
        factor=c(factor,2)
      }else{
        L[i,] <- c(0,0,1)
        factor=c(factor,3)
      }
    }


  }

  df = data.frame(x=x,y=y, Factor=as.factor(factor))



  Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=noise_level), nrow = nrow(L))


  res  <- cmfrec::CMF(X=Z, U=as.matrix(df[,-3]))
  rmse = function(mat1 , mat2){

    squared_diff <- (mat1-  mat2)^2

    # Compute mean of squared differences
    mean_squared_diff <- mean(squared_diff)

    # Compute RMSE
    rmse <- sqrt(mean_squared_diff)
    return (rmse)
  }

  rmse_CMF         <- rmse(c(L%*%f), t (res$matrices$A)%*%res$matrices$B)



  return(out =list( rmse_CMF  = rmse_CMF,
                    #ARI = ARI_out,
                    noise_level = noise_level))

}
