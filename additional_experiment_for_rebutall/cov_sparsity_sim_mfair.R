library(mfair)

sim_func_cov_mfair <- function( N=2000, # number of row
                              L=100, #number of columns
                              K=2, #number of factor
                              P1=10 , # number of cov for row /loadings
                              P2=10, # number of cov for col /factors
                              beta0=0,
                              beta1=3,
                              noise_level= 3,
                              max_iter_cEBMF=10,
                              max_iter_como=10,
                              max_class=10,
                              seed,
                              epoch=50
){


  library(softImpute)
  library(susieR)
  library(mvtnorm)
  data(N3finemapping)
  attach(N3finemapping)
  library(comoR)
  library(tensorflow)
  library(keras)
  if (missing(seed)){
    set.seed(rpois(1, 10))
  }else{
    set.seed(o)
  }









  X_l <-   rmvnorm(N,sigma=cov(N3finemapping$X[1:100,1:P1]))
  X_f <-    rmvnorm(L,sigma=cov(N3finemapping$X[1:100,1:P2]))


  true_pos_l <- 1:10
  true_pos_f <-  1:10




  true_l  <- list()
  true_f  <- list()

  for( k in 1:K){



    samp_prob <- 1/(1 +exp(-(beta0+beta1*X_l[,k])))
    lk <- c()
    mix <- c()
    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      lk <- c(lk, mix[i]*rnorm(1,sd=1))
    }


    samp_prob <- 1/(1 +exp(-(beta0+beta1*X_f[,k])))
    fk <- c()
    mix <- c()
    for ( j in 1:L){

      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[j], samp_prob[j])))
      fk <- c(fk, mix[j]*rnorm(1,sd=1))
    }

    true_l[[k]] <- lk
    true_f[[k]] <- fk

  }


  Y_true <- Reduce("+", lapply( 1:K, function(k) true_l[[k]]%*%t(true_f[[k]])
  )
  )

  Y_obs <- Y_true+ matrix( rnorm(N*L, sd= noise_level), ncol=L)

  mfairObject <- createMFAIR(Y_obs,as.data.frame(X_l), K_max = 3)

  mfairObject <- fitGreedy(mfairObject, sf_para = list(verbose_loop = FALSE))

  Y_hat <- predict(mfairObject)

  rmse = function(mat1 , mat2){

    squared_diff <- (mat1-  mat2)^2

    # Compute mean of squared differences
    mean_squared_diff <- mean(squared_diff)

    # Compute RMSE
    rmse <- sqrt(mean_squared_diff)
    return (rmse)
  }

  rmse_mfair         <- rmse(Y_true,Y_hat)










  par <-  c( N,
             L ,
             K ,
             P1 ,
             P2 ,
             beta0 ,
             beta1 ,
             noise_level ,
             max_iter_cEBMF ,
             max_iter_como
  )
  names( par)  <-  c( "N",
                      "L"  ,
                      "K" ,
                      "P1"  ,
                      "P2" ,
                      "beta0"  ,
                      "beta1" ,
                      "noise_level"  ,
                      "max_iter_cEBMF" ,
                      "max_iter_como"
  )

  out <- list(rmse      =  rmse_mfair ,
              par       = par
  )


  return( out)
}
