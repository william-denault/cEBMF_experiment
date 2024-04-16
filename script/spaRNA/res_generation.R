source("/home/wdenault/cEBMF_RCC_experiments/script/analyse_res_spaRNA.R")

for ( i in 1:12 ) {
  analyse_slice(i)
  print(i)
}
