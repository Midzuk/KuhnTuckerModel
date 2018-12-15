library(tidyverse)



# 初期値
site_num <- 10 # 暫定

attr_keys <- c("D", "E", "F") # 暫定
qual_keys <- c("A", "B", "C") # 暫定
keys <- c("upsilon", "theta", attr_keys, qual_keys)

param <- runif(length(keys)) #暫定
names(param) <- keys



# 効用関数
utility <- function(x, z, qual, attr, err, param) {
  log(z) + 1:site_num %>%
    map(function(i) {
      q <- param[qual_keys] %*% qual[i, qual_keys]
      a <- err[i] + param[attr_keys] %*% attr[attr_keys]
      
      return(a * log(x[i] * q + param["theta"]))
    }) %>%
    reduce(`+`)
}



error <- function(i, param, x, price, income, qual, attr) {
  q <- param[qual_keys] %*% qual[i, qual_keys]
  a <- param[attr_keys] %*% attr[attr_keys]
  
  return(-log(income - price %*% x) + log(price[i]) + log(x[i] * exp(q) + param["theta"]) - a - q)
}



log_likelihood <- function(param, x, price, income, qual, attr) {
  log(abs(jacobian(param=param, x=x, price=price, income=income, qual=qual))) + 1:site_num %>%
    map(function(i) {
      up <- param["upsilon"]
      err <- error(i=i, param=param, x=x, price=price, income=income, qual=qual, attr=attr)
      
      if (x == 0) {
        return(-exp(-err / up))
      } else {
        return(-exp(-err / up) - log(up) - err / up)
      }
    }) %>%
    reduce(`+`)
}


jacobian <- function(param, x, price, income, qual) {
  uses <- x[x > 0]
  
  mat <- matrix(nrow = length(uses), ncol = length(uses))
  
  for (i in 1:length(uses)) {
    for (j in 1:length(uses)) {
      mat[i,j] = if (i = j) {
        q <- param[qual_keys] %*% qual[i, qual_keys]
        
        p[i] * x[i] / (income - price %*% x) + exp(q) / (x[i] * exp(q) + param["theta"])
      } else {
        p[j] * x[j] / (income - price %*% x)
      }
    }
  }
  
  return(det(mat))
}









