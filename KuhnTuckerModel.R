library(tidyverse)
# library(purrr)

# スタディ

# 暫定
f <- function(x, y) {
  x + y ^ 2
}



uses <- c(1,3,5,7) #　暫定

l <- length(uses)



mat <- matrix(nrow = l, ncol = l)

for (i in 1:length(uses)) {
  for (j in 1:length(uses)) {
    mat[i,j] = f(uses[i], uses[j])
  }
}

jacob <- det(mat)


# 初期値
site_num <- 10 # 暫定
qual_keys <- c("A", "B", "C") # 暫定
attr_keys <- c("D", "E", "F") # 暫定

# 効用関数
u <- function(x, z, attr, qual, err, param) {
  log(z) + 1:site_num %>%
    map(function(i) {
      a <- err[i] + attr_keys %>%
        map(function(attr_key) {
          param[attr_key] %*% attr[attr_key]
        }) %>%
        reduce(`+`)
      
      q <- qual_keys %>%
        map(function(qual_key) {
          param[qual_key] %*% qual[i, qual_key]
        }) %>%
        reduce(`+`)
      
      a * log(x[i] * q + param["theta"])
    }) %>%
    reduce(`+`)
}

