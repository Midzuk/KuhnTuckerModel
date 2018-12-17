library(tidyverse)

source("Lib.R")

# 初期値
# 環境質
sites <- read_csv("site_sample.csv") # 暫定
site_num <- nrow(sites)

# 回答者属性
resps <- read_csv("person_sample.csv") # 暫定
resp_num <- nrow(resps)

# パラメータのキー
keys <- c("upsilon", "theta", colnames(resps), colnames(sites))

xs <- matrix(round(runif(5*10) * 10), nrow = 5, ncol = 10)
prices <- 10:1
income <- 10000

ll <- function(param) {
  names(param) <- keys
  log_likelihood(param=param,
                 xs=xs,
                 prices=prices,
                 income=income,
                 sites=sites,
                 resps=resps)
}

ll(c(c(1, 1), rep(1, length(keys) - 2)))

# サンプル
res <- optim(c(c(1, 1), rep(0, length(keys) - 2)),
             ll,
             control=list(fnscale=-1),
             hessian = TRUE)

param <- res$par
names(param) <- keys



# 平均便益
cost_benefit_sum_avg(xs_without=xs,
                     prices_without=prices,
                     prices_with=rep(3,10),
                     income=income,
                     sites_without=sites,
                     sites_with=sites,
                     resps=resps,
                     param=param, 
                     conv=0.001,
                     trials=10)
