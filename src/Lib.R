library(tidyverse)

# 効用関数 (x部分)
utility_x <- function(x, sites, resp, param, err) {
  site_keys <- colnames(sites)
  resp_keys <- colnames(resp)
  
  1:nrow(sites) %>%
    map(function(i) {
      q <- param[site_keys] %*% (sites[i, site_keys] %>% as_vector())
      a <- param[resp_keys] %*% (resp[resp_keys] %>% as_vector())
      
      return(exp(a + err[i]) * log(x[i] * exp(q) + param["theta"]))
    }) %>%
    reduce(`+`)
}

# 効用関数
utility <- function(x, z, sites, resp, param, err) {
  log(z) + utility_x(x=x,
                     sites=sites,
                     resp=resp,
                     param=param,
                     err=err)
}



# 効用関数の推定

# ヤコビアン
jacobian <- function(param, x, prices, income, sites) {
  site_keys <- colnames(sites)
  
  uses <- which(x>0)
  
  mat <- matrix(nrow = length(uses), ncol = length(uses))
  
  for (i in 1:length(uses)) {
    for (j in 1:length(uses)) {
      mat[i,j] =
        if (i == j) {
          q <- param[site_keys] %*% (sites[uses[i], site_keys] %>% as_vector())
        
          prices[uses[i]] * x[uses[i]] / (income - prices %*% x) + 1 / (x[uses[i]] + param["theta"] / exp(q))
        } else {
          prices[uses[j]] * x[uses[j]] / (income - prices %*% x)
        }
    }
  }
  
  return(det(mat))
}



# ランダム項
error <- function(i, param, x, prices, income, sites, resp) {
  site_keys <- colnames(sites)
  resp_keys <- colnames(resp)
  
  q <- param[site_keys] %*% (sites[i, site_keys] %>% as_vector())
  a <- param[resp_keys] %*% (resp[resp_keys] %>% as_vector())

  return(-log(income - prices %*% x) + log(prices[i]) + log(x[i] + param["theta"] / exp(q)) - a)
}



# 対数尤度関数
log_likelihood <- function(param, xs, prices, income, sites, resps) {
  site_keys <- colnames(sites)
  resp_keys <- colnames(resps)
  
  1:nrow(resps) %>%
    map(function(i) {
      resp <- resps[i,]
      x <- xs[i,]
      
      log(abs(jacobian(param=param,
                       x=x,
                       prices=prices,
                       income=income,
                       sites=sites)))
      + 1:nrow(sites) %>%
        map(function(j) {
          up <- param["upsilon"]
          err <- error(i=j,
                       param=param,
                       x=x,
                       prices=prices,
                       income=income,
                       sites=sites,
                       resp=resp)
          if (x[j] == 0) {
            return(-exp(-err / up))
          } else {
            return(-exp(-err / up) - log(up) - err / up)
          }
        }) %>%
        reduce(`+`)
    }) %>%
    reduce(`+`)
}



# 費用便益分析

# ランダム項
error_sample <- function(param, x, prices, income, sites, resp) {
  site_keys <- colnames(sites)
  resp_keys <- colnames(resp)
  
  1:nrow(sites) %>%
    map(function(i) {
      err <- error(i=i,
                   param=param,
                   x=x,
                   prices=prices,
                   income=income,
                   sites=sites,
                   resp=resp)
      
      if (x[i] == 0) {
        up <- param["upsilon"]
        
        return(-log(-log(exp(-exp(-err / up) * runif(1))))) * up
      } else {
        return(err)
      }
      
    }) %>%
    flatten_dbl()
} 

# 個人の便益 (二分法)
cost_benefit <- function(x_without,
                         prices_without,
                         prices_with,
                         income,
                         sites_without,
                         sites_with,
                         resp,
                         param, 
                         conv # 収束条件
) {
  site_keys <- colnames(sites_without)
  resp_keys <- colnames(resp)
  
  err <- error_sample(param=param,
                      x=x_without,
                      prices=prices_without,
                      income=income,
                      sites=sites_without,
                      resp=resp)
  
  z_without <- income - prices_without %*% x_without
  
  utility_without <- utility(x=x_without,
                             z=z_without,
                             sites=sites_with,
                             resp=resp,
                             param=param,
                             err=err)
  
  z_min <- 0
  z_max <- exp(utility_without - utility_x(x=rep(0, nrow(sites_with)),
                                           sites=sites_with,
                                           resp=resp,
                                           param=param,
                                           err=err))
  z_with <- (z_min + z_max) / 2
  
  flg <- TRUE
  
  while (flg) {
    x_with <- 1:nrow(sites_with) %>%
      map(function(i) {
        q <- param[site_keys] %*% (sites_with[i, site_keys] %>% as_vector())
        a <- param[resp_keys] %*% (resp[resp_keys] %>% as_vector())
        
        return(z_with * exp(a + err[i]) / prices_with[i] - param["theta"] / exp(q))
      }) %>%
      flatten_dbl()
    
    utility_with <- utility(x=x_with,
                            z=z_with,
                            sites=sites_with,
                            resp=resp,
                            param=param,
                            err=err)
    
    str(utility_with - utility_without)
    
    if (utility_with - utility_without > conv) {
      z_max <- z_with
      z_with <- (z_min + z_max) / 2
    } else if (utility_with - utility_without < -conv) {
      z_min <- z_with
      z_with <- (z_min + z_max) / 2
    } else {
      flg <- FALSE
    }
  }
  
  browser()
  return(income - (z_with + prices_with %*% x_with))
}



# 平均便益
cost_benefit_sum_avg <- function(xs_without,
                                 prices_without,
                                 prices_with,
                                 income,
                                 sites_without,
                                 sites_with,
                                 resps,
                                 param, 
                                 conv, # 収束条件
                                 trials # 試行回数
) {
  1:trials %>%
    map(function(.) {
      1:nrow(resps) %>%
        map(function(i) {
          x_without <- xs_without[i,]
          resp <- resps[i,]
          
          return(cost_benefit(x_without=x_without,
                              prices_without=prices_without,
                              prices_with=prices_with,
                              income=income,
                              sites_without=sites_without,
                              sites_with=sites_with,
                              resp=resp,
                              param=param, 
                              conv=conv))
        }) %>%
        reduce(`+`)
    }) %>%
    reduce(`+`) / trials
}

