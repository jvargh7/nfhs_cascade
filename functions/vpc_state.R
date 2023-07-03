# Modified from https://github.com/jeswinbaby/nfhs_overweight/blob/main/codes/20_vpc.R

vpc_state = function(fit, val) {
  sds = sqrt(fit@RP[1:3])
  betas = fit@FP
  row.names(val) = val$variable
  vals = val[names(betas),]$value
  fn = sum(betas*vals)
  omega.i = sds[3] #SD of one level (say, PSU)
  tau.i = sds[1] # Round
  theta.i = sds[2] # State
  draws_v = 20
  draws_w = 30
  ##Simulation based ICC
  u = rnorm(200, 0, omega.i)
  w = rnorm(draws_w, 0, tau.i)
  v = rnorm(draws_v, 0, theta.i)
  
  p_var_mat = matrix(nrow=200,ncol=draws_v*draws_w)
  u_var_mat = matrix(nrow=draws_v,ncol=draws_w)
  u_p_mat = matrix(nrow=draws_v,ncol=draws_w)
  w_p = matrix(nrow=1,ncol=draws_w)
  v_p = NA
  
  for(i in 1:length(v)){
    for(j in 1:length(w)){
      p = exp (fn + u + w[j] + v[i])/(1+exp (fn +u + w[j] + v[i]))
      # Binomial variance of p for each combination of u, fixed w, fixed v
      p_var_mat[,((i-1)*draws_w + j)] = p*(1-p)
      
      # Mean probability for each u
      # u_var[i,j] = mean(p)*(1-mean(p))
      
      # Overall variance within PSUs
      u_var_mat[i,j] = var(p)
      
      # Means across all PSUs for splitting variance
      u_p_mat[i,j] = mean(p)
    }
  }
  
  w_p = colMeans(u_p_mat)
  v_p = rowMeans(u_p_mat)
  
  p_var = matrix(p_var_mat, nrow=200*draws_v*draws_w)
  
  v_var = var(v_p)
  w_var = var(w_p)
  
  # To check
  u_p = matrix(u_p_mat,nrow=draws_v*draws_w)
  ## Residual covariance from sampling error
  var(u_p) - (sum(v_var + w_var))
  
  # Mean of within PSU variance
  u_var = mean(u_var_mat)
  
  sim_icc = mean(p_var) / (mean(p_var) + u_var + v_var + w_var)
  psu_icc = u_var / (mean(p_var) + u_var + v_var + w_var)
  rnd_icc = w_var / (mean(p_var) + u_var + v_var + w_var)
  stt_icc = v_var / (mean(p_var) + u_var + v_var + w_var)
  #cat("(From Marginal Model) Degree of heterogeneity between women relative to total as simulated ICC: ", round(sim_icc,3))
  #cat("\n PSU: ", round(psu_icc,3))
  #cat("\nState: ", round(stt_icc,3))
  #cat("\nRound ", round(rnd_icc,3))
  out = c(round(sim_icc,3)
          , round(psu_icc,3)
          , round(stt_icc,3)
          , round(rnd_icc,3))
  names(out)= c("ind",
                "PSU",
                "state",
                "round")
  return(out)
}