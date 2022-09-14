# Primeira ordem

smooth_primeira_ordem = function(y, dlmy_ , data){
  
  yFilt = dlmFilter(y, dlmy_)
  
  ySmooth = dlmSmooth(yFilt)
  
  attach(ySmooth)
  v = unlist(dlmSvd2var(U.S, D.S))
  pl = dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
  pu = dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
  detach()
  
  
  smooth_d = data.frame("Data" = data[,1]
                        ,"Dados" = as.numeric(data[,2])
                        ,"s" = dropFirst(ySmooth$s)
                        ,"pl"=pl
                        ,"pu"=pu)
  return(smooth_d)
}

kalman_segunda_ordem = function(y, dlmy, data){
  
  yFilt_ = dlmFilter(y, dlmy)
  
  ySmooth = dlmSmooth(yFilt_)
  
  
  erro = array(NA,dim=c(2,2,94))
  for(t in 2:95)
  {
    
    erro[,,t-1] = 1.96*sqrt(ySmooth$U.S[[t]] %*% diag(ySmooth$D.S[t,]^2) %*% t(ySmooth$U.S[[t]]))
  }
  
  
  smooth_d = data.frame("Data" = data[,1]
                        ,"Dados" = as.numeric(data[,2])
                        ,"ySmooth_n" = dropFirst(ySmooth$s[,2] )
                        ,"ySmooth_t" = dropFirst(ySmooth$s[,1] )
                        ,"pl"=dropFirst(ySmooth$s[,2])-erro[2,2,]
                        ,"pu"=dropFirst(ySmooth$s[,2])+erro[2,2,]
                        ,"pl1"=dropFirst(ySmooth$s[,1])-erro[1,1,]
                        ,"pu1"=dropFirst(ySmooth$s[,1])+erro[1,1,])
  return(smooth_d)
}