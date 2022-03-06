age_depth <- function(delta, m, c, d){
  if(d == c[length(c)]){
    age = sum(m*delta)}
  else{
  index = 1
  cond = F
  while(cond == F){
    if(d >= c[index] && c[index+1]>=d){ 
      age = sum(m[1:index]*delta)+m[index+1]*(d-c[index])
      cond = T}
    else {
      print(paste("iter", index))
      index = index+1
      
  }
  }
  }
  age
}

age_depth(delta = 1, m = rep(0.5, 29), c = 0:29, d = 29)


28 >=c[28]
c[29]>=28

27 >=c[27]
c[28]>27
