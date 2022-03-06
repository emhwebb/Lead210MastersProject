functions {
  real age_depth(real delta, vector m, vector c, real d, int N, int M){
    real age;
    int index; //M=N+1
    int cond; //condition
    index = 2;
    cond = 0;
    //Handling for if d is the last depth
    if (d == c[M]){
      age = sum(m[1:N]*delta);
      //print("age", age);
    }
    //handling for if d is between 0 and the first depth
    else if (d >= c[1] && c[2] >= d){
      age = m[1]*d;
      //print("age", age); 
    }
    
    else while (cond == 0) {
      //if (d == c[index+1]) { //for when we look at the total accumulation in each core.
      //  age = sum(m[1:index]*delta);
      //  cond = 1;
      //}
      if (d > c[index] && c[index+1] >= d){
        age = sum(m[1:(index-1)]*delta)+m[index]*(d-c[index]);
       // print("age ", age);
        cond = 1;
        }
      else {
        index += 1;
        //print("index ", index)
      }
    }
    return age; //Note: need to eventually add error-handling since depths can't exceed max depth in c
  }
  
}

data {
  int<lower=0> N; // N: number of core sections in sample
  int<lower=0> L; //L, number of cores used to estimate supported activity
  int<lower=0> M; //M=N+1
  int<lower=0> Tot; //Tot=N+L, number total
  int a; //a controls degrees of freedom of t-likelihood. set to a=3 and b=4 for now
  int b; //b helps control variance of t-likelihood. 
  vector[N] x; // x are core section depths  
  vector[L] xL; //xL are core section depths for supported activity
  real<lower=0> delta; // delta is the thickness (in cm)
  vector<lower=0>[N] rho; // rho is density of each core section
  vector<lower=0>[L] rhoL; //rhoL is density for supported activity sections
  vector<lower=0>[N] sigma; // sigma are the estimated standard errors for each section
  vector<lower=0>[L] sigmaL; //sigmaL are estimated standard errors for the supported sections
  vector[N] p; //p: concentration of Pb210 measured at each core section
  vector[L] pL; //pL: concentration of Pb210 measured at the supported activity cores
  real lambda; //decay rate (a constant)
  //prior parameters are declared in the data section
  real<lower = 0> alpha_phi;
  real<lower = 0> beta_phi;
  real a_omega;
  real b_omega;
  //real omega;
  real<lower = 0> alpha_P;
  real<lower = 0> beta_P;
  real<lower = 0> alpha_a;
  real<lower = 0> beta_a;
}

transformed data {
  int<lower=0> K; //K=N-1, number of gamma error terms
  //int<lower=0> M; //M=N+1, number of depths including x0=0
  vector[N] y; // y is rho*p
  vector[L] yL; //yL is rhoL*pL
  vector[Tot] y_tot; //y_tot=(y,yL)
  real df; //degrees of freedom for t-likelihood 2*a
  vector<lower=0>[N] sigma_rho; // sigma are the estimated standard errors for each section
  vector<lower=0>[L] sigmaL_rho; //sigmaL are estimated standard errors for the supported sections
  vector<lower=0>[Tot] sigma_tot; //concatenate sigma and sigmaL
  matrix[N,N] Sigma; //diagonal matrix of variances for sigma
  matrix[L,L] SigmaL; //diagonal matrix of variances for sigmaL
  matrix[Tot,Tot] Sigma_tot; //diagonal matrix for variances for sigma_tot
  matrix[Tot,Tot] Sigma_scal; //variances for sigma_tot scaled by b/a
  real inv_lambda; //inverse decay rate
  vector[M] c; //c are core section depths of the first K=N-1 ##J=K-1## depths with 0 as first depth
  //vector[M] d; //d are core section depths with 0 as first depth and remaining depths contained
  K=N-1;
  //J=K-1;
  //M=N+1;
  df = 2*a;
  y = rho .* p;
  yL = rhoL .* pL;
  y_tot = append_row(y, yL);
  sigma_rho = rho .* sigma;
  sigmaL_rho = rhoL .* sigmaL;
  sigma_tot = append_row(sigma_rho, sigmaL_rho);
  inv_lambda = inv(lambda);
  c[1] = 0;
  c[2:M] = x[1:N];
  //d[1] = 0; 
  //d[2:M] = x[1:N];
  Sigma = diag_matrix(sigma_rho);
  SigmaL = diag_matrix(sigmaL_rho);
  Sigma_tot = diag_matrix(sigma_tot);
  Sigma_scal = Sigma_tot *b/a;
}

parameters {
  real<lower = 0> phi; //total supply
  real<lower = 0, upper = 1> omega; //memory parameter from Beta distribution
  real<lower = 0> alpha[K]; //gamma noise
  real<lower = 0> Ps; //supported activity
  real<lower = 0> mN; //m_N^th accumulation rate
  }
  
transformed parameters {
   vector<lower = 0>[N] A; //supported activity
   vector<lower = 0>[L] As; //supported activity for lead-214 measurements
   vector<lower = 0>[N] m; //sediment accumulation rates of each section
   real<lower = 0> ages[N]; //ages of each section 
   A = rho * Ps;
   As = rhoL * Ps;
   m[N] = mN; 
   for(n in 1:K){
    m[N-n] = omega*m[N-n+1]+(1-omega)*alpha[N-n]; //Compute the gamma autoregressive t.s.
   }
   for(n in 1:N){
    ages[n] = age_depth(delta, m, c, x[n], N, M); //Compute ages of each core section 
   }
}

model {
  vector[N] mu; //mean for core with unsupported activity
  vector[L] muL; //mean for supported portion of core
  vector[Tot] mu_tot; //combined mean
  for(n in 1:N){
    if(n == 1)
      mu[n] = A[n]+phi*inv_lambda*(1-exp(-lambda*ages[n]));
    else
      mu[n] = A[n]+phi*inv_lambda*(exp(-lambda*ages[n-1])-exp(-lambda*ages[n]));
  }
  for(n in 1:L){
    muL[n] = As[n];
  }
  mu_tot = append_row(mu, muL);
  
  //priors
  phi ~  gamma(alpha_phi, beta_phi); //prior for total supply
  omega ~ beta(a_omega, b_omega); //prior for memory parameter
  Ps ~  gamma(alpha_P, beta_P); //prior for gamma parameter
  mN ~ gamma(alpha_a, beta_a); //prior for m_K^th accumulation rate
  alpha ~ gamma(alpha_a, beta_a); //prior for the gamma noise for acumulation rates  
  
  //likelihood take from multivariate t-distribution
  //target += multi_normal_lpdf(y | mu, Sigma)+multi_normal_lpdf(yL | muL, SigmaL);
  target += multi_student_t_lpdf(y_tot | df, mu_tot, Sigma_scal);
}

generated quantities {
  vector[N] accu_rates; //accumulation rates given by 1/m (since m is yr/mm)
  accu_rates = inv(m);
}
