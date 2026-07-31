data{
  int n;
  int N;
  int Jmax;
  int J[n];
  int Nbetas;
  matrix[n, Jmax] time;
  matrix[n, Jmax] delta;
  matrix[N, Nbetas] X;
}

parameters{
  vector[Nbetas] beta;
  real<lower=0> alpha;
}

transformed parameters{
  real lambda = exp( beta[1] );
}

model{
  matrix[n, Jmax] logHaz;
  matrix[n, Jmax] cumHaz;
  int k = 0;
  // FRAILTY SPECIFICATION
  for(i in 1:n){
     for(j in 1:J[i]){
        // Log-hazard function
        logHaz[i,j] = log(alpha) + (alpha-1) * log(time[i,j]) + X[k+j,] * beta;
        // Cumulative hazard function
        cumHaz[i,j] = (time[i,j]^alpha) * exp( X[k+j,] * beta);

        target += delta[i,j] * logHaz[i,j] - cumHaz[i,j];
     }
     k = k + J[i];
  }

  // LOG-PRIORS
  // Coefficients
  target += normal_lpdf(beta | 0, sqrt(100));

  // Weibull shape parameter
  target += gamma_lpdf(alpha | 0.1, 0.1);
}
