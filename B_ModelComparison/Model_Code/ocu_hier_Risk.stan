data {
  int<lower=1> N;// Number of Subjects
  int<lower=1> T;// Trials
  int<lower=1> G; // Different Groups
  int<lower=1, upper=T> Tsubj[N,G];//number of trials for each subject per Group
  int<lower=0, upper=1> choice[N, T, G];
  int<lower=0, upper=3> condition[N, T, G]; // 0: solo, 1: ss, 2: mix, 3: rr
  //int<lower=0, upper=3> group[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  real safe_payoff[N, T, G];
  real risky_payoff[N, T, G];
  real<lower=0, upper=1> p_gamble[N, T, G];
  
}

transformed data {
}

parameters {
  //Group mus.
  real<lower=0, upper=2> mu_rho[G] ;
  real<lower=0> mu_tau[G] ;
  real mu_ocu[G];
  
  //roup Sigmas
  real<lower=0>sigma_rho[G];
  real<lower=0>sigma_tau[G];
  real<lower=0>sigma_ocu[G];

  //individual.
  real rho_p[N,G];
  real tau_p[N,G];
  real ocu_p[N,G];
}

transformed parameters {
 real<lower=0, upper=2> rho[N,G];
 real<lower=0> tau[N,G];
 real<lower=0> ocu[N,G];
 //A Normal(μ,σ) distribution, like other distributions in the location–scale distribution family, can be reparameterized to 
 //be sampled from a unit normal distribution that is multiplied by the scale parameter σ and then shifted with the location parameter μ. Formally,
//  ξ∼Normal(μξ,σξ)
//is mathematically equivalent to
//  ξ′∼Normal(0,1) ........ Which is defined in the nmodel
//   ξ∼Normal(μξ+ξ′·σξ). which is defined over here.

for (g in 1:G){// groups.
  for (i in 1:N) {// subs
      rho[i,g] = Phi_approx(mu_rho[g] + sigma_rho[g] * rho_p[i,g]) * 2;
	  // transform the values so that they cant be smaller than 1. 
      tau[i,g] = exp(mu_tau[g] + sigma_tau[g] * tau_p[i,g]);
      ocu[i,g] = exp(mu_ocu[g] + sigma_ocu[g] * ocu_p[i,g]);
      }//endsubs
    }//endgroups
}//end transfomred params

model {
  // peer_ocu
   //hyper parameters... hyperpriors for all parameteres.
   // i could in principle set different hyperpriors for each 
  for (g in 1:G){// grouploop.
      mu_rho[g]  ~ normal(0,1);
      mu_tau[g]  ~ normal(0,1);
     if (g == 1){
    mu_ocu[g]  ~ normal(0,1);
    } else if (g == 2){
    mu_ocu[g]  ~ normal(0,1);
    }
    
    sigma_rho[g] ~ normal(0, 0.2);
    sigma_tau[g] ~ normal(0, 0.2);
    sigma_ocu[g] ~ cauchy(0, 1.0);
    // individual parameters w/ Matt trick

  // Fuck Vercorizing I define the distributions in the loop.
    for (i in 1:N) {
    rho_p[i,g] ~ normal(0, 1.0);
    tau_p[i,g] ~ normal(0, 1.0);
    ocu_p[i,g] ~ normal(0, 1.0);
    
      for (t in 1:Tsubj[i,g]) {
        real U_safe;
        real U_risky;
        U_safe  = pow(safe_payoff[i, t, g], rho[i,g]);
        U_risky = p_gamble[i, t, g] * pow(risky_payoff[i, t,g], rho[i,g]);
        if (condition[i, t, g] == 1) {  // safe-safe
          U_risky = U_risky + ocu[i,g];
        }
        if (condition[i, t , g] == 3) {  // risky-risky
          U_risky = U_risky + ocu[i,g];
        }
        choice[i, t, g] ~ bernoulli_logit((tau[i,g]) * (U_risky - U_safe));
      }
    }
  }
}


generated quantities {
 real<lower=0, upper=2> mu_rhoPred[G];
 real<lower=0> mu_tauPred[G];
 real mu_ocuPred[G];

  // For log likelihood calculation
  real log_lik[N,T,G];

  // For posterior predictive check
  real y_pred[N,T,G];

  // Set all posterior predictions to 0 (avoids NULL values)
  for (g in 1:G){
    for (i in 1:N) {
      for (t in 1:T) {
        y_pred[i,t,g] = -1;
      }
    }
  }
  
  for (g in 1:G){
  mu_rhoPred[g] = Phi_approx(mu_rho[g]) * 2;
  mu_tauPred[g] = exp(mu_tau[g]);
  mu_ocuPred[g] = mu_ocu[g];
  }
  
  { // local section, this saves time and space
  for (g in 1:G){
      for (i in 1:N) {
        // Initialize values
  
        for (t in 1:Tsubj[i,g]) {
          real U_safe;
          real U_risky;
          U_safe  = pow(safe_payoff[i, t,g], rho[i,g]);
          U_risky = p_gamble[i, t,g] * pow(risky_payoff[i, t,g], rho[i,g]);
          if (condition[i, t,g] == 1) {  // safe-safe
            U_risky = U_risky + ocu[i,g];
          }
          if (condition[i, t,g] == 3) {  // risky-risky
            U_risky = U_risky + ocu[i,g];
          }
          log_lik[i,t,g] = bernoulli_logit_lpmf(choice[i, t,g] |(tau[i,g]) * (U_risky - U_safe));// i defined it before. i dont want it "choice wise"
          // but i produce "pointwise aggregates"
          // generate posterior prediction for current trial
          y_pred[i, t,g] = bernoulli_rng(inv_logit((tau[i,g]) * (U_risky - U_safe)));
        }//end Trials
      }//end Subs
    }//endGroups
  }//end local
}


