data {
  int<lower=1> N;// Number of Subjects
  int<lower=1> T;// Trials
  int<lower=1, upper=T> Tsubj[N];//number of trials for each subject per Group
  int<lower=0, upper=1>choice[N, T];
  int<lower=0, upper=1>risk1Ambig0[N,T]; 
  int<lower=0, upper=3>condition[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  //int<lower=0, upper=3> group[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  real safe_payoff[N, T];
  real risky_payoff[N, T];
	
  real<lower=0, upper=1> ambigLevel[N, T];
  real<lower=0, upper=1> p_gamble[N, T];
}

transformed data {
}

parameters {
  //Group mus.
  real<lower=0, upper=2> mu_rho ;
  real<lower=-1, upper=1> mu_beta ;
  real<lower=0> mu_tau ;
  real mu_ocuSafe;
  real mu_ocuRisk;
  
  //roup Sigmas
  real<lower=0>sigma_rho;
  real<lower=0>sigma_beta;
  real<lower=0>sigma_tau;
  real<lower=0>sigma_ocuSafe;
  real<lower=0>sigma_ocuRisk;

  //individual.
  real rho_p[N];
  real beta_p[N];
  real tau_p[N];
  real  ocu_pSafe[N];
  real  ocu_pRisk[N];
}

transformed parameters {
 real<lower=0, upper=2> rho[N];
 real<lower=-1, upper=1> beta[N];
 real<lower=0> tau[N];
 real  ocuSafe[N];
 real  ocuRisk[N];
 //A Normal(μ,σ) distribution
 //A Normal(μ,σ) distribution, like other distributions in the location–scale distribution family, can be reparameterized to 
 //be sampled from a unit normal distribution that is multiplied by the scale parameter σ and then shifted with the location parameter μ. Formally,
//  ξ∼Normal(μξ,σξ)
//is mathematically equivalent to
//  ξ′∼Normal(0,1) ........ Which is defined in the nmodel
//   ξ∼Normal(μξ+ξ′·σξ). which is defined over here.

  for (i in 1:N) {// subs
	  rho[i] = Phi_approx(mu_rho + sigma_rho * rho_p[i]) * 2; // i dont quite understand this part. but it makes the correct estimates. Good i would say.
	 	  beta[i]= (Phi_approx(mu_beta + sigma_beta * beta_p[i]) * 2)-1;
	       tau[i] = exp(mu_tau + sigma_tau * tau_p[i]);
	       ocuSafe[i] = mu_ocuSafe + sigma_ocuSafe * ocu_pSafe[i];
	       ocuRisk[i] = mu_ocuRisk + sigma_ocuRisk * ocu_pRisk[i];
	  
}
}//end transfomred params

model {
  //hyper parameters... hyperpriors for all parameteres.
   // i could in principle set different hyperpriors for each 
    //hyper parameters... hyperpriors for all parameteres.
   // i could in principle set different hyperpriors for each 
    mu_rho  ~ normal(0,1);
	  mu_beta  ~ normal(0,1);
    mu_tau  ~ normal(0,1);
    mu_ocuSafe  ~ normal(0,1);
    mu_ocuRisk  ~ normal(0,1);
	

    
    sigma_rho ~ normal(0, 0.2);
	sigma_beta ~ normal(0, 0.2);
    sigma_tau ~ normal(0, 0.2);
    sigma_ocuSafe ~ normal(0, 0.2);
    sigma_ocuRisk ~ normal(0, 0.2);
	
    // individual parameters w/ Matt trick


  // I define the distributions in the loop bc of my nested data i have too many dimensions for vectorizing.
    for (i in 1:N) {
    rho_p[i] ~ normal(0, 1.0);
	beta_p[i] ~ normal(0, 1.0);
    tau_p[i] ~ normal(0, 1.0);
    ocu_pSafe[i] ~ normal(0, 1.0);
    ocu_pRisk[i] ~ normal(0, 1.0);
	
      for (t in 1:Tsubj[i]) {
        real U_safe;
        real U_risky;
		// is it a risk trial?
		if(risk1Ambig0[i, t]==1){
	        U_safe  = pow(safe_payoff[i, t], rho[i]);
	        U_risky = p_gamble[i,t] * pow(risky_payoff[i, t], rho[i]);
		}
		if(risk1Ambig0[i, t]==0){// is it an ambiguity trial?
	        U_safe  = pow(safe_payoff[i, t], rho[i]);
	        U_risky = (0.5-(beta[i]*(ambigLevel[i,t]/2))) * pow(risky_payoff[i, t], rho[i]);
		}
		
        if (condition[i, t] == 1) {  // safe-safe
          U_safe = U_safe + ocuSafe[i];
        }
        if (condition[i, t] == 3) {  // risky-risky
          U_risky = U_risky + ocuRisk[i];
        }
        choice[i, t] ~ bernoulli_logit((tau[i])* (U_risky - U_safe));
      }
    }
}


generated quantities {
    real<lower=0, upper=2> mu_rhoPred;
    real<lower=0> mu_tauPred;
    real mu_ocuSafePred;
    real mu_ocuRiskPred;
 
    real<lower=-1, upper=1> mu_betaPred;
     // For log likelihood calculation
     real log_lik[N,T];

     // For posterior predictive check
     real y_pred[N,T];

     // Set all posterior predictions to 0 (avoids NULL values)
       for (i in 1:N) {
         for (t in 1:T) {
           y_pred[i,t] = -1;
   		log_lik[i,t]=0;
         }
       }
  
     mu_rhoPred = Phi_approx(mu_rho) * 2;
     mu_tauPred = exp(mu_tau);
     mu_ocuSafePred = mu_ocuSafe;
     mu_ocuRiskPred = mu_ocuRisk;
  
     mu_betaPred= (Phi_approx(mu_beta) * 2)-1;
  // I NEED TO SETUP AMBIGUITYTHINGS HERE.
  
  { // local section, this saves time and space
      for (i in 1:N) {
        // Initialize values
        for (t in 1:Tsubj[i]) {
          real U_safe;
          real U_risky;
  		if(risk1Ambig0[i, t]==1){
  	        U_safe  = pow(safe_payoff[i, t], rho[i]);
  	        U_risky = p_gamble[i,t] * pow(risky_payoff[i, t], rho[i]);
  		}
  		if(risk1Ambig0[i, t]==0){// is it an ambiguity trial?
  	        U_safe  = pow(safe_payoff[i, t], rho[i]);
  	        U_risky = (0.5-(beta[i]*(ambigLevel[i,t]/2))) * pow(risky_payoff[i, t], rho[i]);
  		}
		
          if (condition[i, t] == 1) {  // safe-safe
            U_safe = U_safe + ocuSafe[i];
          }
          if (condition[i, t] == 3) {  // risky-risky
            U_risky = U_risky + ocuRisk[i];
          }
          log_lik[i,t] = bernoulli_logit_lpmf(choice[i, t] | (tau[i]) * (U_risky - U_safe));// i defined it before. i dont want it "choice wise"
          // but i produce "pointwise aggregates"
          // generate posterior prediction for current trial
          y_pred[i, t] = bernoulli_rng(inv_logit((tau[i]) * (U_risky - U_safe)));
        }//end Trials
      }//end Subs
  }//end local
}


