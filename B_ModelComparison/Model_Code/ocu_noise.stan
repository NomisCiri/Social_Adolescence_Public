data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];//number of trials for each subject
  int<lower=0, upper=1> choice[N, T];
  int<lower=0, upper=3> condition[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  //int<lower=0, upper=3> group[N, T]; // 0: solo, 1: ss, 2: mix, 3: rr
  real safe_payoff[N, T];
  real risky_payoff[N, T];
  real<lower=0, upper=1> p_gamble[N, T];
  
}

transformed data {
}

parameters {
  vector[3] mu_p;
  vector<lower=0>[3] sigma;
  vector[N] rho_p;
  vector[N] tau_p;
  vector[N] ocu_p;
}

//transformed parameters {
 // vector<lower=0, upper=2>[N] rho;
  //vector<lower=0>[N] tau;
 // vector[N] ocu;
// i dont get this hyperparemter stuff jet. but lets see.
//  for (i in 1:N) {
///    rho[i] = Phi_approx(mu_p[1] + sigma[1] * rho_p[i]) * 2;
//  }
//  tau = exp(mu_p[2] + sigma[2] * tau_p);
//  ocu = mu_p[3] + sigma[3] * ocu_p;
//}

model {
  // peer_ocu
  // hyper parameters... hyperpriors for all parameteres.
  //mu_p  ~ normal(0, 1.0);
  //sigma ~ cauchy(0, 5.0);

  // individual parameters w/ Matt trick
  rho_p ~ normal(0, 1.0);
  tau_p ~ normal(0, 1.0);
  ocu_p ~ normal(0, 1.0);

  for (i in 1:N) {
    for (t in 1:Tsubj[i]) {
      real U_safe;
      real U_risky;

      U_safe  = pow(safe_payoff[i, t], rho_p[i]);
      U_risky = p_gamble[i, t] * pow(risky_payoff[i, t], rho_p[i]);
      if (condition[i, t] == 1) {  // safe-safe
        choice[i, t] ~ bernoulli_logit((tau[i]+ocu[i]) * (U_risky - U_safe));
      }
      if (condition[i, t] == 3) {  // risky-risky
       choice[i, t] ~ bernoulli_logit((tau[i]+ocu[i]) * (U_risky - U_safe));
      } else {
      choice[i, t] ~ bernoulli_logit((tau[i]+ocu) * (U_risky - U_safe));
      }
    }
  }
}

generated quantities {
  real<lower=0, upper=2> mu_rho;
  real<lower=0> mu_tau;
  real mu_ocu;

  // For log likelihood calculation
  real log_lik[N];

  // For posterior predictive check
  real y_pred[N, T];

  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_rho = Phi_approx(mu_p[1]) * 2;
  mu_tau = exp(mu_p[2]);
  mu_ocu = mu_p[3];

  { // local section, this saves time and space
    for (i in 1:N) {
      // Initialize values
      log_lik[i] = 0.0;
      
      for (t in 1:Tsubj[i]) {
        real U_safe;
        real U_risky;

        U_safe  = pow(safe_payoff[i, t], rho[i]);
        U_risky = p_gamble[i, t] * pow(risky_payoff[i, t], rho_p[i]);
        if (condition[i, t] == 1) {  // safe-safe
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf(choice[i, t] | (tau[i]+ocu[i]) * (U_risky - U_safe));
        y_pred[i, t] = bernoulli_rng(inv_logit((tau[i]+ocu[i])  * (U_risky - U_safe)));
        }
        if (condition[i, t] == 3) {  // risky-risky
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf(choice[i, t] | (tau[i]+ocu[i])  * (U_risky - U_safe));
        y_pred[i, t] = bernoulli_rng(inv_logit((tau[i]+ocu[i])  * (U_risky - U_safe)));
        }else{
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf(choice[i, t] | tau[i]  * (U_risky - U_safe));
        // generate posterior prediction for current trial
        y_pred[i, t] = bernoulli_rng(inv_logit(tau[i] * (U_risky - U_safe)));
        }

      }
    }
  }
}


