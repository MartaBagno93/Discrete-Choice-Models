# Script of Multinomial Mixed Logit model in Stan. 
# The utility function is U_{njt}=x1_{njt}*\beta_{n}'+x2_{njt}*\beta+ \epsilon_{njt}, where n is the subject, j the alternative and t the choice set.
# X1 matrix of predictors for random (beta_{n}) while X2 is a matrix of predictors for fixed parameters 
# Instead of adopt a multivariate normal distribution for the vector \beta_{n} I use the non-centered parameterization:
# \beta_{n}=\mu+\sigma*z_{n}
# The function used to obtain the first and the last observation in each choice set is taken from the 
# the link: http://rpubs.com/jimsavage/using_decisionmaker_variables_in_mixed_logit
# while the \beta_{n} parameterization is taken from: https://gist.github.com/khakieconomics/d01b43a50c7904bd73e8c190e0a8ecf8

# Clear the workspace
rm(list = ls())

# Load the libraries
library("rstan")
library("dplyr") 
library("rstanarm") 
library("rstudioapi") 

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

### Model ###

mixed = "
data {
int<lower=1> N;  // Number of rows
int<lower=1> P1; //  Number of covariates for random parameters
int<lower=1> P2; //  Number of covariates for fixed parameters
int<lower=1> R;     // Number of respondents
int S;  // Number of choice options
int<lower=1> N2; // Number of rows
matrix[N2, P1]  X1;           // Matrix of predictors for random parameters
matrix[N2, P2]  X2;           // Matrix of predictors for fixed parameters
int<lower=1,upper=S> y[N];    // Alternative selected by respondent in each choice set 
int<lower=1,upper=R> id[N]; // index for respondent
int start[N]; // first observation for each choice set
int end[N]; // last observation for each choice set
}

parameters {
vector[P1] Beta_Random; // location vector of random coefficients
vector[P2] Beta_Fisso; // Vector of fixed coefficients
matrix[R,P1] z; 
vector<lower = 0>[P1] sigma;   // diagonal of covariance matrix
}

transformed parameters {
vector[S] utility[N];
matrix[R,P1] beta_individual;
beta_individual = rep_matrix(Beta_Random', R) + z*diag_matrix(sigma); // non-centered parameterization


for (n in 1:N){
utility[n]= X1[start[n]:end[n]]*beta_individual[id[n]]'; 
utility[n]+=X2[start[n]:end[n]]*Beta_Fisso;  // utility function
}
}

model {
Beta_Random ~ normal(0, 5); // prior distribution for mean of random coefficients
Beta_Fisso ~ normal(0, 5); // prior distribution for fixed coefficients
sigma~ cauchy(0, 5); // prior distribution for diagonal of covariance matrix
to_vector(z) ~ normal(0, 1); 

for (n in 1:N) {
y[n] ~ categorical_logit(utility[n]);  // (unnormalized) log probability
}
}

"

### Data ####
P2=5
P1=6

# Matrix of random predictors
X1=matrix(c(dd$alt1, dd$alt2, dd$alt3, dd$X1, dd$X2, dd$X3), ncol=6, nrow=11616)
dim(X1)

# Matrix of fixed predictors
X2=matrix(c(dd$X4, dd$X5, dd$X6, dd$X7, dd$X8), ncol=5, nrow=11616)
dim(X2)

# Create a data list containing all the data specified in the model above
data_list <- list(R = 484,
                  N=2904,
                  N2=11616,
                  P=11,
                  P1=6,
                  P2=5,
                  S=4,
                  X1=X1,
                  X2=X2,
                  y=y,
                  start = indexes$start,
                  end = indexes$end, 
                  id = id)


### Estimate ###
model_mixed= stan(model_code = mixed, iter = 15000, seed = 1987, warmup = 5000,chains = 4,  data = data_list)
