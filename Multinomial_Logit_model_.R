# Script of Multinomial Logit model in Stan. 
# The utility function is U_{njt}=x_{njt}*\beta+ \epsilon_{njt}, where n is the subject, 
# j the alternative and t the choice set.
# The function used to obtain the first and the last observation in each choice set is taken from the 
# link: http://rpubs.com/jimsavage/using_decisionmaker_variables_in_mixed_logit

# Clear the workspace
rm(list = ls())

# Load the libararies
library("rstan")
library("dplyr") 
library("rstanarm") 
library("rstudioapi") 

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())


### Model ###

mnl<-"
data { 
int<lower=1> P;  //Number of covariates
int<lower=2> S;  // Number of choice options
int<lower=1> N; //Number of rows
int<lower=1,upper=S> y[N];   // Alternative selected by respondent in each choice set 
int<lower=1> N2;  // Number of rows
matrix[N2, P]  X;  // Matrix of predictors
int start[N]; // first observation for each choice set
int end[N]; // last observation for each choice set
}

parameters {
vector[P] beta; // Vector of coefficients
}

transformed parameters {
vector[S] utility[N];
for (n in 1:N)
utility[n]=X[start[n]:end[n]]*beta; // utility function
}

model {
beta ~ normal(0,5); // prior distribution

for (n in 1:N)
y[n] ~ categorical_logit(utility[n]);   // log probabilities
}

"

# Create a data list containing all the data specified in the model above
data_list <- list(N=2904,
                  N2=11616,
                  P=11,
                  S=4,
                  X=X,
                  y=y,
                  start = indexes$start,
                  end = indexes$end)


### Estimate ###
model_mnl= stan(model_code = mnl, iter = 6000, seed = 1987, warmup = 500,chains = 3,  data = data_list)
