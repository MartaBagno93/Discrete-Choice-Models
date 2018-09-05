# Script of Latent Class Multinomial Logit model in Stan. 
# The utility function is U_{njt}=x_{njt}*\beta_{g}+ \epsilon_{njt}, where n is the subject, j the alternative and t the choice set.
# \beta_{g} is the parameter of class g.
# conditional_prob=exp(x_{njt}*\beta_{g})/(\sum_{k=1}^{J} exp(x_{nkt}*\beta_{g}))
# The function used to obtain the first and the last observation in each choice set is taken from the 
# link: http://rpubs.com/jimsavage/using_decisionmaker_variables_in_mixed_logit
# The generated quantites I have used the information in "https://books.google.it/books?id=CLZBDwAAQBAJ&pg=PT602&lpg=PT602&dq=a+student+guide+to+bayesian+statistics+normalized+posterior+probabilities+stan+generated+quantities&source=bl&ots=aMJaiKuq8l&sig=2XoYjfN-rEVJGCPQDxW2FGK7nX0&hl=it&sa=X&ved=0ahUKEwilr--DgbvcAhUP1hoKHTyOD5IQ6AEIKjAA#v=onepage&q=a%20student%20guide%20to%20bayesian%20statistics%20normalized%20posterior%20probabilities%20stan%20generated%20quantities&f=false"
# for relabelling I have used some codes from the link: "https://discourse.mc-stan.org/t/identification-of-mixture-of-multivariate-normal-distributions/4203/2"

# Clear the workspace
rm(list = ls())

# Load the libraries
library("rstan")
library("dplyr") 
library("rstanarm") 
library("rstudioapi") 
library("label.switching")

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

### Model ###

lc = "
data {
int<lower=1> N; // Number of rows
int<lower=1> N2; // Number of rows
int<lower=1> P; //Number of covariates
matrix[N2, P] X; // Matrix of predictors
int<lower=1> G; //Number of classes
int S; // Number of choice options
int R; // Number of respondents
int<lower=1, upper=R> id[N]; // index for respondent
int<lower=1, upper=S> y[N]; // Alternative selected by respondent in each choice set 
int start[N]; // first observation for each choice set
int end[N]; // last observation for each choice set
}


parameters {
vector[P] beta[G]; // vector of coefficients for each class
simplex[G] theta; // class probability
}

transformed parameters{
matrix[S,G] conditional_prob[N];  // probability of observing the sequence of choices of agent n in class g
vector[G] prob_class[R]; // unnormalized conditional probability

for (n in 1:N)
for (g in 1:G)
conditional_prob[n,,g]=softmax(X[start[n]:end[n]]*beta[g]);  

for (r in 1:R){
prob_class[r]= log(theta);
}

for (n in 1:N){
for (g in 1:G)
prob_class[id[n],g] =prob_class[id[n],g]+log(conditional_prob[n,y[n],g]);  
}
}

model {
for (g in 1:G)
beta[g] ~ normal(0, 5); //  coefficients vector prior distribution  

for (g in 1:G)
theta~ dirichlet(rep_vector(1.0,G)); // class probability prior distribution

for (r in 1:R)
target += log_sum_exp(prob_class[r]);  // increment log probability
}


generated quantities{
real log_lik[R];
simplex[G] class_prob[R]; // matrix of classification probabilities

for(r in 1:R)
class_prob[r] = softmax(prob_class[r]);

for(r in 1:R)
log_lik[r] = log_sum_exp(class_prob[r]);
}
"

# Create a data list containing all the data specified in the model above
data_list <- list(N=2904,
                  N2=11616,
                  R=484,
                  P=11,
                  S=4,
                  G=3,
                  X=X,
                  y=y,
                  start = indexes$start,
                  end = indexes$end)

### Estimate ###
lc_fit = stan(model_code = lc, iter = 5000, seed = 1987, warmup = 2000, chains =3,  data = data_list)

### Relabelling: STEPHENS ###
# To relabe I use the label.switching package, link: https://cran.r-project.org/web/packages/label.switching/label.switching.pdf
# The arguments used in the labels.switching package are:
# m= number of MCMC iterations from the posterior distribution (without burn-in period)
# n=r= number of respondents. 
# k=g= number of classes
# J= number of parameters


iter_tot=(lc_fit@sim$iter-lc_fit@sim$warmup)*lc_fit@sim$chains
n_par=R+P
G=3

# To extract the matrix of classification probabilies, required by method Stephens
p=rstan::extract(lc_fit,pars="class_prob")$class_prob
dim(p)
# p= m ? n ? K array 

# convert MCMC output into a m x k x J array
mcmc.params <- rstan::extract(lc_fit,pars=c("prob_class", "beta"))
mcmc.pars <- array(data=NA,dim=c(iter_tot,G,n_par))
for(p in 1:P){
mcmc.pars[,,p]<- mcmc.params$beta[,,p]
}
for(g in 1:G){
for(r in 1:R){
mcmc.pars[,g,P+r]=mcmc.params$prob_class[,r,g]
}
}

stephens = stephens(p = p)
# stephens is a matrix: m x k

# To reorder the MCMC output according the method of stephens
reordered= permute.mcmc(mcmc.pars,stephens$permutations)[[1]]
dim(reordered)
# reordered = m ? K ? J, array of reordered MCMC parameters.

# Now I can computes means, standard deviations, quantiles, 
# Monte Carlo standard errors, split Rhats, and effective sample sizes with R function monitor()
samples_mcmc = lc_fit@sim$iter-lc_fit@sim$warmup
ff = t(apply(reordered, 1 ,as.vector))
dim(ff)
gg = array(NA, dim = c(samples_mcmc,lc_fit@sim$chains,dim(ff)[2]))
dim(gg)
# gg is a 3-D array (iterations * chains * parameters) of MCMC simulations.

# Extract samples mcmc without warmup
for (chains in 1:lc_fit@sim$chains) gg[,chains,] = ff[(1:samples_mcmc)+(chains-1)*samples_mcmc,]
# [1:samples_mcmc+(1-1)*samples_mcmc,] =iterations of the first chain
# [1:samples_mcmc+(2-1)*samples_mcmc,] =iterations of the second chain
# and so on

lc_reordered = monitor(gg, print = F, permuted=FALSE, warmup = 0)
name=paste0("beta[",rep(1:G,P),",",sort(rep(1:P,G)),"]")
name2=paste0("prob_class[",rep(1:G,R),",",sort(rep(1:R,G)),"]" )
name_var=c(name,name2)
rownames(lc_reordered) =name_var
