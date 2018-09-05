### Creation dataset ###
### Simulate some fake data ###

# Clear the workspace
rm(list = ls())

# Load the libarary
library("dplyr") 

# 6 choice sets for 484 individuals, with 4 choice options each. The fourth alternative is "none of these".
R= 484
n_choice_set=6
N= R*n_choice_set
S= 4
A=n_choice_set*S
P= 11 
N2= N*S
altern=rep(1:S, time=N)
length(altern)
table(altern)

# The function "indexes" used to obtain the first and the last observation in each choice set is taken from the 
# link: http://rpubs.com/jimsavage/using_decisionmaker_variables_in_mixed_logit
indexes <- data_frame(task = rep(1:N, each = S),
                      row = 1:(N*S)) %>%
  group_by(task) %>% 
  summarise(start = first(row),
            end = last(row)) 

choice_c=c(rep(1:n_choice_set, each = S, times=R))
length(choice_c)

# index for respondent
index= c(rep(1:R, each = A)) 
table(index)


# choice made by respondent
choice=c(rep(NA,times=N2))
for(i in 1:N){
a=c(rep(0,times=S))
b=sample(1:S,1)
a[b]=1
choice[indexes$start[i]:indexes$end[i]]=a
}
table(choice)

# dummy for each alternative except for the fourth
alt1=c(rep(NA,times=N2))
for (i in 1:N2){
if (altern[i]==1){
alt1[i]=1
}else {alt1[i]=0}
}
table(alt1)


alt2=c(rep(NA,times=N2))
for (i in 1:N2){
  if (altern[i]==2){
    alt2[i]=1
  }else {alt2[i]=0}
}
table(alt2)


alt3=c(rep(NA,times=N2))
for (i in 1:N2){
  if (altern[i]==3){
    alt3[i]=1
  }else {alt3[i]=0}
}
table(alt3)

# matrix of some predictor
X <- matrix(c(alt1, alt2, alt3, rnorm(N2*8)), N2, 11)
# replace the values of fourth row (the row of "none of these") with zero 
for (i in 1:N){
X[i*S, ] = 0
}

dd <- data.frame(choice_c, index, choice,altern, X )
class(dd)
dim(dd)

colnames(dd)[c(8:15)]  <- c("X1","X2", "X3", "X4","X5", "X6", "X7","X8")
colnames(dd)
head(dd)

# create a useful y variable for the three models
y= dd[dd$choice== 1, "altern"]   
table(y)

# extract the respondent index for the alternative choice
id = dd[dd$choice == 1, "index"] 
table(id)





