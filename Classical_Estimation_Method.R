### Classical Estimation Method ###
# These codes are created by Vecchiato and Tempesta (2015) to estimate the Multinomial Logit
# and Mixed Logit models.
# In addition there is the Latent Class Multinomial Logit Model.

#---------------------------
#LOAD THE REQUIRED LIBRARIES
#---------------------------
library('mlogit') #for the Multinomial Logit and the Mixed Logit models
library('gmnl')   #for the Latent Class Multinomial Logit model

#------------------------
### Multinomial Logit ###
#------------------------
f1 <-mFormula(mychoice ~  size_s + size_b + dist3b + dist10b + dist3s + dist10s + cert + cost)
m1<- mlogit(f1, data = datasetFinal,  shape = "long",  alt.var="option", choice = "mychoice", reflevel='none')
summary(m1)

#------------------
### Mixed Logit ###
#------------------
f2 <-mFormula(mychoice ~  size + dist + cert + cost)
mx1<- mlogit(f1, data = datasetFinal,  shape = "long",  alt.var="option", choice = "mychoice", reflevel='none',
             rpar = c("agr:(intercept)"="n", "for:(intercept)"="n", "solar:(intercept)"="n",  size_s="n", size_b = "n", cert="n"),
             R = 400, halton = NA)

#-------------------------------------
### Latent Class Multinomial Logit ###
#-------------------------------------
lc <- gmnl(mychoice ~ solar + BiomFor + BiomAgr + size_s + size_b + dist3b + dist10b
           + dist3s + dist10s + cert + cost| 0 | 0 | 0 | 1,
           reflevel = "none",
           data = Biom,
           model = 'lc',
           R = 1000,
           panel = TRUE,
           Q = 3) 
