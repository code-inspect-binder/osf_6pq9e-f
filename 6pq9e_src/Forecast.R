# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Model",
  modelDescr ="NA",
  indivID    ="ID",
  panelData = FALSE,
  mixing    = TRUE, 
  nCores    = 1)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("db_clean_reg.csv",header=TRUE)

#########Prepare data for HCM estimation#############################
database$prix_diff_na=ifelse(database$prix_diff>0,1,2)
database$prix_diff=ifelse(database$prix_diff==0,1,database$prix_diff)
database$price_tap_na=ifelse(database$price_tap>0,1,2)
database$price_tap=ifelse(database$price_tap==0,1,database$price_tap)
database$m_nit_sout_ar = database$nit_sout_ar - mean(database$nit_sout_ar)
database$m_pt = database$pt - mean(database$pt)
database$m_age = database$age - mean(database$age)
database$m_nchildren = database$nchildren - mean(database$nchildren)
database$m_inc_cont = database$inc_cont - mean(database$inc_cont)
database$mcost_m3 = ifelse(database$mcost_m3==-999,0,database$mcost_m3)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(
  a1_nit_sout_ar	=	0.0033	,
  a1_pt	=	-0.1331	,
  a1_age	=	-0.0172	,
  a1_female	=	0.3589	,
  a1_child = 0,
  a1_inc_cont = 0,
  a1_edu1	=	-0.0589	,
  a1_edu2	=	0.1847	,
  a1_edu3	=	0.1594	,
  a1_edu4	=	0.1063	,
  a1_drur = 0,
  
  a2_nit_sout_ar	=	0.0033	,
  a2_pt	=	-0.1331	,
  a2_age	=	-0.0172	,
  a2_female	=	0.3589	,
  a2_child = 0,
  a2_inc_cont	=	-0.058	,
  a2_edu1	=	-0.0589	,
  a2_edu2	=	0.1847	,
  a2_edu3	=	0.1594	,
  a2_edu4	=	0.1063	,
  a2_drur = 0,
  
  b1_const = 0,
  b1_nit_sout_ar	=	0	,
  b1_pt	=	0	,
  b1_age	=	0	,
  b1_female	=	0	,
  b1_child = 0,
  b1_inc_cont	=	0	,
  b1_edu1	=	0	,
  b1_edu2	=	0	,
  b1_edu3	=	0	,
  b1_edu4	=	0	,
  b1_drur = 0,
  
  b3_const = 0,
  b3_nit_sout_ar	=	0	,
  b3_pt	=	0	,
  b3_age	=	0	,
  b3_female	=	0	,
  b3_child = 0,
  b3_inc_cont	=	0	,
  b3_edu1	=	0	,
  b3_edu2	=	0	,
  b3_edu3	=	0	,
  b3_edu4	=	0	,
  b3_drur = 0,
  
  theta_bottle1	=	0.4041	,
  theta_bottle3	=	0.3348	,
  theta_qual1	=	-0.365	,
  theta_qual3	=	-0.1825	,
  zeta_bottle	=	1.2554	,
  zeta_tap	=	0.001	,
  zeta_m3	=	0.001	,
  zeta_na1	=	0.001	,
  zeta_na2	=	0.001	,
  zeta_na3	=	0.001	,
  zeta_qual	=	-0.6464	,
  zeta_qual_f	=	0.7611	,
  tau_na1	=	0.001	,
  tau_na2	=	0.001	,
  tau_na3	=	0.001	,
  tau_tap	=0.001	,
  tau_bottle1	=	-1	,
  tau_bottle2	=	0	,
  tau_bottle3	=	1	,
  tau_bottle4	=	2	,
  tau_qual1	=	0	,
  tau_qual2	=	1	,
  tau_qual3	=	2	,
  tau_qual_f1	=	0	,
  tau_qual_f2	=	1	,
  sigma_m3	=	8.8748	
  
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 25000,
  interUnifDraws = c(),
  interNormDraws = c("dterm1",
                     "dterm2"),
  
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  
  randcoeff[["LV1"]] =       (a1_nit_sout_ar * m_nit_sout_ar +
                                a1_pt * m_pt  + 
                                a1_age * m_age +
                                a1_female * female +
                                a1_child * m_nchildren +
                                a1_inc_cont * m_inc_cont + 
                                a1_edu1 * (edu==1) +
                                a1_edu2 * (edu==2) +
                                a1_edu3 * (edu==3) +
                                a1_edu4 * (edu==4) +
                                a1_drur * drur +
                                dterm1)
  
  randcoeff[["LV2"]] =       (a2_nit_sout_ar * m_nit_sout_ar +
                                a2_pt * m_pt  + 
                                a2_age * m_age +
                                a2_female * female +
                                a2_child * m_nchildren +
                                a2_inc_cont * m_inc_cont + 
                                a2_edu1 * (edu==1) +
                                a2_edu2 * (edu==2) +
                                a2_edu3 * (edu==3) +
                                a2_edu4 * (edu==4) +
                                a2_drur * drur +
                                dterm2)
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[["WATER1"]]  = (b1_const + 
                      b1_nit_sout_ar * nit_sout_ar +
                      b1_pt * pt  + 
                      b1_age * age +
                      b1_female * female +
                      b1_child * nchildren +
                      b1_inc_cont * inc_cont + 
                      b1_edu1 * (edu==1) +
                      b1_edu2 * (edu==2) +
                      b1_edu3 * (edu==3) +
                      b1_edu4 * (edu==4) + 
                      b1_drur * drur +
                      theta_qual1 * LV1 + theta_bottle1 * LV2) 
  
  V[["WATER2"]]  = 0
  
  V[["WATER3"]]  = (b3_const + 
                      b3_nit_sout_ar * nit_sout_ar +
                      b3_pt * pt  + 
                      b3_age * age +
                      b3_female * female +
                      b3_child * nchildren +
                      b3_inc_cont * inc_cont + 
                      b3_edu1 * (edu==1) +
                      b3_edu2 * (edu==2) +
                      b3_edu3 * (edu==3) +
                      b3_edu4 * (edu==4) + 
                      b3_drur * drur +
                      theta_qual3 * LV1 + theta_bottle3 * LV2) 
  
  ### Define settings for MNL model component
  mnl_settings1 = list(
    alternatives = c(WATER1=1, WATER2=2,WATER3=3),
    avail        = list(WATER1=1, WATER2=1,WATER3=1),
    choiceVar    = choice2,
    V             = V
  )
  
  
  V_OL1 = list()
  V_OL1[["alt1"]] = tau_tap + zeta_tap * LV2
  V_OL1[["alt2"]] = 0
  
  V_OL2 = list()
  V_OL2[["alt1"]] = tau_na1 + zeta_na1 * LV2
  V_OL2[["alt2"]] = 0
  
  V_OL3 = list()
  V_OL3[["alt1"]] = tau_na2 + zeta_na2 * LV2
  V_OL3[["alt2"]] = 0
  
  V_OL4 = list()
  V_OL4[["alt1"]] = tau_na3 + zeta_na3 * LV2
  V_OL4[["alt2"]] = 0
  
  mnl_settings2 = list(alternatives=c(alt1=1,alt2=2),avail=list(alt1=1,alt2=1),choiceVar=price_tap,V=V_OL1,rows=(price_tap_na==1))
  mnl_settings3 = list(alternatives=c(alt1=1,alt2=2),avail=list(alt1=1,alt2=1),choiceVar=prix_diff_na,V=V_OL2)
  mnl_settings4 = list(alternatives=c(alt1=1,alt2=2),avail=list(alt1=1,alt2=1),choiceVar=price_tap_na,V=V_OL3)
  mnl_settings5 = list(alternatives=c(alt1=1,alt2=2),avail=list(alt1=1,alt2=1),choiceVar=cost_m3_na,V=V_OL4)
  
  #ol_settings1 = list(outcomeOrdered=price_tap,V=zeta_tap * LV2,tau=c(tau_tap))
  #ol_settings2 = list(outcomeOrdered=prix_diff_na,V=zeta_na1 * LV2,tau=c(tau_na1))
  #ol_settings3 = list(outcomeOrdered=price_tap_na,V=zeta_na2 * LV2,tau=c(tau_na2))
  #ol_settings4 = list(outcomeOrdered=cost_m3_na,V=zeta_na3 * LV2,tau=c(tau_na3))
  ol_settings5 = list(outcomeOrdered=prix_diff,V=LV2* zeta_bottle,tau=c(tau_bottle1,tau_bottle2,tau_bottle3,tau_bottle4),rows=(prix_diff_na==1))
  
  ol_settingsB1 = list(outcomeOrdered=qual,V=LV1* zeta_qual,tau=c(tau_qual1,tau_qual2,tau_qual3))
  ol_settingsB3 = list(outcomeOrdered=qual_f,V=LV1* zeta_qual_f,tau=c(tau_qual_f1,tau_qual_f2))
  
  normalDensity_settings1=list(outcomeNormal=mcost_m3,xNormal=zeta_m3*LV2,mu=0,sigma=sigma_m3,rows=(cost_m3_na==1))
  
  ### Compute probabilities using MNL model
  P[['choice']]    = apollo_mnl(mnl_settings1, functionality)
  P[["indic_cost_tap"]]  = apollo_mnl(mnl_settings2, functionality)
  P[["indic_na1"]]  = apollo_mnl(mnl_settings3, functionality)
  P[["indic_na2"]]  = apollo_mnl(mnl_settings4, functionality)
  P[["indic_na3"]]  = apollo_mnl(mnl_settings5, functionality)
  P[["indic_cost_bottle"]] = apollo_ol(ol_settings5, functionality)
  P[["indic_bill"]]= apollo_normalDensity(normalDensity_settings1,functionality)
  
  P[["indic_qual"]] =     apollo_ol(ol_settingsB1, functionality)
  P[["indic_qual_f"]] =   apollo_ol(ol_settingsB3, functionality)
  
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_loadModel("Model")

database_original = database
apollo_inputs=apollo_validateInputs()

##################################################
#                     EDU                        #
##################################################

#Predict choices when everybody is edu1
database$edu=1
apollo_inputs=apollo_validateInputs()
predictions_edu1_all = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is edu2
database$edu=2
apollo_inputs=apollo_validateInputs()
predictions_edu2_all = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is edu3
database$edu=3
apollo_inputs=apollo_validateInputs()
predictions_edu3_all = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is edu4
database$edu=4
apollo_inputs=apollo_validateInputs()
predictions_edu4_all = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is base edu (5)
database$edu=5
apollo_inputs=apollo_validateInputs()
predictions_edu_none = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

edu1_1 <- matrix(rep(0*200),200,1)
edu1_2 <- matrix(rep(0*200),200,1)
edu1_3 <- matrix(rep(0*200),200,1)

edu2_1 <- matrix(rep(0*200),200,1)
edu2_2 <- matrix(rep(0*200),200,1)
edu2_3 <- matrix(rep(0*200),200,1)

edu3_1 <- matrix(rep(0*200),200,1)
edu3_2 <- matrix(rep(0*200),200,1)
edu3_3 <- matrix(rep(0*200),200,1)

edu4_1 <- matrix(rep(0*200),200,1)
edu4_2 <- matrix(rep(0*200),200,1)
edu4_3 <- matrix(rep(0*200),200,1)

edu5_1 <- matrix(rep(0*200),200,1)
edu5_2 <- matrix(rep(0*200),200,1)
edu5_3 <- matrix(rep(0*200),200,1)

Diff_edu1_1 <- matrix(rep(0*200),200,1)
Diff_edu1_2 <- matrix(rep(0*200),200,1)
Diff_edu1_3 <- matrix(rep(0*200),200,1)

Diff_edu2_1 <- matrix(rep(0*200),200,1)
Diff_edu2_2 <- matrix(rep(0*200),200,1)
Diff_edu2_3 <- matrix(rep(0*200),200,1)

Diff_edu3_1 <- matrix(rep(0*200),200,1)
Diff_edu3_2 <- matrix(rep(0*200),200,1)
Diff_edu3_3 <- matrix(rep(0*200),200,1)

Diff_edu4_1 <- matrix(rep(0*200),200,1)
Diff_edu4_2 <- matrix(rep(0*200),200,1)
Diff_edu4_3 <- matrix(rep(0*200),200,1)

for (i in 1:200) {

  edu1_1[i,1] <- (sum(predictions_edu1_all$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  edu1_2[i,1] <- (sum(predictions_edu1_all$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  edu1_3[i,1] <- (sum(predictions_edu1_all$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  edu2_1[i,1] <- (sum(predictions_edu2_all$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  edu2_2[i,1] <- (sum(predictions_edu2_all$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  edu2_3[i,1] <- (sum(predictions_edu2_all$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  edu3_1[i,1] <- (sum(predictions_edu3_all$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  edu3_2[i,1] <- (sum(predictions_edu3_all$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  edu3_3[i,1] <- (sum(predictions_edu3_all$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  edu4_1[i,1] <- (sum(predictions_edu4_all$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  edu4_2[i,1] <- (sum(predictions_edu4_all$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  edu4_3[i,1] <- (sum(predictions_edu4_all$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  edu5_1[i,1] <- (sum(predictions_edu_none$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  edu5_2[i,1] <- (sum(predictions_edu_none$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  edu5_3[i,1] <- (sum(predictions_edu_none$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_edu1_1[i,1] <- (sum(predictions_edu1_all$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_edu1_2[i,1] <- (sum(predictions_edu1_all$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_edu1_3[i,1] <- (sum(predictions_edu1_all$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_edu2_1[i,1] <- (sum(predictions_edu2_all$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_edu2_2[i,1] <- (sum(predictions_edu2_all$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_edu2_3[i,1] <- (sum(predictions_edu2_all$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_edu3_1[i,1] <- (sum(predictions_edu3_all$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_edu3_2[i,1] <- (sum(predictions_edu3_all$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_edu3_3[i,1] <- (sum(predictions_edu3_all$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_edu4_1[i,1] <- (sum(predictions_edu4_all$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_edu4_2[i,1] <- (sum(predictions_edu4_all$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_edu4_3[i,1] <- (sum(predictions_edu4_all$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_edu_none$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  }

EDU <-cbind(edu1_1,edu1_2,edu1_3,
            edu2_1,edu2_2,edu2_3,
            edu3_1,edu3_2,edu3_3,
            edu4_1,edu4_2,edu4_3,
            edu5_1,edu5_2,edu5_3)

write.csv(EDU,"edu_detailed.csv")

##################################################
#                     NITRATES                   #
##################################################

#Predict choices when everybody is max nitrates
database = database_original
apollo_inputs=apollo_validateInputs()
database$nit_sout_ar=max(database$nit_sout_ar)
database$m_nit_sout_ar = max(database_original$nit_sout_ar) - mean(database_original$nit_sout_ar)

apollo_inputs=apollo_validateInputs()
predictions_nitmax = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is min nitrates
database = database_original
database$nit_sout_ar=min(database$nit_sout_ar)
database$m_nit_sout_ar = min(database_original$nit_sout_ar) - mean(database_original$nit_sout_ar)

apollo_inputs=apollo_validateInputs()
predictions_nitmin = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is q75 nitrates
database = database_original
database$nit_sout_ar=23.74
database$m_nit_sout_ar = 23.74 - mean(database_original$nit_sout_ar)

apollo_inputs=apollo_validateInputs()
predictions_nitq75 = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is med nitrates
database = database_original
database$nit_sout_ar=15.07
database$m_nit_sout_ar = 15.07 - mean(database_original$nit_sout_ar)

apollo_inputs=apollo_validateInputs()
predictions_nitmed = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

#Predict choices when everybody is q25 nitrates
database = database_original
database$nit_sout_ar=6.91
database$m_nit_sout_ar = 6.91 - mean(database_original$nit_sout_ar)

apollo_inputs=apollo_validateInputs()
predictions_nitq25 = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=200))

nitmax_1 <- matrix(rep(0*200),200,1)
nitmax_2 <- matrix(rep(0*200),200,1)
nitmax_3 <- matrix(rep(0*200),200,1)

nitmin_1 <- matrix(rep(0*200),200,1)
nitmin_2 <- matrix(rep(0*200),200,1)
nitmin_3 <- matrix(rep(0*200),200,1)

nitq75_1 <- matrix(rep(0*200),200,1)
nitq75_2 <- matrix(rep(0*200),200,1)
nitq75_3 <- matrix(rep(0*200),200,1)

nitmed_1 <- matrix(rep(0*200),200,1)
nitmed_2 <- matrix(rep(0*200),200,1)
nitmed_3 <- matrix(rep(0*200),200,1)

nitq25_1 <- matrix(rep(0*200),200,1)
nitq25_2 <- matrix(rep(0*200),200,1)
nitq25_3 <- matrix(rep(0*200),200,1)

Diff_nitmax_1 <- matrix(rep(0*200),200,1)
Diff_nitmax_2 <- matrix(rep(0*200),200,1)
Diff_nitmax_3 <- matrix(rep(0*200),200,1)

Diff_nitq75_1 <- matrix(rep(0*200),200,1)
Diff_nitq75_2 <- matrix(rep(0*200),200,1)
Diff_nitq75_3 <- matrix(rep(0*200),200,1)

Diff_nitq25_1 <- matrix(rep(0*200),200,1)
Diff_nitq25_2 <- matrix(rep(0*200),200,1)
Diff_nitq25_3 <- matrix(rep(0*200),200,1)

Diff_nitmin_1 <- matrix(rep(0*200),200,1)
Diff_nitmin_2 <- matrix(rep(0*200),200,1)
Diff_nitmin_3 <- matrix(rep(0*200),200,1)


for (i in 1:200) {
  
  nitmax_1[i,1] <- (sum(predictions_nitmax$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  nitmax_2[i,1] <- (sum(predictions_nitmax$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  nitmax_3[i,1] <- (sum(predictions_nitmax$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  nitmin_1[i,1] <- (sum(predictions_nitmin$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  nitmin_2[i,1] <- (sum(predictions_nitmin$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  nitmin_3[i,1] <- (sum(predictions_nitmin$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  nitq75_1[i,1] <- (sum(predictions_nitq75$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  nitq75_2[i,1] <- (sum(predictions_nitq75$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  nitq75_3[i,1] <- (sum(predictions_nitq75$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  nitmed_1[i,1] <- (sum(predictions_nitmed$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  nitmed_2[i,1] <- (sum(predictions_nitmed$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  nitmed_3[i,1] <- (sum(predictions_nitmed$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  nitq25_1[i,1] <- (sum(predictions_nitq25$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  nitq25_2[i,1] <- (sum(predictions_nitq25$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  nitq25_3[i,1] <- (sum(predictions_nitq25$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_nitmax_1[i,1] <- (sum(predictions_nitmax$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_nitmax_2[i,1] <- (sum(predictions_nitmax$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_nitmax_3[i,1] <- (sum(predictions_nitmax$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_nitq75_1[i,1] <- (sum(predictions_nitq75$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_nitq75_2[i,1] <- (sum(predictions_nitq75$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_nitq75_3[i,1] <- (sum(predictions_nitq75$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_nitq25_1[i,1] <- (sum(predictions_nitq25$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_nitq25_2[i,1] <- (sum(predictions_nitq25$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_nitq25_3[i,1] <- (sum(predictions_nitq25$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
  
  Diff_nitmin_1[i,1] <- (sum(predictions_nitmin$choice$draws[1:3506,1,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,1,i],na.rm=TRUE))/3506
  Diff_nitmin_2[i,1] <- (sum(predictions_nitmin$choice$draws[1:3506,2,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,2,i],na.rm=TRUE))/3506
  Diff_nitmin_3[i,1] <- (sum(predictions_nitmin$choice$draws[1:3506,3,i],na.rm=TRUE)-sum(predictions_nitmed$choice$draws[1:3506,3,i],na.rm=TRUE))/3506
}


NIT <-cbind(nitmax_1,nitmax_2,nitmax_3,
            nitq75_1,nitq75_2,nitq75_3,
            nitmed_1,nitmed_2,nitmed_3,
            nitq25_1,nitq25_2,nitq25_3,
            nitmin_1,nitmin_2,nitmin_3)

write.csv(NIT,"nit_detailed.csv")



EDU1 = t(cbind(c(mean(edu1_1),sd(edu1_1)),
               c(mean(edu1_2),sd(edu1_2)),
               c(mean(edu1_3),sd(edu1_3)),
               c(mean(Diff_edu1_1),sd(Diff_edu1_1)),
               c(mean(Diff_edu1_2),sd(Diff_edu1_2)),
               c(mean(Diff_edu1_3),sd(Diff_edu1_3))))

EDU2 = t(cbind(c(mean(edu2_1),sd(edu2_1)),
               c(mean(edu2_2),sd(edu2_2)),
               c(mean(edu2_3),sd(edu2_3)),
               c(mean(Diff_edu2_1),sd(Diff_edu2_1)),
               c(mean(Diff_edu2_2),sd(Diff_edu2_2)),
               c(mean(Diff_edu2_3),sd(Diff_edu2_3))))

EDU3 = t(cbind(c(mean(edu3_1),sd(edu3_1)),
               c(mean(edu3_2),sd(edu3_2)),
               c(mean(edu3_3),sd(edu3_3)),
               c(mean(Diff_edu3_1),sd(Diff_edu3_1)),
               c(mean(Diff_edu3_2),sd(Diff_edu3_2)),
               c(mean(Diff_edu3_3),sd(Diff_edu3_3))))

EDU4 = t(cbind(c(mean(edu4_1),sd(edu4_1)),
               c(mean(edu4_2),sd(edu4_2)),
               c(mean(edu4_3),sd(edu4_3)),
               c(mean(Diff_edu4_1),sd(Diff_edu4_1)),
               c(mean(Diff_edu4_2),sd(Diff_edu4_2)),
               c(mean(Diff_edu4_3),sd(Diff_edu4_3))))

EDU5 = t(cbind(c(mean(edu5_1),sd(edu5_1)),
               c(mean(edu5_2),sd(edu5_2)),
               c(mean(edu5_3),sd(edu5_3))))


NITRATES = t(cbind(c(mean(nitmax_1),sd(nitmax_1)),
                   c(mean(nitmax_2),sd(nitmax_2)),
                   c(mean(nitmax_3),sd(nitmax_3)),
                   
                   c(mean(nitq75_1),sd(nitq75_1)),
                   c(mean(nitq75_2),sd(nitq75_2)),
                   c(mean(nitq75_3),sd(nitq75_3)),
                   
                   c(mean(nitmed_1),sd(nitmed_1)),
                   c(mean(nitmed_2),sd(nitmed_2)),
                   c(mean(nitmed_3),sd(nitmed_3)),
                   
                   c(mean(nitq25_1),sd(nitq25_1)),
                   c(mean(nitq25_2),sd(nitq25_2)),
                   c(mean(nitq25_3),sd(nitq25_3)),
                   
                   c(mean(nitmin_1),sd(nitmin_1)),
                   c(mean(nitmin_2),sd(nitmin_2)),
                   c(mean(nitmin_3),sd(nitmin_3)),
  
                   c(mean(Diff_nitmax_1),sd(Diff_nitmax_1)),
                   c(mean(Diff_nitmax_2),sd(Diff_nitmax_2)),
                   c(mean(Diff_nitmax_3),sd(Diff_nitmax_3)),
                   
                   c(mean(Diff_nitq75_1),sd(Diff_nitq75_1)),
                   c(mean(Diff_nitq75_2),sd(Diff_nitq75_2)),
                   c(mean(Diff_nitq75_3),sd(Diff_nitq75_3)),
                   
                   c(mean(Diff_nitq25_1),sd(Diff_nitq25_1)),
                   c(mean(Diff_nitq25_2),sd(Diff_nitq25_2)),
                   c(mean(Diff_nitq25_3),sd(Diff_nitq25_3)),
                   
                   c(mean(Diff_nitmin_1),sd(Diff_nitmin_1)),
                   c(mean(Diff_nitmin_2),sd(Diff_nitmin_2)),
                   c(mean(Diff_nitmin_3),sd(Diff_nitmin_3))))