## Pre-process data
data = read.csv("~/Desktop/447 project/oasis_longitudinal.csv")

data$index = as.integer(as.factor(data$Subject.ID))
data$Group_categorical = ifelse(data$Group == "Demented", 1, 0)

any(is.na(data$SES))
any(is.na(data$MMSE))
data$SES[is.na(data$SES)] = mean(data$SES, na.rm=TRUE)
data$MMSE[is.na(data$MMSE)] = mean(data$MMSE, na.rm=TRUE)

## Build model
require(rstan)
fit = stan(
  seed = 123,
  file = "stan_code.stan", 
  data = list (
    N = nrow(data),
    S = length(unique(data$Subject.ID)),
    
    subject = data$index, 
    Age = data$Age,
    SES = data$SES,
    EDUC = data$EDUC,
    Group = data$Group_categorical,
    Time = data$Visit,
    MMSE = data$MMSE
    ),      
  iter = 1000
)

print(fit)

## Save trace plots
pdf("traceplot.pdf")
traceplot(fit, pars = c("delta_2", "sigma_2", "beta_EDUC", "delta_1"))
dev.off()


## Plot regressions for first 30 subjects
library(ggplot2)
library(dplyr)

## AI-assisted code to generate wrap plot
posterior = rstan::extract(fit)

subject_ids = sort(unique(data$index))
first_30_ids = head(subject_ids, 25)

pred_all = data.frame()

for (s in first_30_ids) {
  subject = data[data$index == s, ]
  t_grid = subject$Visit
  
  pred = sapply(t_grid, function(t)
    posterior$alpha_i[, s] +
    posterior$gamma_i[, s] * t +
    posterior$beta_Age * subject$Age[1] +
    posterior$beta_SES * subject$SES[1] +
    posterior$beta_EDUC * subject$EDUC[1] +
    posterior$beta_Group * subject$Group_categorical[1]
  )
  
  pred_df = data.frame(
    index = s,
    Time = t_grid,
    Mean = apply(pred, 2, mean),
    Lower = apply(pred, 2, quantile, 0.025),
    Upper = apply(pred, 2, quantile, 0.975)
  )
  
  pred_all = rbind(pred_all, pred_df)
}
obs_30 = data[data$index %in% first_30_ids, ]

pdf("regressions.pdf")
ggplot(pred_all, aes(x = Time, y = Mean)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", size = 0.8) +
  geom_point(data = obs_30, aes(x = Visit, y = MMSE), size = 1.5) +
  facet_wrap(~ index, ncol = 5) +
  labs(title = "Predicted MMSE for First 30 Subjects",
       y = "MMSE", x = "Visit") +
  theme_minimal(base_size = 10)
dev.off() 



## Check for model mis-spec using simulated data
simulated_data = data.frame(
  subject = integer(),
  time = numeric(),
  Age = numeric(),
  SES = numeric(),
  EDUC = numeric(),
  Group = integer(),
  MMSE = numeric()
)

numSubjects = length(unique(data$index))

delta_1 = rnorm(1,0,1);
sigma_1 = rexp(1,0.5);

delta_2 = rnorm(1,-1,2);
sigma_2 = rexp(1,0.5);

sigma = rexp(1,0.1);

beta_Age = rnorm(1,-1,3);
beta_SES = rnorm(1,1,3);
beta_EDUC = rnorm(1,1,3);
beta_Group = rnorm(1,-1,3);

alpha_i = rnorm(numSubjects, delta_1,sigma_1)
gamma_i = rnorm(numSubjects, delta_2,sigma_2)
Age = rnorm(numSubjects, 70, 5)
SES = rnorm(numSubjects, 0, 3)
EDUC = rnorm(numSubjects, 15, 3)
Group = rbinom(numSubjects, 1, 0.5)

for (i in 1:length(data$index)) {
  visit = data$Visit[i]
  mu = alpha_i[data$index[i]] + gamma_i[data$index[i]]*visit +
       beta_Group*Group[data$index[i]] + beta_EDUC*EDUC[data$index[i]] + 
       beta_SES*SES[data$index[i]] + beta_Age*Age[data$index[i]]
  mmse = rnorm(1, mu, sigma)
  
  simulated_data[nrow(simulated_data) + 1, ] = 
    list(subject=data$index[i], time=visit, Age=Age[data$index[i]],
         SES=SES[data$index[i]], EDUC=EDUC[data$index[i]], Group=Group[data$index[i]], MMSE=mmse)
}

fit = stan(
  seed = 123,
  file = "stan_code.stan", 
  data = list (
    N = nrow(simulated_data),
    S = length(unique(simulated_data$subject)),
    
    subject = simulated_data$subject, 
    Age = simulated_data$Age,
    SES = simulated_data$SES,
    EDUC = simulated_data$EDUC,
    Group = simulated_data$Group,
    Time = simulated_data$time,
    MMSE = simulated_data$MMSE
  ),      
  iter = 1000
)











