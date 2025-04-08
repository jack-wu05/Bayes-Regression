data = read.csv("~/Desktop/447 project/oasis_longitudinal.csv")

## Change categorical covariates into numerics
data$index = as.integer(as.factor(data$Subject.ID))
data$Group_categorical = ifelse(data$Group == "Demented", 1, 0)

## Impute NAs (do not drop them b/c of limited data)
any(is.na(data$SES))
any(is.na(data$MMSE))
data$SES[is.na(data$SES)] = mean(data$SES, na.rm=TRUE)
data$MMSE[is.na(data$MMSE)] = mean(data$MMSE, na.rm=TRUE)

require(rstan)
fit = stan(
  seed = 123,
  file = "stan_code.stan", 
  data = list (
    N = 373,
    S = length(unique(data$Subject.ID)),
    
    subject = data$index,
    Age = data$Age,
    SES = data$SES,
    EDUC = data$EDUC,
    Group = data$Group_categorical,
    Time = data$Visit,
    MMSE = data$MMSE
    ),      
  iter = 100                 
)

print(fit)

png("traceplot.png")
traceplot(fit, pars = c("sigma", "delta_1", "beta_Age"))
dev.off()