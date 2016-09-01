## Building the initial model and getting the results of anova
head(project.data) ##view the data
project.data.lm = lm(coolingload~. , data=project.data) ##get linear regression model for data
summary(project.data.lm) ##view the coefficients and significant factors etc
aov(project.data.lm) ##anova table for the model
summary(aov(project.data.lm)) ##summary of anova table

##multicollinearity analysis
library('HH') ##to load the package with function of vif
vif(project.data.lm) ##to look for the multicollinearity

##residual analysis
plot(project.data.lm) ##to get the Q-Q plot and residual vs fitted values
library('car') ## package with residualPlots fn
residualPlots(project.data.lm) ## to get the residaul vs the predictors

##transformation of regressor and predictor
projectdata.tsf.lm = lm(log(coolingload) ~ I(Relative.Compactness^-2)+I(Roof.Area^-2)+Wall.Area+Overall.Height+Orientation+Glazing.Area+
                                                Glazing.Area.Distribution , data=project.data) ##model after the variables are transformed
summary(projectdata.tsf.lm) ##view the coefficients and significant factors in transformed model
aov(projectdata.tsf.lm) ##anova table for the transformed model
summary(aov(projectdata.tsf.lm)) ##summary of anova table for transformed model
vif(projectdata.tsf.lm)
plot(projectdata.tsf.lm)
residualPlots(projectdata.tsf.lm)

##influential and leverage points
d1 = cooks.distance(projectdata.tsf.lm) ##cook's D values
r = rstandard(projectdata.tsf.lm) ##standard residuals
projectdata.lv = cbind(project.data , d1 , r) ## combining data,cook's D and std res values into a matrix
projectdata.lv[d1>1 ,] ## looking at values with d1>1 which indicates influential points

##stepwise regression
library('MASS') ##package with stepwise regression function
step = stepAIC(projectdata.tsf.lm , direction='both') ##stepwise selection with option =both
step$anova ## gives initial and final models
summary(step) ##gives the estimate of coefficients,R square, adj R square etc
vif(step) ##gives the vif for the predictors in the final model
plot(step) ##gives the residual vs fitted values plot used to check the variance is constant or not

##further stepwise after removing the relative compactness term after checking the correlation
project.data1 = cbind(project.data$coolingload,project.data$Overall.Height,project.data$Wall.Area,project.data$Roof.Area,project.data$Glazing.Area)
project.data1 = data.frame(project.data1) ##taking only terms needed for further analysis from the main data and converting to dataframe
colnames(project.data1) = c('coolingload','Overall.Height','Wall.Area','Roof.Area','Glazing.Area') ##giving the column headings
projectdata.tsf.lm1 = lm(log(coolingload)~(I(Roof.Area^-2))+Wall.Area+Overall.Height+Glazing.Area , data=project.data1) #new transformed model
step1 = stepAIC(projectdata.tsf.lm1 , direction='both') #stepwise regression
summary(step1) #see the coefficients, estimates etc
vif(step1) #checking the vif for new model







