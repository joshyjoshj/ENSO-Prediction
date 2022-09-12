library(tidyverse)
library(lubridate)
library(caret)
library(forecast)
library(zoo)
library(kableExtra)
library(gridExtra)
library(grid)
library(modelr)
library(vars)
library(purrr)
library(ggrepel)
library(reshape2)
library(formatR)
library(equatiomatic)


####Data Import####
#Nino12
nino12_table <- read.table("~/nino12.data.txt", quote="\"")
colnames(nino12_table) <- (c("Year",seq(1,12,1)))
for (i in 1:nrow(nino12_table)) {
  row <- slice(nino12_table,i)
  year <- row$Year
  val <- row[c(2:13)]
  val <- as.numeric(val)
  df <- data.frame("Year"=rep(year,12),"Month"=seq(1,12,1),"y"=val)
  ifelse({i==1},{data <- df},{data <- rbind(data,df)})
}
nino_12_model_data <- filter(data, y != -99.99)#Removal of missing values 
nino_12_model_data <- nino_12_model_data%>%mutate(type="Nino12",y_ma3=rollmean(y,3,na.pad = TRUE,align = "right"))

#Nino3
nino3_table <- read.table("~/nino3.data.txt", quote="\"")
colnames(nino3_table) <- (c("Year",seq(1,12,1)))
for (i in 1:nrow(nino3_table)) {
  row <- slice(nino3_table,i)
  year <- row$Year
  val <- row[c(2:13)]
  val <- as.numeric(val)
  df <- data.frame("Year"=rep(year,12),"Month"=seq(1,12,1),"y"=val)
  ifelse({i==1},{data <- df},{data <- rbind(data,df)})
}
nino_3_model_data <- filter(data, y != -99.99)#Removal of missing values 
nino_3_model_data <- nino_3_model_data%>%mutate(type="Nino3",y_ma3=rollmean(y,3,na.pad = TRUE,align = "right"))

#Nino4
nino4_table <- read.table("~/nino4.data.txt", quote="\"")
colnames(nino4_table) <- (c("Year",seq(1,12,1)))
for (i in 1:nrow(nino4_table)) {
  row <- slice(nino4_table,i)
  year <- row$Year
  val <- row[c(2:13)]
  val <- as.numeric(val)
  df <- data.frame("Year"=rep(year,12),"Month"=seq(1,12,1),"y"=val)
  ifelse({i==1},{data <- df},{data <- rbind(data,df)})
}
nino_4_model_data <- filter(data, y != -99.99)#Removal of missing values 
nino_4_model_data <- nino_4_model_data%>%mutate(type="Nino4",y_ma3=rollmean(y,3,na.pad = TRUE,align = "right"))

#Nino34
nino34_table <- read.table("~/nino34.data.txt", quote="\"")
colnames(nino34_table) <- (c("Year",seq(1,12,1)))
for (i in 1:nrow(nino34_table)) {
  row <- slice(nino34_table,i)
  year <- row$Year
  val <- row[c(2:13)]
  val <- as.numeric(val)
  df <- data.frame("Year"=rep(year,12),"Month"=seq(1,12,1),"y"=val)
  ifelse({i==1},{data <- df},{data <- rbind(data,df)})
}
nino_34_model_data <- filter(data, y != -99.99)#Removal of missing values 
nino_34_model_data <- nino_34_model_data%>%mutate(type="Nino34",y_ma3=rollmean(y,3,na.pad = TRUE,align = "right"))

#SOI
soi_table <- read.table("~/soi.data.txt", quote="\"")
colnames(soi_table) <- (c("Year",seq(1,12,1)))
for (i in 1:nrow(soi_table)) {
  row <- slice(soi_table,i)
  year <- row$Year
  val <- row[c(2:13)]
  val <- as.numeric(val)
  df <- data.frame("Year"=rep(year,12),"Month"=seq(1,12,1),"y"=val)
  ifelse({i==1},{data <- df},{data <- rbind(data,df)})
}
soi_model_data <- filter(data, y != -99.99)#Removal of missing values 
soi_model_data <- soi_model_data%>%mutate(type="SOI",y_ma3=rollmean(y,3,na.pad = TRUE,align = "right"))


data_list <- list(nino_12_model_data,nino_3_model_data,nino_34_model_data,nino_4_model_data,soi_model_data)
name_list <- c("12","3","34","4","soi")

models <- numeric(length(data_list))



####Model Fitting####

#####Model Fitting and evaluation ####

expr <- map(c(x1="lag(y_ma3)",x2="lag(y_ma3,2)",x3="lag(y_ma3,3)",x4="lag(y_ma3,4)",x5="lag(y_ma3,5)",x6="lag(y_ma3,6)",
              x7="lag(y_ma3,7)",x8="lag(y_ma3,8)",x9="lag(y_ma3,9)",x10="lag(y_ma3,10)",x11="lag(y_ma3,11)",x12="lag(y_ma3,12)",
              x13="lag(y_ma3,13)",x14="lag(y_ma3,14)",x15="lag(y_ma3,15)",x16="lag(y_ma3,16)",x17="lag(y_ma3,17)",x18="lag(y_ma3,18)",
              x19="lag(y_ma3,19)",x20="lag(y_ma3,20)",x21="lag(y_ma3,21)",x22="lag(y_ma3,22)",x23="lag(y_ma3,23)",x24="lag(y_ma3,24)",
              x25="lag(y_ma3,25)",x26="lag(y_ma3,26)",x27="lag(y_ma3,27)",x28="lag(y_ma3,28)",x29="lag(y_ma3,29)",x30="lag(y_ma3,30)",
              x31="lag(y_ma3,31)",x32="lag(y_ma3,32)"), rlang::parse_expr )


i <- 5
mod_data <- data_list[[i]]
name <- name_list[[i]]

#Applying Data transformation, write *formula* in here
mod_data <- mod_data%>%mutate(!!!expr)
#Completing Data Set by removal of NA's
mod_data <- mod_data[complete.cases(mod_data),]


#Reference forecasts
climatology <- var(mod_data$y_ma3)
persistence <- sd(mod_data$y_ma3[1]-mod_data$y_ma3[-1])


#Model fitting
train <- trainControl(method = "cv", number = 10)
#Add *variables* created into formula
model_fit <- train(y_ma3 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32 ,data = mod_data, method = "lm", trControl = train)
#Storing Model 
assign(paste0("model",sep="_",name),model_fit)

soi_ar_mod <- model_fit

#Skill Scores
rmse <- model_fit$results["RMSE"]



#Creation of data frame 
df <- data.frame("Data"=name,"Model Formula"=as.character(model_fit$finalModel)[1],
                 "RMSE"=model_fit$results["RMSE"],
                 "Rsqaured"=model_fit$results["Rsquared"],
                 "MAE"=model_fit$results["MAE"],"RMSESD"=model_fit$results["RMSESD"],
                 "RsquaredSD"=model_fit$results["RsquaredSD"],"MAESD"=model_fit$results["MAESD"],"Climatology"=climatology,"Persistence"=persistence)


mod_results <- df%>%dplyr::select(RMSE,RMSESD,Climatology,Persistence)
mod_results <- mod_results%>%mutate(RMSE=signif(RMSE,6),RMSESD=signif(RMSESD,6),Climatology=signif(Climatology,6),Persistence=signif(Persistence,6))



# Prediction 
#Extract residual standard error from model fit 
sigma2 <- sqrt(deviance(model_fit$finalModel)/df.residual(model_fit$finalModel))

#Applying predictions 
#Prediction Length
pred_length <- 12
for (j in 1:pred_length) {
  #Setting Data Frame for each loop
  ifelse({j==1},
         {mod_pred_data <- mod_data%>%mutate(type="Actual")},
         {mod_pred_data <- mod_pred_data})
  #Adding Predictions
  #Extending data, including months add data mutation applied earlier
  data_extension <- add_row(mod_pred_data)%>%
    mutate(!!!expr,Month=lag(Month)%%12 +1)
  #Switch out "var" for the y variable your predicting
  start <- add_predictions(slice(data_extension,nrow(data_extension)),model_fit$finalMode,var="y_ma3")
  start <- start%>%mutate(type="pred")
  mod_pred_data <- rbind(mod_pred_data,start)
}


mod_pred_data <- mod_pred_data%>%mutate(data=name)


#Propagation of error term for confidence intervals
#Number of samples
sample_length <- 10000
#Matrix Creation
output <- matrix(ncol = pred_length,nrow=sample_length)
#Adding Random Numbers from residual distribution
for (k in 1:pred_length) {
  output[,k] <- rnorm(sample_length,0,sigma2)
}
output <- data.frame(output)
#Confidence Interval
#Defining the quantiles
upper_q <- 0.95
lower_q <- 0.05
preds <- (filter(mod_pred_data,type=="pred"))$y_ma3
#Summing over each iteration 
for (l in 1:pred_length) {
  pred <- preds[l]
  output_sample <- output[c(1:l)]
  vec <- rowSums(output_sample)
  upper <- unname(quantile(vec,c(upper_q)))+pred
  lower <- unname(quantile(vec,c(lower_q)))+pred
  n <- l+36
  type <- "pred"
  df_r <- data.frame(n,upper,lower,pred)
  ifelse({l==1},{conf_df <- df_r},{conf_df <- rbind(conf_df,df_r)})
}



#Extraction of probabilistic forecast 

conf_matrix <- matrix(ncol=pred_length,nrow=sample_length)

for (m in 1:pred_length) {
  pred_2 <- preds[m]
  output_sample_1 <- output[c(1:m)]
  samples_sum <- rowSums(output_sample_1)
  pred_samples <- samples_sum+pred_2
  conf_matrix[,m] <- pred_samples
  
}
conf_matrix <- data.frame(conf_matrix)



conf_df <- conf_df%>%mutate(data=name)




pred_plot_colours <- c("Actual"="red","pred"="blue")
prediction_plot <- ggplot()+
  geom_point(tail(mod_pred_data,pred_length+36),mapping=aes(x=seq(1,pred_length+36,1),y=y_ma3,colour=type,group=1))+
  geom_line(tail(mod_pred_data,pred_length+36),mapping=aes(x=seq(1,pred_length+36,1),y=y_ma3,colour=type,group=1))+
  geom_ribbon(conf_df,mapping = aes(x=n,ymin=lower,ymax=upper),alpha=0.2)+
  geom_hline(yintercept=0,colour="black",lty=2)+
  labs(x="Period",title=name,y="3 Month MA of Anomolies")+
  scale_x_continuous(breaks = seq(1, 49, 12))+
  scale_colour_manual(values=pred_plot_colours,name="")+
  theme_bw()





prob_calc_data <- conf_matrix
for (i in 1:pred_length) {
  v <- prob_calc_data[i]
  p <- 1-sum(v<=0)/sample_length
  df <- data.frame(i,p)
  ifelse({i==1},{probs_data <-df},{probs_data <- rbind(probs_data,df)})
}
prob_plot <- ggplot(probs_data,mapping=aes(x=i,y=p,label=p))+
  geom_point()+
  geom_line()+
  geom_text_repel(box.padding   = 0.35,point.padding = 0.5,segment.color = 'grey50')+
  scale_x_continuous(breaks=seq(1,pred_length,1))+
  theme_bw()+
  labs(x="Months predicted ahead",y="Probability of La Nina")




#Skill Score 

sub_data_length <- 70
data_length <- nrow(mod_data)
#iterations <- data_length-sub_data_length-2
iterations <- 200
for (i in 0:iterations) {
  #Setting start and end points
  start <- i+1
  end <- sub_data_length+i
  #Slicing main data set
  main_data <- slice(mod_data,c(start:end))
  #Creating predicting and reference data 
  predicting <- slice(main_data,c(1:(sub_data_length/2)))
  reference <- slice(main_data,c((sub_data_length/2+1):(sub_data_length/2+12)))
  #Actual Data values
  actual <-reference$y_ma3
  #Predicting 
  #Prediction Length
  pred_length_2 <- 12
  for (j in 1:(pred_length_2)) {
    #Setting Data Frame for each loop
    ifelse({j==1},
           {mod_pred_data_2 <- predicting%>%mutate(type="Actual")},
           {mod_pred_data_2 <- mod_pred_data_2})
    #Adding Predictions
    #Extending data, including months add data mutation applied earlier
    data_extension <- add_row(mod_pred_data_2)%>%
      mutate(!!!expr,Month=lag(Month)%%12 +1)
    #Switch out "var" for the y variable your predicting
    start <- add_predictions(slice(data_extension,nrow(data_extension)),model_fit$finalMode,var="y_ma3")
    start <- start%>%mutate(type="pred")
    mod_pred_data_2 <- rbind(mod_pred_data_2,start)
  }
  #predicted Values
  predicted <- filter(mod_pred_data_2,type=="pred")$y_ma3
  df <- data.frame(predicted,actual,"n"=seq(1,12,1))
  ifelse({i==0},{skill_df <- df},{skill_df <- rbind(skill_df,df)})
}

skill_df <- skill_df%>%mutate(error=predicted-actual)

skill_df_final <-  skill_df%>%group_by(n)%>%summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

skill_df_final <- skill_df_final%>%mutate("ss_c"=1-error_sd/climatology,"ss_p"=1-error_sd/persistence)


ss_plot_colours <- c("Climatology"="green","Persistence"="orange")

skill_score_plot <- ggplot(skill_df_final,mapping = aes(x=n))+
  geom_line(mapping = aes(y=ss_c,colour="Climatology"))+
  geom_line(mapping = aes(y=ss_p,colour="Persistence"))+
  labs(x="Steps ahead predicted",y="Skill Score")+
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_colour_manual(values = ss_plot_colours,name="Reference")+
  theme_bw()



layout_matrix <- rbind(c(1,1,1,1),c(2,2,2,2),c(2,2,2,2),c(3,3,4,4),c(3,3,4,4))

grid.arrange(tableGrob(mod_results),prediction_plot,skill_score_plot,prob_plot,layout_matrix = layout_matrix)


