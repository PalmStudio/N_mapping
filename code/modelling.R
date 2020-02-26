# Modelling N content distribution and effect on photosynthesis
# Author: Reni 
# Date: 12/21/2019

# Imports
library(tidyverse)
library(data.table)
library(xlsx)

# Reading the xlsx file direcly:
# df= read.xlsx(file = "data/Nassimilationmapping.ppt.xlsx", sheetName = "N_Ass_mapping")

df=
  fread(input = "data/Nassimilationmapping.csv", data.table = FALSE)%>%
  rename(N_content= `N(%)`)


# Modelling N content according to leaf rank ------------------------------

ggplot(data= df, aes(x= N_content, y= Rank, color= Progeny))+
  geom_point()

# Using all points:
lm_Ncont_rank= lm(formula = N_content~Rank, data = df)
coef_Ncont_rank= coef(lm_Ncont_rank)
summary(lm_Ncont_rank)

df$N_content_sim= coef_Ncont_rank[1] + coef_Ncont_rank[2] * df$Rank

plot(df$N_content,df$Rank)
lines(df$N_content_sim, df$Rank, col= "red")

ggplot(data= df, aes(y= Rank))+
  geom_point(aes(x= N_content, color= "Measured"))+
  geom_line(aes(x= N_content_sim, color= "Simulated"), lwd= 2)

# Using only point B:
df_posB= df%>%filter(Position=="b")

lm_Ncont_rank_posB= lm(formula = N_content~Rank, data = df_posB)
coef_Ncont_rank_posB= coef(lm_Ncont_rank_posB)
summary(lm_Ncont_rank_posB)
summary(lm_Ncont_rank_posB)$r.squared

df_posB$N_content_sim= coef_Ncont_rank_posB[1] + coef_Ncont_rank_posB[2] * df_posB$Rank

plot(df_posB$N_content,df_posB$Rank)
lines(df_posB$N_content_sim, df_posB$Rank, col= "red")

ggplot(data= df_posB, aes(y= Rank))+
  geom_point(aes(x= N_content, color= "Measured"))+
  geom_line(aes(x= N_content_sim, color= "Simulated"), lwd= 2)


# One model for each position:

lms= 
  df%>%
  group_by(Position)%>%
  do(lm_bypos= lm(formula = N_content~Rank, data = .))%>%
  mutate(intercept= coef(lm_bypos)[1],
         coef_rank= coef(lm_bypos)[2],
         rsquared= summary(lm_bypos)$r.squared)

df_2= full_join(df,lms)
df_2$N_content_sim= df_2$intercept + df_2$coef_rank * df_2$Rank

ggplot(data= df_2, aes(x= Rank))+
  geom_point(aes(y= N_content, color= "Measured"))+
  geom_line(aes(y= N_content_sim, color= "Simulated"), lwd= 2)+
  facet_grid(vars(Position))




# Modelling the effect of Nitrogen content on Photosyntesis ---------------

# Normalizing photosynthesis to be between 0 and 1:
df$Photo_norm= 
  (df$Photo-min(df$Photo, na.rm = TRUE)) / 
  (max(df$Photo, na.rm = TRUE) - min(df$Photo, na.rm = TRUE))

# Using all points:
lm_A_Ncont= lm(formula = Photo_norm~N_content, data = df)
coef_A_Ncont= coef(lm_A_Ncont)
summary(lm_A_Ncont)

df$Photo_norm_sim= coef_A_Ncont[1] + coef_A_Ncont[2] * df$N_content

plot(df$N_content,df$Photo_norm)
lines(df$N_content,df$Photo_norm_sim, col= "red")

ggplot(data= df, aes(x= N_content))+
  geom_point(aes(y= Photo_norm, color= "Measured"))+
  geom_line(aes(y= Photo_norm_sim, color= "Simulated"), lwd= 2)


# One model for each position:

lms_Photo_norm= 
  df%>%
  group_by(Position)%>%
  do(lm_bypos= lm(formula = Photo_norm~N_content, data = .))%>%
  mutate(intercept= coef(lm_bypos)[1],
         coef_N= coef(lm_bypos)[2],
         rsquared= summary(lm_bypos)$r.squared)

df_3= full_join(df,lms_Photo_norm)
df_3$Photo_norm_sim= df_3$intercept + df_3$coef_N * df_3$N_content

ggplot(data= df_3, aes(x= N_content))+
  geom_point(aes(y= Photo_norm, color= "Measured"))+
  geom_line(aes(y= Photo_norm_sim, color= "Simulated"), lwd= 2)+
  facet_grid(vars(Position))



