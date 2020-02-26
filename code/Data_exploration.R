# Exploring data from nitrogen mappingS
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

head(df)
View(df)

# Exploring the data:

plot(x= df$N_content, y= df$Rank)
plot(df$N_content, df$Photo)

# More complicated plots:

# Is the N content different between ranks ?
ggplot(data= df, aes(x= N_content, y= Rank, color= Progeny))+
  geom_point()
# Yes

# Is there any difference of N content between positions on the leaf ?
ggplot(data= df, aes(x= N_content, y= Rank))+
  geom_point()+
  facet_grid(vars(Position))

ggplot(data= df, aes(x= N_content, y= Position, color= Rank))+
  geom_point()
# No

# Does photosynthesis decrease with decreasing N content ?
ggplot(data= df, aes(x= N_content, y= Photo, color= as.factor(Progeny)))+
  geom_point()+
  scale_color_viridis_d()
# Yes

# Does the position of measurement on the leaf influence the N content ?
ggplot(data= df_2, aes(x= Rank))+
  geom_point(aes(y= N_content, color= "Measured"))+
  facet_grid(vars(Position))

# Does the position of measurement on the leaf has an effect on photosynthesis?
ggplot(data= df_2, aes(x= Rank))+
  geom_point(aes(y= Photo, color= "Measured"))+
  facet_grid(vars(Position))

ggplot(data= df_2, aes(x= Position))+
  geom_boxplot(aes(y= Photo, color= "Measured"), notch= TRUE)+
  facet_grid(vars(Progeny))
# Yes, especially for position 1/2 bc, but the explanation is not clear.

# Does the variability of the photosynthesis can be explained by the individual ?
ggplot(data= df, aes(x= Rank, y= Photo,color= as.factor(Palm)))+
  geom_point()+
  geom_smooth(se= FALSE)+
  facet_grid(vars(Progeny))


ggplot(data= df, aes(x= as.factor(Rank), y= Photo, color= Position))+
  geom_point()+
  # geom_smooth(se= FALSE)+
  facet_wrap(vars(Palm))

# Does the position of measurement on the leaf has an effect on SLA?
ggplot(data= df_2, aes(x= Position))+
  geom_boxplot(aes(y= SLA, color= "Measured"), notch= TRUE)
# Rather not

# Summary plot of the effect of N content on photosynthesis and chqnge of N 
# content according to rank
ggplot(data= df, aes(x= N_content, y= Photo, color= Rank))+
  geom_point()+
  scale_color_viridis_c()+
  ylab("Photosynthesis (umol m-2 s-1)")+
  xlab("N content (%)")+
  ggtitle("Assimilation rate VS N content according to the leaf rank")



# Is the SLA different between positions ?
ggplot(data= df_2, aes(x= Position))+
  geom_boxplot(aes(y= SLA, color= "Measured"), notch= TRUE)+
  facet_grid(vars(Progeny))
# Rather not

# Is the SPAD working ?
ggplot(data= df_2, aes(x= N_content, color= Progeny))+
  geom_point(aes(y= SPAD))
# Not at all, the response is flat. We proably already are at the maximum possible values 
 