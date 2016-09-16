library(readr)
library(dplyr)
library(foreign)
library(ggplot2)


# AM = 6 -9am
# MD = 9am - 3pm
# PM = 3 - 7pm
# NT = 7pm - 6am

# assume count column numbers refer to ending hour (e.g. hour 1 = midnight to 1am, hour 6 = 5-6am)

setwd("w:/Ohio3C/9_Integration/validation/MORPC")

linksOR <-read.dbf("160706_fixtrk/MOR10J24ASNBASEwCNTs_160706_fixtrk.dbf")

links <- linksOR %>% 
  mutate(CNT_AM = HR7 + HR8 + HR9,
         CNT_MD = HR10 + HR11 + HR12 + HR13 + HR14 + HR15,
         CNT_PM = HR16 + HR17 + HR18 + HR19,
         CNT_NT = HR20 + HR21 + HR22 + HR23 + HR24 + HR1 + HR2 + HR3 + HR4 + HR5 + HR6,
         CNT_DY = CNT_AM + CNT_PM + CNT_MD + CNT_NT,
         TRC_AM = TR7 + TR8 + TR9,
         TRC_MD = TR10 + TR11 + TR12 + TR13 + TR14 + TR15,
         TRC_PM = TR16 + TR17 + TR18 + TR19,
         TRC_NT = TR20 + TR21 + TR22 + TR23 + TR24 + TR1 + TR2 + TR3 + TR4 + TR5 + TR6,
         TRC_DY = TRC_AM + TRC_MD + TRC_PM + TRC_NT
  )

links[,"ADT"] <- ifelse(is.na(links$ADT),0,links$ADT)         

hascounts <- links%>%
  filter(ADT > 0 )

hastrkcounts <- links%>%
  filter(TRKTOT > 0 )

hasamcounts <- links %>%
  filter(HR7>0 & HR8>0 & HR9>0)

hasmdcounts <- links %>%
  filter(HR10>0 & HR11>0 & HR12>0 & HR13>0 & HR14>0 & HR15>0)

haspmcounts <- links %>%
  filter(HR16>0 & HR17>0 & HR18>0 & HR19)

hasntcounts <- links %>%
  filter(HR20>0 & HR21>0 & HR22>0 & HR23>0 & HR24>0 & HR1>0 & HR2>0 & HR3>0 & HR4>0 & HR5>0 & HR6>0)

hasamtrucks <- links %>%
  filter(TR7>0 & TR8>0 & TR9>0)

hasmdtrucks <- links %>%
  filter(TR10>0 & TR11>0 & TR12>0 & TR13>0 & TR14>0 & TR15>0)

haspmtrucks <- links %>%
  filter(TR16>0 & TR17>0 & TR18>0 & TR19)

hasnttrucks <- links %>%
  filter(TR20>0 & TR21>0 & TR22>0 & TR23>0 & TR24>0 & TR1>0 & TR2>0 & TR3>0 & TR4>0 & TR5>0 & TR6>0)


# Define Functions
#-----------------------------------------------------------------------------------  
eqn = function(lmout,var){
  eq <- substitute(italic(y) == a ~ italic(x)*", "~~italic(r)^2~"="~r2, 
                   list(a = format(summary(lmout)$coefficients[var,"Estimate"], digits = 2),  
                        r2 = format(summary(lmout)$r.squared, digits = 3)))
  as.character(as.expression(eq))                 
}


theme_scatter <- function(){
  theme(
    strip.text = element_text(size=15),
    plot.title = element_text(size=17, face="bold"),
    axis.text = element_text(size=15),
    axis.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.key.width = unit(3,"line")
  )
} 


rmse <- function(df, actual, predicted){
  sqrt( sum( (df[,predicted]- df[,actual])^2, na.rm=TRUE) /nrow(df) )
}

pcrmse <- function(df, actual, predicted){
  sqrt( sum( (df[,predicted]- df[,actual])^2, na.rm=TRUE) /nrow(df) ) * nrow(df) / sum(df[,actual]) *100
}

rmseformat <- function(value, numdig){
  eq <- substitute(italic(RMSE) == ~a, 
                   list(a = format(value, digits = numdig, scientific = FALSE)))
  as.character(as.expression(eq))                 
}  

pcrmseformat <- function(value, numdig){
  eq <- substitute(italic(pcRMSE) == ~a, 
                   list(a = format(value, digits = numdig, scientific = FALSE)))
  as.character(as.expression(eq))                 
} 


#  plotactualpredicted <- function(data, actual, predicted, titlein){
#    eval(paste("totreg <- lm(",actual," ~ 0 + ",predicted,", data = ",df,")"))
#  }
#    # totreg <- lm(actual ~ 0 + predicted, data = df)
#    # maxvol = max(df[,predicted], df[,actual])
#    # RMSE_ <- rmse(df, as.character(predicted), as.character(actual))
#    # PC_RMSE <- pcrmse(df, as.character(predicted), as.character(actual))
#    # ## CB note to self: as.character is being used where quotes used to be
#    # ggplot(data=df, aes(x=predicted, y=actual)) +
#    #   geom_point()+
#    #   geom_abline(intercept = 0, slope=1)+
#    #   geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
#    #   coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
#    #   geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,as.character(actual)), parse=TRUE, size=6)+
#    #   geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
#    #   geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
#    #   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
#    #   theme_scatter()+
#    #   labs(title = as.character(title.text))
# # }   

# Plots
#-----------------------------------------------------------------------------------  
sel <- hascounts

totreg <- lm(VOL24_TOT ~ 0 + ADT, data = sel)
maxvol = max(sel$ADT, sel$VOL24_TOT)
RMSE_ <- rmse(sel, "ADT", "VOL24_TOT")
PC_RMSE <- pcrmse(sel, "ADT", "VOL24_TOT")

ggplot(data=sel, aes(x=ADT, y=VOL24_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"ADT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "Total Daily Flows vs. Counts, all Roadways")

# FACTYPES:
# Operational class or modified functional class
# 10 - Freeway
# 20 - Expressway
# 30 - Ramp - note speed override of 35 mph
# 31 - Freeway to Freeway Ramp (optional) - uses postspd instead of 35 mph
# 32 - Exit Ramp (optional)
# 33 - Entrance Ramp (optional)
# 34 - Turnpike Toll Plaza (optional)
# 40 - Major Road (Arterial)
# 50 - Minor Road (Collector)
# 60 - Local
# 70 - Centroid Connector
# 71 - External Connector (optional)

sel <-hascounts[hascounts$FACTYPE==10 | hascounts$FACTYPE==20,]

totreg <- lm(VOL24_TOT ~ 0 + ADT, data = sel)
maxvol = max(sel$ADT, sel$VOL24_TOT)
RMSE_ <- rmse(sel, "ADT", "VOL24_TOT")
PC_RMSE <- pcrmse(sel, "ADT", "VOL24_TOT")

ggplot(data=sel, aes(x=ADT, y=VOL24_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"ADT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "Total Daily Flows vs. Counts, Freeway or Expressway")

sel <-hascounts[hascounts$FACTYPE==40,]

totreg <- lm(VOL24_TOT ~ 0 + ADT, data = sel)
maxvol = max(sel$ADT, sel$VOL24_TOT)
RMSE_ <- rmse(sel, "ADT", "VOL24_TOT")
PC_RMSE <- pcrmse(sel, "ADT", "VOL24_TOT")

ggplot(data=sel, aes(x=ADT, y=VOL24_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"ADT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "Total Daily Flows vs. Counts, Arterials")


sel <-hascounts[hascounts$FACTYPE==30 |hascounts$FACTYPE==31 | hascounts$FACTYPE==32 | hascounts$FACTYPE==33 | hascounts$FACTYPE==34,]

totreg <- lm(VOL24_TOT ~ 0 + ADT, data = sel)
maxvol = max(sel$ADT, sel$VOL24_TOT)
RMSE_ <- rmse(sel, "ADT", "VOL24_TOT")
PC_RMSE <- pcrmse(sel, "ADT", "VOL24_TOT")

ggplot(data=sel, aes(x=ADT, y=VOL24_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"ADT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "Total Daily Flows vs. Counts, Ramps")


# TOD periods

sel <- hasamcounts
totreg <- lm(VOLAM_TOT ~ 0 + CNT_AM, data = sel)
maxvol = max(sel$CNT_AM, sel$VOLAM_TOT)
RMSE_ <- rmse(sel, "CNT_AM", "VOLAM_TOT")
PC_RMSE <- pcrmse(sel, "CNT_AM", "VOLAM_TOT")

ggplot(data=sel, aes(x=CNT_AM, y=VOLAM_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_AM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "AM Flows vs. Counts, all Roadways")  


sel <- hasamcounts[hasamcounts$FACTYPE==10 | hasamcounts$FACTYPE==20,]
totreg <- lm(VOLAM_TOT ~ 0 + CNT_AM, data = sel)
maxvol = max(sel$CNT_AM, sel$VOLAM_TOT)
RMSE_ <- rmse(sel, "CNT_AM", "VOLAM_TOT")
PC_RMSE <- pcrmse(sel, "CNT_AM", "VOLAM_TOT")

ggplot(data=sel, aes(x=CNT_AM, y=VOLAM_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_AM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "AM Flows vs. Counts, Freeway/Expressway") 

sel <- hasamcounts[hasamcounts$FACTYPE==40,]
totreg <- lm(VOLAM_TOT ~ 0 + CNT_AM, data = sel)
maxvol = max(sel$CNT_AM, sel$VOLAM_TOT)
RMSE_ <- rmse(sel, "CNT_AM", "VOLAM_TOT")
PC_RMSE <- pcrmse(sel, "CNT_AM", "VOLAM_TOT")

ggplot(data=sel, aes(x=CNT_AM, y=VOLAM_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_AM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "AM Flows vs. Counts, Arterials") 

sel <- hasmdcounts
totreg <- lm(VOLMD_TOT ~ 0 + CNT_MD, data = sel)
maxvol = max(sel$CNT_MD, sel$VOLMD_TOT)
RMSE_ <- rmse(sel, "CNT_MD", "VOLMD_TOT")
PC_RMSE <- pcrmse(sel, "CNT_MD", "VOLMD_TOT")

ggplot(data=sel, aes(x=CNT_MD, y=VOLMD_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_MD"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "MD Flows vs. Counts, all Roadways")   

sel <- hasmdcounts[hasmdcounts$FACTYPE==10 | hasmdcounts$FACTYPE==20,]
totreg <- lm(VOLMD_TOT ~ 0 + CNT_MD, data = sel)
maxvol = max(sel$CNT_MD, sel$VOLMD_TOT)
RMSE_ <- rmse(sel, "CNT_MD", "VOLMD_TOT")
PC_RMSE <- pcrmse(sel, "CNT_MD", "VOLMD_TOT")

ggplot(data=sel, aes(x=CNT_MD, y=VOLMD_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_MD"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "MD Flows vs. Counts, Freeways/Expressways") 

sel <- hasmdcounts[hasmdcounts$FACTYPE==40,]
totreg <- lm(VOLMD_TOT ~ 0 + CNT_MD, data = sel)
maxvol = max(sel$CNT_MD, sel$VOLMD_TOT)
RMSE_ <- rmse(sel, "CNT_MD", "VOLMD_TOT")
PC_RMSE <- pcrmse(sel, "CNT_MD", "VOLMD_TOT")

ggplot(data=sel, aes(x=CNT_MD, y=VOLMD_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_MD"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "MD Flows vs. Counts, Arterials") 

sel <- haspmcounts
totreg <- lm(VOLPM_TOT ~ 0 + CNT_PM, data = sel)
maxvol = max(sel$CNT_PM, sel$VOLPM_TOT)
RMSE_ <- rmse(sel, "CNT_PM", "VOLPM_TOT")
PC_RMSE <- pcrmse(sel, "CNT_PM", "VOLPM_TOT")

ggplot(data=sel, aes(x=CNT_PM, y=VOLPM_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_PM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+ 
  labs(title = "PM Flows vs. Counts, all Roadways")  


sel <- haspmcounts[haspmcounts$FACTYPE==10 | haspmcounts$FACTYPE==20,]
totreg <- lm(VOLPM_TOT ~ 0 + CNT_PM, data = sel)
maxvol = max(sel$CNT_PM, sel$VOLPM_TOT)
RMSE_ <- rmse(sel, "CNT_PM", "VOLPM_TOT")
PC_RMSE <- pcrmse(sel, "CNT_PM", "VOLPM_TOT")

ggplot(data=sel, aes(x=CNT_PM, y=VOLPM_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_PM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "PM Flows vs. Counts, Freeway/Expressway") 

sel <- haspmcounts[haspmcounts$FACTYPE==40,]
totreg <- lm(VOLPM_TOT ~ 0 + CNT_PM, data = sel)
maxvol = max(sel$CNT_PM, sel$VOLPM_TOT)
RMSE_ <- rmse(sel, "CNT_PM", "VOLPM_TOT")
PC_RMSE <- pcrmse(sel, "CNT_PM", "VOLPM_TOT")

ggplot(data=sel, aes(x=CNT_PM, y=VOLPM_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_PM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "PM Flows vs. Counts, Arterials")     

sel <- hasntcounts
totreg <- lm(VOLNT_TOT ~ 0 + CNT_NT, data = sel)
maxvol = max(sel$CNT_NT, sel$VOLNT_TOT)
RMSE_ <- rmse(sel, "CNT_NT", "VOLNT_TOT")
PC_RMSE <- pcrmse(sel, "CNT_NT", "VOLNT_TOT")

ggplot(data=sel, aes(x=CNT_NT, y=VOLNT_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_NT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "NT Flows vs. Counts, all Roadways")   

sel <- hasntcounts[hasntcounts$FACTYPE==10 | hasntcounts$FACTYPE==20,]
totreg <- lm(VOLNT_TOT ~ 0 + CNT_NT, data = sel)
maxvol = max(sel$CNT_NT, sel$VOLNT_TOT)
RMSE_ <- rmse(sel, "CNT_NT", "VOLNT_TOT")
PC_RMSE <- pcrmse(sel, "CNT_NT", "VOLNT_TOT")

ggplot(data=sel, aes(x=CNT_NT, y=VOLNT_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_NT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "NT Flows vs. Counts, Freeway/Expressway")    

sel <- hasntcounts[hasntcounts$FACTYPE==40,]
totreg <- lm(VOLNT_TOT ~ 0 + CNT_NT, data = sel)
maxvol = max(sel$CNT_NT, sel$VOLNT_TOT)
RMSE_ <- rmse(sel, "CNT_NT", "VOLNT_TOT")
PC_RMSE <- pcrmse(sel, "CNT_NT", "VOLNT_TOT")

ggplot(data=sel, aes(x=CNT_NT, y=VOLNT_TOT)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"CNT_NT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "NT Flows vs. Counts, Arterials") 

# Trucks
sel <- hastrkcounts
totreg <- lm(VOL24_TRK ~ 0 + TRKTOT, data = sel)
maxvol = max(sel$TRKTOT, sel$VOL24_TRK)
RMSE_ <- rmse(sel, "TRKTOT", "VOL24_TRK")
PC_RMSE <- pcrmse(sel, "TRKTOT", "VOL24_TRK")

ggplot(data=sel, aes(x=TRKTOT, y=VOL24_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRKTOT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "Daily Truck Flows vs. Counts, all Roadways")  

sel <- hasamtrucks
totreg <- lm(VOLAM_TRK ~ 0 + TRC_AM, data = sel)
maxvol = max(sel$TRC_AM, sel$VOLAM_TRK)
RMSE_ <- rmse(sel, "TRC_AM", "VOLAM_TRK")
PC_RMSE <- pcrmse(sel, "TRC_AM", "VOLAM_TRK")

ggplot(data=sel, aes(x=TRC_AM, y=VOLAM_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_AM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "AM Truck Flows vs. Counts, all Roadways")  

sel <- hasamtrucks[hasamtrucks$FACTYPE==10 | hasamtrucks$FACTYPE==20,]
totreg <- lm(VOLAM_TRK ~ 0 + TRC_AM, data = sel)
maxvol = max(sel$TRC_AM, sel$VOLAM_TRK)
RMSE_ <- rmse(sel, "TRC_AM", "VOLAM_TRK")
PC_RMSE <- pcrmse(sel, "TRC_AM", "VOLAM_TRK")

ggplot(data=sel, aes(x=TRC_AM, y=VOLAM_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_AM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "AM Truck Flows vs. Counts, Freeways/Expressways")   

sel <- hasamtrucks[hasamtrucks$FACTYPE==40,]
totreg <- lm(VOLAM_TRK ~ 0 + TRC_AM, data = sel)
maxvol = max(sel$TRC_AM, sel$VOLAM_TRK)
RMSE_ <- rmse(sel, "TRC_AM", "VOLAM_TRK")
PC_RMSE <- pcrmse(sel, "TRC_AM", "VOLAM_TRK")

ggplot(data=sel, aes(x=TRC_AM, y=VOLAM_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_AM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "AM Truck Flows vs. Counts, Arterials") 

sel <- hasmdtrucks
totreg <- lm(VOLMD_TRK ~ 0 + TRC_MD, data = sel)
maxvol = max(sel$TRC_MD, sel$VOLMD_TRK)
RMSE_ <- rmse(sel, "TRC_MD", "VOLMD_TRK")
PC_RMSE <- pcrmse(sel, "TRC_MD", "VOLMD_TRK")

ggplot(data=sel, aes(x=TRC_MD, y=VOLMD_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_MD"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "MD Truck Flows vs. Counts, all Roadways")  

sel <- hasmdtrucks[hasmdtrucks$FACTYPE==10 | hasmdtrucks$FACTYPE==20,]
totreg <- lm(VOLMD_TRK ~ 0 + TRC_MD, data = sel)
maxvol = max(sel$TRC_MD, sel$VOLMD_TRK)
RMSE_ <- rmse(sel, "TRC_MD", "VOLMD_TRK")
PC_RMSE <- pcrmse(sel, "TRC_MD", "VOLMD_TRK")

ggplot(data=sel, aes(x=TRC_MD, y=VOLMD_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_MD"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "MD Truck Flows vs. Counts, Freeway/Expressway") 

sel <- hasmdtrucks[hasmdtrucks$FACTYPE==40,]
totreg <- lm(VOLMD_TRK ~ 0 + TRC_MD, data = sel)
maxvol = max(sel$TRC_MD, sel$VOLMD_TRK)
RMSE_ <- rmse(sel, "TRC_MD", "VOLMD_TRK")
PC_RMSE <- pcrmse(sel, "TRC_MD", "VOLMD_TRK")

ggplot(data=sel, aes(x=TRC_MD, y=VOLMD_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_MD"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "MD Truck Flows vs. Counts, Arterials") 

sel <- haspmtrucks
totreg <- lm(VOLPM_TRK ~ 0 + TRC_PM, data = sel)
maxvol = max(sel$TRC_PM, sel$VOLPM_TRK)
RMSE_ <- rmse(sel, "TRC_PM", "VOLPM_TRK")
PC_RMSE <- pcrmse(sel, "TRC_PM", "VOLPM_TRK")

ggplot(data=sel, aes(x=TRC_PM, y=VOLPM_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_PM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "PM Truck Flows vs. Counts, all Roadways")  

sel <- haspmtrucks[haspmtrucks$FACTYPE==10 | haspmtrucks$FACTYPE==20,]
totreg <- lm(VOLPM_TRK ~ 0 + TRC_PM, data = sel)
maxvol = max(sel$TRC_PM, sel$VOLPM_TRK)
RMSE_ <- rmse(sel, "TRC_PM", "VOLPM_TRK")
PC_RMSE <- pcrmse(sel, "TRC_PM", "VOLPM_TRK")

ggplot(data=sel, aes(x=TRC_PM, y=VOLPM_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_PM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "PM Truck Flows vs. Counts, Freeways/Expressways")  

sel <- haspmtrucks[haspmtrucks$FACTYPE==40,]
totreg <- lm(VOLPM_TRK ~ 0 + TRC_PM, data = sel)
maxvol = max(sel$TRC_PM, sel$VOLPM_TRK)
RMSE_ <- rmse(sel, "TRC_PM", "VOLPM_TRK")
PC_RMSE <- pcrmse(sel, "TRC_PM", "VOLPM_TRK")

ggplot(data=sel, aes(x=TRC_PM, y=VOLPM_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_PM"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "PM Truck Flows vs. Counts, Arterials")  

sel <- hasnttrucks
totreg <- lm(VOLNT_TRK ~ 0 + TRC_NT, data = sel)
maxvol = max(sel$TRC_NT, sel$VOLNT_TRK)
RMSE_ <- rmse(sel, "TRC_NT", "VOLNT_TRK")
PC_RMSE <- pcrmse(sel, "TRC_NT", "VOLNT_TRK")

ggplot(data=sel, aes(x=TRC_NT, y=VOLNT_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_NT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "NT Truck Flows vs. Counts, all Roadways") 

sel <- hasnttrucks[hasnttrucks$FACTYPE==10 | hasnttrucks$FACTYPE==20,]
totreg <- lm(VOLNT_TRK ~ 0 + TRC_NT, data = sel)
maxvol = max(sel$TRC_NT, sel$VOLNT_TRK)
RMSE_ <- rmse(sel, "TRC_NT", "VOLNT_TRK")
PC_RMSE <- pcrmse(sel, "TRC_NT", "VOLNT_TRK")

ggplot(data=sel, aes(x=TRC_NT, y=VOLNT_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_NT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "NT Truck Flows vs. Counts, Freeways/Expressways") 

sel <- hasnttrucks[hasnttrucks$FACTYPE==40,]
totreg <- lm(VOLNT_TRK ~ 0 + TRC_NT, data = sel)
maxvol = max(sel$TRC_NT, sel$VOLNT_TRK)
RMSE_ <- rmse(sel, "TRC_NT", "VOLNT_TRK")
PC_RMSE <- pcrmse(sel, "TRC_NT", "VOLNT_TRK")

ggplot(data=sel, aes(x=TRC_NT, y=VOLNT_TRK)) +
  geom_point()+
  geom_abline(intercept = 0, slope=1)+
  geom_abline(intercept = 0, slope = summary(totreg)$coefficients[1], color="blue")+
  coord_fixed(ratio=1, xlim=c(0,maxvol), ylim=c(0,maxvol))+
  geom_text(x=maxvol*.2, y=maxvol*.9, label= eqn(totreg,"TRC_NT"), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.8, label= rmseformat(RMSE_, 5), parse=TRUE, size=6)+
  geom_text(x=maxvol*.2, y=maxvol*.7, label= pcrmseformat(PC_RMSE, 3), parse=TRUE, size=6)+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_scatter()+
  labs(title = "NT Truck Flows vs. Counts, Arterials")   
