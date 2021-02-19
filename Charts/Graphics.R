#Graphics
format = "%d/%m/%Y"

date = as.Date(date_serie, format)

theme_set(theme_bw() + theme(legend.position = "top"))


delta_hedge_1_graphe <- ggplot(as.data.frame(pf_DeltaReplication), aes(x = date, y = pf_DeltaReplication)) + geom_line(color = "black") + 
  labs(title = "Delta Hedging de Opt K6, sigma = 20 %") + xlab("") + ylab("Value") +
  geom_line(aes(y = option_price_1), color="steelblue")

deltaGamma.hedge_1_graphe <- ggplot(as.data.frame(pf_DeltaGamma_Replication), aes(x = date, y = pf_DeltaGamma_Replication)) + geom_line(color = "black") + 
  labs(title = "Delta-Gamma Hedging de Opt K6 avec Option K6.5, sigma = 20 %") + xlab("") + ylab("Value") +
  geom_line(aes(y = option_price_1), color="steelblue")

pf_DeltaReplication_Opt6_graphe <- ggplot(as.data.frame(pf_DeltaReplication_Opt6), aes(x = date, y = pf_DeltaReplication_Opt6)) + geom_line(color = "black") + 
  labs(title = "Delta Hedging de Option K6 (Colonne C)") + xlab("") + ylab("Value") +
  geom_line(aes(y = obs_option_price_6), color="steelblue")

pf_DeltaGamma_Replication_Opt6_graphe <- ggplot(as.data.frame(pf_DeltaGamma_Replication_Opt6), aes(x = date, y = pf_DeltaGamma_Replication_Opt6)) + geom_line(color = "black") + 
  labs(title = "Delta-Gamma Hedging de Opt K6 avec Opt K6.5 (Colonne D)") + xlab("") + ylab("Value") +
  geom_line(aes(y = obs_option_price_6), color="steelblue")

#Saving and exporting the plots
ggexport(delta_hedge_1_graphe,deltaGamma.hedge_1_graphe, filename = "C:\\Users\\benja\\Downloads\\PartieI.pdf",
         nrow = 2, ncol = 1)

ggexport(pf_DeltaReplication_Opt6_graphe, pf_DeltaGamma_Replication_Opt6_graphe, filename = "C:\\Users\\benja\\Downloads\\PartieII.pdf",
         nrow = 2, ncol = 1)
#--------------------------------------DELTA-VEGA HEDGING--------------------------------


#Delta-vega
pf_DeltaVega_Replication_Opt6_graphe <- ggplot(as.data.frame(pf_Delta_Vega), aes(x = date, y = pf_Delta_Vega)) + geom_line(color = "black") + 
  labs(title = "Delta-Vega Hedging de Opt K6 avec Opt K6.5 (Colonne D)") + xlab("") + ylab("Value") +
  geom_line(aes(y = obs_option_price_6), color="steelblue")

#TEST A SUPPRIMER APRES
pf_DeltaGamma_Replication_Opt6_graphe <- ggplot(as.data.frame(pf_DeltaGamma_Replication_Opt6), aes(x = date, y = pf_DeltaGamma_Replication_Opt6)) + geom_line(color = "black") + 
  labs(title = "Delta-Gamma Hedging de Opt K6 avec Opt K6.5 (Colonne D)") + xlab("") + ylab("Value") +
  geom_line(aes(y = obs_option_price_6), color="steelblue")

ggexport(pf_DeltaVega_Replication_Opt6_graphe,pf_DeltaGamma_Replication_Opt6_graphe, filename = "C:\\Users\\benja\\Downloads\\Delta-Vega.pdf",
         nrow = 2, ncol = 1)

