#Calibration : Delta-Vega Hedging 
library(numDeriv)
library(pracma)
library(ggplot2)
library("ggpubr")


dataset_heavy = read.csv2("C:\\Users\\benja\\OneDrive\\Documents\\Calibration\\Delta-vega_hedged_portfolio_\\Dataset.csv", header = TRUE)

head = head(dataset_heavy, 10) ; head = head[, -c(5,6)]

dataset = dataset_heavy[,1:4]

date_serie = dataset[, 1]

stock_price = dataset[, 2]

obs_option_price_6 = dataset[, 3]

obs_option_price_6.5 = dataset[, 4]

# Maturity arbitrarily chosen
T = "31/12/2018"

T_vector = maturity.vector(date_serie, T)

#Data taken from the question 1
option_price_1 = Vector.PriceOption(stock_price, 6, T_vector, 0, 0.2, "Call")

d1_vect_1 = Vector.d1(stock_price, 6, 0, 0.2, T_vector)

delta_serie_1 = Vector.DeltaOption(d1_vect_1)

#Portfolio delta hedged
pf_DeltaReplication = pf.delta_hedging(delta_serie_1, option_price_1, stock_price) 

#Question 2 : Delta-Gamma Hedging
gamma_serie_1 = Vector.GammaOption(d1_vect_1, stock_price, 0.2, T_vector)

#Greeks of the second Option 
d1_vect_2 = Vector.d1(stock_price, 6.5, 0, 0.2, T_vector)

delta_serie_2 = Vector.DeltaOption(d1_vect_2)

gamma_serie_2 = Vector.GammaOption(d1_vect_2, stock_price, 0.2, T_vector)

option_price_2 = Vector.PriceOption(stock_price, 6.5, T_vector, 0, 0.2, "Call")

#list of parameters
list_prm = as.data.frame(Vector.PrmGamma(gamma_serie_1, gamma_serie_2, delta_serie_1, delta_serie_2, option_price_1, option_price_2, stock_price))

#Portfolio delta-Gamma hedged
pf_DeltaGamma_Replication = pf.deltaGamma_hedging(option_price_1, option_price_2, stock_price, list_prm)

#Question 3

#Evolution of C0 fictif in function of volatility (0-100%)
volat_vector = seq(0, 1, 0.01)

price_Call = PriceCall.Volat(stock_price[1], 6, date_serie[1], T, 0, volat_vector)

plot_priceCall = plot(volat_vector*100, price_Call, type = "s", lty = 3, lwd = 3, col = "dark red", xlab = "Volatility (%)", ylab = "EU Call Price", main = "Price vs Volat")

#Price C0 of a fictive Call
price0 = BS.OptionPricerEU(stock_price[1], 6, T_vector[1], 0, 0.38, type = "Call")

implied_volat <- ImpliedVolat.NewtonRaphson(price0, 1, stock_price[1], 6, T_vector[1], 0)

implied_volat_dicho <- ImpliedVolat.binary.search(-2, 2, price0, stock_price[1], 6, T_vector[1], 0)

#Question 4 : Historical Implied Volatility of two Options
hist_volat_option1 = Historical.ImpliedVolat.NewtonRaphson(obs_option_price_6, 1, stock_price, 6, T_vector, 0)

#The results shows that the volatility can vary over time

hist_volat_option2 = Historical.ImpliedVolat.NewtonRaphson(obs_option_price_6.5, 1, stock_price, 6.5, T_vector, 0)

#Question 5 : Dynamic Hedging for the Option K = 6
d1_vect_Opt6 = Vector.d1.VolatVary(stock_price, 6, 0, hist_volat_option1, T_vector)

delta_serie_Opt6 = Vector.DeltaOption(d1_vect_Opt6)

gamma_serie_Opt6 = Vector.GammaOption.VolatVary(d1_vect_Opt6, stock_price, hist_volat_option1, T_vector)

#Same for the other option K = 6.5
d1_vect_Opt6.5 = Vector.d1.VolatVary(stock_price, 6.5, 0, hist_volat_option2, T_vector)

delta_serie_Opt6.5 = Vector.DeltaOption(d1_vect_Opt6.5)

gamma_serie_Opt6.5 = Vector.GammaOption.VolatVary(d1_vect_Opt6.5, stock_price, hist_volat_option2, T_vector)

#list of parameters
list_prm_Opt6 = as.data.frame(Vector.PrmGamma(gamma_serie_Opt6, gamma_serie_Opt6.5, delta_serie_Opt6, delta_serie_Opt6.5, obs_option_price_6, obs_option_price_6.5, stock_price))
