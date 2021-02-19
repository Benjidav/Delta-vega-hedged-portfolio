#Question 6 : Delta-Vega Hedging

#Computation of vega'option 1 & option 2
vega_Opt6 = Vector.bs_vega(stock_price, 6, T_vector, d1_vect_Opt6, q=0.0, type = "Call")

vega_Opt6.5 = Vector.bs_vega(stock_price, 6.5, T_vector, d1_vect_Opt6.5, q=0.0, type = "Call")

#Volatility increments : sigma & sigma'
volat_increment_option1 = compute_increments(hist_volat_option1)

volat_increment_option2 = compute_increments(hist_volat_option2)

#Computing the dsigma'/dsigma
derived_sigma = compute_derived_sigma(volat_increment_option1, volat_increment_option2)

crossed_vega_option2 = compute_crossed_vega(vega_Opt6.5, derived_sigma)

#list of parameters
list_prm_Opt6_vega = as.data.frame(Vector.PrmVega(vega_Opt6, vega_Opt6.5, delta_serie_Opt6, 
                                                  delta_serie_Opt6.5, obs_option_price_6, obs_option_price_6.5, crossed_vega_option2,
                                                  stock_price))

#Delta-Vega Hedging
pf_Delta_Vega = pf.deltaVega_hedging(obs_option_price_6, obs_option_price_6.5, stock_price, list_prm_Opt6_vega)






