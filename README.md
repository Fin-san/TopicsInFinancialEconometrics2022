# TopicsInFinancialEconometrics2022

Code for Topics in Financial Econometrics Spring 2022, Jens Pedersen og Frederik Findsen


## The Paper
Pairs trading is a statistical arbitrage strategy, pioneered by Nunzo Tartaglia and a group of physicists, mathematicians and computer scientists at Morgan Stanley in the mid-1980s. The strategy is built on monitoring pairs of shares whose prices are assumed to be driven by the same economic forces, thus, has some some kind of co-movement, and trade on the temporary deviations from a supposed long-run equilibrium. The risk-free nature of the strategy arises from the opening of opposing positions for each trade, i.e., by going long in the relatively undervalued stock and short in the relatively overvalued stock, and thus, a profit is made by closing the positions when the spread mean-reverts.


This paper attempts to model a pairs trading strategy with two models; Vasicek Model and Co-Integration. The Vasicek Model was initially introduced as a short-term stochastic interest rate model with mean-reverting properties. We reconfigure and reestimate the Vasicek Model to model spread between two stocks, this enables us to trade on large devitations from the long-term mean. 

In addition, to fitting the two models we introduce a Markov Regime Switching Model into both process. The Markov model is able to adjust for structural breaks in time series, and is therefore a useful tool to address possbile regime/paradgime changes in the spread between our stocks. Both our Vasicek and Co-Integration model gets the Markov Model incorporated with two states; "High" and "Low". 


Our data consists of 1000, delisted and currently listed, stocks from the CRSP Database between 2005 and 2015.


We estiamte on a rolling basis, first we estimate and form pairs over a period of 5 years subsequently we trade over a period of 30 days. This process continues for 14 months.  

