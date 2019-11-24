# Description
# Uncertainty analysis LandscapeDNDC

## LandscapeDNDC

LandscapeDNDC is a simulation framework for terrestrial ecosystem models on site and regional scales (Haas et al. 2013). LandscapeDNDC emerged from the site scale model MoBiLE (Grote et al. 2009b), which was based on the Arable-DNDC and Forest-DNDC models (Li et al. 1992a, b, 1994, 2000; Stange et al. 2000). The modular design of LandscapeDNDC allows plugging in any choice of process descriptions for various parts of different natural ecosystems.

LandscapeDNDC can be downloaded here:
http://ldndc.imk-ifu.kit.edu/ldndc/downloads

## Sensitivity Analysis

### Sensitivity Index

The Sensitivity index algorithm (SI) (Pannell, 1997) is calculated by splitting the parameter ranges into 10 equidistant values from minimum to maximum and by comparing the simulation results with the following formula: 

SI=((Cum_max - Cum_min)) / Cum_max 

where Cum_max and Cum_min are the maximum and minimum cumulative results of 10 simulations. High SI values explain a high sensitivity of the underlying parameter with respect to the model results, whereas low values or even zero indicates low or no sensitivity.

Main code file:
~/sensitivity/code/MainSensitivity.R

### Morris method

With a restrained amount of simulations, the Morris method (Morris, 1991) allows the establishment of a hierarchy of parameters influence on a given output and evaluates whether it presents non-linearity in order to identify the most sensitive parameters. Morris proposes that this order can be assessed through the statistical analysis of the changes in the model output, produced by the "one-step-at-a-time" changes in “n” number of proposed parameters. Incremental steps of each parameter range lead to identifying which ones have substantial influences over the results, without neglecting that some effects could cancel each other out (which is why it is considered a global SA method).

For further information :
Saltelli, A., Tarantola, S., Campolongo, F., and Ratto, M. (2004). Sensitivity Analysis in Practice - A Guide to Assessing Scientific Models. Wiley.

Main code file:
~/sensitivity/code/MainMorris.R

## Metropolis algorithm

Thomas Bayes’ formula, which is related to conditional probabilities, becomes powerful and practical when combined with Markov chain processes and Monte Carlo methods; this is the so-called Markov chain Monte Carlo (MCMC). A Markov chain is a special type of discrete stochastic process wherein the probability of an event depends only on the event that immediately precedes it. Integrating model structure (M), parameters (θ), and data (D) into Bayes’ rule results in the following formula:

(p(θ│D,M)) = (p(D│θ,M))*(p(θ|M)) / (p(D|M))

where given the parameters, the probability of the data, p(D│θ,M), is used to obtain the probability of these parameters updated by the data: p(θ│D,M). 

In this process, the evidence must be computed:

p(D│M)=integral(likelihood∙prior∙dθ)
	
It can be numerically approximated with the aforementioned MCMC methods, even for large parameter spaces (Robert & Casella, 2011). The method updates prior beliefs, and therefore, knowledge about the sources of uncertainty hopefully obtain a narrowed posterior distribution for each of the sources, which can then be propagated through the model to quantify the overall uncertainty in the model results.

I this code the MCMC Metropolis-Hasting algorithm has been used. It is a powerful MCMC method developed by (Metropolis, Rosenbluth, Rosenbluth, Teller, & Teller, 1953) and generalised by (Hastings, 1970; Kruschke, 2011; Müller, 2015; Robert & Casella, 2011).

The MCMC Metropolis–Hastings algorithm results in numerous parameter sets that approximate the posterior joint parameter distribution. To generate these parameter sets, a random walk through the space of parameter values is performed, evaluating the probability of the data obtained from each step. In this manner, the initial uniform parameter distributions are updated. 

For the evaluation, different likelihood functions that relates the data to the parameters has been tested (All are file following the prefix Likelihood*.R).

Main code file:
~/bcMulti/code/Main.R

Numerous studies make use of the Generalised likelihood uncertainty estimation (GLUE) introduced in 1992 by Beven and Binley (K. Beven & Binley, 1992, 2014). While GLUE is flexible in the selection of the likelihood function, it can generate results without statistical validity (Stedinger, Vogel, Lee, & Batchelder, 2008). Still it has been widely accepted and used for the uncertainty quantification of environmental models (He et al., 2016; Mitchell, Beven, & Freer, 2009), including LandscapeDNDC (T. Houska et al., 2017).

The GLUE method has been tested in the code file (~bcMulti/code/MainLHS.R).

## Bibliography:




