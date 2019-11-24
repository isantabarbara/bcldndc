# Uncertainty analysis LandscapeDNDC

## LandscapeDNDC

LandscapeDNDC is a simulation framework for terrestrial ecosystem models on site and regional scales (Haas et al. 2013). LandscapeDNDC emerged from the site scale model MoBiLE (Grote et al. 2009b), which was based on the Arable-DNDC and Forest-DNDC models (Li et al. 1992; Stange et al. 2000). The modular design of LandscapeDNDC allows plugging in any choice of process descriptions for various parts of different natural ecosystems.

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

With a restrained amount of simulations, the Morris method (Morris, 1991) allows the establishment of a hierarchy of parameters influence on a given output and evaluates whether it presents non-linearity in order to identify the most sensitive parameters. Morris proposes that this order can be assessed through the statistical analysis of the changes in the model output, produced by the "one-step-at-a-time" changes in “n” number of proposed parameters. Incremental steps of each parameter range lead to identifying which ones have substantial influences over the results, without neglecting that some effects could cancel each other out (which is why it is considered a global SA method). (Saltelli, A. et al. 2000)

Main code file:
~/sensitivity/code/MainMorris.R

## Metropolis algorithm

Thomas Bayes’ formula, which is related to conditional probabilities, becomes powerful and practical when combined with Markov chain processes and Monte Carlo methods; this is the so-called Markov chain Monte Carlo (MCMC). A Markov chain is a special type of discrete stochastic process wherein the probability of an event depends only on the event that immediately precedes it. Integrating model structure (M), parameters (θ), and data (D) into Bayes’ rule results in the following formula:

(p(θ│D,M)) = (p(D│θ,M))*(p(θ|M)) / (p(D|M))

where given the parameters, the probability of the data, p(D│θ,M), is used to obtain the probability of these parameters updated by the data: p(θ│D,M). 

In this process, the evidence must be computed:

p(D│M)=integral(likelihood∙prior∙dθ)
	
It can be numerically approximated with the aforementioned MCMC methods, even for large parameter spaces (Robert & Casella, 2011). The method updates prior beliefs, and therefore, knowledge about the sources of uncertainty hopefully obtain a narrowed posterior distribution for each of the sources, which can then be propagated through the model to quantify the overall uncertainty in the model results.

I this code the MCMC Metropolis-Hasting algorithm has been used. It is a powerful MCMC method developed by (Metropolis, Rosenbluth, Rosenbluth, Teller, & Teller, 1953) and generalised by (Hastings, 1970; Kruschke, 2011).

The MCMC Metropolis–Hastings algorithm results in numerous parameter sets that approximate the posterior joint parameter distribution. To generate these parameter sets, a random walk through the space of parameter values is performed, evaluating the probability of the data obtained from each step. In this manner, the initial uniform parameter distributions are updated. 

For the evaluation, different likelihood functions that relates the data to the parameters has been tested (All are file following the prefix Likelihood*.R).

Main code file:
~/bcMulti/code/Main.R

Numerous studies make use of the Generalised likelihood uncertainty estimation (GLUE) introduced in 1992 by Beven and Binley (K. Beven & Binley, 1992, 2014). While GLUE is flexible in the selection of the likelihood function, it can generate results without statistical validity (Stedinger, Vogel, Lee, & Batchelder, 2008). Still it has been widely accepted and used for the uncertainty quantification of environmental models (He et al., 2016; Mitchell, Beven, & Freer, 2009), including LandscapeDNDC (T. Houska et al., 2017).

The GLUE method has been tested in the code file (~bcMulti/code/MainLHS.R).

## Bibliography:

- Houska, T., Kraft, P., Liebermann, R., Klatt, S., Kraus, D., Haas, E., … Breuer, L. (2017). Rejecting hydro-biogeochemical model structures by multi-criteria evaluation. Environmental Modelling and Software, 93, 1–12. https://doi.org/10.1016/j.envsoft.2017.03.005

- Stedinger, J. R., Vogel, R. M., Lee, S. U., & Batchelder, R. (2008). Appraisal of the generalized likelihood uncertainty estimation (GLUE) method. Water Resources Research. https://doi.org/10.1029/2008WR006822

- Beven, K., & Binley, A. (1992). The future of distributed models: Model calibration and uncertainty prediction. Hydrological Processes, 6(3), 279–298. https://doi.org/10.1002/hyp.3360060305

- Beven, K., & Binley, A. (2014). GLUE: 20 years on. Hydrological Processes. https://doi.org/10.1002/hyp.10082

- Hastings, W. K. (1970). Monte Carlo sampling methods using Markov chains and their applications. Biometrika, 57(1), 97–109. https://doi.org/10.1093/biomet/57.1.97

- Kruschke, J. K. (2011). Doing Bayesian Data Analysis: A Tutorial with R and BUGS. Europe’s Journal of Psychology, 7, 1–187. https://doi.org/10.5964/ejop.v7i4.163

- Metropolis, N., Rosenbluth, A. W., Rosenbluth, M. N., Teller, A. H., & Teller, E. (1953). Equation of State by Fast Computing Machines. The Journal of Chemical Physics, 21(1953), 1087–1092. https://doi.org/10.1063/1.1699114

- Robert, C., & Casella, G. (2011). A Short History of Markov Chain Monte Carlo: Subjective Recollections from Incomplete Data. Statistical Science, 26(1), 102–115. https://doi.org/10.1214/10-STS351

- Saltelli, A., Tarantola, S., Campolongo, F. (2000). Sensitivity Anaysis as an Ingredient of Modeling. Statistical Science, 15(4), 377–395. https://doi.org/10.1214/ss/1009213004

- Grote, R., Lehmann, E., Brümmer, C., Brüggemann, N., Szarzynski, J., & Kunstmann, H. (2009). Modelling and observation of biosphere-atmosphere interactions in natural savannah in Burkina Faso, West Africa. Physics and Chemistry of the Earth, 34(4–5), 251–260. https://doi.org/10.1016/j.pce.2008.05.003

- Stange, F., Butterbach-Bahl, K., Papen, H., Zechmeister-Boltenstern, S., Li, C., & Aber, J. (2000). A process-oriented model of N2O and NO emissions from forest soils 2. Sensitivity analysis and validation. Journal of Geophysical Research, 105, 4385–4398. https://doi.org/10.1029/1999JD900948

- Li, C., Frolking, S., Frolking, T. A., Changsheng, L., Frolking, S., & Frolking, T. A. (1992). A model of nitrous oxide evolution from soil driven by rainfall events. I - Model structure and sensitivity. II - Model applications. Journal of Geophysical Research, 97(D9), 9777. https://doi.org/10.1029/92JD00509

- Haas, E., Klatt, S., Fröhlich, A., Kraft, P., Werner, C., Kiese, R., … Butterbach-Bahl, K. (2013). LandscapeDNDC: A process model for simulation of biosphere-atmosphere-hydrosphere exchange processes at site and regional scale. Landscape Ecology, 28, 615–636. https://doi.org/10.1007/s10980-012-9772-x



