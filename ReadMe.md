# **Simulating Study Data (17/12/20)**
### Lukas Beinhauer

This page contains an R-script, designed to simulate data from multiple studies. The goal is to assess meta-analytic estimates of heterogeneity, and the impact of various kinds of variance on such estimates.

Currently, the following functions are contained:
- SimStudies2groups(), in SimStudies.R

## SimStudies2groups()

The following function is designed to simulate data from multiple studies, in order to assess the impact of various kinds of variances on measures of heterogeneity. Sample sizes are samples for each study, with small sample sizes being stronlgy favoured.

Additionally, a second script "HeterogeneityDueToVar(ES).R" is included. This deals with the assessment of heterogeneity due to variance in the simulated Effect Sizes, in combination with a changing sampling variance. Its results, only graphical plots as of now, is found in "simulatedHeterogeneityPlots.pdf".
When inspecting the tau^2 estimates, it is possible to assess in how far these resemble the supplied vector of between-effect-variability. As the tau^2 measure is made from a SMD-model, the tau^2 values correspond with the supplied vector values at 1:1, whenever the variances are 1. For an example, see the last plot of the simulated "simulatedHeterogeneityPlots.pdf".

The content of this script is subject to change, and will be developed over time.