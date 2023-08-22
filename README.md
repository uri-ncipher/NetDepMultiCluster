# Network dependence with multiple clusters: a simulation study across different autocorrelation processes
Youjin Lee, Zhejia Dong, Natallia V. Katenka, Jing Wu, and Ashley L. Buchanan

## Overview
Network dependence – statistical dependence within a variable due to network edges – is ubiquitous
across different fields of studies in which subjects for a study are connected with each other through
network edges. Ignoring network dependence can lead to invalid statistical inference. In particular,
when the exposure and the outcome variables of interest are network-dependent on the same or similar
underlying networks, estimates of their association may be spurious, concentrated far from their true
values. Even though this phenomenon has just recently come to attention, very little is known about
when spurious associations occur and which factors affect this issue.

The purpose of this work is to empirically examine the spurious associations problem under different network autocorrelation processes
that generate dependence among observations in a social network. We also investigate the impact of
multiple clusters on the issue. In this work we show empirical evidence demonstrating that spurious
associations may be more pronounced under particular autocorrelation processes and may depend on
the association measure used. We also find that the problem of spurious associations can be mitigated
as the number of independent clusters increases, but only under certain circumstances. This work can
guide future social network study designs to avoid spurious associations, particularly when the true
autocorrelation processes are unknown.

## Code for Reproducibility

* `first_gene_network.R` and `second_gene_network.R` These files generates the network graphs for (i) direct transmission process and (ii) multiple communities formation process.

* `first.direct.R` and `first.uv.R` These files generate the two transmission processes while fixing the total number of nodes to n = 500. The simulation result is presented in the main manuscript.

* `second.direct.R` and `second.uv.R` These files generate the two transmission processes while increasing the number of clusters with a fixed cluster size to 100 for each cluster. The simulation result is presented in the Appendix

* `Summary_results.R` This file calculates the correlation coefficients, distance correlations, standard beta coefficients, the p-values for the linear test and the distance correlation t-test and the rejection rates for the two test statistics. This file can be used to reproduce Table 1 in the main manuscript and Table A1 in the Supporting information.

* `plot.R` This file is used to generate the figures presented in the manuscript and in the Appendix. This script can be used to reproduce Figures 1-5 in the manuscript and Figures A1-A7 in the Appendix. To reproduce these figures, one should first run the `Summary_results.R`.
