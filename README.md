# CIPerm

This is a (draft) R package that implements computationally-efficient construction of confidence intervals from permutation tests for simple differences in means. In other words, if we set up a permutation test to evaluate $H_0: \mu_A - \mu_B = 0$, how do we use those same permutations to cheaply construct a CI for the $(\mu_A - \mu_B)$ parameter?

The method is based on Minh Nguyen's MS thesis paper, 2009, "Nonparametric Inference using Randomization and Permutation Reference Distribution and their Monte-Carlo Approximation."

