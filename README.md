# misclass
 Adjust for misclassification of an exposure variable in pooled data

Adjust for misclassification of an exposure variable in pooled data,
such as in an individual participant data meta-analysis (IPDMA). Using Bayesian
misclassification models, the potential misclassification of an exposure is
accounted for, and the uncertainty is propagated to the variance and CI. 
Modeling of an single data set (without clustering) is also possible.

The misclassification model is fitted using misclass(). This package is 
intended to make using a misclassification model easier. Therefore, it is not 
entirely comprehensive, but it is designed to be flexible. The functions
make.inits(), make.model() and make.monitors() can be used separately to 
tweak the model, but are otherwise called by misclass(). 