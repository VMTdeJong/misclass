# misclass
Adjust for misclassification of an exposure variable in pooled data such as an individual
participant data meta-analysis

## Introduction
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

## Citation
When using this package, please cite this page directly, or cite the paper for which this code was developed:
* de Jong VMT, Campbell H, Maxwell L, Jaenisch T, Gustafson P, Debray TPA. Adjusting for misclassification of an exposure in an individual participant data meta-analysis. arXiv:211101650 [stat]. 2021 Nov 2; Available from: https://doi.org/10.48550/arXiv.2111.01650

## Manual
A pdf version of the manual is available on https://github.com/VMTdeJong/misclass-manual/blob/main/misclass-manual.pdf

## Funding 
This project has received funding from the European Union’s Horizon 2020 research and innovation programme
under ReCoDID grant agreement No 825746.

## Disclaimer
The views expressed in this paper are the personal views of the authors and may not be understood or quoted as being
made on behalf of or reflecting the position of the regulatory agency/agencies or organizations with which the authors are
employed/affiliated.
