.libPaths('~/Rlibs')

if (!require('rstan',lib.loc ='~/Rlibs')) system('cp -r ~dheryadi/myRlibs-3.6 ~/Rlibs/')
if (!require('plotrix',lib.loc ='~/Rlibs')) install.packages('plotrix',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('dplR',lib.loc ='~/Rlibs')) install.packages('dplR',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('Rcpp',lib.loc ='~/Rlibs')) install.packages('Rcpp',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('StanHeaders',lib.loc ='~/Rlibs')) install.packages('StanHeaders',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('RcppEigen',lib.loc ='~/Rlibs')) install.packages('RcppEigen',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('BH',lib.loc ='~/Rlibs')) install.packages('BH',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('spam',lib.loc ='~/Rlibs')) install.packages('spam',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('fields',lib.loc ='~/Rlibs')) install.packages('fields',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)


if (!require('gridExtra',lib.loc ='~/Rlibs')) install.packages('gridExtra',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('ggplotify',lib.loc ='~/Rlibs')) install.packages('ggplotify',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)
if (!require('abind',lib.loc ='~/Rlibs')) install.packages('abind',lib='~/Rlibs',repos='http://cran.us.r-project.org',dependencies=TRUE)

