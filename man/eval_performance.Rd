\name{eval_performance}
\alias{eval_performance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eval_performance(y_ch_predicted, y_ch_real, alpha = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y_ch_predicted}{
%%     ~~Describe \code{y_ch_predicted} here~~
}
  \item{y_ch_real}{
%%     ~~Describe \code{y_ch_real} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y_ch_predicted, y_ch_real, alpha = 1) 
{
    source("code/roses/run.R")
    y_predicted = mnl$prob
    y_real = res \%>\% dplyr::select(CID, AID, CH) \%>\% tidyr::pivot_wider(id_cols = CID, 
        values_from = CH, names_from = AID) \%>\% select(-CID) \%>\% 
        mutate_all(as.numeric) \%>\% as.data.frame()
    y_ch_predicted = max.col(y_predicted)
    y_ch_real = max.col(y_real)
    conf_m = table(y_ch_real, y_ch_predicted)
    ER = (sum(conf_m) - sum(diag(conf_m)))/sum(conf_m)
    AC = sum(diag(conf_m))/sum(conf_m)
    TP = diag(conf_m)
    FP = colSums(conf_m) - diag(conf_m)
    FN = rowSums(conf_m) - diag(conf_m)
    TN = sapply(seq(nrow(conf_m)), function(i) {
        sum(conf_m[-i, -i])
    })
    names(TN) = colnames(conf_m)
    TPR = TP/(TP + FN)
    FPR = FP/(FP + TN)
    TNR = TN/(TN + FP)
    FNR = FN/(FN + TP)
    LRP = TPR/(1 - TNR)
    LRN = (1 - TPR)/TNR
    Precision = TP/(TP + FP)
    Recall = TP/(TP + FN)
    GM_TNR = sqrt(TPR * TNR)
    GM_Precision = sqrt(TPR * Precision)
    F = (1 + alpha) * (Precision * Recall)/(alpha * Precision + 
        Recall)
    CR = (TP + FN)/(FP + TN)
    input_p = summary(y_ch_real)/length(y_ch_real)
    out_p = summary(y_ch_predicted)/length(y_ch_predicted)
    KL = LaplacesDemon::KLD(px = out_p, py = input_p)
    lev = length(unique(y_ch_real))
    input_pm = matrix(0, nrow = lev, ncol = lev)
    diag(input_pm) = summary(y_ch_real)/length(y_ch_real)
    out_pm = conf_m/sum(conf_m)
    RE = LaplacesDemon::KLD(px = out_pm, py = input_pm)
    output = list()
    output$ERR = ERR
    output$AC = AC
    output$TP = TP
    output$FP = FP
    output$TN = TN
    output$FN = FN
    output$TPR = TPR
    output$FPR = FPR
    output$TNR = TNR
    output$FNR = FNR
    output$LRp = LRp
    output$LRn = LRn
    output$Precision = Prec
    output$Recall = Rec
    output$GeomMean1 = GM_1
    output$GeomMean2 = GM_2
    output$Fmeasure = F_a
    output$ClassRatio = r
    output$KLDvec = KL
    output$KLDmat = RE
    output$CM = conf_m
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
