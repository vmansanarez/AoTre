################################################################################
# Univariate (local) tests ----

#' General Mann-Kendall
#'
#' A general version of the Mann-Kendall test, enabling various dependence
#' assumptions.
#'
#' @param X numeric vector, data. IMPORTANT: X is assumed to be
#' regularly-spaced. It uses NA to fill the gaps rather than removing missing
#' values.
#' @param level numeric in (0,1), level of the test.
#' @param dep.option string, option for handling temporal dependence. Available:
#' \enumerate{
#'            \item 'INDE', assume independence (i.e. the standard MK test)
#'            \item 'AR1', assumes AR1 short-term dependence structure (i.e. Hamed and Rao's version of the MK test)
#'            \item 'LTP', assume long-term persistence (i.e. Hamed's version of the MK test)
#'            }
#' @param DoDetrending, logical, only used for dep.option==LTP:
#'        do detrending before estimating Hurst coefficient (default=TRUE as recommended in Hamed's paper)
#' @return A list with the following fields:
#'         \enumerate{
#'             \item H: logical, reject (true) or do not reject (false) H0
#'             \item P: p-value of the test
#'             \item STAT: test statistics
#'             \item TREND: trend estimate (using Sen's slope estimate)
#'             \item DEP: dependence estimate (=0 if dep.option='INDE', =lag-1 autocorrelation if dep.option='AR1',
#'                   =Hurst coefficient if dep.option='LTP')
#'         }
#' @details \enumerate{
#'     \item Handling of ties: Specific formula exist for INDE and AR1, but the LTP case is trickier.
#'        Hammed's paper is unclear on how to handle ties, especially at the step of Hurst coefficient estimation.
#'        There is a normal-score transformation at this step, and one needs to decide how to assign a rank to ties.
#'        What is implemented below is the option ties.method="random", i.e. the rank is randomized for ties.
#'        This is not, strictly speaking, correct because this randomization impacts the dependence structure.
#'        However synthetic runs suggest it works OK.
#'     \item Computational efficiency: Likely poor for case dep.option='LTP'
#'       There is a 4-level loop which leads to a n^4 algorithm.
#'       I attempted to vectorize this loop but it didn't improve things.
#'       => Expect significant running times for dep.option='LTP' when size(X)>50...
#'       (orders of magnitude: 1s for n=30, 10s for n=50, 2-3 minutes for n=100)
#'       On the other hand both options INDE and AR1 are very fast.
#'       }
#' @examples
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' generalMannKendall(X=nhtemp,dep.option='AR1')
#' @references \enumerate{
#'                  \item Hamed, Rao, 1998. A modified Mann-Kendall trend test for autocorrelated data.
#'                        J. Hydrol., 204(1-4): 182-196.
#'                  \item Hamed, 2008. Trend detection in hydrologic data: The Mann-Kendall trend test under the
#'                        scaling hypothesis. J. Hydrol., 349(3-4): 350-363.
#'                  }
#' @export
generalMannKendall<-function(X,level=0.1,dep.option='INDE',DoDetrending=TRUE){
  #***********************************************************************************
  # STEP 0: preliminaries
  #***********************************************************************************
  # Create output list and initialize it
  OUT=list(H=NA,P=NA,STAT=NA,TREND=NA,DEP=NA)
  # Check dep.option is valid
  if(!((dep.option=='INDE')|(dep.option=='AR1')|(dep.option=='LTP'))){warning('Unknown dep.option');return(OUT)}
  # Remove Nas from X to create NA-free vector Z
  Z=X[!is.na(X)];n=length(Z)
  # Don't even try if less than 3 non-missing values
  if(n<3){warning('less than 3 non-missing values');return(OUT)}
  # Get basic MK stat + Sen's trend estimate
  get.MK.basics=getMKStat(X)
  MK=get.MK.basics$stat;OUT$TREND=get.MK.basics$trend

  #***********************************************************************************
  # CASE 1: 'INDE' or 'AR1'
  #***********************************************************************************
  if((dep.option=='INDE')|(dep.option=='AR1')){
    # Compute basic variance
    var0=((n*(n-1)*(2*n+5)))/18
    # Compute ties correction and get ties-corrected variance
    var1=var0-getTiesCorrection(Z)
    if(is.na(var1)){warning('NA variance');return(OUT)}
    if(var1<=0){warning('negative variance');return(OUT)}
    # Compute autocorrelation correction if dep.option=='AR1'
    if(dep.option=='AR1'){
      AR1.correction=getAR1Correction(X)
      correction=AR1.correction$correction
      OUT$DEP=AR1.correction$lag1
    }
    else{correction=1;OUT$DEP=0}
    MKvar=var1*correction
    if(MKvar<=0){warning('negative variance');return(OUT)}
  }

  #***********************************************************************************
  # CASE 2: 'LTP'
  #***********************************************************************************
  if(dep.option=='LTP'){
    # Estimate Hurst Coeff
    Hu=estimateHurst(X,DoDetrending,OUT$TREND)
    OUT$DEP=Hu
    # Get autocorrelation function
    lambda=0:n
    C=0.5*(abs(lambda+1)^(2*Hu)-2*abs(lambda)^(2*Hu)+abs(lambda-1)^(2*Hu))
    # Compute variance of MK using the monstrous 4-level loop...
    var0=0
    for(j in 2:n){
      for(i in 1:(j-1)){
        for(l in 2:n){
          for(k in 1:(l-1)){
            num=C[abs(j-l)+1]-C[abs(i-l)+1]-C[abs(j-k)+1]+C[abs(i-k)+1]
            den=sqrt( (2-2*C[abs(i-j)+1]) * (2-2*C[abs(k-l)+1]) )
            var0=var0+asin(num/den)
          }
        }
      }
    }
    var1=(2/pi)*var0
    if(is.na(var1)){warning('NA variance');return(OUT)}
    if(var1<=0){warning('negative variance');return(OUT)}
    # bias correction
    a0=(1.0024*n-2.5681)/(n+18.6693)
    a1=(-2.2510*n+157.2075)/(n+9.2245)
    a2=(15.3402*n-188.6140)/(n+5.8917)
    a3=(-31.4258*n+549.8599)/(n-1.1040)
    a4=(20.7988*n-419.0402)/(n-1.9248)
    B=a0+a1*Hu+a2*Hu^2+a3*Hu^3+a4*Hu^4
    MKvar=var1*B
    if(MKvar<=0){warning('negative variance');return(OUT)}
  }

  #***********************************************************************************
  # FINAL STEP: Get test statistics, significance, pval, etc.
  #***********************************************************************************
  # Final test statistics
  if(MK>0){stat=(MK-1)/sqrt(MKvar)}
  else if(MK<0){stat=(MK+1)/sqrt(MKvar)}
  else{stat=MK/sqrt(MKvar)}
  OUT$STAT=stat
  # p-val (2-sided test)
  OUT$P=2*stats::pnorm(-1*abs(stat),mean=0,sd=1)
  # decision
  OUT$H=(OUT$P<level)
  return(OUT)
}

######################################################################
# Regional tests ----

#' FDR field significance
#'
#' Field significance using the false detection rate approach
#'
#' @param pvals numeric vector, p-values of local tests
#' @param level numeric in (0,1), level at which field significance is evaluated
#' @return pFDR, the FDR p-value, interpreted as follows: local p-values smaller than pFDR are field-significant.
#' @examples
#' set.seed(123456) # make example reproducible
#' level=0.1 # level of the test
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' # add 29 stationary series to nhtemp series
#' X=matrix(c(nhtemp,rnorm(length(nhtemp)*29)),nrow=length(nhtemp))
#' # Compute local p-values from a MK test
#' pvals=rep(NA,30)
#' for(i in 1:30){pvals[i]=generalMannKendall(X[,i],level=level)$P}
#' # Evaluate field significance
#' pFDR=fieldSignificance_FDR(pvals,level)
#' which(pvals<=level) # locally-significant sites
#' which(pvals<=pFDR) # FDR-significant sites
#' @references Benjamini, Y., and Y. Hochberg (1995), Controlling the false discovery rate:
#'             A practical and powerful approach to multiple testing,
#'             J. R. Stat. Soc., Ser. B., 57, 289–300.
#' @export
fieldSignificance_FDR <- function(pvals,level=0.1){
  n=length(pvals)
  z=sort(pvals)
  local=(z<=(level/n)*(1:n))
  if(all(local==FALSE)){
    pFDR=0
  } else {
    indx=max(which(local))
    pFDR=z[indx]
  }
  return(pFDR)
}

#' Regional consistency test
#'
#' Test for the existence of a common trend in several series
#'
#' @param X numeric matrix, n*p, n=number of time steps, p=number of sites
#' @param level numeric in (0,1), level of the test
#' @return A list with the following fields:
#'         \enumerate{
#'             \item H: logical, reject (true) or do not reject (false) H0
#'             \item P: p-value of the test
#'             \item STAT: test statistics
#'             \item TREND: estimate of the common trend
#'             \item DATA: Normal-scored data used in the test
#'         }
#' @details
#' Handling of missing data: incomplete years (i.e. with a missing value for at least one site)
#' are entirely discarded. It might hence be beneficial to remove patchy sites from X.
#' @examples
#' set.seed(123456) # make example reproducible
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' # Create 15 noisy versions of the time series
#' X=matrix(rep(nhtemp,15)+rnorm(length(nhtemp)*15),nrow=length(nhtemp))
#' # put a few missing values
#' X[10:15,1]=NA;X[20,5]=NA
#' # Apply regional test
#' res=regionalTrend(X)
#' # Plot normal-scored data and common trend
#' matplot(res$DATA$time,res$DATA[,3:NCOL(res$DATA)])
#' lines(res$DATA$time,res$TREND*res$DATA$time)
#' @references Renard, B., et al. (2008), Regional methods for trend detection: Assessing field significance
#'             and regional consistency, Water Resour. Res., doi:10.1029/2007WR006268
#' @importFrom stats pchisq
#' @export
regionalTrend <- function(X,level=0.1){
  # Create output list and initialize it
  OUT=list(H=NA,P=NA,STAT=NA,TREND=NA,DATA=NA)
  # Identify complete rows
  completeRow=apply(is.na(X),1,sum)==0
  X2=X[completeRow,]
  n=NROW(X2);p=NCOL(X2)
  if(n<3) {
    warning(paste0('Number of complete rows (',n,') is too small'))
    return(OUT)
  }
  # Normal-score transformation and compute correlation
  R=apply(X2,2,randomizedNormalScore)
  C=(t(R)%*%R)/n
  Cdet=det(C)
  if(Cdet==0) {
    warning(paste0('Correlation matrix cannot be inverted. Possible reasons: number of complete rows (',
                   n,') is too small compared with number of columns (',p,
                   ') or some columns are perfectly correlated'))
    return(OUT)
  }
  Cinv=solve(C)
  # estimate common trend
  tim=(1:NROW(X))[completeRow];tim=tim-mean(tim) # centered time
  one=matrix(1,p,1)
  num=t(one)%*%Cinv%*%t(R)%*%tim
  denom=t(tim)%*%tim%*%t(one)%*%Cinv%*%one
  beta=as.numeric(num/denom)
  # Likelihoods
  L0=rep(NA,n);L1=L0
  for(i in 1:n){
    L0[i]=-0.5*log(Cdet)-0.5*R[i,]%*%Cinv%*%R[i,]
    L1[i]=-0.5*log(Cdet)-0.5*(R[i,]-beta*tim[i])%*%Cinv%*%(R[i,]-beta*tim[i])
  }
  # Result
  OUT$STAT=-2*(sum(L0)-sum(L1))
  OUT$P=1-pchisq(OUT$STAT,df=1)
  OUT$H=(OUT$P<level)
  OUT$TREND=beta
  OUT$DATA=data.frame(indx=which(completeRow),time=tim,X=R)
  return(OUT)
}

#***************************************************************************----
# Public utilities ----

#' Mann-Kendall statistics
#'
#' Compute MK stat and Sen's trend estimate
#'
#' @param X numeric, data
#' @return A list, with components: $stat, MK statistics; $trend, Sen's estimate.
#' @examples
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' getMKStat(X=nhtemp)
#' @export
getMKStat<-function(X){
  n=length(X);count.p=0;count.m=0;k=0
  slope.list=matrix(NA,((n-1)*n)/2,1)
  for(j in 2:n){for(i in 1:(j-1)){
    k=k+1
    if( (!is.na(X[j])) & (!is.na(X[i])) ){
      slope.list[k]=(X[j]-X[i])/(j-i)
      if(X[j]>X[i]){count.p=count.p+1}
      else if (X[j]<X[i]){count.m=count.m+1}
    }
  }}
  stat=count.p-count.m
  trend=stats::median(slope.list[!is.na(slope.list)])
  return(list(stat=stat,trend=trend))
}

#' Hurst coefficient
#'
#' Estimate the Hurst coefficient of a series
#'
#' @param Z numeric, data (NA-free)
#' @param DoDetrending logical, detrend data before estimating Hurst?
#' @param trend numeric, trend value (only used if detrending required)
#' @return The estimated value of the hurst coefficient
#' @examples
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' estimateHurst(Z=nhtemp)
#' @export
estimateHurst<-function(Z,DoDetrending=TRUE,trend=getMKStat(Z)$trend){
  #~******************************************************************************
  #~* PURPOSE: Get correction for AR(1)-like dependence
  #~******************************************************************************
  #~ IN:  1. Z, data vector
  #~      2. DoDetrending, detrend data before estimating Hurst?
  #~      3. trend, trend value (only used if detrending required)
  #~ OUT: 1. Estimated value of the hurst coefficient
  #~******************************************************************************
  n=length(Z)
  # Detrend if requested
  if(DoDetrending) {Y=Z-trend*(1:n)} else {Y=Z}
  # Transform to normal-score - Note that ties.method="random", might affect autocorrelation! but Hamed's paper is unclear on how to treat ties at this step
  W=randomizedNormalScore(Y)
  Max.Lkh=stats::optimize(f=HurstLkh,interval=c(0.5,1),W,maximum=TRUE)
  H=Max.Lkh$maximum
  return(H)
}

#' Randomized Normal Score
#'
#' Randomized Normal Score transformation
#'
#' @param x numeric, data
#' @return the normal-score-transformed series
#' @examples
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' z=randomizedNormalScore(x=nhtemp)
#' par(mfrow=c(1,2))
#' plot(nhtemp,type='b');plot(z,type='b')
#' @export
randomizedNormalScore<-function(x){
  # empirical frequencies
  p=(rank(x,ties.method="random",na.last="keep"))/(1+sum(!is.na(x)))
  # Normal quantile
  z=stats::qnorm(p)
  return(z)
}

#***************************************************************************----
# Private utilities ----

#' Ties correction
#'
#' Compute correction to the variance of MK statistics to account for ties
#'
#' @param Z numeric, data (NA-free)
#' @return the correction for the variance of MK stat
#' @examples
#' \dontrun{
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' getTiesCorrection(Z=nhtemp)
#' getTiesCorrection(rnorm(100))
#' }
getTiesCorrection<-function(Z){
  n=length(Z)
  w=matrix(NA,n,1);tie=matrix(NA,n,1);v=matrix(NA,n,1)
  for(i in 1:n){w[i]=sum(Z==Z[i])} # counts how many times each value is duplicated
  for(i in 1:n){
    tie[i]=sum(w==i)/i # create a vector containing the number of ties of extent i
    v[i]=tie[i]*i*(i-1)*(2*i+5) # save contribution of i-ties to correction
  }
  return(sum(v)/18)
}

#' AR(1) correction
#'
#' Compute correction to the variance of MK statistics to account for AR(1) autocorrelation
#'
#' @param Z numeric, data (NA-free)
#' @return A list with components: $lag1 (estimated lag-1 correlation coefficient) and
#'          $correction (the correction for the variance of MK stat)
#' @examples
#' \dontrun{
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' getAR1Correction(Z=nhtemp)
#' getAR1Correction(rnorm(100))
#' }
getAR1Correction<-function(Z){
  n=length(Z)
  w=matrix(NA,n-2,1)
  # Compute lag-1 coefficient
  Z0=Z[!is.na(Z)];m=mean(Z0)
  x=Z[1:(n-1)];y=Z[2:n]
  mask=(!is.na(x))&(!is.na(y))
  lag1=sum((x[mask]-m)*(y[mask]-m))/sum((Z0-m)^2)
  #Compute correction
  for(i in 1:(n-2)){w[i]=(n-i)*(n-i-1)*(n-i-2)*((lag1)^(i))} # save contribution of lag i to correction
  correction=1+(2/(n*(n-1)*(n-2)))*sum(w)
  return(list(lag1=lag1,correction=correction))
}

#' Hurst likelihood
#'
#' Compute the likelihood function to be maximized for estimating the Hurst coefficient H
#'
#' @param H numeric,  hurst coeff. value
#' @param x, data sample
#' @return log-likelihood value
#' @examples
#' \dontrun{
#' data(nhtemp) # 	Average Yearly Temperatures in New Haven
#' HurstLkh(H=0.8,x=nhtemp)
#' HurstLkh(H=0.5,x=nhtemp)
#' HurstLkh(H=0.2,x=nhtemp)
#' }
HurstLkh<-function(H,x){
  n=length(x)
  # Compute Cn(H)
  CnH=matrix(NA,n,n)
  for (i in 1:n){
    for (j in 1:n){
      l=abs(i-j);
      CnH[i,j]=0.5*(abs(l+1)^(2*H)-2*(abs(l)^(2*H))+ abs(l-1)^(2*H));
    }
  }
  mask=!is.na(x)
  m=sum(mask)
  v0=stats::qnorm((1:m)/(m+1))
  g0=stats::var(v0)
  L=-0.5*log(det(CnH[mask,mask]))-(t(x[mask])%*%solve(CnH[mask,mask])%*%x[mask])/(2*g0)
  return(L)
}
