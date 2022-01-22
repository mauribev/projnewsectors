#' Function to create coefficients with significance stars
#'
#' @param Regresults data.table with 5 columns. (1) Name of variables, (2) Regression coefficient, (3) Standard value, (4) T-Value, (5) P_value
#' @param arg_round number of significance digits. 4 by default
#' @import data.table
#' @export

f_reg_results <- function(Regresults,arg_round=4) {

  X <- copy(Regresults)
  setnames(X,c(2,3,4,5),c("coef","std","t_value","p_value"))
  X[,coef_star:=ifelse(p_value>0.1,format(round(coef,arg_round),nsmall=2),ifelse(p_value>0.05,paste0(format(round(coef,arg_round),nsmall=2),"*"),ifelse(p_value>0.01,paste0(format(round(coef,arg_round),nsmall=2),"**"),paste0(format(round(coef,arg_round),nsmall=2),"***"))))]
  #coef.J[,coef_star:=ifelse(x==1,paste0(format(round(coef,arg_round),nsmall=2),"***"),ifelse(x==5,paste0(format(round(coef,arg_round),nsmall=2),"**"),ifelse(x==10,paste0(format(round(coef,arg_round),nsmall=2),"*"),format(round(coef,arg_round),nsmall=2))))]
  X[,std_par:=paste0("(",format(round(std,arg_round),nsmall=2),")")]
  return(X)
}

#' Function to ran multiple regressions
#'
#' Currently you can work with 3 different types of regression algorithms: lm, lm_robust and felm
#' Currently the package doesn't allow for instrumental variables
#' @param arg_forlula LIST(!!) with the set of regression formulas
#'      (It has to be a list, but can be a list with 1 element).
#'      Formula argument is of the form: y ~ x1 + x2
#' @param arg_data data.table with the variables of the regression
#' @param arg_weights variable in arg_data containing the weights used in the regression
#' @param method a function either "lm", "lm_robust" or "felm"
#' @param arg_clusters variable in arg_data used to cluster standard errors.
#' @param arg_fe character with the name of the fe variable. This variable will
#'        only be used if you specify the "felm" method. If you have more than
#'        one fixed effect you should specify it like "fe1 + fe2"
#' @param iter_vec vector containing the numbers of the iteration that you want
#'        to be displayed (default is 10)
#' @return A data.table with the regression coefficients in the different models
#' @import data.table
#' @import lfe
#' @import lmtest
#' @import estimatr
#' @import formula.tools
#' @import tictoc
#' @export

f_regressions <- function(arg_formula,arg_data,arg_weights,method,arg_clusters,arg_fe,to_iter = 1000, by_iter = 10) {
  # !!!!! Note 1: There is a conflict if arg_formula is not a list. E.g. if you create a list (LS) but in arg_formula you introduce LS[[i]], the function will try to subset it and that will give an error. If that is the case introduce it as a list, e.g. list(LS[[i]])
  # !!!!! Note 2: For lm and lm_robust the arg_formula argument needs to be a list of formulas.
  #               For felm the arg_formula arguments can be a character or a formula. Make sure that the package formula.tools is loaded

  # Note 3: Instead of copying arg_data I decided to change the names back to their original names at the end of the function.
  # Note4-TO-DO: Allow for the introduction of instrumental variables! (for felm for sure, other methods???)
  # Note5: For an unknown reason when we convert a matrix with just one row to a data.table we ran into trouble
  #        Previous command data.table(A$coefficients[1:4],keep.rownames=TRUE): when only one row, row names are not kept and we simply get a vector of dimension for and column name V1
  #        New command data.table(A$coefficients,keep.rownames=TRUE)[,1:5]: We first convert to data.table and then select the columns that we want

  iter_vec <- seq(from =0, to = to_iter, by = by_iter)
  i <- 1
  i0 <- 0 # Initialize i0

  ## IF argument ARG_CLUSTER is specified
  #* NOTE: I create a new variable because there is an issue if the FE variable is the same as the cluster
  if (!missing(arg_clusters)) {
    setnames(arg_data,c(deparse(substitute(arg_clusters))),c("temp"))
    arg_data[,arg_cl:=temp]
    setnames(arg_data,c("temp"),c(deparse(substitute(arg_clusters))))
  }

  ## IF argument ARG_WEIGHTS is specified
  if (!missing(arg_weights)) {
    setnames(arg_data,c(deparse(substitute(arg_weights))),c("arg_w"))
    if (i %in% iter_vec) {
      tic(paste0("Iteration ",i," to ",min(i+by_iter-1,length(arg_formula))))
      i0 <- i + by_iter -1
    }
    if (method=="lm") {
      A <- summary(lm(arg_formula[[1]],data=arg_data,weights=arg_w,model=FALSE))
      other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=    c(length(A$residuals),format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)) )
    } else if (method=="lm_robust") {
      if (missing(arg_clusters)) {
        A <- summary(lm_robust(arg_formula[[1]],weights=arg_w,se_type="stata",data=arg_data))
      } else {
        A <- summary(lm_robust(arg_formula[[1]],weights=arg_w,clusters=arg_cl,se_type="stata",data=arg_data))
      }
      other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=c(A$nobs,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)))
    } else if (method=="felm") {
      if (missing(arg_fe)) arg_fe <- "0"

      if (!missing(arg_clusters)) {
        #setnames(arg_data,c(deparse(substitute(arg_clusters))),c("arg_cl"))
        A <- summary(felm(as.formula(paste0(as.character(arg_formula[[1]])," | ",arg_fe," | 0 | arg_cl")),weights=arg_data$arg_w,data=arg_data))
      } else {
        A <- summary(felm(as.formula(paste0(as.character(arg_formula[[1]])," | ",arg_fe," | 0 | 0")),weights=arg_data$arg_w,data=arg_data))
      }

      other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf","fstat_proj_value","fstat_proj_numdf","fstat_proj_dendf"),variable=rep("zother",9),model_1=c(A$N,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4)),unname(A$F.fstat[1:3]),unname(A$P.fstat[c(5,3,6)])) )
    }
    #*** Look at Note5
    coef <- f_reg_results( data.table(A$coefficients,keep.rownames=TRUE)[,1:5], arg_round=4 )
    coef <- melt(coef,id.vars = c("rn"), measure.vars = c("coef","std","t_value","p_value","coef_star","std_par"),value.name=c(paste0("model_",1)))
    coef <- rbind(coef,other)
    if (i == i0) toc()
    rm(A,other)

    if (length(arg_formula)>1) {
      for (i in 2:length(arg_formula)) {
        if (i %in% iter_vec) {
          tic(paste0("Iteration ",i," to ",min(i+by_iter-1,length(arg_formula))))
          i0 <- i+by_iter -1
        }
        if (method=="lm") {
          A <- summary(lm(arg_formula[[i]],data=arg_data,weights=arg_w,model=FALSE))
          other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=    c(length(A$residuals),format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)) )
          setnames(other,c(3),c(paste0("model_",i)))
        } else if (method=="lm_robust") {
          if (missing(arg_clusters)) {
            A <- summary(lm_robust(arg_formula[[i]],weights=arg_w,se_type="stata",data=arg_data))
          } else {
            A <- summary(lm_robust(arg_formula[[i]],weights=arg_w,clusters=arg_cl,se_type='stata',data=arg_data))
          }
          other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=c(A$nobs,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)))
          setnames(other,c(3),c(paste0("model_",i)))
        } else if (method=="felm") {

          if (!missing(arg_clusters)) {
            A <- summary(felm(as.formula(paste0(as.character(arg_formula[[i]])," | ",arg_fe," | 0 | arg_cl")),weights=arg_data$arg_w,data=arg_data))
          } else {
            A <- summary(felm(as.formula(paste0(as.character(arg_formula[[i]])," | ",arg_fe," | 0 | 0")),weights=arg_data$arg_w,data=arg_data))
          }
          other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf","fstat_proj_value","fstat_proj_numdf","fstat_proj_dendf"),variable=rep("zother",9),model_1=c(A$N,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4)),unname(A$F.fstat[1:3]),unname(A$P.fstat[c(5,3,6)])) )
          setnames(other,c(3),c(paste0("model_",i)))
        }
        #*** Look at Note5
        t <- f_reg_results( data.table(A$coefficients,keep.rownames=TRUE)[,1:5], arg_round=4)
        t <- melt(t,id.vars = c("rn"), measure.vars = c("coef","std","t_value","p_value","coef_star","std_par"),value.name=c(paste0("model_",i)))
        t <- rbind(t,other)
        coef <- merge(coef,t,by=c("rn","variable"),all=TRUE)
        rm(A,other,t)
        if (i0==i) toc()
      }

    }


    ## NOOO argument ARG_WEIGHTS
  } else {
    if (i %in% iter_vec) {
      tic(paste0("Iteration ",i," to ",min(i+by_iter-1,length(arg_formula))))
      i0 <- i + by_iter -1
    }
    if (method=="lm") {
      A <- summary(lm(arg_formula[[1]],data=arg_data,model=FALSE))
      other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=    c(length(A$residuals),format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)) )
    } else if (method=="lm_robust") {
      if (missing(arg_clusters)) {
        A <- summary(lm_robust(arg_formula[[1]],se_type="stata",data=arg_data))
      } else {
        A <- summary(lm_robust(arg_formula[[1]],clusters=arg_cl,se_type='stata',data=arg_data))
      }
      other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=c(A$nobs,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)))
    } else if (method=="felm") {
      if (missing(arg_fe)) arg_fe <- "0"

      if (!missing(arg_clusters)) {
        #setnames(arg_data,c(deparse(substitute(arg_clusters))),c("arg_cl"))
        A <- summary(felm(as.formula(paste0(as.character(arg_formula[[1]])," | ",arg_fe," | 0 | arg_cl")),data=arg_data))
      } else {
        A <- summary(felm(as.formula(paste0(as.character(arg_formula[[1]])," | ",arg_fe," | 0 | 0")),data=arg_data))
      }

      other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf","fstat_proj_value","fstat_proj_numdf","fstat_proj_dendf"),variable=rep("zother",9),model_1=c(A$N,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4)),unname(A$F.fstat[1:3]),unname(A$P.fstat[c(5,3,6)])) )
    }

    coef <- f_reg_results( data.table(A$coefficients,keep.rownames=TRUE)[,1:5], arg_round=4 )
    coef <- melt(coef,id.vars = c("rn"), measure.vars = c("coef","std","t_value","p_value","coef_star","std_par"),value.name=c(paste0("model_",1)))
    coef <- rbind(coef,other)
    if (i == i0) toc()
    rm(A,other)

    if (length(arg_formula)>1) {
      for (i in 2:length(arg_formula)) {
        if (i %in% iter_vec) {
          tic(paste0("Iteration ",i," to ",min(i+by_iter-1,length(arg_formula))))
          i0 <- i + by_iter -1
        }
        if (method=="lm") {
          A <- summary(lm(arg_formula[[i]],data=arg_data,model=FALSE))
          other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=    c(length(A$residuals),format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)) )
          setnames(other,c(3),c(paste0("model_",i)))
        } else if (method=="lm_robust") {
          if (missing(arg_clusters)) {
            A <- summary(lm_robust(arg_formula[[i]],weights=arg_w,se_type="stata",data=arg_data))
          } else {
            A <- summary(lm_robust(arg_formula[[i]],weights=arg_w,clusters=arg_cl,se_type='stata',data=arg_data))
          }
          other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf"),variable=rep("zother",6),model_1=c(A$nobs,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4),nsmall=2),unname(A$fstatistic)))
          setnames(other,c(3),c(paste0("model_",i)))
        } else if (method=="felm") {

          if (!missing(arg_clusters)) {
            A <- summary(felm(as.formula(paste0(as.character(arg_formula[[i]])," | ",arg_fe," | 0 | arg_cl")),data=arg_data))
          } else {
            A <- summary(felm(as.formula(paste0(as.character(arg_formula[[i]])," | ",arg_fe," | 0 | 0")),data=arg_data))
          }
          other <- data.table(rn=c("nobs","r2","adj.r2","fstat_value","fstat_numdf","fstat_dendf","fstat_proj_value","fstat_proj_numdf","fstat_proj_dendf"),variable=rep("zother",9),model_1=c(A$N,format(round(A$r.squared,4),nsmall=2),format(round(A$adj.r.squared,4)),unname(A$F.fstat[1:3]),unname(A$P.fstat[c(5,3,6)])) )
        }

        t <- f_reg_results( data.table(A$coefficients,keep.rownames=TRUE)[,1:5], arg_round=4)
        t <- melt(t,id.vars = c("rn"), measure.vars = c("coef","std","t_value","p_value","coef_star","std_par"),value.name=c(paste0("model_",i)))
        t <- rbind(t,other)
        coef <- merge(coef,t,by=c("rn","variable"),all=TRUE)
        rm(A,other,t)
        if (i == i0) toc()
      }

    }


  }

  coef <- as.data.table(coef)
  coef[,order:=ifelse(variable=="zother",1,0)]
  setorder(coef,order,rn)
  coef[,order:=NULL]

  if (!missing(arg_weights)) { setnames(arg_data, c("arg_w"), c(deparse(substitute(arg_weights))) )   }

  if (!missing(arg_clusters)) {
    #setnames(arg_data,c("arg_cl"),c(deparse(substitute(arg_clusters))))
    arg_data <- arg_data[,arg_cl:=NULL]
  }
  if (i==length(arg_formula)) toc()
  return(coef)
}

#' Function to organize regression coefficients.
#'
#' @param X data.table containing the variable names order_var
#' @param order List of vectors in the desired order
#' @param order_var Variable in the containing the variable names
#' @import data.table
#' @export

f_order_reg <- function(X,Order,order_var) {
  Y <- copy(X)[,vec_order:=Inf]
  setnames(Y,c(deparse(substitute(order_var))),c("arg_order_var"))
  for (i in 1:length(Order)) {
    Y <- Y[arg_order_var %in% Order[[i]],vec_order:=i]
  }
  Y <- Y[order(vec_order,arg_order_var)][,vec_order:=NULL]
  setnames(Y,c("arg_order_var"),c(deparse(substitute(order_var))))
  return (Y)
}
