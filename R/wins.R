#' Wrapper for the win.stat() function from the WINS package
#'
#' @description A wrapper function for win.stat() function, allowing it to be used
#' with winnify(). It also provides several quality of life improvements to the
#' function's interface, including specifying variables using a formula
#' instead of requiring the data to have specifically named columns.
#'
#' @usage wins(formula, data)
#'
#' @param formula An object of class formula describing the analyses to run. Details of model specification are given in Details
#' @param statistic A function for performing analysis based on a formula and data. Details are given in Details.
#' @param data A data.frame to analyse
#'
#' @details
#'
#' Note that this wrapper uses several different defaults to the win.stat function
#' which either match common practice in other functions, or infer intent based on
#' user input. These differences are:
#' \itemize{
#'      \item The default \code{p-value} is "two-sided", not "one-sided"
#'      \item If unspecified, stratum.weight is inferred based on the presence of stratum variables:
#'      \itemize{
#'       \item If no stratum variable is specified in \code{formula}, the default stratum.weight is "unstratified".
#'       \item If a stratum variable is specified in \code{formula}, the default is "MH-type".
#'      }
#'      \item If unspecified, the default value for \code{method} is inferred:
#'      \itemize{
#'      \item If \code{iptw.weight} is not provided:
#'      \itemize{
#'      \item If \code{formula} doesn't contain any covariates, the default is "unadjusted"
#'      \item If \code{formula} contains covariates, the default is "covipcw"
#'      }
#'      \item If \code{iptw.weight} is provided:
#'      \itemize{
#'      \item If \code{formula} doesn't contain any covariates, the default is "iptw"
#'      \item If \code{formula} contains covariates, the default is "iptw_covipcw"
#'      }
#'      }
#'      \item Comparator vs Reference group is determined based on factor levels, with the first level used as the reference group and the second as the comparator.
#' }
#'
#' The wrapper does not provide arguments for specifying outcome priority or endpoint type.
#' These values are inferred from user input.
#'
#' The left hand side of \code{formula} provides each endpoint as successive terms,
#' with the most important endpoint on the left and the least important on the right.
#' Values for \code{np_direction} and \code{tau} correspond to each of these terms in this order.
#'
#' The right hand side of \code{formula} provides grouping variable, ID variable, strata, time and any covariates used for inverse-probability of censoring weighting.
#' These are marked using the following functions:
#' \itemize{
#' \item \code{arm(x)} - marks \code{x} as being the grouping variable. Is mandatory in the formula.
#' \item \code{id(x)} - marks \code{x} as being the ID variable
#' \item \code{stratum(x)} - marks \code{x} as being the stratum variable
#' \item \code{time(x)} - marks \code{x} as being the time variable for time-varying covariates.
#' }
#' All unmarked terms are assumed to be covariates used for estimating inverse-probability
#' of censoring weighting. Note that win.stat does not support covariate adjustment of the
#' treatment effect estimate. Use the \code{iptw.weight} argument to do this.
#'
#'
#' @examples
#'
#'
wins <- function(formula, data,
                 effect.size = c("winratio","netbenefit","winodds"),
                 iptw.weight=NULL, alpha=0.05,
                 tau = 0,
                 np_direction = "larger",
                 horizon =Inf,
                 win.strategy = NULL,
                 pvalue = c("two-sided","one-sided"),
                 stratum.weight =  c("unstratified","MH-type","wt.stratum1","wt.stratum2","equal"),
                 method  = c("unadjusted","ipcw_tau","ipcw","covipcw","iptw","iptw_ipcw","iptw_covipcw"),
                 summary.print = F,
                 ...){

        effect_size <- match.arg(effect_size)
        pvalue <- match.arg(effect_size)

        # If default matches are dependent on input, check this here.
        # We'll set as NULL if we need to re-set later.
        stratum.weight <- match.arg(stratum.weight, several.ok = TRUE)
        method <- match.arg(method, several.ok = TRUE)

        if(length(stratum.weight)>1) stratum.weight <- NULL
        if(length(method)>1) method <- NULL

        otherArgs <- list(...)
        if(!is.null(otherArgs$ep_type)) stop("argument ep_type is inferred and should not be supplied")
        if(!is.null(otherArgs$priority)) stop("argument priority is inferred and should not be supplied")

        # Formula Parsing --------------------------------

        lhs <- terms.formula(formula[c(1,2)],specials = c("id","group","strata","time"))

        # We shouldn't have any specials on the left hand side
        for(i in c("id","group","stratum","time")){
                if(length(attr(lhs,"specials")[[i]])>0) stop(sprintf("%s() should not be used on left hand side of formula",i))
        }

        rhs <- terms.formula(formula[c(1,3)],specials = c("id","group","strata","time"))
        for(i in c("id","group","stratum","time")){
                if(length(attr(rhs,"specials")[[i]])>1) stop(sprintf("%s() should only be used once in the right hand side of formula",i))
        }


        idVar <- attr(rhs,"specials")$id
        if(!is.null(idVar)){
                idVar <-gsub("^id\\(|\\)$","",attr(rhs,"term.labels")[idVar])
        }

        groupVar <- attr(rhs,"specials")$group
        if(!is.null(groupVar)){
                groupVar <-gsub("^group\\(|\\)$","",attr(rhs,"term.labels")[groupVar])
        } else {
                stop("group() is required")
        }


        strataVar <- attr(rhs,"specials")$strata
        if(!is.null(strataVar)){
                strataVar <-gsub("^strata\\(|\\)$","",attr(rhs,"term.labels")[strataVar])

                if(is.null(stratum.weight)){
                        stratum.weight <- "MH-type"
                }
                if(stratum.weight == "unstratified") warning("stratum.weight is unstratified, but strata was specified")
        } else {
                if(is.null(stratum.weight)){
                        stratum.weight <- "unstratified"
                }
                if(stratum.weight == "unstratified") warning(sprintf("stratum.weight is %s, but strata was not specified",stratum.weight))
        }

        timeVar <- attr(rhs,"specials")$time
        if(!is.null(timeVar)){
                timeVar <-gsub("^time\\(|\\)$","",attr(rhs,"term.labels")[timeVar])
        }

        # Any other variables are covariates for covariate
        otherVars <- attr(rhs,"term.labels")[setdiff(1:length(attr(rhs,"term.labels")), unlist(attr(rhs,"specials")))]

        # Main Data Setup --------------------------------
        main_data <- data.frame(
                arm = eval(parse(text=groupVar),envir = data)
        )

        if(!("factor" %in% main_data[,"arm"])) main_data[,"arm"] <- as.factor(main_data[,"arm"])
        if(length(levels(main_data[,"arm"]))!=2){
                stop(sprintf("%s contains more than two groups",groupVar))
        }

        if(length(idVar)>0){
                main_data <- cbind(
                        main_data,
                        data.frame(
                                id = eval(parse(text=idVar),envir = data)
                        )
                )
        } else {
                main_data <- cbind(
                        main_data,
                        data.frame(
                                id = 1:nrow(main_data)
                        )
                )
        }

        if(length(strataVar)>0){
                main_data <- cbind(
                        main_data,
                        data.frame(
                                stratum = eval(parse(text=strataVar),envir = data)
                        )
                )
        } else {
                # TODO: Check if stratified analysis was specified and error
        }


        # Infer endpoint types, there's zero reason for the user to need to specify this.
        ep_type <- rep(NA,length(attr(lhs,"term.labels")))
        for(i in  1:length(attr(lhs,"term.labels"))){
                thisvar <- eval(parse(text=attr(lhs,"term.labels")[i]),envir = data)

                if("Surv" %in% class(thisvar)){
                        main_data[,sprintf("Delta_%d",i)] <- thisvar[,2]
                        main_data[,sprintf("Y_%d",i)] <- thisvar[,1]
                        ep_type[i] <- "tte"
                } else if(any(c("numeric","integer") %in% class(thisvar))){
                        thisvar <- as.numeric(thisvar)
                        if(length(setdiff(thisvar,0:1))>0){
                                main_data[,sprintf("Y_%d",i)] <- thisvar
                                ep_type[i] <- "binary"
                        } else {
                                main_data[,sprintf("Y_%d",i)] <- thisvar
                                ep_type[i] <- "continuous"
                        }
                } else if ("logical" %in% class(thisvar)){
                        thisvar <- as.numeric(thisvar)
                        main_data[,sprintf("Y_%d",i)] <- thisvar
                        ep_type[i] <- "binary"
                } else {
                        stop(sprintf("%s does not result in a recognisable data type",attr(lhs,"term.labels")[i]))
                }

        }

        # Check these against ep_type
        np_direction <- "larger"
        tau <- 0

        if(length(np_direction)==1){
                np_direction <- rep(np_direction,length(ep_type))
        } else {
                stop("not written yet")
        }

        if(length(tau)==1){
                tau <- rep(tau,length(ep_type))
        } else {
                stop("not written yet")
        }

        priority <- 1:length(ep_type)

        # The main dataset shouldn't have repeated measures.

        # Summarise main_data by id. The main dataset should only have

        main_data <- do.call("rbind",by(main_data,main_data[,"id"],function(x){

                out <- x
                out <- out[1,]

                for(i in colnames(x)){
                        out[,i] <- NA
                        thisVar <- x[,i]
                        thisVar <- unique(thisVar[!is.na(thisVar)])

                        if(length(thisVar)==0){
                                warning("No non-missing value found for ID %s. REMOVED IT",

                                )
                        }
                        if(length(thisVar)>1){
                                stop("Multiple values found for ID %s",

                                )
                        }
                        out[,i] <- thisVar

                }

                if(nrow(out)>1) stop(sprintf("ID %s has multiple outcome values",x[,"id"]))
                out
        }))


        # Covariate Data Setup --------------------------------

        if(length(otherVars)>0){

                if(is.null(method)){
                        if(is.null(iptw.weight)){
                                method  <- "covipcw"
                        } else {
                                method <- "iptw_covipcw"
                        }
                }

                # If no time variable, assume covariates are measured at baseline
                if(is.null(timeVar)){

                        stop("null timeVar isn't written yet")


                } else {

                        if(is.null(idVar)){
                                stop("id variable is required if time-varying covariates are used")
                        }

                        covariate_data <- data.frame(
                                id = eval(parse(text=idVar),envir = data)
                        )

                        covariate_data <- cbind(
                                covariate_data,
                                data.frame(
                                        time = eval(parse(text=timeVar),envir = data)
                                )
                        )

                        for(i in otherVars){
                                covariate_data <- cbind(
                                        covariate_data,
                                        data.frame(
                                                time = eval(parse(text=i),envir = data)
                                        )
                                )
                        }

                        covariate_data_trt <- covariate_data[
                                which(
                                        covariate_data[,"id"] %in% main_data[which(main_data[,"arm"]==levels(main_data[,"arm"])[2]),"id"]
                                ),
                        ]

                        covariate_data_con <- covariate_data[
                                which(
                                        covariate_data[,"id"] %in% main_data[which(main_data[,"arm"]==levels(main_data[,"arm"])[1]),"id"]
                                ),
                        ]

                }

        } else {
                if(is.null(method)){
                        if(is.null(iptw.weight)){
                                method  <- "unadjusted"
                        } else {
                                method <- "iptw"
                        }
                }


                covariate_data_trt <- covariate_data_con <- NULL
        }

        # win.stat() expects ID is expected to be numeric and will
        # crash if a character vector is suppled.
        main_data$id <- factor(main_data$id)
        if(!is.null(covariate_data_trt)){
                covariate_data_trt$id <- factor(factor(covariate_data_trt$id), levels = levels(main_data$id))
                covariate_data_con$id <- factor(factor(covariate_data_con$id), levels = levels(main_data$id))

                covariate_data_trt$id <- as.numeric(covariate_data_trt$id)
                covariate_data_con$id <- as.numeric(covariate_data_con$id)
        }

        main_data$id <- as.numeric(main_data$id)

        out <- win.stat(
                data=main_data,
                ep_type=ep_type,
                Z_t_trt = covariate_data_trt,
                Z_t_con = covariate_data_con,
                iptw.weight = iptw.weight,
                arm.name = rev(levels(main_data[,"arm"])),
                priority,
                alpha = alpha,
                digit=10 ,     # Does nothing, completely arbitrary when summary.print = F
                tau=tau,
                np_direction =np_direction,
                horizon = horizon,
                win.strategy = win.strategy,
                pvalue = pvalue,
                stratum.weight = stratum.weight,
                method  = method,
                summary.print = F,
                ...
        )

        # Format results into a table to report back

        estimates <- do.call("rbind",lapply(names(out$Win_statistic),function(i){
                cbind(outcome=i,
                      as.data.frame(matrix(out$Win_statistic[[i]],nrow=1))
                )
        }))

        colnames(estimates)[-1] <- c("estimate","lower","upper")

        estimates$p <- c(out$p_value)

        if(length(stratum)==0){
                estimates$win <- out$Win_prop$P_trt
                estimates$loss <- out$Win_prop$P_con
                estimates$tie <- 1 - (out$Win_prop$P_trt + out$Win_prop$P_con)

        } else {

                # win.stat doesn't pool win proportions across stratum.
                # Winnify needs this for the plot.

                # Fortunately, this can be manually extracted.

                # N is sample size per strata
                # N_event is number of events per arm.
                # Based on the following code:

                # https://github.com/cran/WINS/blob/master/R/unadjusted.win.stat.R#L146

                # It looks like number of events is based on if there is
                # at least one event on any component, i.e. we fall back to composite
                # tte for estimating these weights.

                xtab <- table(
                        main_data$stratum,
                        main_data$arm
                )

                N = apply(xtab,1,sum)

                total_pairs <- apply(xtab,1,prod)

                if(any(ep_type=="tte")){
                        ind.tte <- which(ep_type == "tte")
                        n_tte <- length(ind.tte)

                        N_event <- apply(as.matrix(
                                main_data[,sprintf("Delta_%d",ind.tte)],
                                ncol = n_tte), 1, function(x) max(x)>0)

                        N_event <- tapply(N_event,main_data$stratum,sum)

                } else {
                        N_event=N
                }

                w_stratum = switch(stratum.weight,
                                   "unstratified" = 1,
                                   "equal" = rep(1/length(N),length(N)),
                                   "MH-type" = (1/N)/sum(1/N),
                                   "wt.stratum1" = N/sum(N),
                                   "wt.stratum2" = N_event/sum(N_event)
                )

                wins <- out$Win_prop[,"P_trt"]*total_pairs
                losses <- out$Win_prop[,"P_con"]*total_pairs
                ties <- total_pairs - wins - losses

                estimates$win <- sum(w_stratum*wins)
                estimates$loss <- sum(w_stratum*losses)
                estimates$tie <- sum(w_stratum*ties)

                total_pairs <- estimates$win + estimates$loss +  estimates$tie

                estimates$win  <-  estimates$win/total_pairs
                estimates$loss  <-  estimates$loss/total_pairs
                estimates$tie  <-  estimates$tie/total_pairs

        }

        estimates

}
