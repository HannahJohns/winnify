#' Wrapper for the win.stat() function from the WINS package
#'
#' @description A wrapper function for win.stat() function, allowing it to be used
#' with winnify(). It also provides several quality of life improvements to the
#' function's interface, including specifying variables using a formula
#' instead of requiring the data to have specifically named columns.
#'
#' @usage wins(formula, data)
#'
#' @param formula An object of class formula describing the analyses to run. Format for model specification are given in Details
#' @param data A data.frame to analyse
#' @param effect.size A character vector indicating which effect size measures to return. Allows partial matches with abbreviations, e.g. "wr" will match to "Win Ratio"
#' @param iptw.weight A numeric vector giving the weights for IPT weighting. See \link[WINS]{win.stat}
#' @param alpha  A numeric argument giving the significance level. See \link[WINS]{win.stat}
#' @param tau A numeric vector argument giving the threshold for minimal clinically important difference. \link[WINS]{win.stat}
#' @param np.direction A chacaracer vector indicating if a larger or smaller value is better. See the \code{np_direction} argument in \link[WINS]{win.stat}
#' @param horizon A numeric argument giving Time horizon of interest. See \link[WINS]{win.stat}
#' @param win.strategy A custom function for determining wins, losses and ties. See \link[WINS]{win.stat}
#' @param pvalue A numeric value giving the significance level. See \link[WINS]{win.stat}
#' @param stratum.weight A character value giving the method for weighting each stratum. See \link[WINS]{win.stat}
#' @param method A character value giving the method for weighting individual participants. See \link[WINS]{win.stat}
#' @param ... Arguments passed from user defined functions for \code{win.strategy}. See \link[WINS]{win.stat}
#'
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
#' Values for \code{np.direction} and \code{tau} correspond to each of these terms in this order.
#'
#' The right hand side of \code{formula} provides grouping variable, ID variable, strata, time and any covariates used for inverse-probability of censoring weighting.
#' These are marked using the following functions:
#' \itemize{
#' \item \code{w_arm(x)} - marks \code{x} as being the grouping variable. Is mandatory in the formula.
#' \item \code{w_id(x)} - marks \code{x} as being the ID variable
#' \item \code{w_stratum(x)} - marks \code{x} as being the stratum variable
#' \item \code{w_time(x)} - marks \code{x} as being the time variable for time-varying covariates.
#' }
#' All unmarked terms are assumed to be covariates used for estimating inverse-probability
#' of censoring weighting. Note that win.stat does not support covariate adjustment of the
#' treatment effect estimate. Use the \code{iptw.weight} argument to do this.
#'
#' @returns this still needs written
#'
#' @examples
#' 
#' #Examples need written still
#'
#'
wins <- function(formula, data,
                 effect.size = c("Win Ratio","Win Odds","Net Benefit"),
                 iptw.weight=NULL,
                 alpha=0.05,
                 tau = 0,
                 np.direction = "larger",
                 horizon =Inf,
                 win.strategy = NULL,
                 pvalue = c("two-sided","one-sided"),
                 stratum.weight =  c("unstratified","MH-type","wt.stratum1","wt.stratum2","equal"),
                 method  = c("unadjusted","ipcw_tau","ipcw","covipcw","iptw","iptw_ipcw","iptw_covipcw"),
                 ...){

        if(!is.null(iptw.weight)) stop("iptw is not working yet we need to make it compatible with long form data")

        # Allow common abbreviations of effect.size as inputs, e.g. wr, wo, nb that can't be caught by pmatch
        effect_size_wr <- effect_size_wo <- effect_size_nb <- rep(NA, length(effect.size))

        effect_size_wr[grepl("w(in)*.*r(atio)*",tolower(effect.size))] <- "Win Ratio"

        # There's probably a regex that can combine these but
        # this is honestly simpler.
        effect_size_wo[grepl("w(in)*.*o(dds)*",tolower(effect.size)) & !grepl("w(in)*.*r(atio)*",tolower(effect.size))] <- "Win Odds"
        effect_size_nb[grepl("n(et)*.*b(enefit)*",tolower(effect.size))] <- "Net Benefit"

        count_es_matches <- as.numeric(!is.na(effect_size_wr)) +   as.numeric(!is.na(effect_size_wo)) +   as.numeric(!is.na(effect_size_nb))

        if(any(count_es_matches>1)){
                stop(sprintf("%s in effect.size matched to multiple of Win Ratio, Win Odds and Net Benefit",
                             paste(effect.size[count_es_matches>1],collapse = ",")
                             ))
        }

        if(any(count_es_matches==0)){
                stop(sprintf("%s in effect.size could not be matched to Win Ratio, Win Odds or Net Benefit",
                             paste(effect.size[count_es_matches==0],collapse = ",")
                ))
        }

        effect.size[!is.na(effect_size_wr)] <- effect_size_wr[!is.na(effect_size_wr)]
        effect.size[!is.na(effect_size_wo)] <- effect_size_wo[!is.na(effect_size_wo)]
        effect.size[!is.na(effect_size_nb)] <- effect_size_nb[!is.na(effect_size_nb)]

        pvalue <- match.arg(pvalue,choices = c("two-sided","one-sided"))

        # If default matches are dependent on other inputs, check this here.
        # We'll set as NULL if we need to re-set later.
        stratum.weight <- match.arg(stratum.weight,choices =  c("unstratified","MH-type","wt.stratum1","wt.stratum2","equal"), several.ok = TRUE)
        method <- match.arg(method, choices = c("unadjusted","ipcw_tau","ipcw","covipcw","iptw","iptw_ipcw","iptw_covipcw"), several.ok = TRUE)

        if(length(stratum.weight)>1) stratum.weight <- NULL
        if(length(method)>1) method <- NULL

        otherArgs <- list(...)
        if(!is.null(otherArgs$ep_type)) stop("argument ep_type is inferred and should not be supplied")
        if(!is.null(otherArgs$priority)) stop("argument priority is inferred and should not be supplied")

        # Formula Parsing --------------------------------

        specials <- c("w_id","w_arm","w_stratum","w_time")

        lhs <- terms.formula(formula[c(1,2)],specials = specials)

        # We shouldn't have any specials on the left hand side
        for(i in specials){
                if(length(attr(lhs,"specials")[[i]])>0) stop(sprintf("%s() should not be used on left hand side of formula",i))
        }

        rhs <- terms.formula(formula[c(1,3)],specials = specials)
        for(i in specials){
                if(length(attr(rhs,"specials")[[i]])>1) stop(sprintf("%s() should only be used once in the right hand side of formula",i))
        }



        idVar <- attr(rhs,"specials")$w_id
        if(!is.null(idVar)){
                idVar <- attr(rhs,"term.labels")[idVar]
        }

        groupVar <- attr(rhs,"specials")$w_arm
        if(is.null(groupVar)){
                stop("group() is required")
        } else {
                groupVar <- attr(rhs,"term.labels")[groupVar]
        }

        strataVar <- attr(rhs,"specials")$w_stratum
        if(is.null(strataVar)){
                if(is.null(stratum.weight)){
                        stratum.weight <- "unstratified"
                }
                if(stratum.weight == "unstratified") warning(sprintf("stratum.weight is %s, but strata was not specified",stratum.weight))
        } else {
                strataVar <- attr(rhs,"term.labels")[strataVar]
        }

        timeVar <- attr(rhs,"specials")$w_time
        if(!is.null(timeVar)){
                timeVar <- attr(rhs,"term.labels")[timeVar]
        }

        # Any other variables are covariates for covariate weighted IPCW
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
                stratum.weight <- "MH-type"

                main_data <- cbind(
                        main_data,
                        data.frame(
                                stratum = eval(parse(text=strataVar),envir = data)
                        )
                )
        } else {
                # TODO: Check if stratified analysis was specified and error

                if(!is.null(stratum.weight)) if(stratum.weight != "unstratified") warning(sprintf("stratum.weight is %s but no w_stratum was specified",stratum.weight ))

                stratum.weight <- "unstratified"
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

        if(length(np.direction)==1){
                np.direction <- rep(np.direction,length(ep_type))
        } else {
                if(length(np.direction) < length(ep_type)){
                        stop("np.direction should be of length 1 or at least of length equal to the number of left hand side terms in formula")
                } else {
                      if(!is.null(names(np.direction))){
                              if(length(setdiff(names(np.direction), attr(lhs,"term.labels")))>0){
                                      stop(sprintf("If named, names for np.direction should match terms in left hand side of formula: %s",paste(attr(lhs,"term.labels"), collapse=", ")))
                              }
                              np.direction <- np.direction[attr(lhs,"term.labels")]
                      } else {
                              np.direction <- np.direction[1:length(ep_type)]
                      }
                }
        }
        names(np.direction) <- attr(lhs,"term.labels")

        if(length(tau)==1){
                tau <- rep(tau,length(ep_type))
        } else {
                if(length(tau) < length(ep_type)){
                        stop("tau should be of length 1 or at least of length equal to the number of left hand side terms in formula")
                } else {
                        if(!is.null(names(tau))){
                                if(length(setdiff(names(tau), attr(lhs,"term.labels")))>0){
                                        stop(sprintf("If named, names for tau should match terms in left hand side of formula: %s",paste(attr(lhs,"term.labels"), collapse=", ")))
                                }
                                tau <- tau[attr(lhs,"term.labels")]
                        } else {
                                tau <- tau[1:length(ep_type)]
                        }
                }
        }
        names(tau) <- attr(lhs,"term.labels")

        priority <- 1:length(ep_type)

        # The main dataset shouldn't have repeated measures.

        # Summarise main_data by id. The main dataset should only have
        # one unique non-missing value of each outcome, arm and strata
        main_data <- do.call("rbind",by(main_data,main_data[,"id"],function(x){
                out <- x
                out <- out[1,]

                for(i in colnames(x)){
                        out[,i] <- NA
                        thisVar <- x[,i]
                        thisVar <- unique(thisVar[!is.na(thisVar)])

                        # This level of granularity of warning is overkill.
                        # Just warn that rows got dropped.

                        # if(length(thisVar)==0){
                        #         warning(sprintf("No non-missing value for %s found in %s %s",
                        #                         i,
                        #                         ifelse(is.null(idVar),"row",idVar),
                        #                         out[,"id"]
                        #                         )
                        #         )
                        # }

                        if(length(thisVar)>1){
                                stop(sprintf("Multiple values of %s found in %s %s",
                                                i,
                                                ifelse(is.null(idVar),"row",idVar),
                                                out[,"id"]
                                )
                                )

                        }
                        out[,i] <- thisVar

                }

                if(nrow(out)>1) stop(sprintf("ID %s has multiple outcome values",x[,"id"]))
                out
        }))

        toDrop <- apply(main_data,1,function(x){any(is.na(x))})
        if(sum(toDrop)>0) warning(sprintf("%d records dropped",sum(toDrop)))

        main_data <- main_data[which(!toDrop),]

        # Covariate Data Setup --------------------------------

        if(length(otherVars)>0){

                if(is.null(method)){
                        if(is.null(iptw.weight)){
                                method  <- "covipcw"
                        } else {
                                method <- "iptw_covipcw"
                        }
                }

                if(length(idVar) == 0 & !is.null(timeVar)){
                        stop("id variable is required if time-varying covariates are used")
                }

                if(length(idVar)>0){
                        covariate_data <- data.frame(
                                id = eval(parse(text=idVar),envir = data)
                        )
                } else {
                        covariate_data <- data.frame(
                                id = 1:nrow(data)
                        )
                }

                if(length(timeVar)>0){
                        covariate_data <- cbind(
                                covariate_data,
                                data.frame(
                                        time = eval(parse(text=timeVar),envir = data)
                                )
                        )
                } else {
                        covariate_data <- cbind(
                                covariate_data,
                                data.frame(
                                        time = rep(0,nrow(covariate_data))
                                )
                        )
                }

                for(i in otherVars){
                        covariate_data[, i] <- eval(parse(text=i),envir = data)
                }

                if(length(timeVar)==0){
                        # Summarise covariate_data by id. If we're assuming everything is measured at baseline,
                        # we should only have one unique non-missing value of each outcome, arm and strata
                        covariate_data <- do.call("rbind",by(covariate_data,covariate_data[,"id"],function(x){
                                out <- x
                                out <- out[1,]

                                for(i in colnames(x)){
                                        out[,i] <- NA
                                        thisVar <- x[,i]
                                        thisVar <- unique(thisVar[!is.na(thisVar)])

                                        # This level of granularity of warning is overkill.
                                        # Just warn that rows got dropped.

                                        # if(length(thisVar)==0){
                                        #         warning(sprintf("No non-missing value for %s found in %s %s",
                                        #                         i,
                                        #                         ifelse(is.null(idVar),"row",idVar),
                                        #                         out[,"id"]
                                        #                         )
                                        #         )
                                        # }

                                        if(length(thisVar)>1){
                                                stop(sprintf("Multiple values of %s found in %s %s",
                                                             i,
                                                             ifelse(is.null(idVar),"row",idVar),
                                                             out[,"id"]
                                                )
                                                )

                                        }
                                        out[,i] <- thisVar

                                }

                                if(nrow(out)>1) stop(sprintf("ID %s has multiple outcome values. Is w_time() missing?",x[,"id"]))
                                out
                        }))
                }

                toDrop <- apply(covariate_data,1,function(x){any(is.na(x))})
                if(sum(toDrop)>0) warning(sprintf("%d records dropped due to missing covariates",sum(toDrop)))
                covariate_data <- covariate_data[which(!toDrop),]

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

        # win.stat() expects ID is expected to be numeric, from 1 to n_trt or n_con, where this
        # is separate across both arms. I.e. there should be two records with
        # an ID of 1, 2, 3, etc. One will be ID 1 in the treatment group
        # and one will be ID 1 in the control group. These are two different people,
        # despite sharing the same ID.

        # I can't change this behavior in a wrapper function, so instead we just transform
        # the input data to comply with this expectation by doing an
        # as.numeric(droplevels(factor())) conversion separately within the control
        # and treatment datasets.

        # We do the same to the main dataset and then join it all back together
        # once we've converted IDs within each arm to numerics separately.

        # This is stupid but it works. I don't think this trick is needed
        # if we haven't supplied covariate data.

        if(!is.null(covariate_data_trt)){

                covariate_data_trt$id <- factor(factor(covariate_data_trt$id))
                covariate_data_con$id <- factor(factor(covariate_data_con$id))
                covariate_data_trt$id <- droplevels(covariate_data_trt$id)
                covariate_data_con$id <- droplevels(covariate_data_con$id)

                covariate_data_trt$id <- as.numeric(covariate_data_trt$id)
                covariate_data_con$id <- as.numeric(covariate_data_con$id)


                con_id <- trt_id <- factor(main_data$id)
                trt_id[main_data$arm==levels(main_data[,"arm"])[1]] <- NA
                con_id[main_data$arm==levels(main_data[,"arm"])[2]] <- NA

                con_id <- droplevels(con_id)
                trt_id <- droplevels(trt_id)

                con_id <- as.numeric(con_id)
                trt_id <- as.numeric(trt_id)

                main_data$id <- ifelse(is.na(con_id),trt_id,con_id)

        }


        # If length(ep_type)==1, win.stat() uses cat() to print a message
        # that lets the user know that ep_type is the outcome type for all
        # variables. This triggers even when there is only a single variable
        # and is impossible to silence.

        # This not great workaround: We intercept this and all other messages
        # returned from win.stat() and if they're not this bugged behaviour
        # we print it using message() so that other people can suppress
        # if they want to.
        messages <- capture.output({
                out <- win.stat(
                        data=main_data,
                        ep_type=ep_type,
                        Z_t_trt = covariate_data_trt,
                        Z_t_con = covariate_data_con,
                        iptw.weight = iptw.weight,
                        arm.name = rev(levels(main_data[,"arm"])),
                        priority=priority,
                        alpha = alpha,
                        digit=10 ,     # Does nothing, completely arbitrary when summary.print = F
                        tau=tau,
                        np_direction = np.direction,
                        horizon = horizon,
                        win.strategy = win.strategy,
                        pvalue = pvalue,
                        stratum.weight = stratum.weight,
                        method  = method,
                        summary.print = FALSE,
                        ...
                )
        })

        for(this_message in messages){
                if(grepl("The outcome type for all the endpoints:",this_message) & length(ep_type)==1) next
                message(this_message)
        }

        # Format results into a table to report back

        estimates <- do.call("rbind",lapply(names(out$Win_statistic),function(i){
                cbind(effect_size=i,
                      as.data.frame(matrix(out$Win_statistic[[i]],nrow=1))
                )
        }))

        estimates$effect_size <- gsub("_"," ",estimates$effect_size)
        colnames(estimates)[-1] <- c("estimate","lower","upper")

        estimates$pval <- c(out$p_value)

        if(length(strataVar)==0){
                estimates$win <- out$Win_prop$P_trt
                estimates$loss <- out$Win_prop$P_con
                estimates$tie <- 1 - (out$Win_prop$P_trt + out$Win_prop$P_con)

        } else {

                # win.stat doesn't pool win proportions across stratum.
                # Winnify needs this for the plot.

                # Fortunately, this can be manually extracted.

                # This code is copied directly from the internal workings of
                # the win.stat() function

                # https://github.com/cran/WINS/blob/master/R/unadjusted.win.stat.R#L146

                # N is sample size per strata
                # N_event is number of events per arm.
                # Based on the following code:

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

        estimates <- estimates[which(estimates$effect_size %in% effect.size),]

        estimates$neutral_point <- ifelse(estimates$effect_size %in% c("Win Ratio","Win Odds"),1,0)
        estimates$tie_handling <- ifelse(estimates$effect_size %in% c("Win Odds","Net Benefit"),"split","drop")

        attr(estimates,"groups") <- levels(main_data[,"arm"])
        attr(estimates,"arguments") <- list(
                formula=formula,
                data=data,
                effect.size = effect.size,
                iptw.weight = iptw.weight,
                arm.name = rev(levels(main_data[,"arm"])),
                alpha = alpha,
                tau=tau,
                np_direction = np.direction,
                horizon = horizon,
                win.strategy = win.strategy,
                pvalue = pvalue,
                stratum.weight = stratum.weight,
                method  = method
        )

        return(estimates)
}