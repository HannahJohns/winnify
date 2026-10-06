.get_coef_confint <- function(x){

    # We want to evaluate coef(x) and confint(x) 
    # reliably, even if the namespace that created
    # x isn't loaded.

    # If x is an S4 object this is easy

    # If matching S4 methods were found, get the result 
    # Otherwise, attempt to fall back to S3 methods

    if(isS4(x)){
       def <- sloop::s4_methods_class(class(x))               

       # If we fail to find coef or confint, just mark it as NULL
       # and attempt an S3 method call.

       function_coef <- tryCatch(getExportedValue(def$source, "coef"), error=function(e){NULL})
       function_confint <- tryCatch(getExportedValue(def$source, "confint"), error=function(e){NULL})
    } else {
        function_coef <- function_confint <- NULL
    }
    
    # The default message for a failure state here is going to be
    # awful to try and understand, so add some additional context
    # about what went wrong

    if(!is.null(function_coef)){
        result_coef <- function_coef(x)
    } else {
        result_coef <- tryCatch(stats::coef(x),error=function(e){e})
        if("error" %in% class(result_coef)){
            stop(sprintf("Error when attempting to evaluate coef.%s(): %s",class(x),result_coef$message))
        }
    }
    
    if(!is.null(function_confint)){
        result_confint <- function_confint(x)
    } else {
        result_confint <- tryCatch(stats::confint(x),error=function(e){e})
        if("error" %in% class(result_coef)){
            stop(sprintf("Error when attempting to evaluate confint.%s(): %s",class(x),result_coef$message))
        }
    }

    return(
        list(
            coef = result_coef,
            confint = result_confint
        )
    )
}
