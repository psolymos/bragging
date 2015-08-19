'logLik.try-error' <- function (object, ...) {
    structure(-.Machine$double.xmax^(1/3), df = 1, 
        nobs = 1, class = "logLik")
}

glm_skeleton <- function(object, ..., CAICalpha=0.5) {
    if (inherits(object, "try-error"))
        return(object)
    out <- structure(list(
        call=object$call,
        formula=formula(object),
        coef=coef(object),
        converge=object$converge,
        logLik=as.numeric(logLik(object)),
        df=attr(logLik(object), "df"),
        nobs=nobs(object)), class="glm_skeleton")
    out$class0 <- class(object)[1L]
    out$aic <- -2*out$logLik + 2*out$df
    out$bic <- -2*out$logLik + log(out$nobs)*out$df
    out$caic <- CAICalpha * out$aic + (1-CAICalpha) * out$bic
    out
}

getTerms <- function(mods, type=c("formula", "list")) {
    type <- match.arg(type)
    x <- unlist(lapply(unlist(mods), function(z) as.character(z)[3]))
#    x <- unname(substr(x, 5, nchar(x)))
    x <- gsub(". + ", "", x, fixed=TRUE)
    x <- unlist(strsplit(x, "+", fixed=TRUE))
    x <- unlist(strsplit(x, "*", fixed=TRUE))
    if (type == "list")
        x <- unlist(strsplit(x, ":", fixed=TRUE))
    x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
    x <- unique(x)
    if (type == "formula")
        x <- as.formula(paste("~", paste(x, collapse=" + ", sep="")))
    x
}

fixNames <- function(x, sep=":") {
    unlist(lapply(x, function(z) {
        paste(sort(strsplit(z, sep)[[1]]), collapse=sep)
    }))
}

Lc_cut <-
function (lam, transform=FALSE) 
{
    if (transform)
        lam <- 1-exp(-lam)
    o <- order(lam)
    x <- lam[o]
    p <- seq_len(length(x))/sum(length(x))
    L <- cumsum(x)/sum(x)
    p <- c(0, p)
    L <- c(0, L)
    J <- p - L

    G <- sum(x * 1:length(x))
    G <- 2 * G/(length(x) * sum(x))
    G <- G - 1 - (1/length(x))

    m1 <- which.max(J)
    list(lam=unname(ifelse(transform, -log(1-x[m1]), x[m1])), L=unname(L[m1+1]), 
        p=unname(p[m1+1]), S=unname(L[m1+1]+p[m1+1]), 
        G=G, J=max(p - L))
}

