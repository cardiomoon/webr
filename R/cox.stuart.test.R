#' Cox-Stuart test for trend analysis
#' The Cox-Stuart test is defined as a little powerful test (power equal to 0.78), but very robust for the trend analysis.
#' It is therefore applicable to a wide variety of situations, to get an idea of the evolution of values obtained.
#' The proposed method is based on the binomial distribution.
#' This function was written by Tommaso Martino<todoslogos@@gmail.com> (See 'References')
#'
#' @param x A numeric vector
#' @return A list with class "htest"
#' @references Original code: \url{http://statistic-on-air.blogspot.kr/2009/08/trend-analysis-with-cox-stuart-test-in.html}
#' @examples
#' customers = c(5, 9, 12, 18, 17, 16, 19, 20, 4, 3, 18, 16, 17, 15, 14)
#' cox.stuart.test(customers)
#' @importFrom stats pbinom
#' @export
cox.stuart.test = function(x) {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng)%%2
    if (apross == 1) {
        delete = (length(x) + 1)/2
        x = x[-delete]
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half + 1):(length(x))]
    difference = x1 - x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs > 0]
    neg = signs[signs < 0]
    if (length(pos) < length(neg)) {
        prop = pbinom(length(pos), length(signcorr), 0.5)
        names(prop) = "Increasing trend, p-value"
        rval <- list(method = method, statistic = prop)
        class(rval) = "htest"
        return(rval)
    } else {
        prop = pbinom(length(neg), length(signcorr), 0.5)
        names(prop) = "Decreasing trend, p-value"
        rval <- list(method = method, statistic = prop)
        class(rval) = "htest"
        return(rval)
    }
}
