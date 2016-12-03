\name{lazy_Cobb_Douglas}
\alias{lazy_Cobb_Douglas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{lazyCD}
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lazy_Cobb_Douglas(utility, income, price, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{utility}{
%%     ~~Describe \code{utility} here~~
}
  \item{income}{
%%     ~~Describe \code{income} here~~
}
  \item{price}{
%%     ~~Describe \code{price} here~~
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
function (utility, income, price, alpha) 
{
    char1 <- substr(utility, 1, 3)
    char2 <- substr(utility, 9, 12)
    f <- paste(char1, alpha, char2, (1 - alpha))
    objectD <- deriv(parse(text = f), c("x1", "x2"))
    str <- as.character(objectD)
    install.packages("stringr", dependencies = TRUE)
    library(stringr)
    test <- strsplit(str, "\n")
    fun1 <- test[[1]][2]
    fun2 <- test[[1]][3]
    f1 <- substr(fun1, (str_locate(fun1, "<-")[2] + 1), nchar(fun1))
    f2 <- substr(fun2, (str_locate(fun2, "<-")[2] + 1), nchar(fun2))
    g1 <- test[[1]][6]
    g2 <- test[[1]][7]
    dv1 <- substr(g1, (str_locate(g1, "<-")[2] + 1), nchar(g1))
    dv2 <- substr(g2, (str_locate(g2, "<-")[2] + 1), nchar(g2))
    d1 <- sub(".expr2", f2, dv1)
    d2 <- sub(".expr1", f1, dv2)
    u1 <- unlist(strsplit(d1, " "))
    u1 <- paste(u1, collapse = "")
    u2 <- unlist(strsplit(d2, " "))
    u2 <- paste(u2, collapse = "")
    u2 <- gsub("(", "", u2, fixed = TRUE)
    u2 <- gsub(")", "", u2, fixed = TRUE)
    char_alpha <- as.character(alpha)
    l <- nchar(char_alpha)
    choose_puiss <- function(u, x) {
        if (substr(u, (str_locate(u, x)[2] + 2), (str_locate(u, 
            x)[2] + 2)) == "-") {
            puiss <- substr(u, (str_locate(u, x)[2] + 2), (str_locate(u, 
                x)[2] + 2 + l))
        }
        else {
            puiss <- substr(u, (str_locate(u, x)[2] + 2), (str_locate(u, 
                x)[2] + 1 + l))
        }
        return(puiss)
    }
    p_x1_1 <- choose_puiss(u1, "x1")
    p_x1_2 <- choose_puiss(u2, "x1")
    p_x2_1 <- choose_puiss(u1, "x2")
    p_x2_2 <- choose_puiss(u2, "x2")
    dif_puis_x1 <- as.numeric(p_x1_1) - as.numeric(p_x1_2)
    dif_puis_x2 <- as.numeric(p_x2_1) - as.numeric(p_x2_2)
    choose_coeff <- function(u, x) {
        coeff <- substr(u, (str_locate(u, x)[1] - l - 1), (str_locate(u, 
            x)[1] - 2))
        return(coeff)
    }
    c1 <- as.numeric(choose_coeff(u1, "x1"))
    c2 <- as.numeric(choose_coeff(u2, "x2"))
    ratio_coeff <- c1/c2
    x1_star <- solve((price[1] + price[2] * (price[1]/price[2]) * 
        (1/ratio_coeff)), income)
    x2_star <- (price[1]/price[2]) * (1/ratio_coeff) * x1_star
    U0 <- x1_star^alpha * x2_star^(1 - alpha)
    x1 <- order(runif(1000, 0, 0.5))
    x2 <- (U0 * x1^(-alpha))^(1/(1 - alpha))
    plot(x1, x2, cex = 0.5, pch = 1, xlim = c(0, round(income/price[1])), 
        xlab = "x1", ylab = "x2")
    abline(a = income/price[2], b = -price[1]/price[2])
    points(x2_star ~ x1_star, pch = 20, col = "red", lwd = 2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
