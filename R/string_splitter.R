string_splitter <- function(x, pattern, where = "last", SIMPLIFY = F) {
    mapply(x = x, y = gregexpr(pattern, x), function(x,y) {
        if (where == "last") {
            return(c(substr(x, 1, y[length(y)]-1),
                     substr(x, y[length(y)]+1, nchar(x))))
        }
        if (where == "first") {
            return(c(substr(x, 1, y[1]-1),
                     substr(x, y[1]+1, nchar(x))))

            return(substr(x, 1, y[1]-1))
        }
    }, SIMPLIFY = SIMPLIFY)
}
# check again - what does it do?
