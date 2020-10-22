library(stringr)
library(DT)



ymd.conv <- function(date.code) {
    if (is.na(date.code)) return(NA)
    if (str_sub(date.code, end = 1) == 3) {
        yr <- as.numeric(str_sub(date.code, start = 2, end = 3)) + 1925
        md <- str_sub(date.code, start = 4, end = 7)
        return(paste0(as.character(yr), md))
        # return(as.POSIXct(paste0(as.character(yr), md), format = '%Y%m%d'))
    }
    else if (str_sub(date.code, end = 1) == 4){
        yr <- as.numeric(str_sub(date.code, start = 2, end = 3)) + 1988
        md <- str_sub(date.code, start = 4, end = 7)
        return(paste0(as.character(yr), md))
        # return(as.POSIXct(paste0(as.character(yr), md), format = '%Y%m%d'))
    }
    else return(NA)
}
vect.ymd.conv <- Vectorize(ymd.conv)


view.table <- function(df, head.n = 100, pagelen = 10, width = '175px') {
    datatable(
        df[1:head.n, ], 
        options = list(
            pageLength = pagelen,
            autoWidth = TRUE,
            scrollX = TRUE,
            columnDefs = list(list(
                width = width,
                targets = seq(1, dim(df)[2]))
                )
            ), 
        class = 'cell-border stripe'
    )
}
