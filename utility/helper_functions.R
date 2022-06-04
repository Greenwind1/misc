library(stringr)
library(DT)

# create empty dataframe ----
create.empty.df <- function(nrow, ncol, colnames = c()) {
    if(missing(ncol) && length(colnames) > 0) {
        ncol <- length(colnames)
    }
    data.frame(
        matrix(vector(), nrow, ncol, dimnames = list(c(), colnames))
    )
}

# ymd converter ----
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


# table viewer ----
# Reference: https://rstudio.github.io/DT/
view.table <- function(df, rownames = TRUE,
                       head.n = 100, pagelen = 10, width = '175px',
                       caption = NULL, is.filter = TRUE) {
  head.n <- ifelse(nrow(df) <= head.n, nrow(df), head.n)
  pagelen <- ifelse(nrow(df) <= pagelen, nrow(df), pagelen)
  filter <- ifelse(is.filter, "top", "none")
  
  datatable(
    df[1:head.n, ], 
    rownames = rownames,
    options = list(
      pageLength = pagelen,
      autoWidth = TRUE,
      scrollX = TRUE,
      columnDefs = list(list(
        width = width,
        targets = seq(1, dim(df)[2]))
      ),
      dom = 'C<"clear">lfrtip', 
      colReorder = list(realtime = TRUE)
    ), 
    class = 'cell-border stripe',
    caption = caption,
    filter = filter,
    extensions = c("KeyTable", "ColReorder")
  )
}
