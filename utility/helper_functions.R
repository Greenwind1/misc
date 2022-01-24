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


# Reference: https://rstudio.github.io/DT/
view.table <- function(df, head.n = 100, pagelen = 10, width = '175px',
                       caption = NULL, is.filter = TRUE) {
  if (nrow(df) < head.n) {
    head.n <- nrow(df)
  } 
  if (is.filter) {
    filter = "top"
  }
  else{
    filter = "none"
  }
  datatable(
    df[1:head.n, ], 
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
