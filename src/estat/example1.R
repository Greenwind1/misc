# ---- Library ----
# https://www.e-stat.go.jp/api/api-info/api-guide
# https://github.com/yutannihilation/estatapi
library("estatapi")


# ---- Env ----
appId <- "xxxxx"

# ---- getStatsList ----
api.list <- estat_getStatsList(appId = appId, searchWord = "年収")
View(api.list)

# ---- estat_getDataCatalog ----
catalog.list <-
    estat_getDataCatalog(appId = appId,
                         searchWord = '年収',
                         dataType = c('XLS'))
View(catalog.list)


# --- Specific work flow ----
# as default, save spreed sheets in   "./input/"   folder. ----
iteration <- 1
# iteration <- nrow(catalog.list)
for(i in 1:iteration){
    file.name <- str_c('./input/ID', catalog.list[i, 1],
                       '_', catalog.list[i, ]$DATASET_NAME, '.xls')
    download.file(url = catalog.list[i, ]$URL,
                  destfile = file.name,
                  method = 'curl')
    cat(catalog.list[i, ]$DATASET_NAME, 'saved...')
}

