packages <- c("tidyverse", "magrittr", "ggiraph")
packages[!which(packages %in% installed.packages()$Package)] %>% install.packages
dummy <- sapply(packages, library, character.only = T)


download.file(url = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IND_2_sf.rds", destfile = "gadm36_IND_2_sf.rds")

map_dat <- readRDS("gadm36_IND_2_sf.rds")
#map_dat1 <- readRDS("gadm36_IND_2_sp.rds")

spplot(map_dat)


lengths <- sapply(map_dat$geometry, function(x){length(unlist(x))/2})
pols <- map_dat$geometry %>% lapply(function(x){
    x <- x %>% lapply(function(u){u[[1]]})
    if(length(x) > 1) 
    {
        x <- x %>% reduce(rbind.data.frame) %>% as.list
    }
    else 
    {
        x <- x %>% data.frame %>% as.list
    }
    return(x)
})

pols_x <- lapply(pols, function(x){x[[1]]})
pols_y <- lapply(pols, function(y){y[[2]]})

df1 <- map_dat
df1 <- df1[, which(colnames(df1) != "geometry")]
df$X <- pols_x
df$Y <- pols_y
df$population <- 123
df <- unnest(df)

p <- ggplot(df, aes(x = X, y = Y, fill = NAME_2)) + 
    geom_polygon_interactive(aes(tooltip = paste(NAME_2, "\n", NAME_1, "\n", population), data_id =NAME_2)) + 
    theme(legend.position = "none")
girafe(code = print(p))
