packages <- c("tidyverse", "magrittr", "ggiraph")
packages[!which(packages %in% installed.packages()$Package)] %>% install.packages
dummy <- sapply(packages, library, character.only = T)

# download.file is not working.. files are broken it seems
#download.file(url = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IND_2_sf.rds", destfile = "gadm36_IND_2_sf.rds")
#download.file(url = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_IND_1_sf.rds", destfile = "gadm36_IND_1_sf.rds")


state_dat <- readRDS("gadm36_IND_1_sf.rds")
dist_dat <- readRDS("gadm36_IND_2_sf.rds")

cleaner_for_ggplot <- function(dat){
    lengths <- sapply(dat$geometry, function(x){length(unlist(x))/2})
    pols <- dat$geometry %>% lapply(function(x){
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
    
    df <- dat
    df <- df[, which(colnames(df) != "geometry")]
    df$X <- pols_x
    df$Y <- pols_y
    df$population <- 123
    df <- unnest(df)
    return(df)
    
}

state_df <- cleaner_for_ggplot(state_dat)
dist_df <- cleaner_for_ggplot(dist_dat)

p <- ggplot() + 
    geom_polygon_interactive(data = dist_df, mapping = aes(x = X, y = Y, fill = NAME_2, 
                                                           tooltip = paste(NAME_2, "\n", NAME_1, "\n", population), data_id =NAME_2)) + 
    geom_polygon(data = state_df, mapping = aes(x = X, y = Y, color = NAME_1), fill = NA) +
    scale_color_manual(values = rep("black", length(unique(state_df$NAME_1)))) +
    theme(legend.position = "none")
girafe(code = print(p))
