get_albedo <- function(template_rast, tme, ALB_DIR, mycredentials){
albedo_download( r = template_rast, tme = tme, pathout = ALB_DIR, 
                 credentials = mycredentials)
alb <- albedo_process(r = template_rast, pathin = ALB_DIR)
return(alb)}