
library(workflowr)


wflow_git_config(user.name = "achiocch", user.email = "andreas.chiocchetti@web.de", overwrite=T)
wflow_start(directory = "/files/scSeq_Hefendehl/", existing = T)

workflowr::wflow_build("./analysis/sc*.Rmd", delete_cache = TRUE)
workflowr::wflow_publish(c("./docs/*", "./analysis/*.Rmd", "./code/*", "./output/Res_202602/*"), delete_cache = TRUE, message = "Refactoring")
system("git push origin master")

workflowr::wflow_publish(c("./analysis/index.Rmd")) 
