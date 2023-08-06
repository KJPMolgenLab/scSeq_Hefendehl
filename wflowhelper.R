library(workflowr)


workflowr::wflow_build()
workflowr::wflow_publish(c("./docs/*", "./analysis/*", "./code/*", "./output/*"))
