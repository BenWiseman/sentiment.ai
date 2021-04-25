if(FALSE){
require(data.table)

default <- fread("default_large.csv")
default_small <- fread("default_short.csv")

save(default, file = "data/default.RData")
save(default_small, file = "data/default_small.RData")
}
