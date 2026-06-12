setwd("d:/USERS/marsti/Documents/GitHub/uwb")

load("data/data.Rda")
obj_name <- ls()[1]
obj <- get(obj_name)
cat("=== data.Rda ===\n")
cat("name:", obj_name, "\n")
cat("class:", paste(class(obj), collapse=", "), "\n")
cat("nrow:", nrow(obj), "\n")
cat("ncol:", ncol(obj), "\n")
cat("names:", paste(names(obj), collapse=", "), "\n")
for (nm in names(obj)) {
  cat(nm, ":", class(obj[[nm]]), "\n")
}

cat("\n=== codebook.Rda ===\n")
load("data/codebook.Rda")
all_objs <- ls()
cb_name <- all_objs[all_objs != obj_name][1]
cb <- get(cb_name)
cat("name:", cb_name, "\n")
cat("class:", paste(class(cb), collapse=", "), "\n")
cat("nrow:", nrow(cb), "\n")
cat("names:", paste(names(cb), collapse=", "), "\n")