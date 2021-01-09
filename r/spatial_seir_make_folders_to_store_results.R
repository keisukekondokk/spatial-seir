
#Results CSV
dirname = "output"
if(!dir.exists(dirname)){
  dir.create(dirname)
}
dirname = "output/csv"
if(!dir.exists(dirname)){
  dir.create(dirname)
}
for(i in 1:7){
  #Results CSV
  dirname <- paste0("output/csv/case", sprintf("%02d", i))
  if(!dir.exists(dirname)){
    dir.create(dirname)
  }
}

#Results Figure
dirname = "output/fig"
if(!dir.exists(dirname)){
  dir.create(dirname)
}
for(i in 1:7){
  dirname <- paste0("output/fig/case", sprintf("%02d", i))
  if(!dir.exists(dirname)){
    dir.create(dirname)
  }
}
dirname = "output/fig/paper"
if(!dir.exists(dirname)){
  dir.create(dirname)
}
