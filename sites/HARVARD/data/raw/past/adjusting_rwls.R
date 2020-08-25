
## FIXING 2 RED OAKS

to_fix = read.tucson('sites/HARVARD/data/raw/rwl/LF_QURU.rwl')

# 143, LF3061, change start date from 63 to 88 (add 25)
which(!(is.na(to_fix[,176])))
temp_save = to_fix[59:93,176]
to_fix[,176] = NA
to_fix[84:118, 176] = temp_save
to_fix[,176]

# 144, LF3072, change start date from 92 to 70 (subtract 22)
which(!(is.na(to_fix[,177])))
temp_save = to_fix[31:100,177]
to_fix[,177] = NA
to_fix[25:94, 177] = temp_save
to_fix[,177]

which(!(is.na(to_fix[,178])))
temp_save = to_fix[31:78,178]
to_fix[,178] = NA
to_fix[25:72, 178] = temp_save
to_fix[,178]

write.tucson(to_fix, 'sites/HARVARD/data/raw/rwl/LF_QURU.rwl', long.names = TRUE)

## FIXING 2 RED MAPLES

to_fix = read.tucson('sites/HARVARD/data/raw/rwl/LF_ACRU.rwl')

# 78, LF2040, add in zeros for recent years... still alive, just add to one column
which(is.na(to_fix[,64]))
to_fix[141:154,64] = 0
to_fix[,64]

# 104, LF3016, change start date from 108 to 115 (add 7)
which(!(is.na(to_fix[,81])))
temp_save = to_fix[71:147,81]
to_fix[,81] = NA
to_fix[79:155, 81] = temp_save
to_fix[,81]

write.tucson(to_fix, 'sites/HARVARD/data/raw/rwl/LF_ACRU.rwl', long.names = TRUE)

## FIXING 1 YELLOW BIRCH

to_fix = to_fix = read.tucson('sites/HARVARD/data/raw/rwl/LF_BEAL.rwl')

# 27, LF1029, change start date from 89 to 92 (add 3)
which(!(is.na(to_fix[,8])))
temp_save = to_fix[7:79,8]
to_fix[,8] = NA
to_fix[10:82, 8] = temp_save
to_fix[,8]

write.tucson(to_fix, 'sites/HARVARD/data/raw/rwl/LF_BEAL.rwl', long.names = TRUE)
