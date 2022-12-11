

dat <- read.delim("~/Downloads/farallon3_timestampsV6.txt")
dat2 = read.csv("~/Downloads/farallon3_timestamps_wmissing.csv")

dat[,c("ocr_last")] = dat2[match(dat[,"src_image_path"], dat2[,"src_image_path"]), c("ocr_last")]
dat$ocr_last = gsub("\n", "", dat$ocr_last)

t2 = dat[,c("ocr_last")]

t3 = paste0(substr(t2, 1, 4), substr(t2, 6, 7), substr(t2, 9, 10), substr(t2, 16, 17), substr(t2, 19, 20), substr(t2, 22, 23))

dat$t_e_ocr2 = as.POSIXct(t3, format = "%Y%m%d %H%M%S")
dat$final_diff2 = as.numeric(difftime(dat$t_e_ocr2, dat$start_final, units = "sec"))



write.table(dat, "~/Downloads/farallon3_timestampsV7.txt", row.names = FALSE, sep = ";")




dat$time_name = as.POSIXct(substr(dat$src_image_path, 31, 45), format = "%Y%m%d_%H%M%S")
t1 = dat$ocr_first
t2 = dat2$ocr_last



t1 = gsub("-", "", t1)
t1 = gsub(":", "", t1)
t1 = gsub(" ", "", t1)



t2 = gsub("-", "", t2)
t2 = gsub(":", "", t2)
t2 = gsub(" ", "", t2)
t2 = gsub("\n", "", t2)


dat2$t2 = t2
dat2$time_end_ocr = as.POSIXct(paste(substr(t2, 1, 8), substr(t2, 12, 17)), format = "%Y%m%d %H%M%S")
start = as.POSIXct(dat2$start_final)
dat2$diff_start_end = as.numeric(difftime(dat2$time_end_ocr, start, "sec"))


#save
write.table(dat2[,c("src_image_path", "t2", "start_final", "time_end_ocr", "diff_start_end")], "~/Downloads/farallon3_timestampsV5.txt", row.names = FALSE, sep = ";")



table(nchar(dat$src_image_path))



