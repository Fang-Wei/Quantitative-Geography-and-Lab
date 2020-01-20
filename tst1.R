d <- data.frame(a = c(1,2,4,8,16,32),
                g = c(2,4,8,16,32,64),
                x = c(3,6,12,24,48,96))
write.table(d, file="tst1.txt", row.names=FALSE)