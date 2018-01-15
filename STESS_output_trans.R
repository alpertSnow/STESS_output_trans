###

### settings
num.range <- c('101', '104') # 每次模拟编上不同的号即可一次完成多个模拟结果的转化
i.range1 <- c(155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 188, 191, 194, 197) # 出入口node num
i.range2 <- c(153, 156, 159, 162, 165, 168, 171, 174, 177, 180, 183, 186, 189, 192, 195) # 上游node num
i.range3 <- c(154, 157, 160, 163, 166, 169, 172, 175, 178, 181, 184, 187, 190, 193, 196) # 下游node num
i.lastLine <- c(197) # sigout 最后一行的node number

### body
for (num in num.range){
sigout <- paste0('sigout', num, '.dat')
data <- readLines(sigout)
data.list <- strsplit(data, "\\s+")
ignore <- grep('TYPE', data.list)
data.list <- data.list[-ignore]
data.vec <- unlist(data.list)

V1 <- vector()
V2 <- vector()
V3 <- vector()
V4 <- vector()
V5 <- vector()
V6 <- vector()
# 出入口渗出渗入
for (i in i.range1){
        node.char <- paste0('^', i, '$')
        n <- grep(node.char, data.vec)
        n.plus <- n + 5
        n.minus <- n + 10
        if (any(i == i.lastLine)){
                n.plus <- n + 1 + length(i.lastLine)
                n.minus <- n + 2 * (1 + length(i.lastLine))
        }
        V1 <- rbind(V1, data.vec[n.plus])
        V2 <- rbind(V2, data.vec[n.minus])
}

# 上游隧道渗入渗出
for (i in i.range2){
        node.char <- paste0('^', i, '$')
        n <- grep(node.char, data.vec)
        n.plus <- n + 5
        n.minus <- n + 10
        if (any(i == i.lastLine)){
                n.plus <- n + 1 + length(i.lastLine)
                n.minus <- n + 2 * (1 + length(i.lastLine))
        }
        V3 <- rbind(V3, data.vec[n.plus])
        V4 <- rbind(V4, data.vec[n.minus])
}

# 下游隧道渗入渗出
for (i in i.range3){
        node.char <- paste0('^', i, '$')
        n <- grep(node.char, data.vec)
        n.plus <- n + 5
        n.minus <- n + 10
        if (any(i == i.lastLine)){
                n.plus <- n + 1 + length(i.lastLine)
                n.minus <- n + 2 * (1 + length(i.lastLine))
        }
        V5 <- rbind(V5, data.vec[n.plus])
        V6 <- rbind(V6, data.vec[n.minus])
}
output <- cbind(V1,V2,V3,V4,V5,V6)
output.file <- paste0('output', num, '.csv')
write.csv(output, output.file, row.names = FALSE)
}