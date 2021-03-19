indel300Count <- 0
indelVec <- NULL 
fileName <- "genliken1-1.csv"
gln <- file(fileName,"r")
idSize <- max(count.fields(gln,","))
genliken <- t(read.csv(file = fileName, header = FALSE,stringsAsFactors=FALSE,
                       col.names = paste0("V",seq_len(idSize)), fill = TRUE))
# to fill indel's (After row 12) with entries if they do not exist
close.connection(gln)
ymax  <- ncol(genliken) 
# ymax <- 34
clipmatchRows <- 11
# xmax <- max(as.numeric(genliken[1:(nrow(genliken)-1),1:ymax]))
xmax <- max(as.numeric(genliken[1:11,1:ymax])) # update as added indel info, clip and match data is in rows 1 to 11, read qname in row 12]

 plot(1, type="n", xlab="", ylab="", xlim=c(0, xmax), ylim=c(0, (ymax+50)) )

# for (i in 1:length(region)) {
qname <- genliken[12,1]
y <- c(1,1)
for (a in 1:ymax) {
  
  if (genliken[12,a] != qname) {
    y <- c((a-0.5),(a-0.5))
    x <- c(1,xmax)
    lines(y ~x , col=rgb(0.9,0.9,0.9,0.6) , lwd=0.5 , pch=19 , type="l" )
  }
  qname <- genliken[12,a]
  
  y <- c((a),(a))
  yclip  <- c((a+50),(a+50))  # +> clips offset
  i=2  # front soft clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(yclip ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
  i=4  # front hard clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(yclip ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
  i = 6 # matching sequence
  x <- as.numeric(genliken[i:(i+1),a])
  lines(y ~x , col=rgb(0.0,0.0,0.9,0.9) , lwd=1 , pch=13 , type="l" )
  i=8  # rear soft clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(yclip ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
  i=10  # rear hard clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(yclip ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
  
  # indel
  genlik <- nrow(genliken)  # for column a
  for (b in 13:genlik) {  # indel data starts at row 13
    if (!(is.na(genliken[b,a]))) {
      p <- as.numeric(genliken[b,a])
      if((b %% 2) != 0) {  # odd have position, odd have indel (+/-) length
        indelPos <- p
        indelSize <- as.numeric(genliken[b+1,a])
        if ( (abs(indelSize) > 90) && (abs(indelSize) <370) ) {
          indelScaled <- indelSize * 0.1 * ymax/300  #  a 300 indel = 1/10 of yscale
          idx <- c(indelPos,indelPos)
          #idy <- c(a,a+indelScaled)
          idy <- c(200,200+indelScaled)
          lines(idy ~idx , col=rgb(0.0,0.9,0.9,0.9) , lwd=1 , pch=15 , type="l" )
          indel300Count <- indel300Count + 1
          indelVec <- append(indelVec,indelPos)
        }      
      }   # lines(yclip ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
    }
  }
}
print (indel300Count)