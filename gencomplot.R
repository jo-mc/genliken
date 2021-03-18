# redox3

par(mfrow=c(5,1))    # https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html
par(mar=c(1,3,1,1)) # https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
 # access plot panel (not used yet ) https://stackoverflow.com/questions/17410951/change-plot-panel-in-multipanel-plot-in-r

genliken <- t(read.csv(file = 'genliken.csv', header = FALSE,stringsAsFactors=FALSE))
ymax  <- ncol(genliken)
xmax <- max(as.numeric(genliken[1:(nrow(genliken)-1),1:547]))

z <- as.numeric(genliken[6,])  # to work out y axis for subplots

for (ag in 1:5) {
# find V6 > (ag)*50000000 = column of yaxis
    v1yax <- which.max(z > (ag-1)*50000000)  # find spot in vector:  https://stackoverflow.com/questions/29388334/find-position-of-first-value-greater-than-x-in-a-vector
    v2yax <- which.max(z > (ag)*50000000)
    plot(1, type="n", xlab="", ylab="",
       xlim=c(((ag-1)*50000000), (ag*50000000 + 1000000)),
       ylim=c(v1yax,v2yax))
    cat(sprintf("\"%d\" \"%d\"\n", v1yax,v2yax))
    cat(sprintf("\"%d\" \"%d\"\n", (ag-1)*50000000,(ag)*50000000))
  qname <- genliken[12,1]
for (a in 1:ymax) {
  if (genliken[12,a] != qname) {
    y <- c((a-0.5),(a-0.5)); x <- c(1,xmax)
    lines(y ~x , col=rgb(0.9,0.9,0.9,0.6) , lwd=0.5 , pch=19 , type="l" )
  }
  qname <- genliken[12,a]
  y <- c((a),(a));  i=2  # front soft clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
  i=4  # front hard clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
  i = 6 # matching sequence
  x <- as.numeric(genliken[i:(i+1),a])
  lines(y ~x , col=rgb(0.0,0.0,0.9,0.9) , lwd=1 , pch=13 , type="l" )
  i=8  # rear soft clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
  i=10  # rear hard clip
  x <- as.numeric(genliken[i:(i+1),a])
  lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
}
}



categories <- c("Soft Clip", "Hard Clip", "Match")
colors <- c("red", "green", "blue")
legend("bottomright", col=colors, categories, bg="white", lwd=1, 
       cex = 0.75, y.intersp=.5, bty = "n")





# redo redo  +> clips offset

par(mfrow=c(1,1)) 
genliken <- t(read.csv(file = 'genliken.csv', header = FALSE,stringsAsFactors=FALSE))
ymax  <- ncol(genliken) + 50
# ymax <- 34
xmax <- max(as.numeric(genliken[1:(nrow(genliken)-1),1:547]))
plot(1, type="n", xlab="", ylab="", xlim=c(0, xmax), ylim=c(0, ymax))

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
}

categories <- c("Soft Clip", "Hard Clip", "Match")
colors <- c("red", "green", "blue")
legend("bottomright", col=colors, categories, bg="white", lwd=1, 
       cex = 0.75, y.intersp=.5, bty = "n")


{
  if (TRUE) {stop("script must end here")}
  
  print("Script did NOT end!")
}

# ======================================================== develop =========




region=c(1,1,1.1,1.1)
softclip=c(0,0,568801,1228829)
hardclip=c(209946,868680,0,0)
align=c(234059,409043,935719,1228822)
ymax = trunc(max(region) + 1)
xmax = max(softclip,hardclip,align)

plot(1, type="n", xlab="", ylab="", xlim=c(0, xmax), ylim=c(0, ymax))

# for (i in 1:length(region)) {
  i = 1 
  if ( ( i %% 2 ) == 1 ) {
    print (i)
  x <- softclip[i:(i+1)]
  y <- region[i:(i+1)]
  cat("x :",x);   cat("y :",y);   print ("")
  lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="b" )
  x <- hardclip[i:(i+1)]
  lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="b" )
  x <- align[i:(i+1)]
  lines(y ~x , col=rgb(0.0,0.0,0.9,0.9) , lwd=1 , pch=13 , type="b" )
  }


# TEST
a=c(1:5)
b=c(5,3,4,5,5)
c=c(4,5,4,3,1)

# Make a basic graph
plot( b~a , type="b" , bty="o" , xlab="value of a" , ylab="value of b" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17 , ylim=c(1,5) )

lines(c ~a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
# end TEST


# ----------------------------------------new format
n <- c(1, 0, 0, 209946, 234059, 234059, 405398, 0, 0, 405399, 865035)
ymax = trunc(n[1] + 1)
xmax = max(n)
plot(1, type="n", xlab="", ylab="", xlim=c(0, xmax), ylim=c(0, ymax))

# for (i in 1:length(region)) {

y <- c(n[1],n[1])
i=2  # front soft clip
x <- n[i:(i+1)]
lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
i=4  # front hard clip
x <- n[i:(i+1)]
lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
i = 6 # matching sequence
x <- n[i:(i+1)]
lines(y ~x , col=rgb(0.0,0.0,0.9,0.9) , lwd=1 , pch=13 , type="l" )
i=8  # rear soft clip
x <- n[i:(i+1)]
lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
i=10  # rear hard clip
x <- n[i:(i+1)]
lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )





  genliken <- t(read.csv(file = 'genliken.csv', header = FALSE,stringsAsFactors=FALSE))
  ymax <- 5
  xmax <- max(genliken)
  plot(1, type="n", xlab="", ylab="", xlim=c(0, xmax), ylim=c(0, ymax))
  
  # for (i in 1:length(region)) {
  
  y <- c(1,1)
  for (a in 1:4) {
    y <- c((1+a/4),(1+a/4))
  i=2  # front soft clip
  x <- genliken[i:(i+1),a]
  lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
  i=4  # front hard clip
  x <- genliken[i:(i+1),a]
  lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
  i = 6 # matching sequence
  x <- genliken[i:(i+1),a]
  lines(y ~x , col=rgb(0.0,0.0,0.9,0.9) , lwd=1 , pch=13 , type="l" )
  i=8  # rear soft clip
  x <- genliken[i:(i+1),a]
  lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
  i=10  # rear hard clip
  x <- genliken[i:(i+1),a]
  lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
  }
  
  #REDO  no transpose to keep data as integer
  
  genliken <- (read.csv(file = 'genliken.csv', header = FALSE,stringsAsFactors=FALSE))
  ymax <- nrow(genliken)
  xmax <- max(genliken[1:11])
  plot(1, type="n", xlab="", ylab="", xlim=c(0, xmax), ylim=c(0, ymax))
  
  # for (i in 1:length(region)) {
  
  y <- c(1,1)
  for (readR in 1:ymax) {
    y <- c((1+readR/4),(1+readR/4))
    i=2  # front soft clip
    x <- genliken[readR,i:(i+1)]
    lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
    i=4  # front hard clip
    x <- genliken[readR,i:(i+1)]
    lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
    i = 6 # matching sequence
    x <- genliken[readR,i:(i+1)]
    lines(y ~x , col=rgb(0.0,0.0,0.9,0.9) , lwd=1 , pch=13 , type="l" )
    i=8  # rear soft clip
    x <- genliken[readR,i:(i+1)]
    lines(y ~x , col=rgb(0.9,0.0,0.0,0.9) , lwd=1 , pch=19 , type="l" )
    i=10  # rear hard clip
    x <- genliken[readR,i:(i+1)]
    lines(y ~x , col=rgb(0.0,0.9,0.0,0.9) , lwd=1 , pch=15 , type="l" )
  }
  
  
  # some ideas from
  
  # https://stackoverflow.com/questions/12640122/r-plotting-rows-of-a-list
  # spacing https://stackoverflow.com/questions/38332355/vertical-spaces-in-legend