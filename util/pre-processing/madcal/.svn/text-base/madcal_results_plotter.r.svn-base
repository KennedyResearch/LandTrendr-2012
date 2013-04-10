#-----inputs--------------------------------------------------------------------
width = 10  # a pdf with trejectories is created, what should the pdf width be (inches) ?
height = 7  # a pdf with trejectories is created, what should the pdf height be (inches) ?
columns = 2  # a pdf with trejectories is created, how many columns of figures do you want per page - note that plot text can be lost with too many
rows = 3 # a pdf with trejectories is created, how many rows of figures do you want per page
outdir = "T:/Groups/Spacers-Annex/Scenes/2628/madcal/"
src_file = "T:/Groups/Spacers-Annex/Scenes/2628/madcal/026028madcal_results.csv"
#########################################################################################

vert = read.csv(src_file, header=T, sep=",")
attach(vert)

out_file = paste(outdir, "madcal_results_summary", ".pdf", sep="")
pdf(file=out_file, width=width, heigh=height) #size of the pdf page
par(mfrow=c(columns,rows)) #number of trajectories to place on a page (columns, rows)  

goods = which(B1_SLOPE != 0)

b1_slope = B1_SLOPE[goods]
b2_slope = B2_SLOPE[goods]
b3_slope = B3_SLOPE[goods]
b4_slope = B4_SLOPE[goods]
b5_slope = B5_SLOPE[goods]
b6_slope = B6_SLOPE[goods]

b1_corr = B1_CORR[goods]
b2_corr = B2_CORR[goods]
b3_corr = B3_CORR[goods]
b4_corr = B4_CORR[goods]
b5_corr = B5_CORR[goods]
b6_corr = B6_CORR[goods]

b1_int = B1_INT[goods]
b2_int = B2_INT[goods]
b3_int = B3_INT[goods]
b4_int = B4_INT[goods]
b5_int = B5_INT[goods]
b6_int = B6_INT[goods]

x_vals = YEAR[goods]
refline = which(B1_SLOPE == 0.0)
ref_year =YEAR[refline]
ref_doy = DOY[refline]

#b1_slope
reg1 = lm(b1_slope~x_vals)
plot(x_vals, b1_slope, xlab="year", ylab="slope", type="n", main="b1_slope", ylim=c(0,3))
points(x_vals, b1_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b2_slope
reg1 = lm(b2_slope~x_vals) 
plot(x_vals, b2_slope, xlab="year", ylab="slope", type="n", main="b2_slope", ylim=c(0,3))
points(x_vals, b2_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b3_slope
reg1 = lm(b3_slope~x_vals)
plot(x_vals, b3_slope, xlab="year", ylab="slope", type="n", main="b3_slope", ylim=c(0,3))
points(x_vals, b3_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b4_slope
reg1 = lm(b4_slope~x_vals) 
plot(x_vals, b4_slope, xlab="year", ylab="slope", type="n", main="b4_slope", ylim=c(0,3))
points(x_vals, b4_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b5_slope
reg1 = lm(b5_slope~x_vals) 
plot(x_vals, b5_slope, xlab="year", ylab="slope", type="n", main="b5_slope", ylim=c(0,3))
points(x_vals, b5_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b6_slope
reg1 = lm(b6_slope~x_vals) 
plot(x_vals, b6_slope, xlab="year", ylab="slope", type="n", main="b6_slope", ylim=c(0,3))
points(x_vals, b6_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)

#b1_corr
reg1 = lm(b1_corr~x_vals)
plot(x_vals, b1_corr, xlab="year", ylab="corr", type="n", main="b1_corr", ylim=c(0.5,1.0))
points(x_vals, b1_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b2_corr
reg1 = lm(b2_corr~x_vals) 
plot(x_vals, b2_corr, xlab="year", ylab="corr", type="n", main="b2_corr", ylim=c(0.5,1.0))
points(x_vals, b2_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b3_corr
reg1 = lm(b3_corr~x_vals)
plot(x_vals, b3_corr, xlab="year", ylab="corr", type="n", main="b3_corr", ylim=c(0.5,1.0))
points(x_vals, b3_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b4_corr
reg1 = lm(b4_corr~x_vals) 
plot(x_vals, b4_corr, xlab="year", ylab="corr", type="n", main="b4_corr", ylim=c(0.5,1.0))
points(x_vals, b4_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b5_corr
reg1 = lm(b5_corr~x_vals) 
plot(x_vals, b5_corr, xlab="year", ylab="corr", type="n", main="b5_corr", ylim=c(0.5,1.0))
points(x_vals, b5_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b6_corr
reg1 = lm(b6_corr~x_vals) 
plot(x_vals, b6_corr, xlab="year", ylab="corr", type="n", main="b6_corr", ylim=c(0.5,1.0))
points(x_vals, b6_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)

#b1_int
reg1 = lm(b1_int~x_vals)
plot(x_vals, b1_int, xlab="year", ylab="int", type="n", main="b1_int", ylim=c(-50,10))
points(x_vals, b1_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b2_int
reg1 = lm(b2_int~x_vals) 
plot(x_vals, b2_int, xlab="year", ylab="int", type="n", main="b2_int", ylim=c(-50,10))
points(x_vals, b2_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b3_int
reg1 = lm(b3_int~x_vals)
plot(x_vals, b3_int, xlab="year", ylab="int", type="n", main="b3_int", ylim=c(-50,10))
points(x_vals, b3_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b4_int
reg1 = lm(b4_int~x_vals) 
plot(x_vals, b4_int, xlab="year", ylab="int", type="n", main="b4_int", ylim=c(-50,10))
points(x_vals, b4_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b5_int
reg1 = lm(b5_int~x_vals) 
plot(x_vals, b5_int, xlab="year", ylab="int", type="n", main="b5_int", ylim=c(-50,10))
points(x_vals, b5_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b6_int
reg1 = lm(b6_int~x_vals) 
plot(x_vals, b6_int, xlab="year", ylab="int", type="n", main="b6_int", ylim=c(-50,10))
points(x_vals, b6_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_year, untf = FALSE, col=rgb(1,0,0), lwd=3)

x_vals = DOY[goods]

#b1_slope
reg1 = lm(b1_slope~x_vals)
plot(x_vals, b1_slope, xlab="day of year", ylab="slope", type="n", main="b1_slope", ylim=c(0,3))
points(x_vals, b1_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b2_slope
reg1 = lm(b2_slope~x_vals) 
plot(x_vals, b2_slope, xlab="day of year", ylab="slope", type="n", main="b2_slope", ylim=c(0,3))
points(x_vals, b2_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b3_slope
reg1 = lm(b3_slope~x_vals)
plot(x_vals, b3_slope, xlab="day of year", ylab="slope", type="n", main="b3_slope", ylim=c(0,3))
points(x_vals, b3_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b4_slope
reg1 = lm(b4_slope~x_vals) 
plot(x_vals, b4_slope, xlab="day of year", ylab="slope", type="n", main="b4_slope", ylim=c(0,3))
points(x_vals, b4_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b5_slope
reg1 = lm(b5_slope~x_vals) 
plot(x_vals, b5_slope, xlab="day of year", ylab="slope", type="n", main="b5_slope", ylim=c(0,3))
points(x_vals, b5_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b6_slope
reg1 = lm(b6_slope~x_vals) 
plot(x_vals, b6_slope, xlab="day of year", ylab="slope", type="n", main="b6_slope", ylim=c(0,3))
points(x_vals, b6_slope, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)

#b1_corr
reg1 = lm(b1_corr~x_vals)
plot(x_vals, b1_corr, xlab="day of year", ylab="corr", type="n", main="b1_corr", ylim=c(0.5,1.0))
points(x_vals, b1_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b2_corr
reg1 = lm(b2_corr~x_vals) 
plot(x_vals, b2_corr, xlab="day of year", ylab="corr", type="n", main="b2_corr", ylim=c(0.5,1.0))
points(x_vals, b2_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b3_corr
reg1 = lm(b3_corr~x_vals)
plot(x_vals, b3_corr, xlab="day of year", ylab="corr", type="n", main="b3_corr", ylim=c(0.5,1.0))
points(x_vals, b3_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b4_corr
reg1 = lm(b4_corr~x_vals) 
plot(x_vals, b4_corr, xlab="day of year", ylab="corr", type="n", main="b4_corr", ylim=c(0.5,1.0))
points(x_vals, b4_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b5_corr
reg1 = lm(b5_corr~x_vals) 
plot(x_vals, b5_corr, xlab="day of year", ylab="corr", type="n", main="b5_corr", ylim=c(0.5,1.0))
points(x_vals, b5_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b6_corr
reg1 = lm(b6_corr~x_vals) 
plot(x_vals, b6_corr, xlab="day of year", ylab="corr", type="n", main="b6_corr", ylim=c(0.5,1.0))
points(x_vals, b6_corr, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)

#b1_int
reg1 = lm(b1_int~x_vals)
plot(x_vals, b1_int, xlab="day of year", ylab="int", type="n", main="b1_int", ylim=c(-50,10))
points(x_vals, b1_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b2_int
reg1 = lm(b2_int~x_vals) 
plot(x_vals, b2_int, xlab="day of year", ylab="int", type="n", main="b2_int", ylim=c(-50,10))
points(x_vals, b2_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b3_int
reg1 = lm(b3_int~x_vals)
plot(x_vals, b3_int, xlab="day of year", ylab="int", type="n", main="b3_int", ylim=c(-50,10))
points(x_vals, b3_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b4_int
reg1 = lm(b4_int~x_vals) 
plot(x_vals, b4_int, xlab="day of year", ylab="int", type="n", main="b4_int", ylim=c(-50,10))
points(x_vals, b4_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b5_int
reg1 = lm(b5_int~x_vals) 
plot(x_vals, b5_int, xlab="day of year", ylab="int", type="n", main="b5_int", ylim=c(-50,10))
points(x_vals, b5_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)
#b6_int
reg1 = lm(b6_int~x_vals) 
plot(x_vals, b6_int, xlab="day of year", ylab="int", type="n", main="b6_int", ylim=c(-50,10))
points(x_vals, b6_int, col=4, cex=2 )
abline(reg1)
abline(v=ref_doy, untf = FALSE, col=rgb(1,0,0), lwd=3)

detach(vert)
dev.off()

#DONE