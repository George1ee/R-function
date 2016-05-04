###Slove An Initial Value ODE
##function defination
vdpol <- function (t, y, mu) {
   list(c(
      y[2],
      mu * (1 - y[1]^2) * y[2] - y[1]
   ))
}
##initial condition defination
library(deSolve)
yini <- c(y1 = 2, y2 = 0)
##刚性问题
stiff <- ode(y = yini, func = vdpol,
             times = 0:3000, parms = 1000)
##非刚性问题
nonstiff <- ode(y = yini, func = vdpol,
                times = seq(0, 30, by = 0.01),
                parms = 1)
##view the solution
head(stiff, n = 3)
stiff
###plot
plot(stiff, type = "l", which = "y1",
     lwd = 2, ylab = "y",
     main = "IVP ODE, stiff")

plot(nonstiff, type = "l", which = "y1",
     lwd = 2, ylab = "y",
     main = "IVP ODE, nonstiff")

###Slove A Boundary Value ODE
##function defination
Prob14 <- function(x, y, xi){
   list(c(
      y[2],
      1/xi * (y[1] - (xi*pi*pi+1) * cos(pi*x))
   ))
}
#We use three values of x, and solve the problem with the shooting
#x = 0.0025
library(bvpSolve)
x <- seq(-1, 1, by = 0.01)
shoot <- bvpshoot(yini = c(0, NA),
                  yend = c(0, NA), x = x, parms = 0.01,
                  func = Prob14)
twp <- bvptwp(yini = c(0, NA), yend = c(0,
                                        NA), x = x, parms = 0.0025,
              func = Prob14)
coll <- bvpcol(yini = c(0, NA),
               yend = c(0, NA), x = x, parms = 1e-04,
               func = Prob14)
xi <- 0.0025
analytic <- cos(pi * x) + exp((x-1)/sqrt(xi)) + exp(-(x + 1)/sqrt(xi))
max(abs(analytic - twp[, 2]))
#Solution of the BVP ODE problem, for dif- ferent values of parameter x
plot(shoot[, 1], shoot[, 2], type = "l", lwd = 2,
     ylim = c(-1, 1), col = "blue",
     xlab = "x", ylab = "y", main = "BVP ODE")
lines(twp[, 1], twp[, 2], col = "red", lwd = 2)
lines(coll[, 1], coll[, 2], col = "green", lwd = 2)
legend("topright", legend = c("0.01", "0.0025",
                              "0.0001"), col = c("blue", "red", "green"),
       title = expression(xi), lwd = 2)

###Slove An Differential Algebraic Equations
daefun<-function(t, y, dy, parms) {
   res1 <- - dy[1] - 0.04 * y[1] +
      1e4 * y[2] * y[3]
   res2 <- - dy[2] + 0.04 * y[1] -
      1e4 * y[2] * y[3] - 3e7 * y[2]^2
   res3 <- y[1] + y[2] + y[3] - 1
   list(c(res1, res2, res3),
        error = as.vector(y[1] + y[2] + y[3]) - 1)
}
yini <- c(y1=1,y2=0,y3=0);dyini <- c(-0.04, 0.04, 0)
times <- 10 ^ seq(-6,6,0.1)
library(deSolve)
print(system.time(out <-daspk(y = yini,
                              dy = dyini, times = times, res = daefun,
                              parms = NULL)))
plot(out, ylab = "conc.", xlab = "time",
     type = "l", lwd = 2, log = "x")
mtext("IVP DAE", side = 3, outer = TRUE,
      line = -1)

###Slove A Partial Differential Equations
library(ReacTran)
Grid <- setup.grid.1D(N = 1000, L = 10)
pde1D <-function(t, C, parms)  {
   tran <- tran.1D(C = C, D = D,
                   C.down = Cext, dx = Grid)$dC
   list(tran - Q)  # return value: rate of change
}
D<-1;Q<-1;Cext<-20
library(rootSolve)
print(system.time(
   std   <- steady.1D(y = runif(Grid$N),
                      func = pde1D, parms = NULL, nspec = 1)
))
plot (Grid$x.mid, std$y, type = "l",
      lwd = 2, main = "steady-state PDE",
      xlab = "x", ylab = "C", col = "red")
analytical <- Q/2/D*(Grid$x.mid^2 - 10^2) + Cext
max(abs(analytical - std$y))
require(deSolve)
times <- seq(0, 100, by = 1)
system.time(
   out <- ode.1D(y = rep(1, Grid$N),
                         times = times, func = pde1D,
                         parms = NULL, nspec = 1)
)
image(out, xlab = "time, days",
      ylab = "Distance, cm",
      main = "PDE", add.contour = TRUE)
