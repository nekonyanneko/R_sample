library(nnet)

plot.nn <- function(
  nn,
  xmin.max = c(-1, 1),
  ymin.max = c(-1, 1),
  dx = xmin.max[2] - xmin.max[1],
  dy = ymin.max[2] - ymin.max[1],
  y.padding = dy * 0.05,
  lwd.w = function(w) w * 3,
  col.w = function(w) ifelse(w > 0, "#ff400040", "#0000ff40"),
  w.min = 0.01,
  pch.i = c(22, 21, 21),
  col.i = c("#000000", "#000000", "#000000"),
  bg.i = c("#ffffff", "#ffffff", "#ffffff"),
  cex.i = function(n) { cex <- 10 / sqrt(n); min(cex, 5) },
  cex.text = 0.7
) {
  v.n <- nn$n
  list.x <- lapply(
    v.n, function(n)
      seq(xmin.max[1], xmin.max[2], length = n + 2)[2:(n + 1)]
  )
  v.y <- seq(ymin.max[1] + y.padding, ymin.max[2] - y.padding, length = 3)
  wts <- coef(nn)
  wts.normalized <- wts / sd(wts[grep("^[^b]", names(wts))])
  get.tag <- function(i, ii, ij) ifelse(
    i == 1,
    sprintf("i%i->h%i", ii, ij),
    ifelse(
      v.n[3] > 1,
      sprintf("h%i->o%i", ii, ij),
      sprintf("h%i->o", ii)
    )
  )
  get.tag.b <- function(i, ii) ifelse(
    i == 2,
    sprintf("b->h%i", ii),
    ifelse(
      v.n[3] > 1,
      sprintf("b->o%i", ii),
      sprintf("b->o", ii)
    )
  )
  # plot
  par(mar = c(0, 0, 0, 0), mgp = c(0, 0, 0))
  plot(
    numeric(0), numeric(0),
    type = "n",
    xlim = xmin.max,
    ylim = ymin.max,
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  # lines
  for (i in 1:2) {
    j <- i + 1
    v.xi <- list.x[[i]]
    v.xj <- list.x[[j]]
    for (ii in 1:v.n[i]) {
      for (ij in 1:v.n[j]) {
        tag <- get.tag(i, ii, ij)
        w <- wts.normalized[tag]
        #cat(tag, "\n")
        if (abs(w) > w.min) lines(
          x = c(v.xi[ii], v.xj[ij]),
          y = c(v.y[i], v.y[j]),
          lwd = lwd.w(w),
          col = col.w(w)
        )
      }
    }
  }
  # points
  for (i in 1:3) points(
    x = list.x[[i]],
    y = rep(v.y[i], v.n[i]),
    pch = pch.i[i],
    bg = bg.i[i],
    cex = cex.i(v.n[i])
  )
  # input layer
  text(
    x = list.x[[1]],
    y = v.y[1],
    labels = nn$coefnames,
    cex = cex.text
  )
  # hidden and output layers
  for (i in 2:3) {
    v.xi <- list.x[[i]]
    for (ii in 1:v.n[i]) {
      tag <- get.tag.b(i, ii)
      text(
        v.xi[ii], v.y[i],
        sprintf("%.2f", wts[tag]),
        cex = cex.text
      )
    }
  }
}