LLC_r1 <- function(x, l, a) sum(pmax(0, pmin(x - a, l)))
LLC_r2 <- function(x, l, a) sum(pmax.int(0, pmin.int(x - a, l)))

LLC_fd <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_fd, x, l, a)
}
LLC_fe <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_fe, x, l, a)
}
LLC_f <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_f, x, l, a)
}
LLC_fs <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_fs, x, l, a)
}
LLC_fl <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_fl, x, l, a)
}
LLC_fls <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_fls, x, l, a)
}

LLC_c <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_c, x, l, a)
}
LLC_cs <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_cs, x, l, a)
}
LLC_cl <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_cl, x, l, a)
}
LLC_cls <- function(x, l, a) {
  if (!is.double(x)) {storage.mode(x) <- 'double'}
  if (!is.double(l)) {storage.mode(l) <- 'double'}
  if (!is.double(a)) {storage.mode(a) <- 'double'}
  .Call(c_llc_cls, x, l, a)
}