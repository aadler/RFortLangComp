set.seed(741)

library(CppLangComp)      # C++ functions
library(microbenchmark)   # Timing
library(ggplot2)          # Graphing
library(scales)           # Axes
library(ggridges)         # Ridgeplots may be good alternative to boxplots
library(data.table)       # Manipulation

ColPal8 <- c("#003cad",
             "#00d06d",
             "#300bb8",
             "#738200",
             "#5b0068",
             "#87d2df",
             "#ff7262",
             "#ff62aa")

EXP <- data.table(expr = c('LLC_r1(X, 2e+06, 1e+06)',
                           'LLC_r2(X, 2e+06, 1e+06)',
                           'LLC_cpp(X, 2e+06, 1e+06)',
                           'LLC_cpps(X, 2e+06, 1e+06)',
                           'LLC_cppl(X, 2e+06, 1e+06)',
                           'LLC_cppls(X, 2e+06, 1e+06)',
                           'LLC_fd(X, 2e+06, 1e+06)',
                           'LLC_fe(X, 2e+06, 1e+06)',
                           'LLC_f(X, 2e+06, 1e+06)',
                           'LLC_fs(X, 2e+06, 1e+06)',
                           'LLC_fl(X, 2e+06, 1e+06)',
                           'LLC_fls(X, 2e+06, 1e+06)',
                           'LLC_c(X, 2e+06, 1e+06)',
                           'LLC_cs(X, 2e+06, 1e+06)',
                           'LLC_cl(X, 2e+06, 1e+06)',
                           'LLC_cls(X, 2e+06, 1e+06)'),
                  Expression = c("Base R pmin",
                                 "Base R pmin.int",
                                 "Rcpp Serial Loop",
                                 "Rcpp SIMD Loop",
                                 "Rcpp Parallel Loop",
                                 "Rcpp Parallel SIMD Loop",
                                 "Fortran Direct Sum",
                                 "Fortran Elemental Func",
                                 "Fortran Serial Loop",
                                 "Fortran SIMD Loop",
                                 "Fortran Parallel Loop",
                                 "Fortran Parallel SIMD Loop",
                                 "C Serial Loop",
                                 "C SIMD Loop",
                                 "C Parallel Loop",
                                 "C Parallel SIMD Loop"),
                  Language = c(rep("R", 2), rep("Rcpp", 4), rep("Fortran", 6),
                               rep("C", 4)),
                  Method = c("R", "R.int", "Serial", "SIMD", "Parallel",
                             "Parallel SIMD", "Direct", "Elemental", "Serial",
                             "SIMD", "Parallel", "Parallel SIMD", "Serial",
                             "SIMD", "Parallel", "Parallel SIMD")
)

setkey(EXP, expr)

Q <- data.table()

for (i in (10 ^ (seq_len(7) - 1))) {
  X <- rlnorm(i, 14, 1)
  T <- microbenchmark(LLC_r1(X, 2e6, 1e6),
                      LLC_r2(X, 2e6, 1e6),
                      LLC_cpp(X, 2e6, 1e6),
                      LLC_cpps(X, 2e6, 1e6),
                      LLC_cppl(X, 2e6, 1e6),
                      LLC_cppls(X, 2e6, 1e6),
                      LLC_fd(X, 2e6, 1e6),
                      LLC_fe(X, 2e6, 1e6),
                      LLC_f(X, 2e6, 1e6),
                      LLC_fs(X, 2e6, 1e6),
                      LLC_fl(X, 2e6, 1e6),
                      LLC_fls(X, 2e6, 1e6),
                      LLC_c(X, 2e6, 1e6),
                      LLC_cs(X, 2e6, 1e6),
                      LLC_cl(X, 2e6, 1e6),
                      LLC_cls(X, 2e6, 1e6),
                      times = 1000L,
                      control = list(order = 'block'))
  setDT(T)
  T[, `:=`(expr = as.character(expr))]
  setkey(T, expr)
  Z <- EXP[T]
  M <- Z[, .(Med = median(time), Mean = mean(time)), keyby = Expression]
  
  Z$Expression <- factor(Z$Expression,
                         levels = M$Expression[order(
                           M$Med, M$Mean, decreasing = TRUE)])
  
  Q <- rbind(Q, Z[, .(Min = min(time/1e3),
                      LQ = quantile(time/1e3, .25),
                      Median = median(time/1e3),
                      UQ = quantile(time/1e3, .75),
                      Max = max(time/1e3),
                      Mean = mean(time/1e3),
                      SD = sd(time/1e3),
                      CV = sd(time)/mean(time),
                      MedianRatio = median(time) / min(M[, 2]),
                      Size = i,
                      Reps = .N),
                  keyby = c("Language", "Method")][order(Median, Mean)])
  
  png(paste0("./Boxplot_",i,".png"), width = 1280, height = 720,
      type = 'cairo', res = 128)
  print(ggplot(Z, aes(y = time, color = Expression, x = Expression)) + 
          geom_boxplot() + coord_flip() + scale_y_log10(labels = comma) +
          ylab("Time in Nanoseconds - Log Scale") +
          theme(legend.position = "none") +
          ggtitle(paste0("Layer Loss Cost - ",prettyNum(
            i, big.interval = 3L, big.mark = ",", format = "d") ," Losses")))
  dev.off()
  
  if (i > 1e2) { # The ggridges package had issues with i <= 100
    png(paste0("./Ridgeplot_",i,".png"), width = 1280, height = 720,
        type = 'cairo', res = 128)
    print(ggplot(Z, aes(x = time, fill = Expression, y = Expression,
                        color = Expression)) +
            geom_density_ridges(rel_min_height = 0.01, scale = 1) +
            scale_x_log10(labels = comma) +
            xlab("Time in Nanoseconds - Log Scale") +
            theme(legend.position = "none") + ggtitle(paste0(
              "Layer Loss Cost - ",prettyNum(
                i, big.interval = 3L, big.mark = ",", format = "d") ,
              " Losses")))
    dev.off()
  }
}

png("./MedianTimes.png", width = 1280, height = 720, type = 'cairo', res = 128)
print(ggplot(Q, aes(y = Median, x = Size, color = Method)) +
  geom_line(aes(linetype = Language)) + scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma) + scale_color_manual(values = ColPal8) +
  ggtitle("Median Time in Nanoseconds by Vector Length: log-log scale"))
dev.off()

# knitr::kable(Q, digits = 2, format.args = list(scientific = FALSE,
#                                                big.mark = ','))







