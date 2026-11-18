########### RETRANSFORMATION AND DISTRIBUTIONAL CHOICE IN THE ESTIMATION OF FEDERAL SENTENCING DISPARITIES ###########
########### REPLICATION CODE ###########
########### ETHAN DOSHI ###########

# USSC Monitoring of Federal Criminal Sentences, file format used for each fiscal year (ICPSR study number)
# 2001 (03497): fixed-width text with SPSS setup file
# 2002 (04110): fixed-width text with SPSS setup file
# 2003 (04290): fixed-width text with SPSS setup file
# 2004 (04633): fixed-width text with SPSS setup file
# 2005 (04630): fixed-width text with SPSS setup file
# 2006 (20120): tab-separated values
# 2007 (22623): tab-separated values
# 2008 (25424): tab-separated values
# 2009 (28602): tab-separated values
# 2010 (35336): tab-separated values
# 2011 (35339): tab-separated values
# 2012 (35342): SPSS binary (.sav)
# 2013 (35345): SPSS binary (.sav)
# 2014 (36568): tab-separated values
# 2015 (36571): tab-separated values
# 2016 (36962): tab-separated values
# 2017 (37975): tab-separated values
# 2018 (37987): tab-separated values
# 2019 (37990): tab-separated values
# 2020 (38549): tab-separated values
# 2021 (38552): tab-separated values


library(dplyr)
library(tidyr)
library(data.table)
library(haven)
library(pscl)
library(parallel)


path <- "PATH TO DOWNLOADED USSC DATA"
cores <- max(1, detectCores() - 1)
years <- 2001:2021
reps <- 250
reps01 <- 1000
mcreps <- 500
inner <- 100

files <- data.frame(
  year = years,
  id = c("03497", "04110", "04290", "04633", "04630", "20120", "22623", "25424", "28602", "35336", "35339",
         "35342", "35345", "36568", "36571", "36962", "37975", "37987", "37990", "38549", "38552"),
  type = c(rep("txt", 5), rep("tsv", 6), rep("sav", 2), rep("tsv", 8))
)

keep <- c("NEWRACE", "CITIZEN", "NEWCIT", "MONSEX", "AGE", "GLMIN", "SENTTOT0", "SENTTOT", "PRISDUM", "DEPART",
          "DEPART_A", "BOOKERCD", "SENTRNGE", "NEWCNVTN", "DISTRICT", "XCRHISSR", "CRIMHIST", "NOCOUNTS")

layout <- function(sps) {
  x <- readLines(sps, warn = FALSE)
  x <- x[!grepl("^\\s*[*|]", x) & !grepl("\\|\\s*$", x)]
  s <- grep("^\\s*DATA LIST", x, ignore.case = TRUE)[1]
  e <- grep("^\\s*(VARIABLE LABELS|VALUE LABELS|FORMATS|MISSING VALUES|RECODE|EXECUTE)", x, ignore.case = TRUE)
  e <- c(e[e > s], length(x) + 1)[1]
  b <- gsub("/\\s*\\d+", " ", gsub("\\s+", " ", paste(x[(s + 1):(e - 1)], collapse = " ")))
  m <- regmatches(b, gregexpr("([A-Za-z_][A-Za-z0-9_]*)\\s+(\\d+)(\\s*-\\s*(\\d+))?", b, perl = TRUE))[[1]]
  p <- do.call(rbind, lapply(m, function(z) {
    v <- regmatches(z, regexec("([A-Za-z_][A-Za-z0-9_]*)\\s+(\\d+)(\\s*-\\s*(\\d+))?", z, perl = TRUE))[[1]]
    data.frame(name = v[2], start = as.integer(v[3]), end = as.integer(ifelse(v[5] == "", v[3], v[5])))
  }))
  p[!duplicated(p$name), ]
}

readtxt <- function(dat, sps) {
  p <- layout(sps)
  p <- p[p$name %in% keep, ]
  x <- readLines(dat, warn = FALSE)
  out <- lapply(seq_len(nrow(p)), function(i) {
    type.convert(trimws(substr(x, p$start[i], p$end[i])), as.is = TRUE, na.strings = c("", "NA", "."))
  })
  names(out) <- p$name
  as.data.frame(out)
}

load <- function(yr) {
  f <- files[files$year == yr, ]
  dat <- file.path(path, paste0(f$id, "-0001-Data.", f$type))
  if (f$type == "txt") return(readtxt(dat, file.path(path, paste0(f$id, "-0001-Setup.sps"))))
  if (f$type == "sav") return(as.data.frame(read_sav(dat, col_select = any_of(keep))))
  h <- names(fread(dat, nrows = 0, sep = "\t"))
  as.data.frame(fread(dat, select = intersect(h, keep), sep = "\t", na.strings = c("", "NA")))
}

maps <- list(
  old = c("0" = 0, "1" = 1, "2" = 2, "3" = 3, "8" = 8),
  protect = c("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 2, "5" = 3, "6" = 2, "7" = 3, "8" = 8, "9" = 3),
  booker = c("0" = 0, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 3, "6" = 2, "7" = 2, "8" = 2, "9" = 2, "10" = 2, "11" = 2),
  range = c("0" = 0, "1" = 1, "2" = 3, "3" = 2, "4" = 2, "5" = 2, "6" = 1, "7" = 2, "8" = 2)
)
depvar <- list(old = "DEPART", protect = c("DEPART", "DEPART_A"), booker = "BOOKERCD", range = "SENTRNGE")
regime <- function(yr) if (yr <= 2002) "old" else if (yr <= 2004) "protect" else if (yr <= 2017) "booker" else "range"

ages <- c("18-20", "21-29", "30-39", "40-49", "50-59", "60 and over")
pct <- c(3.6, 24.1, 28.4, 24.2, 13.9, 5.8, 4.0, 26.4, 31.9, 24.9, 9.6, 3.3,
         5.5, 45.7, 30.7, 12.7, 4.3, 1.1, 4.8, 41.2, 30.9, 15.5, 6.1, 1.5,
         9.3, 42.2, 27.7, 13.6, 5.4, 1.8, 8.0, 42.0, 26.4, 16.2, 5.4, 2.0)
tot <- rep(c(12464, 2847, 10276, 2024, 4810, 1084), each = 6)
target <- data.frame(race = rep(c("White", "Black", "Hispanic"), each = 12),
                     gender = rep(rep(c("Male", "Female"), each = 6), 3),
                     age = rep(ages, 6), n = round(pct / 100 * tot))

prep <- function(yr) {
  raw <- load(yr)
  r <- regime(yr)
  v <- depvar[[r]][depvar[[r]] %in% names(raw)][1]
  d <- data.frame(
    race = as.integer(raw$NEWRACE),
    cit = if ("CITIZEN" %in% names(raw)) as.integer(raw$CITIZEN) else as.integer(raw$NEWCIT == 1),
    sex = as.integer(raw$MONSEX),
    years = as.numeric(raw$AGE),
    gl = as.numeric(raw$GLMIN),
    sen = if ("SENTTOT0" %in% names(raw)) as.numeric(raw$SENTTOT0) else as.numeric(raw$SENTTOT),
    pris = as.integer(raw$PRISDUM),
    dep = as.integer(unname(maps[[r]][as.character(raw[[v]])])),
    conv = as.integer(raw$NEWCNVTN),
    dist = as.character(raw$DISTRICT),
    crim = if ("XCRHISSR" %in% names(raw)) as.integer(raw$XCRHISSR) else as.integer(raw$CRIMHIST),
    counts = as.numeric(raw$NOCOUNTS)
  )
  d$sen[which(d$pris == 0 & is.na(d$sen))] <- 0
  d <- d[which(d$cit == 1), ]
  d <- d[which(d$years >= 18), ]
  d <- d[which(d$race %in% 1:3), ]
  d <- d[which(d$dep %in% c(0, 2, 3)), ]
  d <- d[complete.cases(d[, c("race", "sex", "years", "crim", "counts", "pris", "sen", "gl", "dist", "conv", "dep")]), ]
  y <- ifelse(d$sen >= 470, 470, d$sen)
  y[d$pris == 1 & y == 0] <- NA
  d$y <- y
  d$ly <- ifelse(d$pris == 1 & d$y > 0, log(d$y), NA)
  d$gmin <- ifelse(d$gl == 996, 470, ifelse(d$gl %in% c(990, 993, 994, 995), NA, ifelse(d$gl > 470, 470, d$gl)))
  d$race <- factor(c("White", "Black", "Hispanic")[d$race], levels = c("White", "Black", "Hispanic"))
  d$gender <- factor(d$sex, levels = c(0, 1), labels = c("Male", "Female"))
  d$female <- as.numeric(d$gender == "Female")
  d$age <- factor(as.character(cut(d$years, c(17, 20, 29, 39, 49, 59, Inf), labels = ages)), levels = ages)
  d$down <- as.numeric(d$dep == 2)
  d$sa <- as.numeric(d$dep == 3)
  d$trial <- as.numeric(d$conv == 1)
  d$multi <- as.numeric(d$counts > 1)
  d$inc <- as.numeric(d$pris == 1)
  d <- d[!is.na(d$gmin), ]
  d <- d[!(d$pris == 1 & is.na(d$y)), ]
  d <- d[d$dist %in% names(which(table(d$dist) > 1)), ]
  d$dist <- factor(d$dist)
  cell <- paste(d$race, d$gender, d$age)
  n <- table(cell)
  w <- target$n / as.numeric(n[paste(target$race, target$gender, target$age)])
  d$w <- w[match(cell, paste(target$race, target$gender, target$age))]
  d[, c("race", "gender", "female", "age", "crim", "gmin", "down", "sa", "trial", "multi", "dist", "w", "inc", "y", "ly")]
}

rhs <- ~ female + race + age + crim + gmin + down + sa + trial + multi

design <- function(d) {
  X <- model.matrix(rhs, d)[, -1, drop = FALSE]
  sc <- apply(X, 2, function(x) max(sd(x), 1))
  list(X = sweep(X, 2, sc, "/"), d = as.integer(d$dist), D = nlevels(d$dist), sc = sc, lev = levels(d$dist))
}

rsum <- function(x, g, D) {
  r <- rowsum(x, g, reorder = FALSE)
  out <- matrix(0, D, ncol(r))
  out[as.integer(rownames(r)), ] <- r
  out
}

wls <- function(X, y, g, w, D) {
  sw <- as.numeric(rsum(w, g, D))
  mn <- rsum(cbind(X, y) * w, g, D) / pmax(sw, 1e-30)
  k <- ncol(X)
  xm <- mn[, seq_len(k), drop = FALSE]
  ym <- mn[, k + 1]
  XC <- X - xm[g, , drop = FALSE]
  b <- as.numeric(qr.solve(crossprod(XC, XC * w), crossprod(XC, w * (y - ym[g])), tol = 1e-10))
  a <- ym - drop(xm %*% b)
  list(b = b, a = a, eta = drop(X %*% b) + a[g], act = sw > 0)
}

glmfe <- function(X, y, g, w, D, fam, st = NULL, tol = 1e-9, maxit = 2000) {
  ok <- w > 0
  X <- X[ok, , drop = FALSE]; y <- y[ok]; g <- g[ok]; w <- w[ok]
  dev <- function(eta) {
    if (fam == "binomial") {
      p <- pmin(pmax(plogis(eta), 1e-15), 1 - 1e-15)
      -2 * sum(w * (y * log(p) + (1 - y) * log1p(-p)))
    } else {
      mu <- exp(pmin(eta, 700))
      2 * sum(w * (y / mu - 1 - log(y / mu)))
    }
  }
  eta <- if (is.null(st)) wls(X, if (fam == "binomial") qlogis((y + 0.5) / 2) else log(y), g, w, D)$eta else drop(X %*% st$b) + st$a[g]
  old <- dev(eta)
  for (it in seq_len(maxit)) {
    if (fam == "binomial") {
      mu <- pmin(pmax(plogis(eta), 1e-10), 1 - 1e-10)
      v <- mu * (1 - mu)
      f <- wls(X, eta + (y - mu) / v, g, w * v, D)
    } else {
      mu <- exp(pmin(eta, 700))
      f <- wls(X, eta + y / mu - 1, g, w, D)
    }
    nxt <- f$eta
    new <- dev(nxt)
    step <- 0
    while ((!is.finite(new) || new > old + 1e-7) && step < 30) {
      nxt <- (nxt + eta) / 2
      new <- dev(nxt)
      step <- step + 1
    }
    if (step > 0) f <- wls(X, nxt, g, w, D)
    if (abs(new - old) / (0.1 + abs(new)) < tol) return(f)
    eta <- nxt
    old <- new
  }
  stop("glm did not converge")
}

nbfe <- function(X, y, g, w, D, st = NULL) {
  ok <- w > 0 & y > 0
  X <- X[ok, , drop = FALSE]; y <- y[ok]; g <- g[ok]; w <- w[ok]
  k <- ncol(X)
  uy <- sort(unique(y)); iy <- match(y, uy); lf <- lgamma(y + 1)
  if (is.null(st)) {
    s <- wls(X, log(y), g, w, D)
    st <- c(s$b, s$a + 0.15, log(3.5))
  } else st <- c(st$b, st$a, log(st$theta))
  lastp <- NULL; lastv <- NULL
  both <- function(par) {
    if (!is.null(lastp) && identical(par, lastp)) return(lastv)
    b <- par[seq_len(k)]; a <- par[k + seq_len(D)]; th <- exp(par[k + D + 1])
    eta <- drop(X %*% b) + a[g]; mu <- exp(eta); tm <- th + mu
    l0 <- -th * log1p(mu / th); p0 <- exp(l0); den <- -expm1(l0); q <- p0 / den
    ll <- lgamma(uy + th)[iy] - lgamma(th) - lf + l0 + y * (eta - log(tm)) - log(den)
    de <- th * (y - mu) / tm - q * th * mu / tm
    dt <- digamma(uy + th)[iy] - digamma(th) + log(th / tm) + 1 - (th + y) / tm + q * (log(th / tm) + mu / tm)
    gr <- -c(drop(crossprod(X, w * de)), as.numeric(rsum(w * de, g, D)), sum(w * dt) * th)
    lastp <<- par
    lastv <<- list(value = -sum(w * ll), gradient = gr)
    lastv
  }
  ds <- as.numeric(rsum(w, g, D))
  ps <- sqrt(sum(w) / pmax(ds, 1)); ps[ds == 0] <- 1
  o <- optim(st, function(p) both(p)$value, function(p) both(p)$gradient, method = "BFGS",
             control = list(maxit = 500, reltol = 1e-12, fnscale = sum(w), parscale = c(rep(1, k), ps, 1)))
  if (o$convergence != 0 || !is.finite(o$value)) stop("nb did not converge")
  list(b = o$par[seq_len(k)], a = o$par[k + seq_len(D)], theta = exp(o$par[k + D + 1]))
}

fit <- function(d, des, m = rep(1, des$D), st = NULL) {
  w <- d$w * m[des$d]
  pos <- d$inc == 1 & !is.na(d$ly)
  l <- glmfe(des$X, d$inc, des$d, w, des$D, "binomial", st$l)
  o <- wls(des$X[pos, , drop = FALSE], d$ly[pos], des$d[pos], w[pos], des$D)
  np <- sum(m[des$d[pos]])
  rk <- ncol(des$X) + sum(m[tabulate(des$d[pos], nbins = des$D) > 0])
  s2 <- weighted.mean((d$ly[pos] - o$eta)^2, w[pos]) * np / (np - rk)
  g <- glmfe(des$X[pos, , drop = FALSE], d$y[pos], des$d[pos], w[pos], des$D, "gamma", st$g)
  n <- nbfe(des$X, d$yi, des$d, w, des$D, st$n)
  list(l = l, o = o, g = g, n = n, s2 = s2)
}

nbmean <- function(mu, th) mu / (-expm1(-th * log1p(mu / th)))

pred <- function(f, des, nd, m = rep(1, des$D), cnt) {
  X <- sweep(model.matrix(rhs, nd)[, -1, drop = FALSE], 2, des$sc, "/")
  pi <- plogis(outer(drop(X %*% f$l$b), f$l$a, "+"))
  geo <- exp(outer(drop(X %*% f$o$b), f$o$a, "+"))
  gm <- exp(outer(drop(X %*% f$g$b), f$g$a, "+"))
  mu <- exp(outer(drop(X %*% f$n$b), f$n$a, "+"))
  q <- cnt * m
  q[!f$o$act] <- 0
  q <- q / sum(q)
  av <- function(z) as.numeric(z %*% q)
  data.frame(geo = av(pi * geo), ts = av(pi * geo) * exp(f$s2 / 2), lnh = av(pi * geo * exp(f$s2 / 2)),
             gamma = av(pi * gm), nb = av(pi * nbmean(mu, f$n$theta)))
}

ape <- function(f, des, d, m) {
  w <- d$w * m[des$d]
  w[!f$o$act[des$d]] <- 0
  one <- function(z) {
    X <- des$X
    if (z %in% c("white", "black", "hispanic")) {
      X[, "raceBlack"] <- (z == "black") / des$sc["raceBlack"]
      X[, "raceHispanic"] <- (z == "hispanic") / des$sc["raceHispanic"]
    } else X[, "female"] <- (z == "female") / des$sc["female"]
    pi <- plogis(drop(X %*% f$l$b) + f$l$a[des$d])
    geo <- exp(drop(X %*% f$o$b) + f$o$a[des$d])
    gm <- exp(drop(X %*% f$g$b) + f$g$a[des$d])
    mu <- exp(drop(X %*% f$n$b) + f$n$a[des$d])
    c(geo = weighted.mean(pi * geo, w), nb = weighted.mean(pi * nbmean(mu, f$n$theta), w), gamma = weighted.mean(pi * gm, w))
  }
  p <- lapply(c("white", "black", "hispanic", "male", "female"), one)
  data.frame(con = c("Black-White", "Hispanic-White", "Female-Male"), rbind(p[[2]] - p[[1]], p[[3]] - p[[1]], p[[5]] - p[[4]]))
}

coefs <- function(f, des) {
  out <- lapply(c("l", "o", "g", "n"), function(s) {
    b <- f[[s]]$b / des$sc
    names(b) <- colnames(des$X)
    data.frame(spec = c(l = "logit", o = "ols", g = "gamma", n = "nbcount")[[s]], term = c("female", "raceBlack", "raceHispanic"),
               est = as.numeric(b[c("female", "raceBlack", "raceHispanic")]))
  })
  do.call(rbind, out)
}

ctrl <- c("crim", "gmin", "down", "sa", "trial", "multi")
temps <- function(d) {
  grid <- expand.grid(age = ages, gender = c("Male", "Female"), race = c("White", "Black", "Hispanic"), stringsAsFactors = FALSE)[, c("race", "gender", "age")]
  a <- grid
  for (v in ctrl) a[[v]] <- weighted.mean(d[[v]], d$w)
  b <- grid
  idx <- split(seq_len(nrow(d)), paste(d$race, d$gender, d$age))[paste(grid$race, grid$gender, grid$age)]
  for (v in ctrl) b[[v]] <- sapply(idx, function(i) if (is.null(i)) NA else weighted.mean(d[[v]][i], d$w[i]))
  t <- rbind(cbind(prof = "mean", a), cbind(prof = "subgroup", b))
  t$cell <- rep(1:36, 2)
  t$female <- as.numeric(t$gender == "Female")
  t$race <- factor(t$race, levels = levels(d$race))
  t$age <- factor(t$age, levels = levels(d$age))
  t
}

conv <- function(d, k) {
  pos <- d$inc == 1
  if (k == "floored") d$y <- ifelse(pos, pmax(1, d$y), d$y)
  if (k == "rounded") d$y <- ifelse(pos, pmax(1, round(d$y)), d$y)
  if (k == "zeroed") {
    s <- pos & d$y < 1
    d$inc[s] <- 0
    d$y[s] <- 0
    pos <- d$inc == 1
  }
  if (k != "recorded") d$ly <- ifelse(pos, log(d$y), NA)
  d
}

run <- function(yr, wt, k) {
  d <- conv(dat[[as.character(yr)]], k)
  d$yi <- ifelse(d$inc == 1, pmax(1L, as.integer(round(d$y))), 0L)
  if (wt == "unweighted") d$w <- rep(1, nrow(d))
  stopifnot(all((d$yi > 0) == (d$inc == 1)))
  des <- design(d)
  cnt <- tabulate(des$d, nbins = des$D)
  tp <- temps(d)
  f0 <- fit(d, des)
  id <- tp[, c("prof", "cell", "race", "gender", "age")]
  B <- if (yr == 2001 && wt == "weighted") reps01 else reps
  one <- function(i) {
    set.seed(20261003L + as.integer(yr) * 10000L + i)
    m <- tabulate(sample.int(des$D, des$D, replace = TRUE), nbins = des$D)
    f <- fit(d, des, m, f0)
    list(g = cbind(id, pred(f, des, tp, m, cnt), rep = i, s2 = f$s2),
         a = cbind(ape(f, des, d, m), rep = i, s2 = f$s2),
         b = cbind(coefs(f, des), rep = i))
  }
  r <- parLapplyLB(cl, seq_len(B), one)
  out <- list(year = yr, wt = wt, k = k, s2 = f0$s2, theta = f0$n$theta,
              g = cbind(id, pred(f0, des, tp, cnt = cnt)), a = ape(f0, des, d, rep(1, des$D)), b = coefs(f0, des),
              bg = do.call(rbind, lapply(r, `[[`, "g")), ba = do.call(rbind, lapply(r, `[[`, "a")), bb = do.call(rbind, lapply(r, `[[`, "b")))
  if (yr == 2001 && wt == "weighted" && k == "recorded") out <- c(out, list(d = d, des = des, f = f0))
  out
}

pick <- data.frame(con = c("Black-White", "Hispanic-White", "Female-Male"), race = c("Black", "Hispanic", "White"), gender = c("Male", "Male", "Female"))
vars <- c("ts", "nb", "gamma", "fixed", "geo")

cts <- function(g, s2) {
  g <- g[g$age == "30-39", ]
  g$fixed <- g$geo * exp(s2 / 2)
  if (!"rep" %in% names(g)) g$rep <- 0
  ref <- g[g$race == "White" & g$gender == "Male", ]
  do.call(rbind, lapply(1:3, function(i) {
    z <- merge(g[g$race == pick$race[i] & g$gender == pick$gender[i], ], ref, by = c("rep", "prof"))
    out <- data.frame(rep = z$rep, prof = z$prof, con = pick$con[i])
    for (v in vars) out[[v]] <- z[[paste0(v, ".x")]] - z[[paste0(v, ".y")]]
    out
  }))
}

panel <- function(r) {
  p0 <- rbind(cts(r$g, r$s2),
              data.frame(rep = 0, prof = "ape", con = r$a$con, ts = r$a$geo * exp(r$s2 / 2), nb = r$a$nb, gamma = r$a$gamma,
                         fixed = r$a$geo * exp(r$s2 / 2), geo = r$a$geo))
  pb <- rbind(cts(r$bg, r$s2),
              data.frame(rep = r$ba$rep, prof = "ape", con = r$ba$con, ts = r$ba$geo * exp(r$ba$s2 / 2), nb = r$ba$nb, gamma = r$ba$gamma,
                         fixed = r$ba$geo * exp(r$s2 / 2), geo = r$ba$geo))
  s <- pb %>%
    group_by(prof, con) %>%
    summarise(across(all_of(vars), list(se = sd, lo = ~ quantile(.x, 0.025), hi = ~ quantile(.x, 0.975))), .groups = "drop")
  left_join(p0[, -1], s, by = c("prof", "con")) %>%
    mutate(year = r$year, wt = r$wt, k = r$k, sigma = sqrt(r$s2), ratio = ts_se / nb_se, gratio = ts_se / gamma_se,
           fratio = fixed_se / nb_se, oratio = geo_se / nb_se)
}

coefpanel <- function(r) {
  s <- r$bb %>% group_by(spec, term) %>% summarise(se = sd(est), lo = quantile(est, 0.025), hi = quantile(est, 0.975), .groups = "drop")
  left_join(r$b, s, by = c("spec", "term")) %>% mutate(year = r$year, wt = r$wt, k = r$k)
}

cl <- makeCluster(cores)
clusterExport(cl, c("rhs", "rsum", "wls", "glmfe", "nbfe", "fit", "nbmean", "pred", "ape", "coefs"))

dat <- lapply(years, prep)
names(dat) <- years

jobs <- rbind(expand.grid(year = years, wt = c("weighted", "unweighted"), k = "floored", stringsAsFactors = FALSE),
              expand.grid(year = years, wt = "weighted", k = c("recorded", "rounded", "zeroed"), stringsAsFactors = FALSE))
runs <- lapply(seq_len(nrow(jobs)), function(i) run(jobs$year[i], jobs$wt[i], jobs$k[i]))

pan <- bind_rows(lapply(runs, panel))
cof <- bind_rows(lapply(runs, coefpanel))

r01 <- runs[[which(jobs$year == 2001 & jobs$wt == "weighted" & jobs$k == "recorded")]]
s01 <- r01$s2
lng <- function(g) g %>% mutate(fixed = geo * exp(s01 / 2)) %>% pivot_longer(c(ts, nb, gamma, lnh, fixed, geo), names_to = "spec")
pt <- lng(r01$g)
bt <- lng(r01$bg)
cells <- left_join(pt, bt %>% group_by(prof, cell, spec) %>%
                     summarise(se = sd(value), lo = quantile(value, 0.025), hi = quantile(value, 0.975), mean = mean(value), .groups = "drop"),
                   by = c("prof", "cell", "spec"))
refv <- function(x) x %>% group_by(across(any_of(c("rep", "prof", "spec")))) %>%
  mutate(value = value - value[race == "White" & gender == "Male" & age == "18-20"]) %>% ungroup()
disp <- left_join(refv(pt), refv(bt) %>% group_by(prof, cell, spec) %>%
                    summarise(se = sd(value), lo = quantile(value, 0.025), hi = quantile(value, 0.975), .groups = "drop"),
                  by = c("prof", "cell", "spec")) %>% mutate(sig = lo > 0 | hi < 0)

heads <- data.frame(race = c("Black", "Black", "Black", "Hispanic", "Hispanic", "White", "White"),
                    gender = c("Male", "Male", "Male", "Male", "Male", "Male", "Female"),
                    age = c("30-39", "21-29", "18-20", "21-29", "18-20", "18-20", "60 and over"))
t3 <- inner_join(heads, cells %>% filter(prof == "subgroup", spec %in% c("ts", "nb", "gamma")) %>% select(race, gender, age, spec, ey = value, ey_se = se),
                 by = c("race", "gender", "age")) %>%
  left_join(disp %>% filter(prof == "subgroup") %>% select(race, gender, age, spec, delta = value, delta_se = se), by = c("race", "gender", "age", "spec"))

t4 <- inner_join(heads, r01$g %>% filter(prof == "subgroup"), by = c("race", "gender", "age")) %>%
  transmute(race, gender, age, before = geo, correction = ts - geo, gap = geo - gamma, net = ts - gamma)

t5 <- cells %>% filter(spec %in% c("ts", "lnh")) %>% select(prof, cell, spec, se) %>% pivot_wider(names_from = spec, values_from = se) %>%
  group_by(prof) %>% summarise(ts_se = mean(ts), lnh_se = mean(lnh), ratio = mean(ts) / mean(lnh), maxdiff = max(abs(ts - lnh)))

d1 <- r01$d
f1 <- as.formula(paste("~", paste(c(attr(terms(rhs), "term.labels"), "dist"), collapse = " + ")))
pos1 <- d1[d1$inc == 1, ]
mols <- lm(update(f1, ly ~ .), data = pos1, weights = w)
mgam <- glm(update(f1, y ~ .), data = pos1, weights = w, family = Gamma(link = "log"))
mnb <- suppressWarnings(hurdle(as.formula(paste("yi ~", paste(c(attr(terms(rhs), "term.labels"), "dist"), collapse = " + "), "|",
                                                paste(c(attr(terms(rhs), "term.labels"), "dist"), collapse = " + "))),
                               data = d1, weights = d1$w, dist = "negbin", zero.dist = "binomial", link = "logit"))
yp <- pos1$yi
wp <- pos1$w
mln <- predict(mols, newdata = pos1)
smle <- sqrt(sum(wp * (log(yp) - mln)^2) / sum(wp))
mg <- predict(mgam, newdata = pos1, type = "response")
phi <- summary(mgam)$dispersion
shp <- 1 / phi
mn <- predict(mnb, newdata = pos1, type = "count")
th <- mnb$theta
mt <- mn / (1 - (th / (th + mn))^th)
pear <- c(ln = sqrt(weighted.mean(((log(yp) - mln) / sqrt(s01))^2, wp)),
          nb = sqrt(weighted.mean(((yp - mt) / sqrt(mt * (1 + mt / th)))^2, wp)),
          gamma = sqrt(weighted.mean(((yp - mg) / (mg * sqrt(phi)))^2, wp)))
plnint <- function(y, mu, s) {
  hi <- y + 0.5; lo <- y - 0.5
  up <- plnorm(hi, mu, s) > 0.5
  p <- numeric(length(y))
  p[!up] <- plnorm(hi[!up], mu[!up], s) - plnorm(lo[!up], mu[!up], s)
  p[up] <- plnorm(lo[up], mu[up], s, lower.tail = FALSE) - plnorm(hi[up], mu[up], s, lower.tail = FALSE)
  p / plnorm(0.5, mu, s, lower.tail = FALSE)
}
pgint <- function(y, mu, a, phi) {
  hi <- y + 0.5; lo <- y - 0.5; sc <- mu * phi
  up <- pgamma(hi, shape = a, scale = sc) > 0.5
  p <- numeric(length(y))
  p[!up] <- pgamma(hi[!up], shape = a, scale = sc[!up]) - pgamma(lo[!up], shape = a, scale = sc[!up])
  p[up] <- pgamma(lo[up], shape = a, scale = sc[up], lower.tail = FALSE) - pgamma(hi[up], shape = a, scale = sc[up], lower.tail = FALSE)
  p / pgamma(0.5, shape = a, scale = sc, lower.tail = FALSE)
}
pnbint <- function(y, th, mu) dnbinom(y, size = th, mu = mu) / (1 - dnbinom(0, size = th, mu = mu))
cap <- yp >= 470
pl <- plnint(yp, mln, smle)
pg <- pgint(yp, mg, shp, phi)
pn <- pnbint(yp, th, mn)
plc <- pl; plc[cap] <- plnorm(469.5, mln[cap], smle, lower.tail = FALSE) / plnorm(0.5, mln[cap], smle, lower.tail = FALSE)
pgc <- pg; pgc[cap] <- pgamma(469.5, shape = shp, scale = mg[cap] * phi, lower.tail = FALSE) / pgamma(0.5, shape = shp, scale = mg[cap] * phi, lower.tail = FALSE)
pnc <- pn; pnc[cap] <- pnbinom(469, size = th, mu = mn[cap], lower.tail = FALSE) / (1 - dnbinom(0, size = th, mu = mn[cap]))
t1 <- data.frame(spec = c("ln", "nb", "gamma"), pearson = pear,
                 continuous = c(sum(wp * (dnorm(log(yp), mln, smle, log = TRUE) - log(yp))),
                                sum(wp * (dnbinom(yp, size = th, mu = mn, log = TRUE) - log1p(-dnbinom(0, size = th, mu = mn)))),
                                sum(wp * dgamma(yp, shape = shp, scale = mg * phi, log = TRUE))),
                 discretized = c(sum(wp * log(pl)), sum(wp * log(pn)), sum(wp * log(pg))),
                 censored = c(sum(wp * log(plc)), sum(wp * log(pnc)), sum(wp * log(pgc))))
t1$vs_nb <- t1$censored - t1$censored[t1$spec == "nb"]

casepred <- function(f, des, d) {
  X <- sweep(model.matrix(rhs, d)[, -1, drop = FALSE], 2, des$sc, "/")
  g <- match(d$dist, des$lev)
  pi <- plogis(drop(X %*% f$l$b) + f$l$a[g])
  mu <- exp(drop(X %*% f$n$b) + f$n$a[g])
  data.frame(ts = pi * exp(drop(X %*% f$o$b) + f$o$a[g] + f$s2 / 2), nb = pi * nbmean(mu, f$n$theta),
             gamma = pi * exp(drop(X %*% f$g$b) + f$g$a[g]))
}
err <- function(p, keep, lab) {
  do.call(rbind, lapply(names(p), function(s) {
    data.frame(set = lab, spec = s, n = sum(keep), rmse = sqrt(weighted.mean((d1$y[keep] - p[[s]][keep])^2, d1$w[keep])),
               mae = weighted.mean(abs(d1$y[keep] - p[[s]][keep]), d1$w[keep]))
  }))
}
p1 <- casepred(r01$f, r01$des, d1)
set.seed(20260922)
fold <- sample(rep(1:10, length.out = nrow(d1)))
cv <- p1 * NA
for (j in 1:10) {
  tr <- d1[fold != j, ]
  de <- design(tr)
  cv[fold == j, ] <- casepred(fit(tr, de), de, d1[fold == j, ])
}
t2 <- rbind(err(p1, rep(TRUE, nrow(d1)), "in sample"), err(p1, d1$y < 470, "in sample without cap"),
            err(cv, rep(TRUE, nrow(d1)), "10-fold cv"), err(cv, d1$y < 470, "10-fold cv without cap"))
band <- cut(d1$y, c(-1, 0, 40, 151, 360, 469, Inf), labels = c("0", "1-40", "41-151", "152-360", "361-469", "470"))
ta2 <- do.call(rbind, lapply(levels(band), function(b) err(p1, band == b, b)))

per <- function(y) ifelse(y <= 2002, "2001-02", ifelse(y <= 2017, "2003-17", "2018-21"))
fl <- pan %>% filter(k == "floored")

ta1 <- fl %>% filter(prof == "mean") %>% group_by(wt, con) %>%
  summarise(mean = mean(ratio), min = min(ratio), max = max(ratio), above1 = sum(ratio > 1), .groups = "drop")

ta3 <- fl %>% filter(wt == "weighted", prof == "mean") %>% arrange(year, con) %>%
  transmute(year, con, ts, ts_se, ts_lo, ts_hi, ts_t = ts / ts_se, nb, nb_se, nb_lo, nb_hi, nb_t = nb / nb_se,
            gamma, gamma_se, gamma_lo, gamma_hi, gamma_t = gamma / gamma_se, ratio, tratio = abs(ts_t) / abs(nb_t))

ta4 <- fl %>% filter(wt == "weighted") %>% group_by(prof, con) %>%
  summarise(ratio = mean(ratio), fixed = mean(fratio), geo = mean(oratio), .groups = "drop")

ta5 <- cof %>% filter(k == "floored", spec %in% c("ols", "nbcount")) %>% select(year, wt, term, spec, se) %>%
  pivot_wider(names_from = spec, values_from = se) %>%
  mutate(ratio = ols / nbcount, period = ifelse(year <= 2002, "2001-02", ifelse(year <= 2007, "2003-07", ifelse(year <= 2017, "2008-17", "2018-21")))) %>%
  group_by(wt, term) %>%
  summarise(p1 = mean(ratio[period == "2001-02"]), p2 = mean(ratio[period == "2003-07"]), p3 = mean(ratio[period == "2008-17"]),
            p4 = mean(ratio[period == "2018-21"]), all = mean(ratio), above1 = sum(ratio > 1), .groups = "drop")

aside <- function(yr) {
  d <- dat[[as.character(yr)]]
  d <- d[d$inc == 1, ]
  b <- d[d$y >= 1, ]
  b$ly <- log(b$y)
  de <- design(b)
  f <- wls(de$X, b$ly, de$d, b$w, de$D)
  data.frame(year = yr, below1 = mean(d$y < 1), at1 = mean(pmax(1, round(d$y)) == 1),
             aside = sqrt(sum(b$w * (b$ly - f$eta)^2) / sum(b$w) * nrow(b) / (nrow(b) - ncol(de$X) - de$D)))
}
sig <- bind_rows(lapply(years, aside)) %>%
  left_join(pan %>% filter(wt == "weighted") %>% distinct(year, k, sigma) %>% pivot_wider(names_from = k, values_from = sigma, names_prefix = "sigma_"), by = "year")
t7 <- sig %>% filter(year %in% c(2001, 2002, 2005, 2010, 2015, 2017, 2018, 2019, 2020, 2021)) %>%
  transmute(year, floored = sigma_floored, floored_factor = exp(sigma_floored^2 / 2), recorded = sigma_recorded,
            recorded_factor = exp(sigma_recorded^2 / 2), aside)

mr <- function(kk) pan %>% filter(k == kk, wt == "weighted", prof == "mean") %>% group_by(year) %>% summarise(r = mean(ratio), .groups = "drop")
ta8 <- sig %>% transmute(year, below1, at1, sigma_recorded, sigma_floored, aside) %>%
  left_join(mr("recorded") %>% rename(ratio_recorded = r), by = "year") %>%
  left_join(pan %>% filter(k == "recorded", wt == "weighted", prof == "mean") %>% group_by(year) %>% summarise(gamma_recorded = mean(gratio)), by = "year") %>%
  left_join(mr("floored") %>% rename(ratio_floored = r), by = "year") %>%
  left_join(mr("rounded") %>% rename(ratio_rounded = r), by = "year") %>%
  left_join(mr("zeroed") %>% rename(ratio_zeroed = r), by = "year") %>%
  left_join(pan %>% filter(k == "recorded", wt == "weighted", prof == "mean", con == "Female-Male") %>% select(year, female_recorded = ts), by = "year") %>%
  left_join(pan %>% filter(k == "floored", wt == "weighted", prof == "mean", con == "Female-Male") %>% select(year, female_floored = ts), by = "year")

gen <- function(dist, het, cap = TRUE, short = 0, n = 5000) {
  demo <- sample(0:2, n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  g <- rgamma(n, shape = 2, scale = 30)
  cid <- sample(1:10, n, replace = TRUE)
  ce <- rnorm(10, 0, 0.15)[cid]
  lm0 <- 2.8 + c(0, 0.6, 1.2)[demo + 1] + 0.005 * (g - 60) + ce
  pos <- rbinom(n, 1, plogis(qlogis(0.65) + c(0, 1, 2.5)[demo + 1] + 0.01 * (g - 60) + ce))
  h <- pmax(switch(het, none = 1, mild = 1 + 0.3 * (g - 60) / 60, strong = 1 + 0.7 * (g - 60) / 60), 0.3)
  s <- 0.7 * h
  if (dist == "lognormal") yp <- exp(rnorm(n, lm0, s))
  if (dist == "gamma") {
    a <- 1 / (exp(0.49) - 1)
    yp <- rgamma(n, shape = a, rate = a / exp(lm0))
  }
  if (dist == "nb2") yp <- pmax(rnbinom(n, mu = exp(lm0), size = 3.5), 1)
  if (dist == "mix") {
    tail <- rbinom(n, 1, 0.1)
    yp <- exp(rnorm(n, lm0, ifelse(tail == 1, s * 3, s)))
  }
  if (cap) yp <- pmin(yp, 470)
  if (short > 0) {
    sh <- rbinom(n, 1, short) == 1
    yp[sh] <- sample.int(15, sum(sh), replace = TRUE) / 30
  }
  data.frame(y = ifelse(pos == 1, yp, 0), pos = pos == 1, demo, g, cid)
}

mcfit <- function(x, m, st = NULL) {
  X <- x$X; g <- x$g; w <- m[g]; pos <- x$y > 0
  l <- glmfe(X, as.numeric(x$pos), g, w, 10, "binomial", st$l)
  o <- wls(X[pos, , drop = FALSE], log(x$y[pos]), g[pos], w[pos], 10)
  ga <- glmfe(X[pos, , drop = FALSE], x$y[pos], g[pos], w[pos], 10, "gamma", st$g)
  n <- nbfe(X, ifelse(x$pos, pmax(1, round(x$y)), 0), g, w, 10, st$n)
  np <- sum(w[pos])
  rk <- ncol(X) + sum(m[tabulate(g[pos], nbins = 10) > 0])
  s2 <- weighted.mean((log(x$y[pos]) - o$eta)^2, w[pos]) * np / (np - rk)
  Xg <- rbind(c(0, 0, 0), c(0, 1, 0))
  pi <- plogis(outer(drop(Xg %*% l$b), l$a, "+"))
  geo <- exp(outer(drop(Xg %*% o$b), o$a, "+"))
  gm <- exp(outer(drop(Xg %*% ga$b), ga$a, "+"))
  mu <- exp(outer(drop(Xg %*% n$b), n$a, "+"))
  q <- tabulate(g, nbins = 10) * m
  q <- q / sum(q)
  dl <- function(z) sum((z[2, ] - z[1, ]) * q)
  list(l = l, g = ga, n = n, est = c(ts = dl(pi * geo) * exp(s2 / 2), nb = dl(pi * nbmean(mu, n$theta)), g = dl(pi * gm)))
}

scen <- data.frame(name = c("lognormal_homo", "lognormal_het_mild", "lognormal_het_strong", "gamma", "nb2", "lognormal_mixture",
                            "lognormal_mixture_uncapped", "lognormal_short", "lognormal_short_floored"),
                   dist = c("lognormal", "lognormal", "lognormal", "gamma", "nb2", "mix", "mix", "lognormal", "lognormal"),
                   het = c("none", "mild", "strong", "none", "none", "none", "none", "none", "none"),
                   cap = c(rep(TRUE, 6), FALSE, TRUE, TRUE), short = c(rep(0, 7), 0.05, 0.05),
                   floor = c(rep(FALSE, 8), TRUE), seed = c(1:8, 8))

mcone <- function(s, i) {
  z <- scen[s, ]
  set.seed(20261003L + z$seed * 100000L + i)
  x <- gen(z$dist, z$het, z$cap, z$short)
  y <- if (z$floor) ifelse(x$pos, pmax(1, x$y), 0) else x$y
  dat <- list(X = cbind(mid = as.numeric(x$demo == 1), disp = as.numeric(x$demo == 2), guide = (x$g - 60) / 60),
              g = as.integer(factor(x$cid)), y = y, pos = x$pos)
  f0 <- mcfit(dat, rep(1, 10))
  b <- t(sapply(seq_len(inner), function(j) mcfit(dat, tabulate(sample.int(10, 10, replace = TRUE), nbins = 10), f0)$est))
  data.frame(scenario = z$name, rep = i, spec = c("ts", "nb", "g"), est = f0$est, se = apply(b, 2, sd),
             lo = apply(b, 2, quantile, 0.025), hi = apply(b, 2, quantile, 0.975))
}

clusterExport(cl, c("gen", "mcfit", "scen", "mcone", "inner"))
tasks <- expand.grid(s = 1:9, i = seq_len(mcreps))
mc <- bind_rows(parLapplyLB(cl, split(tasks, seq_len(nrow(tasks))), function(z) mcone(z$s, z$i)))
stopCluster(cl)

lncap <- function(m, s) exp(m + s * s / 2) * pnorm((log(470) - m - s * s) / s) + 470 * pnorm((log(470) - m) / s, lower.tail = FALSE)
capmean <- function(c, kind, dd) {
  m <- 2.8 + 1.2 * dd + c
  mu <- exp(m)
  if (kind == "lognormal") return(lncap(m, 0.7))
  if (kind == "mixture") return(0.9 * lncap(m, 0.7) + 0.1 * lncap(m, 2.1))
  if (kind == "uncapped") return(exp(m) * (0.9 * exp(0.7^2 / 2) + 0.1 * exp(2.1^2 / 2)))
  if (kind == "short") return(0.95 * lncap(m, 0.7) + 0.05 * mean((1:15) / 30))
  if (kind == "gamma") {
    a <- 1 / expm1(0.49)
    return(mu * pgamma(470, a + 1, scale = mu / a) + 470 * pgamma(470, a, scale = mu / a, lower.tail = FALSE))
  }
  vapply(mu, function(u) sum((1:469) * dnbinom(1:469, mu = u, size = 3.5)) + 470 * pnbinom(469, mu = u, size = 3.5, lower.tail = FALSE) +
           dnbinom(0, mu = u, size = 3.5), numeric(1))
}
truth1 <- function(kind) {
  integrate(function(c) (plogis(qlogis(0.65) + 2.5 + c) * capmean(c, kind, 1) - plogis(qlogis(0.65) + c) * capmean(c, kind, 0)) * dnorm(c, sd = 0.15),
            -2, 2, rel.tol = 1e-10)$value
}
kinds <- c("lognormal", "lognormal", "lognormal", "gamma", "nb2", "mixture", "uncapped", "short", "short")
truth <- setNames(sapply(kinds, truth1), scen$name)

t6 <- mc %>% mutate(truth = truth[scenario], err = est - truth, pct = lo <= truth & hi >= truth, wald = abs(err) <= 1.96 * se) %>%
  group_by(scenario, spec) %>%
  summarise(truth = first(truth), bias = mean(err), rmse = sqrt(mean(err^2)), sd = sd(est), mean_se = mean(se), se_sd = mean(se) / sd(est),
            cover_pct = mean(pct), cover_wald = mean(wald), bias_mcse = sd(err) / sqrt(n()),
            rmse_mcse = sqrt(var(err^2) / n()) / (2 * sqrt(mean(err^2))), pct_mcse = sqrt(mean(pct) * (1 - mean(pct)) / n()),
            wald_mcse = sqrt(mean(wald) * (1 - mean(wald)) / n()), .groups = "drop") %>%
  arrange(match(scenario, scen$name), match(spec, c("ts", "nb", "g")))

results <- list(table1 = t1, table2 = t2, table3 = t3, table4 = t4, table5 = t5, table6 = t6, table7 = t7,
                tableA1 = ta1, tableA2 = ta2, tableA3 = ta3, tableA4 = ta4, tableA5 = ta5, tableA6 = t6, tableA7 = t6, tableA8 = ta8,
                cells2001 = cells, disparities2001 = disp, panel = pan, coefficients = cof, montecarlo = mc, truth = truth)
saveRDS(results, "results.rds")
for (x in names(results)[1:15]) {
  cat("\n", x, "\n")
  print(as.data.frame(results[[x]]))
}
