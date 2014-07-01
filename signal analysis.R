dtt <- data.frame(
  trl.vwap.1 = runif(100),
  trl.vwap.2 = runif(100),
  cog.1 = runif(100),
  cog.2 = runif(100),
  fwd.vwap.1 = runif(100),
  fwd.vwap.2 = runif(100)
)

# for each RIC
# break dataset into 60-20-20 training-validation-test sets
# with the training set:
### run regression for each fwd.adv with all the cog and trailing vwap factors (except normalizer)
### run regression for each fwd.adv with only trailing vwap factors (except normalizer)
# with each validation set:
### predict with each model and measure the rms error
# table results of RMS eror and winner for each #adv
# with the test set and the winner of the validation set
### predict with each model (trailing and full winner) and save the RMS errors


n <- round( nrow(dtt) * c( 0.6, 0.8, 1 ))
idx <- sample.int(n[3])
ds <- list(
  train=dtt[ idx[    1:n[1] ], ],
  valid=dtt[ idx[ n[1]:n[2] ], ],
  test =dtt[ idx[ n[2]:n[3] ], ] 
)

fwd.labels  <- names(dtt)[ grep( "fwd.*", names(dtt) ) ]
trl.labels  <- names(dtt)[ grep( "trl.*", names(dtt) ) ]
hist.label  <- "trl.vwap.1" # closest correspondance to what we have now in prod
full.labels <- names(dtt)[ grep( "(trl|cog).*", names(dtt) ) ]
rms.fn <- function(x,y) sqrt(sum((x-y)*(x-y)))
m <- list()
rms <- data.frame()
for( fwd.label in fwd.labels )
{
  fm <- eval(parse(text=paste( fwd.label, "~ .", collapse="" )))
  m$full.lm <- lm( fm, data=ds$train[, c(fwd.label, full.labels)] )
  m$trl.lm  <- lm( fm, data=ds$train[, c(fwd.label, trl.labels )] )
  
  prd.train.full.lm <- predict( m$full.lm )
  prd.valid.full.lm <- predict( m$full.lm, newdata=ds$valid[, c(fwd.label, full.labels)] )
  prd.test.full.lm  <- predict( m$full.lm, newdata=ds$test[ , c(fwd.label, full.labels)] )
  rms.train.full <- rms.fn(prd.train.full.lm, ds$train[,fwd.label])
  rms.valid.full <- rms.fn(prd.valid.full.lm, ds$valid[,fwd.label])
  rms.test.full  <- rms.fn(prd.test.full.lm,  ds$test[ ,fwd.label])
  
  prd.train.trl.lm <- predict( m$trl.lm )
  prd.valid.trl.lm <- predict( m$trl.lm, newdata=ds$valid[, c(fwd.label, trl.labels)] )
  prd.test.trl.lm  <- predict( m$trl.lm, newdata=ds$test[ , c(fwd.label, trl.labels)] )
  rms.train.trl <- rms.fn(prd.train.trl.lm, ds$train[,fwd.label])
  rms.valid.trl <- rms.fn(prd.valid.trl.lm, ds$valid[,fwd.label])
  rms.test.trl  <- rms.fn(prd.test.trl.lm,  ds$test[ ,fwd.label])
    
  rms.train.hist <- rms.fn(ds$train[,hist.label], ds$train[,fwd.label])
  rms.valid.hist <- rms.fn(ds$valid[,hist.label], ds$valid[,fwd.label])
  rms.test.hist  <- rms.fn(ds$test[,hist.label],  ds$test[ ,fwd.label])
  
  rms <- rbind(rms,
    data.frame(
      output=fwd.label,
      model=c("full", "trl", "hist"),
      train.rms=c( rms.train.full, rms.train.trl, rms.train.hist ),
      valid.rms=c( rms.valid.full, rms.valid.trl, rms.valid.hist ),
      test.rms =c( rms.test.full,  rms.test.trl,  rms.test.hist  )
  ))
}  

best.rms <- do.call("rbind", by(rms, rms$model, function(x) x[which(x$valid.rms == min(x$valid.rms)),] ))