summary.out <- function(x) {
  require(reshape)
  require(plyr)
  require(stringi)
  require(formattable)

  t <- as.data.frame(summary(x))
  t <- t[!is.na(t$Freq),]
  f <- ldply(stri_split_fixed(str = as.character(t$Freq), pattern = ":", n =2))
  t <- cbind(t$Var2,f)
  names(t) <- c("Variable", "Function", "Value")
  t$Function <- factor(t$Function)
  t$Value <- as.numeric(t$Value)
  t <- t[!is.na(t$Value),]
  t$Value <- as.numeric(t$Value)
  t <- cast(t, Variable ~ Function, value = "Value")
  formattable(t)
}