
codes <- c(380, 124, 818)
codes
country <- c("italy", "canada","egypt")
country
codes <- c(italy=380, canada=124, egypt=818)
print(codes)
class(codes)
codes <- c("italy"=380, "canada"=124, "egypt"=818)
codes
names(codes) <- country
country
codes
seq(1,10)
seq(1,10,2)
1:10
codes
codes[2]
codes[c(1,3)]
codes[1:2]
codes["canada"]
codes[ c("egypt", "italy") ]
