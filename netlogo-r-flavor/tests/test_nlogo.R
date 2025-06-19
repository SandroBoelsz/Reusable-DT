# Check if NETLOGO_HOME is set
netlogo_home <- Sys.getenv("NETLOGO_HOME")
if (netlogo_home != "") {
  cat("NETLOGO_HOME is set to:", netlogo_home, "\n")
} else {
  cat("NETLOGO_HOME is not set.\n")
}

# Check if NETLOGO_JAR is set
netlogo_jar <- Sys.getenv("NETLOGO_JAR")
if (netlogo_jar != "") {
  cat("NETLOGO_JAR is set to:", netlogo_jar, "\n")
} else {
  cat("NETLOGO_JAR is not set.\n")
}
 