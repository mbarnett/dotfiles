#! /usr/bin/awk -f

{
  pdir = "/Users/matt/Source/wtm/opal"
  search = $0
  gsub(/ /, "", search)
    
  split(search, a, "")
  len = length(search)
  sregex = "/"
  
  for(i = 1; i <= len; i++) {
    sregex = sregex "([" tolower(a[i]) toupper(a[i]) "])(.*)"
  }
  sregex = sregex "/"

  system("find " pdir " -type f | grep -vE '.git' | sed 's:" pdir "/::'  | awk 'match($0, " sregex ") { print RLENGTH \"\t\" $0 }' | sort -k1n | cut -f2")

  print "DONE"
}