for(def i in 1..20){
    print "type ("  
    print ((1..i).collect { "'x${it}" }.join(", "))
    print ") field$i = { " 
    print ((1..i).collect { "fd$it : 'x$it" }.join("; "))
    print "}\n"
}

println "------------"

for(def i in 1..20) {

  print "  let map$i\n    " 
  print ((1..i).collect {"(f$it: 'x$it conv)"}.join("\n    "))
  print "\n    (ast: ast_value): (" 
  print ((1..i).collect { "'x${it}" }.join(", "))
  print ") field$i ="   
  print "\n    {"
  print ((1..i).collect { "fd${it} = f${it} ast" }.join("; "))
  print "}\n\n"


}

