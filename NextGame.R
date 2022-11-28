##NextGame

system("node New.js > aux_time.txt")
as.vector(system("grep 'gameplayMain:' aux_time.txt"))
