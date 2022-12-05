let hltb = require('howlongtobeat');
let hltbService = new hltb.HowLongToBeatService();

/*
// print process.argv
process.argv.forEach(function (val, index, array) {
  console.log(index + ': ' + val);

});
*/

hltbService.search(process.argv[2]).then(result => console.log(result));
//hltbService.search("Trine 2").then(result => console.log(result));

// Warhammer 4?
// Que salga "DUCATI - 90th Anniversary", pero no "FLY'N". Guardar el más similar, mostrar entre paréntesis y generar un file con este output

//hltbService.detail('50087').then(result => console.log(result)).catch(e => console.error(e));
//hltbService.search('Nioh');