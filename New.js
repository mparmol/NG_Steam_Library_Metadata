let hltb = require('howlongtobeat');
let hltbService = new hltb.HowLongToBeatService();

/*
// print process.argv
process.argv.forEach(function (val, index, array) {
  console.log(index + ': ' + val);

});
*/

hltbService.search(process.argv[2]).then(result => console.log(result));

//"Terraria: 49h"
//Football_Manager_2017
//FLY'N: 55h"
//FLYN: 6h"
//"King's Quest: 19h"


//hltbService.detail('50087').then(result => console.log(result)).catch(e => console.error(e));
//hltbService.search('Nioh');