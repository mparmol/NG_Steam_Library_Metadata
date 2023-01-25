let hltb = require('howlongtobeat');
let hltbService = new hltb.HowLongToBeatService();
hltbService.search(process.argv[2]).then(result => console.log(result));
