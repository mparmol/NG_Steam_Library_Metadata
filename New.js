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


//hltbService.detail('50087').then(result => console.log(result)).catch(e => console.error(e));
//hltbService.search('Nioh');

/*
(function(console){

    console.save = function(data, filename){
    
        if(!data) {
            console.error('Console.save: No data')
            return;
        }
    
        if(!filename) filename = 'console.json'
    
        if(typeof data === "object"){
            data = JSON.stringify(data, undefined, 4)
        }
    
        var blob = new Blob([data], {type: 'text/json'}),
            e    = document.createEvent('MouseEvents'),
            a    = document.createElement('a')
    
        a.download = filename
        a.href = window.URL.createObjectURL(blob)
        a.dataset.downloadurl =  ['text/json', a.download, a.href].join(':')
        e.initMouseEvent('click', true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null)
        a.dispatchEvent(e)
     }
    })(console)
    */