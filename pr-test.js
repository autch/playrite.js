const util = require('util');
const fs = require('fs');
const prParser = require('./playrite.js');

let p = new prParser.Parser();

fs.readFile(process.argv[2], 'utf8', (err, data) => {
    const r = p.parse(data);

    console.log(JSON.stringify(r, null, 2));
});
