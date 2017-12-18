const fs = require('fs')

function prettifyJSON(filePath) {
  fs.readFile(filePath, (err, data) => {
    console.log(JSON.stringify(JSON.parse(data), null, 4));
  });
};

const argFilePath = process.argv[2];

prettifyJSON(argFilePath)

