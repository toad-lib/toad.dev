const fs = require('fs')

const readdir = d => fs.readdirSync(d).reduce((a, de) => !fs.statSync(d + '/' + de).isDirectory() ? [...a, d + '/' + de] : [...a, ...readdir(d + '/' + de)], [])

readdir('test').forEach(p => {
  let c = fs.readFileSync(p, 'utf8');
  c = c.replace(/Kwap/g, 'Toad')
  fs.writeFileSync(p, c, 'utf8')
});
