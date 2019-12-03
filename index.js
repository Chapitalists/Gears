/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/

const fs = require('fs')
    , staticRoute = require('static-route')
    , port = 12345
    , soundPath = process.argv[2] || './sons/'
    , soundExtensions = ['.wav']
    , savePath = process.argv[3] || './saves/'
    , backupPath = process.argv[4] || './backup/'
    , saveExtension = '.gears'

console.log('Checking directories')

if (!fs.existsSync(soundPath)) fs.mkdirSync(soundPath)
if (!fs.existsSync(savePath)) fs.mkdirSync(savePath)
if (!fs.existsSync(backupPath)) fs.mkdirSync(backupPath)

function soundExtensionFilter (name) {
  return soundExtensions.some(ext=>name.toLowerCase().endsWith(ext))
}

function saveExtensionFilter (name) {
  return name.toLowerCase().endsWith(saveExtension)
}


const internCallback = staticRoute({dir:__dirname, tryfiles:['ports.html']})

    , externCallback = staticRoute()

    , dynamicCallbacks = {
      
        soundList : (req, res) => {
            res.end(fs.readdirSync(soundPath)
                .filter(soundExtensionFilter)
                .join('\\'))
        }

        , savesList : (req, res) => {
            res.end(fs.readdirSync(savePath)
                .filter(saveExtensionFilter)
                .join('\\'))
        }

        , saveFile : (req, res) => {
            if (req.method == 'POST') {
                let data = ""

                req.on('data', chunk => data += chunk)

                req.on('end', () => {
                    if (writeFile(JSON.parse(data))) res.statusCode = 201
                    res.end()
                })
            } else {
                res.statusCode = 404
                res.end()
            }
        }

    }



    , callback = (req, res) => {
        const dir = req.url.split('/')[1]
            , cb = (dir == 'sons' || dir == 'saves') ? externCallback : dynamicCallbacks[dir]
        ;(cb || internCallback)(req, res)
    }


function writeFile (data) {
    let filePath = savePath + data.name + saveExtension
      , dateStr = new Date().toISOString().replace(/:/g, '-').replace(/\./g, '-')
      , created = true
    if (fs.existsSync(filePath)) {
        fs.copyFileSync(
            filePath,
            backupPath + data.name + dateStr + saveExtension
        )
        created = false
    }
    fs.writeFileSync(filePath, JSON.stringify(data.data, null, 2))
    return created
}

require('http').createServer(callback).listen(port)

console.log('Ready on port ' + port)
