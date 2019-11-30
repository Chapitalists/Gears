/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/

const soundPath = './sons/'
    , soundExtensions = ['.wav']
    , savePath = './saves/'
    , backupPath = './backup/'
    , saveExtension = '.gears'

function soundExtensionFilter (name) {
  return soundExtensions.some(ext=>name.toLowerCase().endsWith(ext))
}

function saveExtensionFilter (name) {
  return name.toLowerCase().endsWith(saveExtension)
}


const fs = require('fs')
    , staticCallback = require('static-route')()



    , dynamicCallbacks = {
      
        test : (req, res) => {
            console.log('got a test')
            res.write('Stringlist')
            res.end()
        }
      
        , soundList : (req, res) => {
            res.write(fs.readdirSync(soundPath)
                .filter(soundExtensionFilter)
                .join('\\'))
            res.end()
        }

        , savesList : (req, res) => {
            res.write(fs.readdirSync(savePath)
                .filter(saveExtensionFilter)
                .join('\\'))
            res.end()
        }

        , saveFile : (req, res) => {
            if (req.method == 'POST') {
                let data = ""

                req.on('data', chunk => data += chunk)

                req.on('end', () => writeFile(JSON.parse(data)))
            }
        }
      
    }



    , callback = (req, res) => {
      const cb = dynamicCallbacks[req.url.slice(1)]
      ;(cb || staticCallback)(req, res)
    }


function writeFile (data) {
    if (!fs.existsSync(savePath)) fs.mkdirSync(savePath)
    if (!fs.existsSync(backupPath)) fs.mkdirSync(backupPath)

    let filePath = savePath + data.name + saveExtension
        dateStr = new Date().toISOString()
            .replace(':', '-').replace(/\..+/, '')
    if (fs.existsSync(filePath))
        fs.copyFileSync(
            filePath,
            backupPath + data.name + dateStr + saveExtension
        )

    fs.writeFileSync(filePath, JSON.stringify(data.data, null, 2))
}

require('http').createServer(callback).listen(12345)
