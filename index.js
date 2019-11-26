/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/

const soundPath = './sons/'
    , soundExtensions = ['.wav']

function soundExtensionFilter (name) {
  return soundExtensions.some(ext=>name.toLowerCase().endsWith(ext))
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
      
      }



    , callback = (req, res) => {
      const cb = dynamicCallbacks[req.url.slice(1)]
      ;(cb || staticCallback)(req, res)
    }

require('http').createServer(callback).listen(12345)
