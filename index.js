/*
Copyright ou © ou Copr. Clément Bossut, (2024)
<bossut.clement@gmail.com>
*/

if (!__dirname.includes("snapshot")) process.chdir(__dirname)

console.log('args : port soundDir saveDir backUpDir')

if (process.argv[2]) {
  let port = parseInt(process.argv[2])
  if ( ! (Number.isInteger(port) && port > 1028 && port < 49151) ) {
    console.log('Invalid port in first arguments : ' + port)
    console.log('Port should be an integer in [1029, 49150]')
    return;
  }
}


const fs = require('fs')
    , readdirRec = require('fs-readdir-recursive')
    , staticRoute = require('static-route')
    , formidable = require('formidable')
    , port = process.argv[2] || 12345
    , soundPath = process.argv[3] || './sons/'
    , soundExtensions = ['.wav']
    , savePath = process.argv[4] || './saves/'
    , backupPath = process.argv[5] || './backup/'
    , saveExtension = '.gears'

console.log('Checking directories')

if (!fs.existsSync(soundPath)) fs.mkdirSync(soundPath)
if (!fs.existsSync(savePath)) fs.mkdirSync(savePath)
if (!fs.existsSync(backupPath)) fs.mkdirSync(backupPath)

function hidedFileFilter (name) {
  return name.split('.')[0]
}

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
          let tab = readdirRec(soundPath)
                .filter(hidedFileFilter)
                .filter(soundExtensionFilter)
                .map(decode)
          Promise.all(tab).then(t => {console.log(t);res.end(t.join('\u0000'))})
        }

        , savesList : (req, res) => {
            res.end(fs.readdirSync(savePath)
                .filter(hidedFileFilter)
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

        , upSound : (req, res) => {
            if (req.method == 'POST') {
                let form = formidable({uploadDir : backupPath})
                form.parse(req, (err, fields, files) => {
                    if (err) {
                        console.log(err)
                        res.statusCode = 500
                        res.end()
                        return;
                    }
                    if (files.file.type != 'audio/wav' && files.file.type != 'audio/x-wav') {
                        console.log("Wrong type " + files.file.type + ", need WAV")
                        res.statusCode = 501
                        res.end()
                        return;
                    }
                    if (fs.existsSync(soundPath + files.file.name)) {
                        console.log("Sound already exists " + files.file.name)
                        res.statusCode = 403
                        res.end()
                        return;
                    }
                    let path = soundPath
                        + (fields.from ? fields.from + '-' : '')
                        + (fields.type ? fields.type + '/' : '')
                    if (!fs.existsSync(path)) fs.mkdirSync(path, {recursive:true})
                    fs.renameSync(files.file.path, path + files.file.name)
                    res.end()
                })
            } else {
                res.statusCode = 404
                res.end()
            }
        }

        , upSave : (req, res) => {
            if (req.method == 'POST') {
                let form = formidable({uploadDir : backupPath, keepExtensions : true})
                form.parse(req, (err, fields, files) => {
                    if (err) {
                        console.log(err)
                        res.statusCode = 500
                        res.end()
                        return;
                    }
                    if (files.file.name.split('.').slice(-1)[0] != 'gears') {
                        console.log("Wrong type " + files.file.name + ", need *.gears")
                        res.statusCode = 501
                        res.end()
                        return;
                    }
                    if (fs.existsSync(savePath + files.file.name)) {
                        backItUp(savePath + files.file.name, files.file.name.split('.').slice(0,-1).join('.'), saveExtension)
                        fs.unlinkSync(savePath + files.file.name)
                    }
                    fs.renameSync(files.file.path, savePath + files.file.name)
                    res.end()
                })
            } else {
                res.statusCode = 404
                res.end()
            }
        }

    }



    , callback = (req, res) => {
        console.log(new Date(), req.method + ' ' + req.headers.host + req.url)
        const dir = req.url.split('/')[1]
            , cb = (dir == 'sons' || dir == 'saves') ? externCallback : dynamicCallbacks[dir]
        ;(cb || internCallback)(req, res)
    }


function writeFile (data) {
    let filePath = savePath + data.name + saveExtension
      , dateStr = new Date().toISOString().replace(/:/g, '-').replace(/\./g, '-')
      , created = true
    if (fs.existsSync(filePath)) {
        backItUp(filePath, data.name, saveExtension)
        created = false
    }
    fs.writeFileSync(filePath, JSON.stringify(data.data, null, 2))
    return created
}

function backItUp (filePath, fileName, extension) {
    let dateStr = new Date().toISOString().replace(/:/g, '-').replace(/\./g, '-')
    fs.copyFileSync(
        filePath,
        backupPath + fileName + dateStr + extension
    )
}

/* Inspired by https://github.com/andreasgal/node-wav */

async function decode(path) {
  let pos = 0, end = 44;

  let buffer = new ArrayBuffer(end)
  let v = new DataView(buffer);
  
  try {
    fs.readSync(fs.openSync(soundPath + '/' + path), v)

    function u8() {
      let x = v.getUint8(pos);
      pos++;
      return x;
    }

    function u16() {
      let x = v.getUint16(pos, true);
      pos += 2;
      return x;
    }

    function u32() {
      let x = v.getUint32(pos, true);
      pos += 4;
      return x;
    }

    function string(len) {
      let str = '';
      for (let i = 0; i < len; ++i)
        str += String.fromCharCode(u8());
      return str;
    }

    if (string(4) !== 'RIFF')
      throw new TypeError('Invalid WAV file');
    u32();
    if (string(4) !== 'WAVE')
      throw new TypeError('Invalid WAV file');

    let fmt;

    while (pos < end) {
      let type = string(4);
      let size = u32();
      let next = pos + size;
      switch (type) {
      case 'fmt ':
        let formatId = u16();
        if (formatId !== 0x0001 && formatId !== 0x0003 && formatId !== 0xFFFE)
          throw new TypeError(`Unsupported format in WAV file: ${formatId.toString(16)}`);
        fmt = {
          format: 'lpcm',
          floatingPoint: formatId === 0x0003,
          channels: u16(),
          sampleRate: u32(),
          byteRate: u32(),
          blockSize: u16(),
          bitDepth: u16(),
        };
        break;
      case 'data':
        if (!fmt)
          throw new TypeError('Missing "fmt " chunk.');
        let samples = Math.floor(size / fmt.blockSize);
        let channels = fmt.channels;
        let sampleRate = fmt.sampleRate;
        return path + '\u001e' + samples + '\u001f' + channels + '\u001f' + sampleRate;
  //      let channelData = [];
  //      for (let ch = 0; ch < channels; ++ch)
  //        channelData[ch] = new Float32Array(samples);
  //      lookup(data_decoders, fmt.bitDepth, fmt.floatingPoint)(buffer, pos, channelData, channels, samples);
  //      return {
  //        sampleRate: sampleRate,
  //        channelData: channelData
  //      };
  //      break;
      }
      pos = next;
    }
  } catch(error) {
    console.error(error, path, v)
    return null;
  }
}

require('http').createServer(callback).listen(port)

console.log('Ready on port ' + port)
