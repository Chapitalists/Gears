const app = Elm.Main.init(
    {
    flags :
      { hasSinkId : AudioContext.prototype.hasOwnProperty('setSinkId')
      , screen : {width : window.innerWidth, height : window.innerHeight}
      }
    }
)

if (app.ports.loadSound) app.ports.loadSound.subscribe(loadSound)
if (app.ports.toEngine) app.ports.toEngine.subscribe(engine)
if (app.ports.toggleRecord) app.ports.toggleRecord.subscribe(toggleRecord)
if (app.ports.requestSoundDraw) app.ports.requestSoundDraw.subscribe(drawSound)
if (app.ports.requestCutSample) app.ports.requestCutSample.subscribe(cutSample)
if (app.ports.requestDeviceList) app.ports.requestDeviceList.subscribe(getDeviceList)
if (app.ports.changeSink) app.ports.changeSink.subscribe(setCtxSink)
if (app.ports.openMic) app.ports.openMic.subscribe(openMic)
if (app.ports.inputRec) app.ports.inputRec.subscribe(inputRec)

const buffers = {}
    , ro = new ResizeObserver(sendSize)
ro.observe(document.getElementById('svgResizeObserver'))

let deb = null

function sendSize(entries) {
    app.ports.newSVGSize.send(entries[0].contentRect)
}

function drawSound(sv) {
  let buf = buffers[sv.soundPath]
  if (buf) {
    let size = Math.round(buf.length / sv.zoomFactor)
      , start = Math.round(buf.length * sv.startPercent)
      , drawFunc = () => { // TODO mix channels ?
          drawSamples('waveform', Array.from(buf.getChannelData(0).slice(start, start + size)))
          if (sv.waveformMap) drawSamples('waveformMap', Array.from(buf.getChannelData(0)))
        }
    if (sv.wait) setTimeout(drawFunc, 10)
    else drawFunc()
    app.ports.soundDrawn.send(sv.soundPath)
  } else console.log(sv.soundPath + ' isn’t loaded, cannot draw')
}

function loadSound(soundPath) {
  if (buffers[soundPath]) {
    app.ports.soundLoaded.send(soundPath + ' already Loaded')
  } else {
    createBuffer(soundPath).then(b => {
      buffers[soundPath] = b
      loadOk(soundPath)
    }).catch(err => loadErr(err, soundPath))
  }
}

async function createBuffer(soundPath) {
  const response = await fetch('./sons/' + soundPath)
      , arrayBuffer = await response.arrayBuffer()
      , audioBuffer = await ctx.decodeAudioData(arrayBuffer)
  return audioBuffer
}

function loadOk(soundPath) {
  app.ports.soundLoaded.send(
  { path : soundPath
  , length : buffers[soundPath].duration
  })
}

function loadErr(err, soundPath) {
  console.error(err)
  app.ports.soundLoaded.send(soundPath + ' got ' + err)
}

let recorders = [new Recorder(masterGain)]
function toggleRecord(args) {
  let channels = args[0] //-1 for stop
    , saveName = args[1]
  if (channels ==-1) {
    let zip = new JSZip()
      , proms = []
    recorders.forEach((r,i) => {
      r.stop()
      proms.push(
        new Promise((res, rej) =>
          r.exportWAV(bl => {zip.file(saveName+'-'+i+'.wav', bl);res()})
        )
      )
      r.clear()
    })
    Promise.all(proms).then(() => {
      zip.generateAsync({type:'blob'})
        .then(bl => {
            let link = document.createElement('a')
            link.download = saveName + '.zip'
            link.href = URL.createObjectURL(bl)
            link.click()
        })
    })
  } else {
    recorders = [recorders[0]]
    for (let i = 1 ; i <= channels ; i++) {
      auxGains[i] = ctx.createGain()
      recorders[i] = new Recorder(auxGains[i])
    }
    recorders.forEach(el => el.record())
  }
}

let tmpStream = null
function getDeviceList() {
  tmpStream = navigator.mediaDevices.getUserMedia({audio:true}) // ensure users permission (trick from https://set-sink-id.glitch.me/)
  navigator.mediaDevices.enumerateDevices().then(dl => {
    ctx.onsinkchange = () => {
      app.ports.gotMaxChannel.send(ctx.destination.maxChannelCount)
    }
    app.ports.gotMaxChannel.send(ctx.destination.maxChannelCount)
    app.ports.gotDeviceList.send(
      dl.filter(d => d.kind === 'audiooutput')// && d.deviceId !== 'default')
    )
    // Stop audio tracks, as we don't need them running now the permission has been granted
    tmpStream.getAudioTracks().forEach((track) => track.stop());
    tmpStream = null
  })
}

function setCtxSink(id) {console.log(id)
  ctx.setSinkId(id)
}

let mic
  , micToRecord
  , micRecorder
  , recording
function openMic() {
  navigator.mediaDevices.getUserMedia({audio : true}).then(stream => {
    mic = stream
    micToRecord = ctx.createMediaStreamSource(mic)
    micRecorder = new Recorder(micToRecord)
    app.ports.micOpened.send(null)
  }).catch(console.error)
}

function inputRec(args) {
  let name = args[0]
    , start = args[1]
  if (name) {
    micRecorder.stop()
    micRecorder.exportWAV(bl => app.ports.gotNewSample.send(
        { type : "rec"
        , file : new File([bl], name + ".wav", {type: "audio/wav"})
        }))
    micRecorder.clear()
    recording = false
    if (!scheduler.running) ctx.suspend()
  } else {
    if (mic) {
      if (start) ctx.resume()
      micRecorder.record()
      recording = true
    } else console.error("won’t record mic if it ain’t opened !")
  }
}

function cutSample(infos) {
    if (!buffers[infos.fromSoundPath]) {console.error(infos.fromFileName + " ain’t loaded, cannot cut");return;}

    let buf = buffers[infos.fromSoundPath]
      // TODO maybe round ?
      , start = infos.percents[0] * buf.length - 1
      , end = infos.percents[1] * buf.length + 1
      , newBuf = new AudioBuffer(
        { length : end - start
        , numberOfChannels : buf.numberOfChannels
        , sampleRate : buf.sampleRate
        })

    for (let i = 0 ; i < buf.numberOfChannels ; i++) {
        let chan = buf.getChannelData(i).slice(start, end)
        newBuf.copyToChannel(chan, i)
    }

    app.ports.gotNewSample.send(
        { type : "cut"
        , from : infos.fromSoundPath
        , file : new File([audioBufferToWav(newBuf)], infos.newFileName + ".wav", {type: "audio/wav"})
        })
}

function engine(o) {
  let model = null
  switch ( o.action ) {
    case "stopReset" :
        scheduler.stop()
        break;
    case "playPause" :
        scheduler.startThenPlay(o.gears)
        break;
    case "mute" :
        model = o.beadIndexes.reduce(
            (acc, v) => {
              if (acc && acc.subWheels) return acc.subWheels[v]
            }
          , scheduler.playingTopModels[o.id]
          )
        if (model) {
          model.mute = o.value
          model.updateVolume()
        }
        break;
    case "volume" :
        model = o.beadIndexes.reduce(
            (acc, v) => {
              if (acc && acc.subWheels) return acc.subWheels[v]
            }
          , scheduler.playingTopModels[o.id]
          )
        if (model) {
          model.volume = o.value
          model.updateVolume()
        }
        break;
    }
}

function drawSamples(id, samples) {
  let canvas = document.getElementById(id)
    , ctx = canvas.getContext('2d')
    , {width, height} = canvas
    , pxPerSample = width / samples.length

  ctx.clearRect(0, 0, width, height)

  ctx.strokeStyle = 'black'
  ctx.beginPath()
  ctx.moveTo(0, height / 2)
  ctx.lineTo(width, height / 2)
  ctx.stroke()

  ctx.strokeRect(0, 0, width, height)

  if (pxPerSample < 0.5) {
    for (let x = 0 ; x < width ; x++) {
      let px = samples.slice(Math.floor(x / pxPerSample), Math.floor((x + 1) / pxPerSample))
        , minPoint = (Math.min.apply(null, px) + 1) * height / 2
        , maxPoint = (Math.max.apply(null, px) + 1) * height / 2
      ctx.strokeStyle = 'black'
      ctx.beginPath()
      ctx.moveTo(x, minPoint)
      ctx.lineTo(x, maxPoint)
      ctx.stroke()

      let rms = Math.sqrt(px.reduce((acc,v,i,a) => acc + Math.pow(v, 2)) / px.length)
        , minRmsPoint = (1 - rms) * height / 2
        , maxRmsPoint = (1 + rms) * height / 2
      if (minRmsPoint > minPoint && maxRmsPoint < maxPoint) {
          ctx.strokeStyle = 'gray'
          ctx.beginPath()
          ctx.moveTo(x, minRmsPoint)
          ctx.lineTo(x, maxRmsPoint)
          ctx.stroke()
      }
    }
  } else {
    ctx.strokeStyle = 'black'
    ctx.beginPath()
    ctx.moveTo(0, (samples[0] + 1) * height / 2)
    for (let i = 1 ; i < samples.length ; i++) {
      ctx.lineTo(i * pxPerSample, (samples[i] + 1) * height / 2)
    }
    ctx.stroke()
  }
}
