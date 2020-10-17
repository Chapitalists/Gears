const app = Elm.Main.init({flags : {width : window.innerWidth, height : window.innerHeight}})

if (app.ports.loadSound) app.ports.loadSound.subscribe(loadSound)
if (app.ports.toEngine) app.ports.toEngine.subscribe(engine)
if (app.ports.toggleRecord) app.ports.toggleRecord.subscribe(toggleRecord)
if (app.ports.requestSoundDraw) app.ports.requestSoundDraw.subscribe(drawSound)
if (app.ports.requestCutSample) app.ports.requestCutSample.subscribe(cutSample)
//if (app.ports.openMic) app.ports.openMic.subscribe(openMic)
//if (app.ports.inputRec) app.ports.inputRec.subscribe(inputRec)

const buffers = {}
    , ro = new ResizeObserver(sendSize)
    , ctx = new AudioContext()
ctx.suspend()
//    , ctx = new AudioContext()
//    , nodeToRecord = Tone.context._context.createGain()
//    , recorder = new Recorder(nodeToRecord)
//    , mic = new Tone.UserMedia()
//    , micToRecord = Tone.context.createGain()
//    , micRecorder = new Recorder(micToRecord)
//Tone.Master.connect(nodeToRecord)
//mic.connect(micToRecord)
ro.observe(document.getElementById('svgResizeObserver'))

let playing = {}

let deb = null

function sendSize(entries) {
    app.ports.newSVGSize.send(entries[0].contentRect)
}

function drawSound(soundName) {
  if (buffers[soundName]) {
    drawSamples(Array.from(buffers[soundName].getChannelData(0))) // TODO mix channels ?
    app.ports.soundDrawn.send(soundName)
  } else console.log(soundName + ' isn’t loaded, cannot draw')
}

function loadSound(soundName) {
  if (buffers[soundName]) {
    app.ports.soundLoaded.send(soundName + ' already Loaded')
  } else {
    createBuffer(soundName).then(b => {
      buffers[soundName] = b
      loadOk(soundName)
    }).catch(err => loadErr(err, soundName))
  }
}

async function createBuffer(soundName) {
  const response = await fetch('./sons/' + soundName)
      , arrayBuffer = await response.arrayBuffer()
      , audioBuffer = await ctx.decodeAudioData(arrayBuffer)
  return audioBuffer
}

function loadOk(soundName) {
  app.ports.soundLoaded.send(
  { path : soundName
  , length : buffers[soundName].duration
  })
}

function loadErr(err, soundName) {
  console.log(err)
  app.ports.soundLoaded.send(soundName + ' got ' + err)
}

function toggleRecord(bool) {
    if (bool) recorder.record()
    else {
        recorder.stop()
        recorder.exportWAV(bl => app.ports.gotRecord.send(URL.createObjectURL(bl)))
        recorder.clear()
    }
}

//function openMic() {
//  Tone.start()
//  mic.open().then(
//    () => app.ports.micOpened.send(null)
//  ).catch(console.error)
//}
//
//function inputRec(name) {
//  if (name) {
//    micRecorder.stop()
//    micRecorder.exportWAV(bl => app.ports.gotNewSample.send(new File([bl], name + ".wav", {type: "audio/wav"})))
//    micRecorder.clear()
//  } else if (mic.state == "started") micRecorder.record()
//  else console.error("won’t record mic if it ain’t opened !")
//}

function cutSample(infos) {
    if (!buffers[infos.fromFileName]) {console.error(infos.fromFileName + " ain’t loaded, cannot cut");return;}

    let buf = buffers[infos.fromFileName]
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

    app.ports.gotNewSample.send(new File([audioBufferToWav(newBuf)], infos.newFileName + ".wav", {type: "audio/wav"}))
}

function engine(o) {
  let model = null
  switch ( o.action ) {
    case "stopReset" :
        for ( id in playing) {
            stop(playing[id])
        }
        playing = {}
        break;
    case "playPause" :
        let t = Tone.now()+0.1
        o.gears.map(g=>playPause(g,t))
        break;
    case "mute" :
        model = o.beadIndexes.reduce((acc, v) => {if (acc && acc.players) return acc.players[v]}, playing[o.id])
        if (model) {
            model.mute = o.value
            setVolume(model)
        }
        break;
    case "volume" :
        model = o.beadIndexes.reduce((acc, v) => {if (acc && acc.players) return acc.players[v]}, playing[o.id])
        if (model) {
            model.volume = o.value
            setVolume(model)
        }
        break;
    }
}

function playPause(model,t) {
    if (!playing[model.id]) {
        playing[model.id] = prepare(model)
    }
    if (playing[model.id].paused) {
        play(playing[model.id], t, model)
    }
    else {
        pause(playing[model.id], t)
    }
}

function drawSamples(samples) {
  setTimeout(() => {
      let canvas = document.getElementById('waveform')
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
  }, 10)
}
