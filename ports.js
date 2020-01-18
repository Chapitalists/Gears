const app = Elm.Main.init({flags : {width : window.innerWidth, height : window.innerHeight}})

if (app.ports.loadSound) app.ports.loadSound.subscribe(createBuffer)
if (app.ports.toEngine) app.ports.toEngine.subscribe(engine)

const buffers = {}
    , ro = new ResizeObserver(sendSize)
ro.observe(document.getElementById('svgResizeObserver'))

let playing = {}

let deb = null

function sendSize(entries) {
    app.ports.newSVGSize.send(entries[0].contentRect)
}

function createBuffer(soundName) {
  if (buffers[soundName]) {
    app.ports.soundLoaded.send(soundName + ' already Loaded')
  } else {
    buffers[soundName] = new Tone.Buffer('./sons/' + soundName, ()=>loadOk(soundName), e=>loadErr(e, soundName))
  }
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

function engine(o) {
  let model = null
  switch ( o.action ) {
    case "stopReset" :
        if (playing.clocks) stop(playing)
        else for ( id in playing) {
            stop(playing[id])
        }
        playing = {}
        break;
    case "playPause" :
        let t = Tone.now()+0.1
        o.gears.map(g=>playPause(g,t))
        break;
    case "playCollar" :
        if (!playing.rate) playing = prepare(o)
        if (playing.paused) play(playing, Tone.now()+0.1, o)
        else pause(playing, Tone.now()+0.1)
        break;
    case "muteBead" :
        if (!playing.rate) break;
        model = playing.players[o.index]
        if (model) {
            model.mute = o.value
            setVolume(model)
        }
        break;
    case "volumeBead" :
        if (!playing.rate) break;
        model = playing.players[o.index]
        if (model) {
            model.volume = o.value
            setVolume(model)
        }
    case "mute" :
        model = playing[o.gearId]
        if (model) {
            model.mute = o.value
            setVolume(model)
        }
        break;
    case "volume" :
        model = playing[o.gearId]
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
