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
        if (playing.clocks) stop(playing) // UNUSED CURRENTLY
        else for ( id in playing) {
            stop(playing[id])
            playing[id].view.animate().play().finish()
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
    }
}

function playTopCollar(beads, baseId) {
    let model = playing
      , part = []
      , t = 0
    for (let i in beads) {
        part.push([t, i])
        t += beads[i].length
        beads[i].id = baseId + i
    }
    model.beads = beads.map(v => prepare(v))
    model.player = new Tone.Part( ((t, i) => {model.beads[i].once(t)}), part )
    model.player.loopEnd = t
    model.player.loop = true
    model.player.start()
    Tone.Transport.start()
}

function playPauseTopCollar(beads) {
    Tone.Transport.pause()
    Tone.Transport.start()
}

function playPause(model,t) {
    if (!playing[model.id]) {
        playing[model.id] = prepare(model)
        playing[model.id].view = SVG.adopt(document.getElementById(model.id))
        playing[model.id].view.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0}).loop()
    }
    if (playing[model.id].paused) {
        play(playing[model.id], t, model)
        playing[model.id].view.animate().play()
    }
    else {
        pause(playing[model.id], t)
        playing[model.id].view.animate().pause()
    }
}
