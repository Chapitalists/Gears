const app = Elm.Main.init({flags : {width : window.innerWidth, height : window.innerHeight}})

if (app.ports.loadSound) app.ports.loadSound.subscribe(createBuffer)
if (app.ports.toEngine) app.ports.toEngine.subscribe(engine)

const buffers = {}
    , playing = {}
    , ro = new ResizeObserver(sendSize)
ro.observe(document.getElementById('svgResizeObserver'))

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
  switch ( o.action ) {
    case "stopReset" :
        for ( id in playing) stop(id)
        break;
    case "playPause" :
        o.gears.map(playPause)
        break;
    case "mute" :
        mute(o.gearId, o.value)
        break;
    case "volume" :
        changeVolume(o.gearId, o.value)
    }
}

function playPause(model) {
    if (!playing[model.id]) {
        playing[model.id] = model
        play(model.id)
    } else if (playing[model.id].paused) unpause(model.id)
    else pause(model.id)
}

function play(id) {
    let model = playing[id]
      , g = model.gear = SVG.adopt(document.getElementById(model.id))
    model.paused = false
    g.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0}).loop()
    playMusic(model)
}

function playMusic(model, parentVol = 1, parentRate = 1, parentMute = false) {
    if (model.soundName) playSound(model, parentVol, parentRate, parentMute)
    else if (model.mobile) playMobile(model, parentVol, parentRate, parentMute)
    return model
}

function pause(id) {
    let model = playing[id]
    model.paused = true
    model.gear.animate().pause()
    model.player.stop()
    model.pauseOffset = (Tone.context.now() - model.startTime) * model.rate
}

function unpause(id) {
    let model = playing[id]
    model.paused = false
    model.gear.animate().play()
    unpauseMusic(model)
}

function unpauseMusic(model, parentVol = 1, parentMute = false) {
    if (model.soundName) unpauseSound(model, parentVol, parentMute)
    else if (model.mobile) unpauseMobile(model, parentVol, parentMute)
}

function stop(id) {
    let model = playing[id]
    if (!model) return;
    model.gear.animate().play().finish()
    model.player.stop()
    playing[id] = null
}

function mute(id, mute) {
    let model = playing[id]
    if (!model) return;
    model.mute = mute
    model.setVolume()
}

function changeVolume(id, volume) {
    let model = playing[id]
    if (!model) return;
    model.volume = volume
    model.setVolume()
}

function playSound(model, pv, pr, pm) {
    let s = model.player = new Tone.Player(buffers[model.soundName]).toMaster()
    s.loop = true
    model.setVolume = function (mod = 1, mute = false) {
        mute || this.mute ? this.player.mute = true : this.player.volume.value = ((this.volume * mod) - 1) * 60
    }
    model.setVolume(pv, pm)
    s.playbackRate = model.rate = pr * s.buffer.duration / model.length
    s.start()
    model.startTime = Tone.context.now()
}

function unpauseSound(model, pv, pm) {
    model.setVolume(pv, pm)
    model.player.start(Tone.context.now(), model.pauseOffset)
    model.startTime = Tone.context.now() - model.pauseOffset / model.rate
}

function playMobile(model, pv, pr, pm) {
    model.rate = pr * model.mobile.length / model.length
    model.player = model.mobile.gears
                    .map(m => playMusic(m, model.volume * pv, model.rate * pr, model.mute || pm))
    model.player.stop = function () {this.map(m => m.player.stop())}
    model.setVolume = function (mod = 1, mute = false) {
        this.mobile.gears.map(m => m.setVolume(this.volume * mod, this.mute || mute))
    }
}

function unpauseMobile(model, pv, pm) {
    model.player.map(m => unpauseMusic(m, model.volume * pv, model.mute || pm))
}
