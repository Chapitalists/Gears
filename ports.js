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
        if (playing.player) stop(playing) // UNUSED CURRENTLY
        else for ( id in playing) {
            stop(playing[id])
            playing[id].view.animate().play().finish()
        }
        Tone.Transport.stop()
        playing = {}
        break;
    case "playPause" :
        o.gears.map(playPause)
        Tone.Transport.start()
        break;
    case "playCollar" :
        if (playing.player) {stop(playing);playing = {}}
        else {
            playing = prepare(o)
            play(o,o)
            Tone.Transport.start()
        }
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
// model.collar.beads[i].id = baseId + i

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

function playPause(model) {
    if (!playing[model.id]) {
        playing[model.id] = prepare(model)
        playing[model.id].view = SVG.adopt(document.getElementById(model.id))
        playing[model.id].view.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0}).loop()
    }
    if (playing[model.id].paused) {
        play(playing[model.id], model)
        playing[model.id].view.animate().play()
    }
    else {
        pause(playing[model.id])
        playing[model.id].view.animate().pause()
    }
}

function prepareOld(model, top = true, v = 1, r = 1, m = false) {
    if (top) {
        model.view = SVG.adopt(document.getElementById(model.id))
        model.animation = function() {return this.view.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0})}
    }
    model.paused = true
    model.pauseOffset = 0
    model.start = function (t) {
        this.paused = false
        this.changeVolume()
        this.startTime = t || Tone.now()
        this.player.restart(t)
    }
    if (model.soundName) prepareSound(model, v, r, m)
    else if (model.mobile) prepareMobile(model, v, r, m)
    else if (model.collar) prepareCollar(model, v, r, m)
    return model
}

function prepareSound(model, volume, rate, mute) {
    model.player = new Tone.Player(buffers[model.soundName]).toMaster()
    model.player.playbackRate = model.rate = rate * model.player.buffer.duration / model.length
    model.loop = function () {
        this.player.loop = true
        if (this.view) this.animation().loop()
        this.start()
    }
    model.once = function (t) {
        this.player.loop = false
        if (this.view) this.animation().play() // TODO shouldn’t ignore t !!!
        this.start(t)
    }
    model.pause = function () {
        this.paused = true
        if (this.view) this.view.pause()
        this.pauseOffset = (Tone.now() - this.startTime) * this.rate
        this.player.stop()
    }
    model.unpause = function (volume, mute) {
        this.paused = false
        this.changeVolume()
        if (this.view) this.view.play()
        this.startTime = Tone.now() - this.pauseOffset / this.rate
        this.player.start(Tone.now(), this.pauseOffset)
    }
    model.playPause = function (newModel) {
        model.volume = newModel.volume
        model.mute = newModel.mute
        this.paused ? this.unpause() : this.pause()
    }
    model.stop = function () {
        if (this.view) this.view.play().finish()
        this.player.stop()
        this.player = this.view = null
    }
    model.changeVolume = function () {
        mute || model.mute ?
            this.player.mute = true :
            this.player.volume.value = ((this.volume * volume) - 1) * 60
    }
}

function prepareMobile(model, volume, rate, mute) {
    model.rate = rate * model.mobile.length / model.length
    model.gears = model.mobile.gears
        .map(v=>prepare(v, model.volume * volume, model.rate * rate, model.mute || mute))
    model.player = {}
    model.player.restart = (t) => model.gears.map(v=>v.restart(t))
    model.loop = function () {
        if (this.view) this.animation().loop()
        this.gears.map(v => v.loop())
    }
    model.once = function (t) {
        if (this.view) this.animation().play() // TODO shouldn’t ignore t
        this.gears.map(v => v.stop(t + (this.length / this.rate))) // TODO mobile once
    }
    model.pause = function () {
        if (this.view) this.view.pause()
        this.gear.map(v => v.pause())
    }
    model.unpause = function (model, volume, mute) {
        this.changeVolume()
        if (this.view) this.view.play()
        this.gears.map((v, i) => v.unpause(model.gears[i], ))
    }
}
function prepareCollar(model) {
    let m = {}
    m.playPause = function(){}
    return m
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

function playCollar(model, pv, pr, pm) {
}

function unpauseCollar(model, pv, pm) {
}
