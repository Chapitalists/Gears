// TODO changeVolume, ChangeMute, Animation

function prepare(model, rate = 1) {
    model.paused = true
    model.pauseOffset = 0
    if (model.soundName) {
        model.player = new Tone.Player(buffers[model.soundName]).toMaster()
        model.player.playbackRate = model.rate = rate * model.player.buffer.duration / model.length
        model.player.loop = true
    }
    if (model.mobile) {
        model.rate = rate * model.mobile.length / model.length
        model.gears = model.mobile.gears.map(v => prepare(v, model.rate))
    }
    if (model.collar) {
        model.rate = (rate * model.collar.length / model.length) || 1 // TODO when preparing top collar, no model.length
        let part = []
          , t = 0
        for (let i in model.collar.beads) {
            part.push([t,i])
            t += model.collar.beads[i].length
        }
        model.players = model.collar.beads.map(v => prepare(v, model.rate))
        model.player = new Tone.Part( ((t, i) => {model.current = i;once(model.players[i], t)}), part )
        model.player.loopEnd = t
        model.player.loop = true
    }
    return model
}

function callback(t, i) {
    model.current = i
    once(model)
}

function once(model, t) {
    model.player.loop = false
    if (t) model.player.start(t)
    else model.player.start(Tone.now(), model.pauseOffset)
}

function play(model, newModel = {}, volume = 1, mute = false) { // TODO What if no new model (first play)
    model.paused = false
    model.volume = newModel.volume || 1 // TODO cf first TODO
    model.mute = newModel.mute || false // TODO cf first TODO
    model.startTime = Tone.now() - model.pauseOffset / model.rate
    if (model.soundName) {
        if (mute || model.mute) model.player.mute = true
        else model.player.volume.value = ((model.volume * volume) - 1) * 60
        model.player.start(Tone.now(), model.pauseOffset)
    }
    if (model.mobile) {
        model.gears.map((v,i) => play(v, model.gears[i], model.volume * volume, model.mute || mute))
    }
    if (model.collar) {
        model.players.map((v,i) => {
            v.mute = newModel.collar.beads[i].mute
            v.volume = newModel.collar.beads[i].volume
            if (v.mute || mute || model.mute) v.player.mute = true
            else v.player.volume.value = ((model.volume * volume * v.volume) - 1) * 60
        })
        model.player.start(Tone.now(), model.pauseOffset)
        if (model.current) once(model.players[model.current])
    }
}

function pause(model) {
    model.paused = true
    model.pauseOffset = (Tone.now() - model.startTime) * model.rate
    if (model.soundName) {
        model.player.stop()
    }
    if (model.mobile) {
        model.gears.map(pause)
    }
    if (model.collar) {
        model.player.stop()
        pause(model.players[model.current])
    }
}

function stop(model) {
    if (model.soundName) model.player.stop()
    if (model.mobile) model.gears.map(stop)
    if (model.collar) {
        model.player.stop()
        stop(model.players[model.current])
    }
}