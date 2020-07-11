// TODO clean changeVolume, use Tone.Draw

// pauseOffset = sound time after the start point = position of the animation in sound time
// startTime = date of the corresponding start = date of the last 0% of the animation
// startPercent = percent of the whole sound where to start

function prepare(model, rate = 1) {
    model.paused = true
    model.pauseOffset = 0
    if (model.view && model.id) {
        model.view = SVG.adopt(document.getElementById(model.id))
       /* model.once
          ? model.view.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0}).pause()
          : */model.view.animate(model.length * 1000).transform({rotation:360, cx:0, cy:0}).loop().pause()
        let flip = document.querySelector('#' + model.id + '>.flip')
        if (flip) model.flip = Array.from(flip.childNodes, SVG.adopt)
    }
    if (model.soundName) {
        model.player = new Tone.Player(buffers[model.soundName]).toMaster()
        model.duration = model.loopPoints[1] - model.loopPoints[0]
        model.player.playbackRate = model.rate = rate * model.duration / model.length
        model.player.setLoopPoints.apply(model.player, model.loopPoints)
        model.player.loop = true
    }
    if (model.mobile) {
        model.duration = model.mobile.length
        model.rate = rate * model.duration / model.length
        model.gears = model.mobile.gears.map(v => prepare(v, model.rate))
    }
    if (model.collar) {
        model.duration = model.collar.length // TODO when preparing top collar, duration should ignored
        model.rate = (rate * model.duration / model.length) || 1 // TODO when preparing top collar, no model.length
        model.durs = model.collar.beads.map(v => v.length / model.rate)
        let totalDur = model.durs.reduce((a,b) => a+b, 0)
        model.players = model.collar.beads.map(v => prepare(v, model.rate))
        model.clocks = model.players.map((subModel,i,a) => {
            return new Tone.Clock(t => {
                if (model.paused && (model.progPause <= t)) return;
              // if paused, clock t < progPause
                model.progClock = t
                model.current = i
                let prec = (i + a.length - 1) % a.length
                play(subModel, t, subModel, model.volume, model.mute)
                pause(a[prec], t, model.paused, true) // hence force recalculation
                if (model.paused) pause(subModel, model.progPause) // and pause next
                // console.log(i, Math.min(model.players[i].duration - model.players[i].pauseOffset, model.players[i].pauseOffset)) // TODO Small drift…
            }, 1/totalDur)
        })
    }
    setVolume(model)
    return model
}

function play(model, t, newModel = {}, volume = 1, mute = false) { // TODO What if no new model (first play)
    if (!model.paused) return;
    model.paused = false
    model.volume = newModel.volume || model.volume // TODO cf first TODO
    model.mute = newModel.mute || model.mute // TODO cf first TODO
    model.startTime = t - model.pauseOffset / model.rate
    if (model.view) {
        Tone.Draw.schedule(() => model.view.animate().play(), t)
    }
    if (model.flip && model.flip.length) {
        let n = model.flip.length
          , d = model.length / n
          , doFlip = (i, t) => () => {console.log(i,t)
                model.flip[(i+n-1)%n].attr('opacity', 0)
                model.flip[i].attr('opacity', 1)
                Tone.Draw.schedule(doFlip((i+1)%n, t+d), t)
            }
        Tone.Draw.schedule(doFlip(1,t+d), t)
    }
    if (model.soundName && model.player.output) {
        model.player.start(t, model.pauseOffset + (model.startPercent * model.player.buffer.duration))
    }
    if (model.mobile) {
        model.gears.map((v,i) => play(v, t, model.gears[i], model.volume * volume, model.mute || mute))
    }
    if (model.collar) {
        let current = 0
          , acc = 0
          , ratedOffset = model.pauseOffset / model.rate
        while (acc <= ratedOffset) {
          acc += model.durs[current]
          current = (current + 1) % model.durs.length
        }
        let modelToPlay = model.players[(current + model.durs.length - 1) % model.durs.length]
        play(modelToPlay, t, modelToPlay, model.volume, model.mute)
        acc = t + acc - ratedOffset
        for (let i = 0 ; i < model.clocks.length ; i++) {
          let j = (i + current) % model.clocks.length
          model.clocks[j].start(acc)
          acc += model.durs[j]
        }
    }
}

function pause(model, t, force = false, clocked = false) {
    if (model.paused && !force) return;
    model.paused = true
    model.pauseOffset = ((t - model.startTime) * model.rate) % model.duration
    if (model.view){//} && !clocked) {
        Tone.Draw.schedule(() => model.view.animate().pause().at((model.pauseOffset/model.length/model.rate) % 1), t)
        Tone.Draw.cancel(t+0.1)
    }
    if (model.soundName && model.player.output) {
        model.player.stop(t)
    }
    if (model.mobile) {
        model.gears.map(v => pause(v, t))
    }
    if (model.collar) {
        model.progPause = t
        if (t <= model.progClock) {
            let prec = (model.current + model.clocks.length - 1) % model.clocks.length
            pause(model.players[prec], t, true)
            pause(model.players[model.current], model.progClock)
        }
        model.clocks.map(v => v.stop(t))
        model.players.map(v => pause(v, t))
    }
}

function stop(model) {
    if (model.view) model.view.animate().play().finish().stop()
    if (model.flip) {
        Tone.Draw.cancel()
        model.flip.map((v,i,a) => v.attr('opacity', i==a.length-1?1:0))
    }
    if (model.soundName) model.player.stop().dispose()
    if (model.mobile) model.gears.map(stop)
    if (model.collar) {
        model.clocks.map(v => v.stop())
        model.players.map(stop)
    }
}

function setVolume(model, volume = 1, mute = false) {
    if (model.soundName) {
        if (mute || model.mute) {
            model.player.toMaster()
            model.player.disconnect(Tone.Master)
        }
        else {
            model.player.toMaster()
            model.player.volume.value = ((model.volume * volume) - 1) * 60
        }
    }
    if (model.mobile) model.gears.map(v => setVolume(v, model.volume * volume, model.mute || mute))
    if (model.collar) model.players.map(v => setVolume(v, model.volume * volume, model.mute || mute))
}