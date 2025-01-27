// TODO to prevent rounding, all calculations making time progress should be in scheduler timespace
// presently, scheduling values get sometimes incremented by values coming from durations
// Or do everything in percentage ?

const playPauseLatency = .1
    , decimalCount = 9
    , ctx = new AudioContext()
    , masterGain = ctx.createGain()
let auxGains = []
ctx.suspend()
masterGain.connect(ctx.destination)

ctx.audioWorklet.addModule("./lib/phase-vocoder.js")

let scheduler = {
    interval : playPauseLatency * 250
  , lookAhead : 2000
  , running : false
  , initialized : false
  , intervalId : -1
  , startTime : -1

  , getTime() {
    if (!this.running) return -1;
    return ctx.currentTime - this.startTime
  }
  , toCtxTime(t) {
    if (!this.running) return -1;
    return t + this.startTime
  }

  , startThenPlay(topGears, isMix) {
    if (this.running) this.playPause(topGears)

    else {
      this.running = true

      if (!this.initialized) { // mix initialization
        this.isMix = isMix
        if (isMix) auxGains = []
      }

      ctx.resume().then(() => {
        this.startTime = ctx.currentTime
        this.playPause(topGears) // prepare all, creates auxGains

        if (this.isMix && !this.initialized) { // has to prepare merger
          ctx.destination.channelCount = auxGains.length
          this.merger = ctx.createChannelMerger(auxGains.length)
          this.merger.connect(masterGain)
          auxGains.map((v,i) => v.connect(this.merger, 0, i))
          this.initialized = true
        }

        this.intervalId = setInterval(() => this.work(), this.interval)
        this.work()
        this.nextRequestId = requestAnimationFrame(() => this.draw())
      })
    }
  }

  , stop() {
    if (!this.running) return;

    clearInterval(this.intervalId)
    cancelAnimationFrame(this.nextRequestId)

    this.running = false
    if (this.isMix) {
      auxGains = []
      this.merger = null
      this.isMix = false
      this.initialized = false
    }

    let stopWheel = model => {
      if (model.soundPath) {
        model.players.forEach(pl => pl.node.stop())
      }
      if (model.collar || model.mobile) {
        model.subWheels.forEach(stopWheel)
      }
    }
    for (let id in this.playingTopModels) {
      stopWheel(this.playingTopModels[id])
    }

    for (let model of this.modelsToDraw) {
      model.view.moveTo(0)
    }

    ctx.suspend()

    this.intervalId = -1
    this.nextRequestId = -1
    this.startTime = -1
    this.modelsToDraw = []
    this.playingTopModels = {}
  }


  , playingTopModels : {}
  , prepare(t, model, destination, parentRate, parentPitch, parentAuxed) {
    // TODO this is creating a new func instance for each method for each model
    // It’s bad!! Should be in proto ?
    model.lastScheduledTime = t
    model.playPauseTimes = [
        {date : 0, play : false, percent : 0, done : true} // used by draw, percent of animation loop
      , {date : t, play : false, percent : 0, done : true} // and one at t to prevent past scheduling warn
    ]
    model.lastPlayPauseIndexAt = function(now) {
      for (let i = this.playPauseTimes.length - 1 ; i >= 0 ; i--)
        if (this.playPauseTimes[i].date <= now)
          return i
      return -1
    }

    let gain = ctx.createGain()
      , auxed = parentAuxed

    if (destination === null) { // topGear mixing
      gain.channelCountMode = 'explicit'
      gain.channelCount = 1 // downMix to mono
      let channel = (model.channel || 1) - 1 // channel 0 defaults to 1, indexes are from 0
        , aux = auxGains[channel] || ctx.createGain()
      gain.connect(aux)
      auxGains[channel] = aux
      auxed = true
    } else {
      gain.connect(destination)
      if (!parentAuxed && model.channel && auxGains[model.channel]) {
        gain.connect(auxGains[model.channel])
        auxed = true
      }
    }
    model.gainNode = gain
    model.updateVolume = function() {
      this.gainNode.gain.value = this.mute ? 0 : this.volume
    } // TODO volume should rather be in dB
    model.updateVolume()


// TODO from main wheel to engine
/* //// WHEEL PART
 * wheelId
 * interval
 * mute // UNCHANGED should be in sound ?
 * volume // UNCHANGED should be in sound ?
 * wheelStartPercent // ADAPTED
 * view 
 *
 * //// PUPIL PART
 * pupilDuration // ADAPTED
 *
 * //// SOUND PART
 * soundPath // UNCHANGED
 * soundPercents // ADAPTED
 * 
 * //// NEEDED
 * soundStartPercent or pupilStartPercent
 */
      ////// PUPIL ADAPTER
//      model.loopPercents = model.soundPercents // TODO chose a name !
//      model.length = model.pupilDuration
//      model.startPercent = model.wheelStartPercent
//      model.soundStartPercent = 0
      ////// END PUPIL ADAPTER

    if (model.soundPath) this.prepareSound(t, model, parentRate)

    if (model.interval) this.prepareInterval(t, model, parentRate)
    
    if (model.collar) this.prepareCollar(t, model, parentRate)

    if (model.mobile) this.prepareMobile(t, model, parentRate)

    model.realLength = model.length / parentRate
    model.lengthBeforeParentRate = model.length
    model.length = model.realLength

    if (model.view && model.id) {
      let el = document.getElementById(model.id)
        , anim = el.getElementsByClassName('anim')[0]
        , tr = svg.createSVGTransform()
        , sc = svg.createSVGTransform()
      tr.setRotate(0,0,0)
      sc.setScale(0,0)
      el.transform.baseVal.initialize(tr)
      anim.transform.baseVal.initialize(sc)

      model.view = {
          tr : tr
        , sc : sc
        , moveTo : function (percent) {
          this.tr.setRotate(percent * 360, 0, 0)
          this.sc.setScale(percent, percent)
        }
      }

      this.modelsToDraw.push(model)
    }
    return model
  }

  , playPause(topGears) {
    let t = this.getTime() + playPauseLatency
    for (let model of topGears) {
      if (!this.playingTopModels[model.id])
        this.playingTopModels[model.id] = this.prepare(t, model, this.isMix ? null : masterGain, 1, 1)
      model = this.playingTopModels[model.id]

      let running = model.playPauseTimes[model.playPauseTimes.length - 1].play

      if (running) {
        model.playPauseTimes.push({date : t, play : false})
      } else {
        model.playPauseTimes.push({date : t, play : true})
      }
    }
  }

  , work() {
    let now = this.getTime()
      , max = now + this.lookAhead / 1000
    for (let id in this.playingTopModels) {
      this.schedule(this.playingTopModels[id], now, max)
    }
  }

  , schedule(model, now, max) {
// TODO split into funcs per type (pauseMobile, startMobile, playMobile, etc…) for readability
    let ppt = model.playPauseTimes
    // For now, considering that playPauseTimes is filled chronologically and alternatively of play and pause
    // This is the assumption of user play and pause
    // collar or another source of play pause should manage their specificities
    // An item in PPT is marked as done by advanceState after it has been managed in schedule
    // it shoult then has a percent filled in

    // Clean play pause events before last
    ppt.splice(
      0,
      Math.min(
        ppt.findIndex(v => !v.done),
        ppt.findIndex(v => v.date >= Math.min(now, model.lastScheduledTime))
      ) - 1
    )

    let nextStateIndex = ppt.findIndex(v => !v.done) // Next is first not done
      , nextState = ppt[nextStateIndex]
      , lastState = ppt[nextStateIndex - 1] || ppt[ppt.length - 1]
      , scheduleTime = nextState ? Math.min(nextState.date, model.lastScheduledTime) : model.lastScheduledTime
    // advanceState only puts the done flag, but percent should have been filled
    // TODO so if done is only a flag indicating that percent is filled, it’s a second source of truth and should disappear ?
      , advanceState = () => {
        nextState.done = true
        lastState = nextState
        nextState = ppt[++nextStateIndex]
      }

    while (scheduleTime < max) {

      let t = scheduleTime
      if (now > scheduleTime) console.error("scheduler is late, now : " + now + " scheduler : " + t)

      if (lastState.play) { // If we’re playing

        if (nextState && nextState.date < max) { // And should pause

          let safeNextDate = safeFloat(nextState.date)
          if (safeNextDate < safeFloat(t) || safeNextDate < safeFloat(model.lastScheduledTime)) { // If we sheduled ahead of next
            t = nextState.date // Bring back the time and undo
            if (t <= now) console.error("undoing the past, now : " + now + " scheduler : " + t)

            if (model.soundPath) this.undoSound(t, nextState, model, now)

            if (model.interval) this.undoInterval()

            if (model.collar) this.undoCollar(t, nextState, model, now)

            // nothing to undo in mobile, normal pause
            if (model.mobile) this.pauseMobile(t, lastState, nextState, model)

          } else { // Normal pause

            if (model.soundPath) this.pauseSound(t, nextState, model)

            if (model.interval) this.pauseInterval()

            if (model.collar) this.pauseCollar(t, nextState, model)

            if (model.mobile) this.pauseMobile(t, lastState, nextState, model)

            t = nextState.date

          }

          advanceState()

        } else { // And keep playing

          if (model.soundPath) t = this.playSound(t, max, model)
          
          if (model.interval) t = this.playInterval(t, max, model)

          if (model.collar) t = this.playCollar(t, max, model)

          if (model.mobile) t = this.playMobile(t, max)

        }

      } else { // If we’re paused

        if (nextState && nextState.date < max) { // And should play
          t = nextState.date
          if (t <= now) console.error("starting in the past, now : " + now + " scheduler : " + t)

          let contentPercent = clampPercent(lastState.percent + model.startPercent)

          if (model.soundPath) t = this.unpauseSound(t, contentPercent, model)

          if (model.interval) t = this.unpauseInterval(t, contentPercent, model)

          if (model.collar) t = this.unpauseCollar(t, contentPercent, model)

          if (model.mobile) this.unpauseMobile(t, contentPercent, model)

          nextState.percent = lastState.percent
          
          advanceState()

          if (model.mobile) { // now nextState has advanced !
            if (nextState) t = nextState.date
            else t = max
          }

        } else { // And keep pausing

          t = max

        }
      }
      scheduleTime = t
    }
    model.lastScheduledTime = scheduleTime

    if (model.subWheels) {
      model.subWheels.forEach(v => this.schedule(v, now, max))
    }
  }
  
  , prepareSound(t, model, parentRate) {

    // WARNING in model, startPercent is of whole sound, here it’s of content
    model.startPercent = (model.startPercent - model.loopPercents[0]) / (model.loopPercents[1] - model.loopPercents[0])
    model.players = []
    // TODO this is creating a new func instance for each method for each model
    model.freePlayer = function(startTime) {
      setTimeout(
          () => this.players = this.players.filter(
            v => v.startTime !== startTime
              //TODO should’em be safeFloats ?
          )
          , scheduler.lookAhead
      )
    }
    model.buffer = buffers[model.soundPath]
    // TODO beware, buffer duration could differ from saved duration in Elm model (due to resampling)
    // probably it’s preferable to use saved duration from elm
    // but, is it compensated by downward TODO ? (in schedulePlayer)
    model.bufferDuration = model.buffer.duration
    model.loopStartDur = model.loopPercents[0] * model.bufferDuration
    model.loopEndDur = model.loopPercents[1] * model.bufferDuration
    model.duration = model.loopEndDur - model.loopStartDur
    model.rate = parentRate * model.duration / model.length
  }
  , playSound(t, max, model) {
    let newPlayers = this.scheduleLoop(t, max, model)
    model.players = model.players.concat(newPlayers)
    return model.players[model.players.length - 1].stopTime
  }
  // TODO doesn’t use lastState ?
  , pauseSound(t, pauseState, model) {
    if (pauseState.date <= t) { // No need to play more, even partially

      pauseState.percent = clampPercent(0 - model.startPercent)

    } else {
      let newPlayers = []
        , startTime

      if (pauseState.date > t + model.length) { // At least one full play
        newPlayers = this.scheduleLoop(t, pauseState.date - model.length, model)
        startTime = newPlayers[newPlayers.length - 1].stopTime
      } else { // Just a partial play
        startTime = t
      }
      let length = pauseState.date - startTime
        , duration = length * model.rate

      newPlayers.push(
        this.schedulePlayer(startTime, model, model.loopStartDur, duration, length)
      )

      model.players = model.players.concat(newPlayers)

      pauseState.percent = clampPercent(length / model.length - model.startPercent)
    }
  }
  , unpauseSound(t, contentPercent, model) {
    let offsetDur = contentPercent * model.duration + model.loopStartDur
      , newPlayer = this.scheduleStart(t, model, offsetDur)
    model.players.push(newPlayer)
    return newPlayer.stopTime
  }
  , undoSound(t, pauseState, model, now) {
    for (let pl of model.players) {
      if (pl.startTime <= t && t <= pl.stopTime) {
        pl.node.stop(this.toCtxTime(t))
        pauseState.percent = clampPercent((t - pl.startTime) / model.length + pl.startContentPercent / model.duration - model.startPercent)
      }
      if (pl.startTime > t) pl.node.stop()
    }
    if (!isFinite(pauseState.percent)) {
      // TODO sometimes when Playing a collar, we get there for a bead that tries to undo nothing (found a misregistered lastScheduledTime
      console.error("couldn’t find pausing player, unknown pause percent : t, now ", t, now, pauseState, model)
      pauseState.percent = 0
    }
  }
  
  , prepareInterval(t, model, parentRate) {
    model.rate = parentRate * model.duration / model.length
      
    // 3 cas : pupille, rosace ratio, rosace sans ratio
    // 1 contenu, n contenus, l’infini de contenus (qui disparaissent ? qui se recyclent ?)
// TODO Rosace
//      if (model.ratio) {
//        model.pupil.length = model.interval * model.ratio
//      }
    // MEMBERS NEEDED :
    // interval
    // pupil(s)
    // ratio(s) (if pupil is exactly n times interval, only spawn n occurences) (N or N/N ?)
    // startPercent
    // id
    // mute
    // volume
    // view ?
    // TODO handle startPercent if we want content to be playing already
    // PROPOSAL negative startPercent = no / positive startPercent = yes => to compute
    // THEN pour les rosaces, startPercent > 1 => plusieurs contents déjà en cours
    model.content = this.prepare(t, model.pupil, model.gainNode, parentRate)
// subwheelS is an array cause a rosace will contain multiple subWheels later
    model.subWheels = [model.content]
// TODO Rosace
//      if (model.ratio) {
//        for (let i = 0 ; i < ratio ; i++) {
//          model.subWheels.push(this.spawnContent(t, model.contents[i]))
//        }
//      }
  }
  , playInterval(t, max, model) {
    while (t <= max) {
      if (model.ratio) {
        // pop last and unshift ?
        // est-ce que ça importe ? Dans quel cas serait-il utile d’avoir les contenus dans l’ordre ?
      } else {
        let newContent = this.spawnContent(t, model.contents)
      }
      t += model.interval
    }
  }
  , pauseInterval() {}
  , unpauseInterval(t, contentPercent, model) {
    // this is just launching, could be done in play
    // TODO should unpause any paused subwheels
    // TODO => chose a way to notice if a subwheel is paused or waiting its launch time
    if (contentPercent === 0) {
      model.subWheels.forEach(v => v.playPauseTimes.push({date : t, play : true}))
      return t + model.interval
    }
  }
  , undoInterval() {}
  
  , prepareCollar(t, model, parentRate) {
    // WARNING collarOffset : ignore collar startPercent because it’s broken now (see todolist)
    model.startPercent = 0

    model.nextBead = 0
    model.duration = model.collar.duration
    model.beadsDurs = model.collar.beads.map(v => v.length)
    model.beadsCumulDurs = []
    for (let cumul = 0, i = 0 ; i < model.beadsDurs.length ; i++) {
      cumul += model.beadsDurs[i]
      model.beadsCumulDurs.push(cumul)
    }
    model.rate = parentRate * model.duration / model.length
    model.subWheels = model.collar.beads.map(
      v => this.prepare(t, v, model.gainNode, model.rate)
    )
  }
  , playCollar(t, max, model) {
    while (t <= max) {
      let length = model.beadsDurs[model.nextBead] / model.rate
      this.scheduleBead(t, model, length)
      t += length
    }
    return t
  }
  // TODO doesn’t use playState ?
  , pauseCollar(t, pauseState, model) {
    let nextLength = model.beadsDurs[model.nextBead] / model.rate
    while (t + nextLength <= pauseState.date) {
      this.scheduleBead(t, model, nextLength)
      t += nextLength
      nextLength = model.beadsDurs[model.nextBead] / model.rate
    }

    let length = pauseState.date - t
      , cumul = model.beadsCumulDurs[model.nextBead - 1] / model.rate || 0
    this.scheduleBead(t, model, length, false)

    pauseState.percent = clampPercent((cumul + length) / model.length)
  }
  , unpauseCollar(t, contentPercent, model) {
    let cumulDur = model.beadsCumulDurs[model.nextBead]
      , offsetDur = contentPercent * model.duration
      , length = (cumulDur - offsetDur) / model.rate
    this.scheduleBead(t, model, length)
    return t + length
  }
  , undoCollar(t, pauseState, model, now) {//WIP TODO
    // undo playPause of subWheels
    let pausingBeadIndex = -1
      , beadPlayTime
    for (let i = 0 ; i < model.subWheels.length ; i++) {
      let sub = model.subWheels[i]
        , lastStateIndex = sub.lastPlayPauseIndexAt(t)
        , subLastState = sub.playPauseTimes[lastStateIndex]

      sub.playPauseTimes = sub.playPauseTimes.slice(0, lastStateIndex + 1)

      if (subLastState.play) {
        pausingBeadIndex = i
        beadPlayTime = subLastState.date
        sub.playPauseTimes.push({date : t, play : false})
      } else { // WARNING hack to force the subWheel undoing
        // TODO this hack could be replaced generally by transmitting the undo info to subWheels
        sub.playPauseTimes.push({date : t, play : true})
        sub.playPauseTimes.push({date : t, play : false})
      }
    }
    if (pausingBeadIndex === -1) {
      console.error("couldn’t find pausing bead, unknown pause percent and next Bead", now, pauseState)
      pauseState.percent = 0
      model.nextBead = 0
    } else {
      let length = t - beadPlayTime
        , cumul = model.beadsCumulDurs[pausingBeadIndex - 1] / model.rate || 0
      pauseState.percent = clampPercent((cumul + length) / model.length)
      model.nextBead = pausingBeadIndex
    }
  }
  
  , prepareMobile(t, model, parentRate) {
    // WARNING mobileOffset : ignore mobile startPercent because it’s broken now (see todolist)
    model.startPercent = 0

    model.duration = model.mobile.duration
    model.rate = parentRate * model.duration / model.length
    model.subWheels = model.mobile.gears.map(
      v => this.prepare(t, v, model.gainNode, model.rate)
    )
  }
  , playMobile(t, max) {
    return max
  }
  , pauseMobile(t, playState, pauseState, model) { // playState is last, pauseState is next
    model.subWheels.forEach(
      v => v.playPauseTimes.push({date : pauseState.date, play : false})
    )
    pauseState.percent = clampPercent(
      playState.percent
      + (pauseState.date - model.lastStartTime) / model.length
    )
  }
  , unpauseMobile(t, contentPercent, model) { // t is playTime
    model.lastStartTime = t
    model.subWheels.forEach(
      v => v.playPauseTimes.push({date : t, play : true})
    )
  }

  , scheduleBead(t, model, length, advanceBead = true) {
    // playing and pausing beads to keep track of a mobile content state
    let beadPPT = model.subWheels[model.nextBead].playPauseTimes
    beadPPT.push({date : t, play : true})
    beadPPT.push({date : t + length, play : false})
    if (advanceBead) model.nextBead = (model.nextBead + 1) % model.subWheels.length
  }
  , scheduleLoop(t, maxT, model) {
    return [
      this.schedulePlayer(t, model, 0, model.duration, model.length)
    ].concat(t + model.length >= maxT ? [] : this.scheduleLoop(t + model.length, maxT, model))
  }
  , scheduleStart(t, model, contentPercent) {
    let dur = (1 - contentPercent) * model.duration
      , len = dur / model.rate
    return this.schedulePlayer(t, model, contentPercent, dur, len)
  }
  , schedulePlayer(t, model, contentPercent, duration, length) {
    let player = ctx.createBufferSource()
      , ctxStartTime = t + this.startTime
      , ctxStopTime = ctxStartTime + length
    player.buffer = model.buffer
    player.playbackRate.value = model.rate // TODO to go realTime rate, maybe use setValueAtTime
    player.connect(model.pitcher || model.gainNode)
    player.onended = () => model.freePlayer(t)
    player.start(ctxStartTime, contentPercent * model.duration + model.loopStartDur, duration)
    player.stop(ctxStopTime) // TODO stop and duration in schedulePlayer do the same thing, is it good ? Does it compensate for inexact buffer.duration ? See upward in prepare sound
    return {
        node : player
      , startTime : t
      , stopTime : t + length
      , startContentPercent : contentPercent
    }
  }


  , nextRequestId : -1
  , modelsToDraw : []

  , draw() {
    // TODO keeps drawing event when paused. is it bad ?
    // TODO percent keeps growing, will it overflow ?
    let now = scheduler.getTime()
    for (let model of this.modelsToDraw) {
      let lastStateIndex = model.lastPlayPauseIndexAt(now)
        , lastState = model.playPauseTimes[lastStateIndex]
        , percent = 0

      if (!lastState || !lastState.done) { // TODO what is this case ? an error ?
        lastState = model.playPauseTimes[--lastStateIndex]
        console.error("no lastState in draw (or not done), taking previous instead :", lastState, "index", lastStateIndex, "model", model)
      }

      if (lastState && isFinite(lastState.percent)) {
        percent = clampPercent(lastState.play ?
            lastState.percent + (now - lastState.date) / model.length :
            lastState.percent)
      } else console.error("lastState was not done in draw :", lastState, "time is", now, "model", model)

      model.view.moveTo(safeFloat(percent))
    }
    this.nextRequestId = requestAnimationFrame(() => this.draw())
  }
}

function clampPercent(p) {
  return p - Math.floor(p)
}

function safeFloat(f) {
  let big = Math.pow(10, decimalCount)
  return Math.round(f * big)/big
}
