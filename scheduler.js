// TODO to prevent rounding, all calculations making time progress should be in scheduler timespace
// presently, scheduling values get sometimes incremented by values coming from durations
// The true perfection should be to work in samples ?


/////// WARNING the use of length and duration can be confusing
// in buffer source nodes, duration refers to the sound, counting samples from the buffer
// in my scheduler, length refers to the real time expected with the playbackRate applied
// TODO change naming to be more intelligible ?


const playPauseLatency = .1 // fixed latency for user interaction
    , decimalCount = 9 // for safeFloat, time values beeing in seconds in WebAudio
    // , maxRosaceInstances = 100 managed by interface ?
    , ctx = new AudioContext()
    , masterGain = ctx.createGain()
ctx.suspend()
masterGain.connect(ctx.destination)

let scheduler = {
    interval : 1000 * playPauseLatency / 4 // interval is in ms
  , lookAhead : 2 // work done in advance in s
  , running : false
  , intervalId : -1
  , startTime : -1

  , playingTopModels : {}

  , getTime() {
    if (!this.running) return -1;
    return ctx.currentTime - this.startTime
  }
  , toCtxTime(t) {
    if (!this.running) return -1;
    return t + this.startTime
  }

  , startThenPlay(topGears) {
    if (this.running) this.playPause(topGears)

    else {
      this.running = true

      ctx.resume().then(() => {
        this.startTime = ctx.currentTime
        this.playPause(topGears)
        this.intervalId = setTimeout(() => this.work(), 0)
        this.nextRequestId = requestAnimationFrame(() => this.draw())
      })
    }
  }

  , playPause(topGears) {
    let t = this.getTime() + playPauseLatency
    for (let model of topGears) {
      if (!this.playingTopModels[model.id])
        this.playingTopModels[model.id] = this.prepare(t, model, masterGain, 1)
      model = this.playingTopModels[model.id]

      let running = model.playPauseTimes[model.playPauseTimes.length - 1].play

      if (running) {
        model.playPauseTimes.push({date : t, play : false})
      } else {
        model.playPauseTimes.push({date : t, play : true})
      }
    }
  }

  , stop() {
    if (!this.running) return;

    clearTimeout(this.intervalId)
    cancelAnimationFrame(this.nextRequestId)

    this.running = false

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
  
  , prepare(t, model, destination, parentRate) {    
    // TODO this is creating a new func instance for each method for each model
    // It’s bad!! Should be in proto ?
    model.lastScheduledTime = t
    model.playPauseTimes = [
        {date : 0, play : false, percent : 0, done : true} // used by draw
      , {date : t, play : false, percent : 0, done : true} // and one at t to prevent past scheduling warn
    ]
    model.lastPlayPauseIndexAt = function(now) {
      for (let i = this.playPauseTimes.length - 1 ; i >= 0 ; i--)
        if (this.playPauseTimes[i].date <= now)
          return i
      return -1
    }

    let gain = ctx.createGain()
    gain.connect(destination)
    model.gainNode = gain
    model.updateVolume = function() {
      this.gainNode.gain.value = this.mute ? 0 : this.volume
    } // TODO volume should rather be in dB
    model.updateVolume()

    if (model.soundPath) this.prepareSound(t, model, parentRate)

    if (model.interval) this.prepareInterval(t, model, parentRate)
    
    if (model.collar) this.prepareCollar(t, model, parentRate)

    if (model.mobile) this.prepareMobile(t, model, parentRate)

    model.realLength = model.length / parentRate
    model.lengthBeforeParentRate = model.length
    model.length = model.realLength

    if (model.view && model.id) {
      this.prepareDraw(model)
    }
    return model
  }

  , work() {
    let now = this.getTime()
      , max = now + this.lookAhead
    for (let id in this.playingTopModels) {
      this.schedule(this.playingTopModels[id], now, max)
    }
    this.intervalId = setTimeout(() => this.work(), this.interval)
  }

  , schedule(model, now, max) { // TODO comment MORE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    let ppt = model.playPauseTimes
    // For now, considering that playPauseTimes is filled chronologically and alternatively of play and pause
    // This is the assumption of user play and pause
    // collar or another source of play pause should manage their specificities
    // An item in PPT is marked as done by advanceState after it has been managed in schedule
    // it should then has a percent filled in

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
          if (safeNextDate < safeFloat(t) || safeNextDate < safeFloat(model.lastScheduledTime)) { // If we scheduled ahead of next
            t = nextState.date // Bring back the time and undo
            if (t <= now) console.error("undoing the past, now : " + now + " scheduler : " + t)

            if (model.soundPath) this.undoSound(nextState, model, now)

            if (model.interval) this.undoInterval(lastState, nextState, model, now)

            if (model.collar) this.undoCollar(nextState, model, now)

            // nothing to undo in mobile, normal pause
            if (model.mobile) this.pauseMobile(t, lastState, nextState, model)

          } else { // Normal pause

            if (model.soundPath) this.pauseSound(t, nextState, model)

            if (model.interval) this.pauseInterval(t, nextState, model)

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

          let contentPercent = lastState.percent

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
    model.loopStartDur = model.soundPercents[0] * model.bufferDuration
    model.loopEndDur = model.soundPercents[1] * model.bufferDuration
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
    // TODO full plays could be managed by playSound (see pauseInterval)
    if (pauseState.date <= t) { // No need to play more, even partially

      pauseState.percent = clampPercent(0 - model.soundStartPercent)

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

      pauseState.percent = clampPercent(length / model.length - model.soundStartPercent)
    }
  }
  , unpauseSound(t, contentPercent, model) {
    let offsetDur = contentPercent * model.duration + model.loopStartDur
      , newPlayer = this.scheduleStart(t, model, offsetDur)
    model.players.push(newPlayer)
    return newPlayer.stopTime
  }
  , undoSound(pauseState, model, now) {
    let t = pauseState.date
    for (let pl of model.players) {
      if (pl.startTime <= t && t <= pl.stopTime) {
        pl.node.stop(this.toCtxTime(t))
        pauseState.percent = clampPercent((t - pl.startTime) / model.length + pl.startOffsetDur / model.duration - model.soundStartPercent)
      }
      if (pl.startTime > t) pl.node.stop()
    }
    if (!isFinite(pauseState.percent)) {
      // TODO sometimes when Playing a collar, we get there for a bead that tries to undo nothing (found a misregistered lastScheduledTime)
      console.error("couldn’t find pausing player, unknown pause percent : t, now ", t, now, pauseState, model)
      pauseState.percent = 0
    }
  }
  
  , prepareInterval(t, model, parentRate) {
    // MEMBERS NEEDED :
    // interval
    // pupil(s)
    // instances [id-1, id-2]
    // launchPercent
    // id
    // mute
    // volume
    // view
    // restartMobiles TODO add this to collars
    // shotWheel
    // TODO handle launchPercent if we want content to be playing already
    // PROPOSAL negative launchPercent = no / positive launchPercent = yes => to compute
    // THEN pour les rosaces, launchPercent > 1 => plusieurs contents déjà en cours
    model.length = model.interval // for draw, every model has a length
    model.content = this.prepare(t, model.pupil, model.gainNode, parentRate)
    model.subWheels = []
    //model.subWheels = model.instances.forEach(v => this.spawnContent(t, model.content, v))
    model.transmitPause = !model.shotWheel
  }
  // plays as many full intervals as possible, t is always an unscheduled launch time
  , playInterval(t, max, model) {
    while (t <= max) {
      model.subWheels.push(this.schedulePupil(t, model.content))
      t += model.interval
    }
    return t
  }
  , pauseInterval(t, pauseState, model) { // TODO maybe copy this logic to other pauses (use play)
    this.playInterval(t, pauseState.date, model)

    for (let pupil of model.subWheels) {
      // pupil instances always have pause for their destruction
      let pupilPPT = pupil.playPauseTimes
        , lastPPT = pupilPPT[pupilPPT.length - 1]
      console.log("pause", lastPPT)
      if (safeFloat(pauseState.date) < safeFloat(lastPPT.date)) {
        lastPPT.date = pauseState.date
        lastPPT.done = false
        lastPPT.percent = undefined
        pupil.expireTime = undefined
      }
    }

    pauseState.percent = clampPercent((pauseState.date - t) / model.interval)
  }
  , unpauseInterval(t, contentPercent, model) {
    model.lastStartTime = t

    let offset = contentPercent * model.interval
    
    for (let pupil of model.subWheels) {
      if (!pupil.expireTime) {
        let pupilPPT = pupil.playPauseTimes
          , lastPPT = pupilPPT[pupilPPT.length - 1]
          , timeLeft = (1 - lastPPT.percent) * pupil.length
        console.log("unpause", lastPPT)
        pupil.playPauseTimes.push({date: t, play: true})
        pupil.playPauseTimes.push({date: t + timeLeft, play: false})
        pupil.expireTime = t + timeLeft
      }
    }
    
    return t + model.interval - offset
  }
  , undoInterval(lastState, pauseState, model, now) { // TODO maybe copy this logic to other undos (use lastState to compute percent)
    // undo playPause of subWheels
    let t = pauseState.date
    for (let pupil of model.subWheels) {
      //, contentPercent = clampPercent(lastState.percent + (pauseState.date - model.lastStartTime) / model.interval)
      //, lastLaunchTime = t - contentPercent * model.interval
      let lastStateIndex = pupil.lastPlayPauseIndexAt(t)
        , lastPupilState = pupil.playPauseTimes[lastStateIndex]

      pupil.playPauseTimes = pupil.playPauseTimes.slice(0, lastStateIndex + 1)
      if (lastPupilState.play) {
        pupil.playPauseTimes.push({date: t, play: false})
      } else {
        // TODO same hack as collar, does it works ? Test play & pause during same interval
        // TODO NOPE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        pupil.playPauseTimes.push({date: t, play: true})
        pupil.playPauseTimes.push({date: t, play: false})
      }
    }
    
    pauseState.percent = clampPercent(lastState.percent + (pauseState.date - model.lastStartTime) / model.interval)
  }
  
  , prepareCollar(t, model, parentRate) {
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
  , undoCollar(pauseState, model, now) {//WIP TODO
    // undo playPause of subWheels
    let t = pauseState.date
      , pausingBeadIndex = -1
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

  , schedulePupil(t, pupil) {
    let newP = this.spawnContent(t, pupil)
    let pupilPPT = newP.playPauseTimes
    newP.expireTime = t + pupil.length

    pupilPPT.push({date : t, play : true})
    pupilPPT.push({date : newP.expireTime, play : false})
    return newP
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
      this.schedulePlayer(t, model, model.loopStartDur, model.duration, model.length)
    ].concat(t + model.length >= maxT ? [] : this.scheduleLoop(t + model.length, maxT, model))
  }
  , scheduleStart(t, model, offsetDur) {
    let dur = model.loopEndDur - offsetDur
      , len = dur / model.rate
    return this.schedulePlayer(t, model, offsetDur, dur, len)
  }
  // Just to schedule the start and stop of a player
  // Hence, used only for simple wheels containing sound
  //
  // t is the time when to start (using ctxStartTime as the reference)
  // model is the model of the wheel
  // startOffset is the point where to start in the buffer (in seconds as if the rate was 1)
  // duration is the time in seconds to play from the buffer (again ignoring the rate)
  // length is the real time the player should play, used to stop it using our rounding
  , schedulePlayer(t, model, startOffset, duration, length) {
    let player = ctx.createBufferSource()
      , ctxStartTime = t + this.startTime
      , ctxStopTime = ctxStartTime + length
    player.buffer = model.buffer
    player.playbackRate.value = model.rate // TODO to go realTime rate, maybe use setValueAtTime
    player.connect(model.gainNode)
    player.onended = () => model.freePlayer(t)
    player.start(ctxStartTime, startOffset, duration)
    player.stop(ctxStopTime) // TODO stop and duration in schedulePlayer do the same thing, is it good ? Does it compensate for inexact buffer.duration ? See upward in prepare sound
    // According to https://github.com/WebAudio/web-audio-api/issues/1660#issuecomment-3991786334
    // duration counts the samples in the sound, hence bufferDuration (but in seconds)
    // and stop(time) takes the playbackRate into account
    // So I would think that using both is good ? But, in case of
    // WARNING implementation could change ? check specs from times to times
    //
    // Last thoughts : the difference between the two will be the rounding differences between implementations and my scheduler
    return {
        node : player
      , startTime : t
      , stopTime : t + length
      , startOffsetDur : startOffset
    }
  }
  , spawnContent(t, content, id) {
    let o = Object.create(content)
    o.playPauseTimes = [
      {date : 0, play : false, percent : 0, done : true} // used by draw
      , {date : t, play : false, percent : 0, done : true} // and one at t to prevent past scheduling warn
    ]
    o.lastScheduledTime = t
    o.players = []
    // TODO create deep copies of all scheduler variables for each subWheel type
    // beads, collar, mobile, … Maybe each prepare func be split in variable & const

    if (o.view) {
      let view = o.view = Object.create(o.view)
        , parent = view.parent = view.node.parentNode
        , node = view.node = view.node.cloneNode(true)
      view.tr = node.transform.baseVal.getItem(0)
      node.setAttribute("opacity", 1)
      view.seen = false
    }
    this.modelsToDraw.push(o)

    return o
  }

  , nextRequestId : -1
  , modelsToDraw : []

  , prepareDraw(model) {
    let el = document.getElementById(model.id)
      , tr = svg.createSVGTransform()
      , rx = parseFloat(el.getAttribute("rx"))
      , ry = parseFloat(el.getAttribute("ry"))
    //tr.setRotate(0,0,0)
    el.transform.baseVal.initialize(tr)

    model.view = {
        node : el
      , tr : tr
      , rx : rx
      , ry : ry
      , moveTo : function (percent) {
        this.tr.setRotate(percent * 360, this.rx, this.ry)
      }
    }

    this.modelsToDraw.push(model)
  }
  , draw() {
    // TODO keeps drawing event when paused. is it bad ?
    let now = scheduler.getTime()
      , modelsToRemove = []
    for (let model of this.modelsToDraw) {
      let lastStateIndex = model.lastPlayPauseIndexAt(now)
        , lastState = model.playPauseTimes[lastStateIndex]
        , percent = 0

      if (!lastState || !lastState.done) { // TODO what is this case ? an error ?
        lastState = model.playPauseTimes[--lastStateIndex]
        console.error("no lastState in draw (or not done), taking previous instead :", lastState, "index", lastStateIndex, "model", model)
      }

      if (lastState && isFinite(lastState.percent)) {
        percent = clampPercent(lastState.play ?
            lastState.percent + (now - lastState.date) / model.length :
            lastState.percent)
      } else console.error("lastState was not done in draw :", lastState, "time is", now, "model", model)
console.log(model.view.parent, percent, model.view.seen, model)
      if (model.view.parent && safeFloat(percent) > 0 && !model.view.seen) {console.log("yes!")
        model.view.parent.appendChild(model.view.node)
        model.view.seen = true
      }

      if (model.view.seen && ( safeFloat(percent) <= 0 || safeFloat(percent) >= 1)) {
        model.view.parent.removeChild(model.view.node)
        model.view.seen = false
        modelsToRemove.push(model)
      }

      model.view.moveTo(safeFloat(percent))
    }

    for (let model of modelsToRemove) {
      this.modelsToDraw.splice(this.modelsToDraw.findIndex(v => v == model), 1)
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
