let scheduler = {
    interval : 50
  , lookAhead : 2000
  , running : false
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
  
  , startThenPlay(topGears) {
    if (this.running) this.playPause(topGears)
    
    else {
      this.running = true

      ctx.resume().then(() => {
        this.startTime = ctx.currentTime
        this.intervalId = setInterval(() => this.work(), this.interval)
        this.work()
        this.nextRequestId = requestAnimationFrame(() => this.draw())
        this.playPause(topGears)
      })
    }
  }
  
  , stop() {
    if (!this.running) return;
    
    clearInterval(this.intervalId)
    cancelAnimationFrame(this.nextRequestId)
    
    this.running = false
    
    for (let id in this.playingTopModels) { // TODO loop also inside collars and mobiles, make a map function walking all sounds in the tree to use also in work and ?
      let model = this.playingTopModels[id]
      for (let player of model.players) {
        player.node.stop()
      }
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
  , prepare(model, destination, parentRate) {
    // TODO this is creating a new func instance for each method for each model
    // It’s bad!! Should be in proto ?
    model.playPauseTimes = [{date : 0, play : false, percentPaused : 0, done : true}]
    model.lastScheduledTime = 0
    model.lastPlayPauseAt = function(now) {
      return this.playPauseTimes.slice().reverse().find(v => v.date <= now)
    }

    let gain = ctx.createGain()
    gain.connect(destination)
    model.gainNode = gain
    model.updateVolume = function() {
      this.gainNode.gain.value = this.mute ? 0 : this.volume
    } // TODO volume should rather be in dB
    model.updateVolume()
    
    if (model.soundName) {
      model.players = []
      model.freePlayer = function(startTime) {
        this.players = this.players.filter(v => v.startTime != startTime)
      }
      model.buffer = buffers[model.soundName]
      // TODO beware, buffer duration could differ from saved duration in Elm model (due to resampling)
      // probably it’s preferable to use saved duration from elm
      // but, is it compensated by downward TODO ? (in schedulePlayer)
      model.bufferDuration = model.buffer.duration
      model.loopStartDur = model.loopPercents[0] * model.bufferDuration
      model.loopEndDur = model.loopPercents[1] * model.bufferDuration
      model.duration = model.loopEndDur - model.loopStartDur
      model.rate = parentRate * model.duration / model.length
      model.offsetDur = model.startPercent * model.duration
    }
    
    if (model.collar) {
      model.duration = model.collar.length
      model.rate = parentRate * model.duration / model.length
      model.offsetDur = model.startPercent * model.duration
      model.subWheels = model.collar.beads.map(v => this.prepare(v, model.gainNode, model.rate))
    }

    if (model.view && model.id) {
      let el = document.getElementById(model.id)
        , tr = svg.createSVGTransform()
      tr.setRotate(0,0,0)
      el.transform.baseVal.initialize(tr)

      model.view = {
          tr : tr
        , moveTo : function (percent) {
          this.tr.setRotate(percent * 360, 0, 0)
        }
      }
      
      this.modelsToDraw.push(model)
    }
    this.playingTopModels[model.id] = model
  }
  
  , playPause(topGears) {
    let t = this.getTime() + playPauseLatency
    for (let model of topGears) {
      if (!this.playingTopModels[model.id]) this.prepare(model, masterGain, 1)
      model = this.playingTopModels[model.id]

      let running = model.playPauseTimes[model.playPauseTimes.length - 1].play
      
      if (running) {
        model.playPauseTimes.push({date : t, play : false})
      } else {
        model.playPauseTimes.push({date : t, play : true})
      }
    }
  }
  
  , work() { // TODO presently specific to soundWheels, todo collar & mobile
    let now = this.getTime()
      , max = now + this.lookAhead / 1000
    for (let id in this.playingTopModels) {
      let model = this.playingTopModels[id]
        , ppt = model.playPauseTimes
      // For now, considering that playPauseTimes is filled chronologically and alternatively of play and pause
      // This is the assumption of user play and pause
      // collar or another source of play pause should manage their specificities
      
      // Clean play pause events before last
      ppt.splice(0, ppt.findIndex(v => v.date >= Math.min(now, model.lastScheduledTime)) - 1)
      
      let nextStateIndex = ppt.findIndex(v => !v.done) // Next is first not done
        , nextState = ppt[nextStateIndex]
        , lastState = ppt[nextStateIndex - 1] || ppt[ppt.length - 1]
        , scheduleTime = model.lastScheduledTime
        , advanceState = () => {
          nextState.done = true
          lastState = nextState
          nextState = ppt[++nextStateIndex]
        }
      if (nextState && nextState.date < scheduleTime) { // If we sheduled ahead of next
        scheduleTime = nextState.date // Bring back the scheduler
        if (!nextState.play) { // If we should’ve pause
          for (let pl of model.players) {
            if (pl.startTime <= scheduleTime && scheduleTime < pl.stopTime) {
              pl.node.stop(this.toCtxTime(scheduleTime))
              nextState.percentPaused = (scheduleTime - pl.startTime) / model.length
            }
            if (pl.startTime > scheduleTime) pl.node.stop()
          }
          advanceState()
        }
      }
      
      if (now > scheduleTime) console.error("scheduler is late, now : " + now + " scheduler : " + scheduleTime)
      
      while (scheduleTime < max) {
        
        let t = scheduleTime
        
        if (lastState.play) { // If we’re playing
          
          if (nextState && nextState.date < max) { // And should pause
            
            let newPlayers = this.scheduleLoop(t, nextState.date - model.length, model)
            
              , startTime = newPlayers[newPlayers.length - 1].stopTime
              , length = nextState.date - startTime
              , duration = length * model.rate
            
            newPlayers.push(this.schedulePlayer(startTime, model, model.loopStartDur, duration, length))
            
            model.players = model.players.concat(newPlayers)
            
            nextState.percentPaused = length / model.length
            
            scheduleTime = nextState.date
            
            advanceState()
            
          } else { // And keep playing
            
            let newPlayers = this.scheduleLoop(t, max, model)
            model.players = model.players.concat(newPlayers)
            scheduleTime = model.players[model.players.length - 1].stopTime
            
          }
          
        } else { // If we’re paused
          
          if (nextState && nextState.date < max) { // And should play
            
            let newPlayer = this.scheduleStart(t, model, lastState.percentPaused * model.duration + model.loopStartDur)
            model.players.push(newPlayer)
            scheduleTime = newPlayer.stopTime
            nextState.percentStarted = lastState.percentPaused
            advanceState()
            
          } else { // And keep pausing
            
            scheduleTime = max
            
          }
        }
      }
      model.lastScheduledTime = scheduleTime
    }
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
    return {
        node : player
      , startTime : t
      , stopTime : t + length
    }
  }
  
  
  , nextRequestId : -1
  , modelsToDraw : []
  
  , draw() {
    // TODO keeps drawing event when paused. is it bad ?
    // TODO percent keeps growing, will it overflow ?
    for (let model of this.modelsToDraw) {
      let now = scheduler.getTime()
        , lastState = model.lastPlayPauseAt(now)
      
      let percent = lastState.play ?
          lastState.percentStarted + (now - lastState.date) / model.length :
          lastState.percentPaused
      
      model.view.moveTo(percent)
    }
    this.nextRequestId = requestAnimationFrame(() => this.draw())
  }
}
