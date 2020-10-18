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
    }
    
    if (model.collar) {
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
      model.subWheels = model.collar.beads.map(v => this.prepare(v, model.gainNode, model.rate))
    }
    
    model.realLength = model.length / parentRate
    model.lengthBeforeParentRate = model.length
    model.length = model.realLength

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
    return model
  }
  
  , playPause(topGears) {
    let t = this.getTime() + playPauseLatency
    for (let model of topGears) {
      if (!this.playingTopModels[model.id])
        this.playingTopModels[model.id] = this.prepare(model, masterGain, 1)
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
      this.schedule(this.playingTopModels[id], now, max)
    }
  }
      
  , schedule(model, now, max) {
    let ppt = model.playPauseTimes
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

    while (scheduleTime < max) {

      let t = scheduleTime
      if (now > scheduleTime) console.error("scheduler is late, now : " + now + " scheduler : " + t)

      if (lastState.play) { // If we’re playing

        if (nextState && nextState.date < max) { // And should pause

          if (nextState.date < t) { // If we sheduled ahead of next
            t = nextState.date // Bring back the time and undo
            if (t <= now) console.error("undoing the past, now : " + now + " scheduler : " + t)

            if (model.soundName) {
              for (let pl of model.players) {
                if (pl.startTime <= t && t < pl.stopTime) {
                  pl.node.stop(this.toCtxTime(t))
                  nextState.percentPaused = clampPercent((t - pl.startTime) / model.length - model.startPercent)
                }
                if (pl.startTime > t) pl.node.stop()
              }
            }

            if (model.collar) { // If collar, undo playPause of subWheels
              // TODO collar undo
            }

          } else { // Normal pause
          
            if (model.soundName) {
              if (nextState.date <= t) { // No need to play more, even partially

                nextState.percentPaused = clampPercent(0 - model.startPercent)

              } else {
                let newPlayers = []
                  , startTime

                if (nextState.date > t + model.length) { // At least one full play
                  newPlayers = this.scheduleLoop(t, nextState.date - model.length, model)
                  startTime = newPlayers[newPlayers.length - 1].stopTime
                } else { // Just a partial play
                  startTime = t
                }
                let length = nextState.date - startTime
                  , duration = length * model.rate

                newPlayers.push(this.schedulePlayer(startTime, model, model.loopStartDur, duration, length))

                model.players = model.players.concat(newPlayers)

                nextState.percentPaused = clampPercent(length / model.length - model.startPercent)
              }
            }

            if (model.collar) {
              // TODO collar pause
            }

            t = nextState.date

          }
          
          advanceState()

        } else { // And keep playing

          if (model.soundName) {
            let newPlayers = this.scheduleLoop(t, max, model)
            model.players = model.players.concat(newPlayers)
            t = model.players[model.players.length - 1].stopTime
          }

          if (model.collar) {
            while (t <= max) {
              let length = model.beadsDurs[model.nextBead] / model.rate
                , beadPPT = model.subWheels[model.nextBead].playPauseTimes
              beadPPT.push({date : t, play : true})
              beadPPT.push({date : t + length, play : false})
              model.nextBead = (model.nextBead + 1) % model.subWheels.length
              t += length
            }
          }

        }

      } else { // If we’re paused

        if (nextState && nextState.date < max) { // And should play
          t = nextState.date
          if (t <= now) console.error("starting in the past, now : " + now + " scheduler : " + t)
          
          let contentPercent = clampPercent(lastState.percentPaused + model.startPercent)

          if (model.soundName) {
            let offsetDur = contentPercent * model.duration + model.loopStartDur
              , newPlayer = this.scheduleStart(t, model, offsetDur)
            model.players.push(newPlayer)
            t = newPlayer.stopTime
          }

          if (model.collar) {
            let cumulDur = model.beadsCumulDurs[model.nextBead]
              , offsetDur = contentPercent * model.duration
              , length = (cumulDur - offsetDur) / model.rate
              , beadPPT = model.subWheels[model.nextBead].playPauseTimes
            beadPPT.push({date : t, play : true})
            beadPPT.push({date : t + length, play : false})
            model.nextBead = (model.nextBead + 1) % model.subWheels.length
            t += length
          }

          nextState.percentStarted = lastState.percentPaused
          advanceState()


        } else { // And keep pausing

          t = max

        }
      }
      scheduleTime = t
    }
    model.lastScheduledTime = scheduleTime

    if (model.collar) {
      model.subWheels.forEach(v => this.schedule(v, now, max))
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
        , percent = 0
      
      if (lastState && lastState.done) {
        percent = clampPercent(lastState.play ?
            lastState.percentStarted + (now - lastState.date) / model.length :
            lastState.percentPaused)
      } else console.error("lastState was not done in draw :", lastState, "time is", now)
      
      model.view.moveTo(percent)
    }
    this.nextRequestId = requestAnimationFrame(() => this.draw())
  }
}

function clampPercent(p) {
  return p - Math.floor(p)
}
