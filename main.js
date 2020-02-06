/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/


let canvas = document.getElementById('canvas')
  , {width, height} = canvas
  , ctx = canvas.getContext('2d')
  , buf = new Tone.Buffer('./sons/LOOP.wav', drawSound)
  , samples, pxPerSample

function drawSound() {
  samples = Array.from(buf.getChannelData())
  pxPerSample = width / samples.length
  
  ctx.strokeStyle = 'black'
  ctx.beginPath()
  ctx.moveTo(0, height / 2)
  ctx.lineTo(width, height / 2)
  ctx.stroke()
  
  ctx.strokeRect(0, 0, width, height)
  
  if (pxPerSample < 0.5) {
    for (let x = 0 ; x < width ; x++) {
      let px = samples.slice(Math.floor(x / pxPerSample), Math.floor((x + 1) / pxPerSample))
      ctx.strokeStyle = 'black'
      ctx.beginPath()
      ctx.moveTo(x, (Math.min.apply(null, px) + 1) * height / 2)
      ctx.lineTo(x, (Math.max.apply(null, px) + 1) * height / 2)
      ctx.stroke()
      
      let rms = Math.sqrt(px.reduce((acc,v,i,a) => acc + Math.pow(v, 2)) / px.length)
      ctx.strokeStyle = 'gray'
      ctx.beginPath()
      ctx.moveTo(x, (1 - rms) * height / 2)
      ctx.lineTo(x, (1 + rms) * height / 2)
      ctx.stroke()
    }
  } else {
    ctx.strokeStyle = 'black'
    ctx.beginPath()
    ctx.moveTo(0, (samples[0] + 1) * height / 2)
    for (let i = 1 ; i < samples.length ; i++) {
      ctx.lineTo(i * pxPerSample, (samples[i] + 1) * height / 2)
    }
    ctx.stroke()
  }
}


function debug(o) {
  getId('debug').innerHTML = o instanceof Object ? Object.keys(o) : Array.from(arguments).join(' ')
}

const draw = SVG('svg')
      globInter = interact(draw.native())
    , gearSym = draw.symbol().viewbox(0,0,100,100)

gearSym.circle(100).fill('red').stroke('yellow')
gearSym.rect(2,10).fill('purple').cx(50)

let gears = []

function addGear(x, y, size) {
  size = size || Math.random()*20 + 20
  x = x || (Math.random()*100 + (size/2))
  y = y || (Math.random()*100 + (size/2))
  
  let g = draw.group().center(0,0).dmove(x,y)
  g.gearSym = g.use(gearSym).size(size)
  
  Object.assign(g, gearMixin)
  
  interact(g.native())
    .draggable({
      onstart: e => {
        globInter
          .on('down', e => {
            if ( e.pointerId != 1 ) return;
            globInter.prec = [e.clientX, e.clientY]
          })
          .on('move', e => {
            if ( e.pointerId != 1 ) return;
            g.gearSym.size(Math.max(g.gearSym.width() + e.clientX - globInter.prec[0], 20))
            globInter.prec = [e.clientX, e.clientY]
          })
      }
    , onmove: e => {
      g.dmove(e.dx, e.dy)
    }
    , onend: () => {
      globInter.unset()
      globInter = interact(draw.native())
    }})
    .on('tap', e => g.turn())
    .on('doubletap', e => g.stop())
  
  gears.push(g)
}

addButton('TURN', ()=>{for (let g of gears) g.turn()})
addButton('PAUSE', ()=>{for (let g of gears) g.pause()})
addButton('PLAY', ()=>{for (let g of gears) g.play()})
addButton('STOP', ()=>{for (let g of gears) g.stop()})
addButton('ADD', addGear)
body.appendChild(document.createElement('br'))


let urlTMP = window.location.href.split('/')
urlTMP.pop()
let baseUrl = urlTMP.join('/')
  , sound = new Tone.Player(baseUrl + '/sons/boom2.wav').toMaster()

let sons = ['boom1.wav','boom2.wav','tchi1.wav','tchi2.wav']

//sound.buffer.onload = ()=>console.log('loaded')
sound.loop = true

addButton('PLAY', ()=>sound.start())
let slider = document.createElement('input')
slider.setAttribute('type', 'range')
slider.setAttribute('min', '0')
slider.setAttribute('max', '100')
slider.setAttribute('step', '.1')
slider.setAttribute('value', '1')
slider.onchange = e => sound.playbackRate = slider.valueAsNumber
body.appendChild(slider)
addButton('PLAY+', ()=>{
  let s = new Tone.Player(sound.buffer).toMaster()
  s.loop = true
  s.playbackRate = Math.random()*5+.1
  s.start()
})
addButton('PLAY++', ()=>{
  let s = new Tone.Player(baseUrl + '/sons/' + sons[Math.floor(Math.random()*sons.length)]).toMaster()
  s.loop = true
  s.playbackRate = Math.random()*2+.1
  s.autostart = true
})
addButton('LOOP', ()=>{
  let l = new Tone.Player(baseUrl + '/sons/LOOP.wav').toMaster()
  l.loop = true
  l.autostart = true
})

let players = [ new Tone.Player(baseUrl + '/sons/' + sons[0]).toMaster()
              , new Tone.Player(baseUrl + '/sons/' + sons[2]).toMaster()
              ]
players[0].loop = players[1].loop = true
addButton('COLLAR', ()=>{
  let durs = players.map(v=>v.buffer.duration)
    , totalDur = durs.reduce((a,b)=>a+b, 0)
    , clocks = []
    , pos = [0,0]
    , estimated = 0
  console.log(durs, totalDur)
  for (let i in players) {
    clocks.push(new Tone.Clock(t=>{
      console.log(Math.round((t - estimated)*1000000), t, estimated)
      estimated += durs[i]
    }, 1/totalDur))
  }
  estimated = Tone.now() + 0.1
  clocks[0].start("+0.1")
  clocks[1].start("+"+(durs[0]+0.1))
})
//addButton('COLLAR', ()=>{
//  let offsets = [0, -1]
//    , part = [[0, 0], [players[0].buffer.duration, 1]]
//    , estimated = 0.1
//    , p = new Tone.Part((t, i) => {
//      players[i].start(t, offsets[i])
//      offsets[i] = t
//      console.log(t - estimated)
//      estimated += players[i].buffer.duration
//      if (offsets[1-i] == -1) offsets[1-i] = 0
//      else {
//        players[1-i].stop(t)
//        offsets[1-i] = t - offsets[1-i]
//      }
//      console.log(t, i, offsets[1-i], players[1-i].buffer.duration - offsets[1-i])
//    }, part)
//  players[0].loop = players[1].loop = p.loop = true
//  p.loopEnd = players[0].buffer.duration + players[1].buffer.duration
//  p.start()
//  Tone.Transport.start()
//})

const gearMixin = {
  turn: function (dur) {
    dur = dur || (Math.random()*2000 + 1000)
    
    this.gearSym.animate(dur).transform({rotation:360}).loop()
  }
, stop: function () {
    this.gearSym.animate().finish()
  }
, pause: function () {
    this.gearSym.animate().pause()
  }
, play: function () {
    this.gearSym.animate().play()
  }
}


/*

const svg = document.getElementsByTagName('svg')[1]
    , gears = getId('gears')

function addGear() {
  let g = document.createElementNS(svg.namespaceURI, 'use')
  g.setAttributeNS('http://www.w3.org/1999/xlink', 'href', '#gear')
  let size = Math.random()*10+5
  g.setAttribute('width', size)
  g.setAttribute('height', size)
  g.setAttribute('x', Math.random()*(100-size))
  g.setAttribute('y', Math.random()*(100-size))
  svg.appendChild(g)
  return g
}

addGear()

*/
