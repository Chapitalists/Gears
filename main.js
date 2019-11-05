/*
Copyright ou © ou Copr. Clément Bossut, (2018)
<bossut.clement@gmail.com>
*/

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
