const app = Elm.Main.init()

app.ports.loadSound.subscribe(createBuffer)
app.ports.play.subscribe(play)

const buffers = {}

function createBuffer(soundName) {
  console.log('create '+soundName)
  if (buffers[soundName]) {
    app.ports.soundLoaded.send(soundName + ' already Loaded')
  } else {
    buffers[soundName] = new Tone.Buffer('./sons/' + soundName, ()=>loadOk(soundName), e=>loadErr(e, soundName))
  }
}

function loadOk(soundName) {
  app.ports.soundLoaded.send(soundName + ' ok')
}

function loadErr(err, soundName) {
  console.log(err)
  app.ports.soundLoaded.send(soundName + ' got ' + err)
}

function play(model) {
  if (model.type == "gear") {
    console.log('play '+ model.soundName)
    let s = new Tone.Player(buffers[model.soundName]).toMaster()
      , g = SVG.adopt(document.getElementById(model.gearId))
    s.loop = true
    s.start()
    g.animate(s.buffer.duration * 1000).transform({rotation:360, cx:0, cy:0}).loop()
  }
}
