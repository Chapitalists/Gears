/*
Copyright ou © ou Copr. Clément Bossut, (2024)
<bossut.clement@gmail.com>
*/

class ScratchPlayerProcessor extends AudioWorkletProcessor {
  
  static get parameterDescriptors() {
    return [{
      name: 'playbackRate',
      defaultValue: 1
    }]
  }
  
  constructor() {
    super()
    
    this.port.onmessage = e => {
      this.buffer = e.data
    }
  }
  
  process(inputs, outputs, parameters) {
    const rate = parameters.playbackRate
    
    if (rate.length === 1) {
      
    } else {
      
    }
    
    return true;
  }
}

registerProcessor('scratch-player', ScratchPlayerProcessor)
