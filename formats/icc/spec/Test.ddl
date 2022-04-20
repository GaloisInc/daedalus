import Daedalus
import ICC

def Main =
  block
    let spec = Main1
    Guard (length spec.tags == 0)

