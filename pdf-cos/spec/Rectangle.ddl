-- Rectangle: the rectangle data type (Sec. 7.9.5)
import Stdlib
import PdfValue

def Rectangle = Between "[" "]" {
  llx = Token Number;
  lly = Token Number;
  urx = Token Number;
  ury = Token Number;
}
