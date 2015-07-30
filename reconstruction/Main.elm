import Cell
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage)

(kCANVAS_WIDTH, kCANVAS_HEIGHT) = (600, 600)

main : Element
main = collage kCANVAS_WIDTH kCANVAS_HEIGHT [Cell.drawCell Cell.Dead]