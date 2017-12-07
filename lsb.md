# Least Significant Bit Encyrption

Use the least significant bits of each pixel to hide a byte sequence into an image. 

# Method

Defined method for RGB8 images
Write name of file, null terminator, bytes of file, null terminator. 

max bytes that can be stored = 
  width * height / (8 / (bits per pixel * 3))

b = number of bits per color channel
More bits cause more distortion but more information can be hidden
b * 3 = number of bits per pixel

l = length of byte sequence
l * 8 = bits in sequence

Pixels used = (l * 8) / (b * 3)

Location of bit in sequence to pixel location
w = width of image
h = height o fimage

Hide information in order of top left, progressing right to left, then top to bottom
a = bit in sequence location = (byte index) * 8 + (bit index)

p = pixel index = a / (b * 3)
px = p modulo w
py = p / w
// red = 0, blue = 1, green = 2
c = channel used = (a modulo (b * 3)) / b
d = bit index = (a modulo (b * 3)) modulo b

