# convert font bitmap into character binary

import imageio.v2 as imageio
import sys
import datetime
import time

def read_block(image, row, col):
    """ read 8x8 block from file """
    block = []
    for bit_row in range(8):
        byte = 0
        for bit_col in range(8):
            pixel = image[row * 8 + bit_row][col * 8 + bit_col]
            #print((0 if pixel[0] else 1),end="")
            byte = (byte << 1) | (0 if pixel[0] else 1)
        # print("=",byte)
        block.append(byte)
    return block


# what character is in each block
charmap = "                                " + \
          "abcdefghijklm nopqrs' tuvw x    " + \
          "                                " + \
          "                                " + \
          "  yzþ@&ð- ,.;                   " + \
          "                                " + \
          "                                " + \
          "                                "
# what ascender is in each block
asc_map = " b d   h  kl        '           " + \
          "                                " + \
          "                                " + \
          "    þ@&ð    '                   " + \
          "                                " + \
          "                                " + \
          "                                " + \
          "                                "

des_map = "                                " + \
          "                                " + \
          "     fg  j      pqr        x    " + \
          "                                " + \
          "                                " + \
          "  y þ@&ð  , ;                   " + \
          "                                " + \
          "                                "

widemap = "                                " + \
          "             m            w     " + \
          "                                " + \
          "                                " + \
          "                                " + \
          "                                " + \
          "                                " + \
          "                                "

_, filename, out_dir = sys.argv
blocks = {}
blank = 0xDF

print(f'reading {filename}')
with open(filename, "rb") as file:
    image = imageio.imread(file)
    for block_row in range(8):
        for block_col in range(32):
            code=block_row*32+block_col
            blocks[code] = read_block(image, block_row, block_col)


with open(out_dir+"font_bin.bin", "wb") as ofile:
    print(f"writing {ofile.name}")
    for code in range(256):
        if code in blocks:
            ofile.write(bytes(blocks[code]))
        else:
            ofile.write(bytes([0]*8))

with open(out_dir+"font_asc.bin", "wb") as ofile:
    print(f"writing {ofile.name}")
    for i, ch in enumerate(charmap):   
        try:
            if ch == ' ':
                val = blank
            else:
                val = asc_map.index(ch)
        except ValueError:
            val = blank
        ofile.write(bytes([val]))

with open(out_dir+"font_des.bin", "wb") as ofile:
    print(f"writing {ofile.name}")
    for i, ch in enumerate(charmap):
        try:
            if ch == ' ':
                val = blank
            else:
                val = des_map.index(ch)
        except ValueError:
            val = blank
        ofile.write(bytes([val]))

with open(out_dir+"font.inc", "w") as ofile:
    print(f"writing {ofile.name}")
    print(f'; autogenerated by {__file__} {datetime.datetime.now()}', file=ofile)
    print('font_asc: .incbin "font_asc.bin"', file=ofile)
    print('font_des: .incbin "font_des.bin"', file=ofile)
    print('font: .incbin "font_bin.bin"', file=ofile)


with open(out_dir+"font_map.inc", "w", encoding="utf-8") as ofile:
    print(f"writing {ofile.name}")
    print(
        f'; autogenerated by {__file__} {datetime.datetime.now()}', file=ofile)    
    for i, ch in enumerate(charmap):
        if ch != ' ' or i == blank:
            print(f".charmap ${ord(ch):02x}, ${i:02x} ; {ch}", file=ofile)


time.sleep(2)