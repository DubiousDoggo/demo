help="""
Convert a font bitmap into assembleable character data.
input a filename and an output directory as command line arguments.
"""
   
import sys
import imageio.v2 as imageio

if len(sys.argv) < 3 or any(x in ["-?","/?","-h","/h","--help"] for x in sys.argv):
  print(help)
  exit()


_, filename, out_dir = sys.argv

# TODO: take the font map as input
var_font_map = "abcdefghhijklmmnopqrstuvwwxyyyz --,.;'&*1234567890______[]\""

FONT_HEIGHT = 3 # this is defined in the assembly

# TODO: take a kern table file as input
kern_table = {
    #      a   b  c  d  e   f  g  h  i  j   k   l  m  n  o  p  q  r  s  t  u  v  w  x   y  z     -  ,  .  ;  '  &  *
    'a': [ 1, -1, 1, 1, 0, -1, 0,-1, 0, 0, -1, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0,-1,-1, 0,-12, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    'b': [ 0, -1, 1, 1, 0, -1, 0,-1, 0, 0, -1, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,-11, 0, 1, 1, 0, 1, 1, 1, 1, 1],
    'c': [ 0, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'd': [ 0, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1,-11, 1, 1, 1, 0, 1, 1, 1, 1, 1],
    'e': [ 1, -1, 1, 1, 1,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'f': [ 1, -1, 1, 1, 0,  0, 1,-1, 1, 1, -1, -1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'g': [ 0, -1, 1, 1,-1, -1, 1,-1, 1, 1, -1, -1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 0, 1, 1],
    'h': [ 0, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'i': [ 1, -1, 1, 1, 0,  0, 1,-1, 1, 0, -1, -1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'j': [ 1, -1, 1, 1, 1,  1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'k': [ 1, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'l': [ 1, -1, 1, 1, 0, -1, 1,-1, 0, 0, -1, -1, 1, 0, 1, 0, 1, 0, 0, 0, 0,-1,-1, 0,-12, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'm': [ 0, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'n': [ 0, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'o': [ 0, -1, 1, 1, 0,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'p': [ 0, -1, 1, 1, 0, -1, 1,-1, 0, 0, -1, -1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'q': [ 1, -1, 1, 1, 1,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'r': [ 0, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    's': [ 1, -1, 1, 1, 0,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    't': [ 0, -1, 1, 1, 1,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'u': [ 0, -1, 1, 1, 0, -1, 1,-1, 0, 0, -1, -1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'v': [ 0, -1, 0, 0, 0,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'w': [ 0, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1,-1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'x': [ 0, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'y': [ 0, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    'z': [ 1, -1, 1, 1, 0, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    ' ': [ 1, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, -8, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    '-': [ 1, -1, 1, 1, 1,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    ',': [ 1, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    '.': [ 1, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    ';': [ 1, -1, 1, 1, 1, -1, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    "'": [-2,  1,-3, 1,-3,  0, 0,-1, 0, 0,  1,  1,-2,-1,-2,-1,-3,-2,-3,-2,-1,-1,-1,-1,-12,-1, 1, 1, 1, 1, 1, 1, 1, 1],
    '&': [ 1, -1, 1, 1, 1,  0, 1,-1, 1, 1, -1, -1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    "*": [ 1,  1, 1, 1, 1,  1, 1, 1, 1, 1,  1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,-11, 1, 1, 1, 1, 1, 1, 1, 1, 1]
}


chars = {}
char_widths = {}
char_block_width = {}

LABELS = {
    ' ': 'sp',
    '-': 'ds',
    ',': 'cm',
    '.': 'pt',
    ';': 'sc',
    "'": 'ap',
    '&': 'et',
    '*': 'st',
    '[': 'lb',
    ']': 'rb',
    '"': 'dq',
}

def read_block(image, row, col):
    """ read an 8x8 block from image file """
    block = []
    for bit_row in range(8):
        byte = 0
        for bit_col in range(8):
            pixel = image[row * 8 + bit_row][col * 8 + bit_col]
            byte = (byte << 1) | (0 if pixel[0] else 1)
        block.append(byte)
    return block

print(f'reading {filename}')
with open(filename, "rb") as file:
    image = imageio.imread(file)
    for block_col,char in enumerate(var_font_map):
        col = [read_block(image, block_row, block_col) for block_row in range(FONT_HEIGHT)]
        chars.setdefault(char, []).append(col)

def block_width(column):
    flat_column = [row for block in column for row in block]
    max_width = 0
    for row in flat_column:
        row_width = 8
        for i in range(8):
            if row & 1: break
            row >>= 1
            row_width -= 1
        if row_width > max_width:
            max_width = row_width
    
    if max_width == 0: return 7 # make sure space doesent break things
    return max_width 

for char,v in chars.items():
    char_widths[char] = (len(v) - 1) * 8 + block_width(v[-1])
    char_block_width[char] = len(v)

# generate the font tables
with open(out_dir+"font.inc", "w") as ofile:
    print(f"writing {ofile.name}")
    print(f'; autogenerated by build_font.py', file=ofile)

    for char,data in chars.items():
        LABELS.setdefault(char, char)
        print(f'font_{LABELS[char]}:', file=ofile)
        for column in data:
            print(f"\t.byte " + ', '.join(f"${b:02x}" for block in column for b in block), file=ofile)
    print('font_hi:\n\t.byte ' + ', '.join(f">font_{LABELS[char]}" for char in chars.keys()), file=ofile)
    print('font_lo:\n\t.byte ' + ', '.join(f"<font_{LABELS[char]}" for char in chars.keys()), file=ofile)


    # set default advance width of 1
    for char in chars.keys():
        if char in kern_table.keys(): continue
        kern_table.setdefault(char, [1]*len(kern_table['a']))
        for v in kern_table.values():
            v.append(1)

    for char,data in kern_table.items():
        print(f'kern_{LABELS[char]}:', file=ofile)
        print(f'\t.byte '+', '.join(f"${(v + char_widths[char]) % 256:02x}" for v in data), file=ofile)
    print('kern_hi:\n\t.byte ' + ', '.join(f">kern_{LABELS[char]}" for char in kern_table.keys()), file=ofile)
    print('kern_lo:\n\t.byte ' + ', '.join(f"<kern_{LABELS[char]}" for char in kern_table.keys()), file=ofile)

    #print('font_width:\n\t.byte ' + ', '.join(f"${w:02x}" for w in char_widths.values()), file=ofile)
    print('font_columns:\n\t.byte ' + ', '.join(f"${c:02x}" for c in char_block_width.values()), file=ofile)


# generate character mapping directive file
with open(out_dir+"font_map.inc", "w", encoding="utf-8") as ofile:
    print(f"writing {ofile.name}")
    print(f'; autogenerated by build_font.py', file=ofile)    
    for i, ch in enumerate(chars.keys()):
        print(f".charmap ${ord(ch):02x}, ${i:02x} ; {ch}", file=ofile)


