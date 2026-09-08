#! /usr/bin/env python3

def draw_step(_, saddr):
    print(saddr)

def draw_squiral(screen_address: int):
    d = 0
    addresses = list()

    # directions: (delta)
    directions = [-40, 1, 40, -1]  # up, right, down, left

    while screen_address >= 0 and screen_address < 1000:
        for k, delta in enumerate(directions):
            # move d steps
            for _ in range(d + 1):
                addresses.append((screen_address, delta))
                screen_address += delta
                if screen_address < 0 or screen_address >= 1000:
                    break

            # increase step count after every second direction
            if k % 2 == 1:
                d += 2
    return addresses


def code_gen(addrndirs):
    dirbits = {
        -40 : "%01000000",
        1 : "%00000000",
        40 : "%11000000",
        -1 : "%10000000",
    }        
    if len(addrndirs) > 1:
        print("\tLDA\tRNR_screen0+$%04X" % addrndirs[1][0])
        print("\tAND\t#%00111111")
        print(f"\tORA\t#{dirbits[addrndirs[0][1]]}")
        print("\tSTA\tRNR_screen0+$%04X" % addrndirs[0][0])
        code_gen(addrndirs[1:])
    

addrndirs = [_ for _ in draw_squiral(40 * 13 + 20)]
print(addrndirs)
addrndirs.reverse()
code_gen(addrndirs)
