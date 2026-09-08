#!/usr/bin/env python3
"""xor_qadz.py ----------- Reads two C64 images, extracts the
8000-byte bitmap from each, XORs them together, and writes the result
to an output file.  Optionally compresses the output with the QADZ
LZ77-like algorithm ported from qadz.cc.

Artstudio file layout (9002 bytes total):
    Offset     Size   Description
    -------  ------   ---------------------------
      0          2    Load address (skipped)
      2       8000    Bitmap data  ← XORed here
   8002       1000    Screen RAM
   9002          1    Border
  -------  -------
  Total:      9002

Usage:
    python xor_qadz.py <file1.koa> <file2.koa> <outfile> [--compress]

"""

import argparse
import sys
from typing import List


# ---------------------------------------------------------------------------
# Constants mirrored from qadz.cc
# ---------------------------------------------------------------------------
LOOK_BACK     = 255
MAX_LEN       = 128
MAX_PLAIN_LEN = 127


# ---------------------------------------------------------------------------
# QADZ compressor (Python port of crunch_qadz)
# ---------------------------------------------------------------------------

class _OutClass:
    """Accumulates plain-literal and back-reference tokens into a byte list."""

    def __init__(self) -> None:
        self._buf: List[int] = []   # pending literal bytes
        self.out: List[int] = []    # final output bytes

    # -- internal helpers ----------------------------------------------------

    def _flush(self) -> None:
        """Emit any buffered literals as a length-prefixed literal run."""
        if self._buf:
            self.out.append(len(self._buf) & 0xFF)   # positive length byte
            self.out.extend(self._buf)
            self._buf.clear()

    # -- public interface -----------------------------------------------------

    def putc(self, byte: int) -> None:
        """Append a literal byte; flush if the literal buffer is full."""
        self._buf.append(byte & 0xFF)
        if len(self._buf) == MAX_PLAIN_LEN:
            self._flush()

    def puttoken(self, pos: int, length: int) -> None:
        """Emit a back-reference token (negative length byte + position byte)."""
        self._flush()
        # Negative length stored as a signed byte: -length in two's complement
        self.out.append((-length) & 0xFF)
        self.out.append(pos & 0xFF)

    def finalize(self) -> bytes:
        """Flush remaining literals, append the end-of-stream zero, return bytes."""
        self._flush()
        self.out.append(0)           # end-of-stream sentinel
        return bytes(self.out)


def crunch_qadz(data: bytes) -> bytes:
    """
    LZ77-like compressor ported from crunch_qadz() in qadz.cc.

    Encoding:
      - A *positive* byte N followed by N literal bytes.
      - A *negative* byte -N (stored as unsigned two's-complement) followed
        by a single position byte P: repeat N bytes starting at
        current_output_pos - P.
      - A zero byte terminates the stream.

    Parameters
    ----------
    data : bytes
        Raw input data to compress.

    Returns
    -------
    bytes
        Compressed output.
    """
    datasize = len(data)
    outclass = _OutClass()

    pos = 0
    while pos < datasize:
        best_pos   = 0
        best_match = 0          # length of best match found so far

        # How far back can we look?
        max_look_back = min(pos, LOOK_BACK)

        # Search every possible back-reference distance.
        for cmpj in range(max_look_back, 0, -1):
            # Count how many bytes match starting at (pos - cmpj) vs pos.
            cmpi = 0
            while cmpi < MAX_LEN:
                if pos + cmpi >= datasize:
                    break
                if data[pos - cmpj + cmpi] != data[pos + cmpi]:
                    break
                cmpi += 1

            if cmpi > best_match:
                best_pos   = cmpj
                best_match = cmpi

        if best_match < 3:
            # Not worth encoding as a back-reference; emit as a literal.
            outclass.putc(data[pos])
            pos += 1
        else:
            outclass.puttoken(best_pos, best_match)
            pos += best_match       # advance past the matched region

    return outclass.finalize()


# ---------------------------------------------------------------------------
# Artstudio format constants
# ---------------------------------------------------------------------------
ARTSTUDIO_SIZE        = 9003
ARTSTUDIO_BITMAP_OFF  = 2       # byte offset where the bitmap starts
ARTSTUDIO_BITMAP_LEN  = 8000    # bytes of raw bitmap data


# ---------------------------------------------------------------------------
# XOR helper
# ---------------------------------------------------------------------------

def xor_artstudio_bitmaps(path1: str, path2: str) -> bytes:
    """
    Read two C64 Artstudio files, extract the 8000-byte bitmap from each
    (bytes 2–8001, i.e. after the 2-byte load address), and return
    their XOR.
    """
    with open(path1, "rb") as f1:
        raw1 = f1.read()
    with open(path2, "rb") as f2:
        raw2 = f2.read()

    # Do not check sizes...
    #if len(raw1) != ARTSTUDIO_SIZE:
    #    sys.exit(f"Error: '{path1}' is {len(raw1)} bytes, expected {ARTSTUDIO_SIZE} (Artstudio format).")
    #if len(raw2) != ARTSTUDIO_SIZE:
    #    sys.exit(f"Error: '{path2}' is {len(raw2)} bytes, expected {ARTSTUDIO_SIZE} (Artstudio format).")

    bitmap1 = raw1[ARTSTUDIO_BITMAP_OFF : ARTSTUDIO_BITMAP_OFF + ARTSTUDIO_BITMAP_LEN]
    bitmap2 = raw2[ARTSTUDIO_BITMAP_OFF : ARTSTUDIO_BITMAP_OFF + ARTSTUDIO_BITMAP_LEN]

    return bytes(a ^ b for a, b in zip(bitmap1, bitmap2))


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description=(
            "XOR the bitmap sections of two C64 Artstudio Painter images "
            "and optionally compress the result with the QADZ LZ77 algorithm."
        )
    )
    parser.add_argument("file1",   help="First Artstudio image (9003 bytes)")
    parser.add_argument("file2",   help="Second Artstudio image (9003 bytes)")
    parser.add_argument("outfile", help="Output file path")
    parser.add_argument(
        "--compress", "-c",
        action="store_true",
        help="Compress the XOR output with the QADZ algorithm before writing",
    )
    args = parser.parse_args()

    # Step 1: XOR the bitmaps
    print(f"Reading '{args.file1}' and '{args.file2}' …")
    xored = xor_artstudio_bitmaps(args.file1, args.file2)
    print(f"XOR result: {len(xored)} bytes (bitmap only)")

    # Step 2: optionally compress
    if args.compress:
        print("Compressing with QADZ …")
        output = crunch_qadz(xored)
        ratio  = len(output) / len(xored) * 100
        print(f"Compressed: {len(output)} bytes  ({ratio:.1f}% of original)")
    else:
        output = xored

    # Step 3: write output
    with open(args.outfile, "wb") as f:
        f.write(output)
    print(f"Written to '{args.outfile}'.")


if __name__ == "__main__":
    main()
