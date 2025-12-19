#!/usr/bin/env python3
#
#
# V1.2 November 2025

import argparse
import os
import sys

DEBUG = False

# Default character bitmaps
character_bitmaps = {
    'A': [
        "  #  ",
        " # # ",
        "#   #",
        "#####",
        "#   #",
    ],
    'B': [
        "### ",
        "#  #",
        "### ",
        "#  #",
        "### ",
    ],
    'C': [
        " ###",
        "#   ",
        "#   ",
        "#   ",
        " ###",
    ],
    'D': [
        "###  ",
        "#  # ",
        "#   #",
        "#  # ",
        "###  ",
    ],
    'E': [
        "####",
        "#   ",
        "####",
        "#   ",
        "####",
    ],
    'F': [
        "###",
        "#  ",
        "###",
        "#  ",
        "#  ",
    ],
    'G': [
        " ### ",
        "#    ",
        "#  ##",
        "#   #",
        " ### ",
    ],
    'H': [
        "#  #",
        "#  #",
        "####",
        "#  #",
        "#  #",
    ],
    'I': [
        "#",
        "#",
        "#",
        "#",
        "#",
    ],
    'J': [
        " ##",
        "  #",
        "  #",
        "# #",
        " # ",
    ],
    'K': [
        "#  #",
        "# # ",
        "##  ",
        "# # ",
        "#  #",
    ],
    'L': [
        "#   ",
        "#   ",
        "#   ",
        "#   ",
        "####",
    ],
    'M': [
        "#   #",
        "## ##",
        "# # #",
        "#   #",
        "#   #",
    ],
    'N': [
        "#  #",
        "## #",
        "# ##",
        "# ##",
        "#  #",
    ],
    'O': [
        " ## ",
        "#  #",
        "#  #",
        "#  #",
        " ## ",
    ],
    'P': [
        "#### ",
        "#   #",
        "#### ",
        "#    ",
        "#    ",
    ],
    'Q': [
        " ### ",
        "#   #",
        "#   #",
        "#  ##",
        " ####",
    ],
    'R': [
        "### ",
        "#  #",
        "### ",
        "# # ",
        "#  #",
    ],
    'S': [
        " ### ",
        "#    ",
        " ### ",
        "    #",
        "###  ",
    ],
    'T': [
        "#####",
        "  #  ",
        "  #  ",
        "  #  ",
        "  #  ",
    ],
    'U': [
        "#   #",
        "#   #",
        "#   #",
        "#   #",
        " ### ",
    ],
    'V': [
        "#   #",
        "#   #",
        " # # ",
        " # # ",
        "  #  ",
    ],
    'W': [
        "#   #",
        "#   #",
        "# # #",
        "## ##",
        "#   #",
    ],
    'X': [
        "# #",
        "# #",
        " # ",
        "# #",
        "# #",
    ],
    'Y': [
        "#   #",
        " # # ",
        "  #  ",
        "  #  ",
        "  #  ",
    ],
    'Z': [
        "#####",
        "   # ",
        "  #  ",
        " #   ",
        "#####",
    ],
    'a': [
        "     ",
        " ####",
        "#   #",
        "#   #",
        " ####",
    ],
    'b': [
        "#   ",
        "### ",
        "#  #",
        "#  #",
        "### ",
    ],
    'c': [
        "    ",
        " ###",
        "#   ",
        "#   ",
        " ###",
    ],
    'd': [
        "   #",
        " ###",
        "#  #",
        "#  #",
        " ###",
    ],
    'e': [
        "    ",
        " ## ",
        "####",
        "#   ",
        " ###",
    ],
    'f': [
        "  ##",
        " #  ",
        "### ",
        " #  ",
        " #  ",
    ],
    'g': [
        "    ",
        " ###",
        "#  #",
        " ###",
        "   #",
        "### ",
    ],
    'h': [
        "#   ",
        "### ",
        "#  #",
        "#  #",        
        "#  #",
    ],
    'i': [
        "#",
        " ",
        "#",
        "#",
        "#",
    ],
    'j': [
        "  # ",
        "    ",
        "  # ",
        "  # ",
        "# # ",
        " #  ",
    ],
    'k': [
        "#  ",
        "# #",
        "## ",
        "## ",
        "# #",
    ],
    'l': [
        "# ",
        "# ",
        "# ",
        "# ",
        " #",
    ],
    'm': [
        "     ",
        "#### ",
        "# # #",
        "# # #",
        "# # #",
    ],
    'n': [
        "    ",
        "### ",
        "#  #",
        "#  #",
        "#  #",
    ],
    'o': [
        "     ",
        " ### ",
        "#   #",
        "#   #",        
        " ### ",
    ],
    'p': [
        "    ",
        "### ",
        "#  #",
        "### ",
        "#   ",
        "#   ",
    ],
    'q': [
        "    ",
        " ###",
        "#  #",
        " ###",
        "   #",
        "   #",
    ],
    'r': [
        "   ",
        "###",
        "#  ",
        "#  ",
        "#  ",
    ],
    's': [
        "    ",
        " ###",
        " #  ",
        "  # ",
        "### ",
    ],
    't': [
        " # ",
        "###",
        " # ",
        " # ",
        " ##",
    ],
    'u': [
        "    ",
        "#  #",
        "#  #",
        "#  #",
        " ###",
    ],
    'v': [
        "    ",
        "#  #",
        "#  #",
        " ## ",
        " ## ",
    ],
    'w': [
        "     ",
        "#   #",
        "# # #",
        "# # #",
        " # # ",
    ],
    'x': [
        "    ",
        "# ##",
        " ## ",
        "##  ",
        "# ##",
    ],
    'y': [
        "    ",
        "#  #",
        " ###",
        "   #",
        "### ",
    ],
    'z': [
        "    ",
        "####",
        "  # ",
        " #  ",
        "####",
    ],
    ' ': [
        "   ",
        "   ",
        "   ",
        "   ",
        "   ",
    ],
    '0': [
        " ## ",
        "#  #",
        "#  #",
        "#  #",
        " ## ",
    ],
    '1': [
        " # ",
        "## ",
        " # ",
        " # ",
        "###",
    ],
    '2': [
        "### ",
        "   #",
        " ## ",
        "#   ",
        "####",
    ],
    '3': [
        "### ",
        "   #",
        " ## ",
        "   #",
        "### ",
    ],
    '4': [
        "#  #",
        "#  #",
        "####",
        "   #",
        "   #",
    ],
    '5': [
        "####",
        "#   ",
        "### ",
        "   #",
        "### ",
    ],
    '6': [
        " ## ",
        "#   ",
        "### ",
        "#  #",
        " ## ",
    ],
    '7': [
        "####",
        "   #",
        "  # ",
        " #  ",
        " #  ",
    ],
    '8': [
        " ## ",
        "#  #",
        " ## ",
        "#  #",
        " ## ",
    ],
    '9': [
        " ## ",
        "#  #",
        " ###",
        "   #",
        " ## ",
    ],
    '!': [
        "#",
        "#",
        "#",
        " ",
        "#",
    ],
    '?': [
        " ## ",
        "#  #",
        "  # ",
        "    ",
        " #  ",
    ],
    '.': [
        " ",
        " ",
        " ",
        " ",
        "#",
    ],
    ',': [
        "  ",
        "  ",
        "  ",
        " #",
        "# ",
    ],
    ':': [
        " ",
        "#",
        " ",
        "#",
        " ",
    ],
    ';': [
        "  ",
        " #",
        "  ",
        " #",
        "# ",
    ],
    '-': [
        "   ",
        "   ",
        "###",
        "   ",
        "   ",
    ],
    '+': [
        "  ",
        " #",
        "###",
        " #",
        "  ",
    ],
    '=': [
        "   ",
        "###",
        "   ",
        "###",
        "   ",
    ],
    '*': [
        "    ",
        "# # ",
        " #  ",
        "# # ",
        "    ",
    ],
    '/': [
        "   #",
        "  # ",
        " #  ",
        "#   ",
        "    ",
    ],
    '\\': [
        "#   ",
        " #  ",
        "  # ",
        "   #",
        "    ",
    ],
    '(': [
        " #",
        "# ",
        "# ",
        "# ",
        " #",
    ],
    ')': [
        "# ",
        " #",
        " #",
        " #",
        "# ",
    ],
    '[': [
        "##",
        "# ",
        "# ",
        "# ",
        "##",
    ],
    ']': [
        "##",
        " #",
        " #",
        " #",
        "##",
    ],
    '<': [
        "  #",
        " # ",
        "#  ",
        " # ",
        "  #",
    ],
    '>': [
        "#  ",
        " # ",
        "  #",
        " # ",
        "#  ",
    ],
    "'": [
        "#",
        "#",
        " ",
        " ",
        " ",
    ],
    '"': [
        "# #",
        "# #",
        "   ",
        "   ",
        "   ",
    ],
    '#': [
        " # # ",
        "#####",
        " # # ",
        "#####",
        " # # ",
    ],
    '%': [
        "#  # ",
        "  #  ",
        " #   ",
        "#  # ",
        "     ",
    ],
    '&': [
        " #  ",
        "# # ",
        " #  ",
        "# # ",
        " # #",
    ],
    '@': [
        " ### ",
        "#   #",
        "# ###",
        "# ###",
        " ### ",
    ],
}

def load_charset_from_file(filepath):
    """Load character bitmaps from a file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        charset = {}
        lines = content.strip().split('\n')
        
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            
            # Look for character definitions like 'A':
            if line and line[0] in ("'", '"') and ':' in line:
                # Extract character
                char = line[1]
                
                # Skip the opening bracket line
                i += 1
                if i < len(lines) and '[' in lines[i]:
                    i += 1
                
                # Collect bitmap lines
                bitmap = []
                while i < len(lines):
                    bline = lines[i].strip()
                    
                    # Check if this is a bitmap line (starts with quote)
                    if bline and bline[0] in ('"', "'"):
                        # Extract the content between quotes
                        start_idx = 1
                        end_idx = bline.rfind(bline[0])
                        if end_idx > start_idx:
                            bitmap.append(bline[start_idx:end_idx])
                    
                    # Check if we've reached the end of this character
                    if ']' in bline or (i + 1 < len(lines) and lines[i+1].strip() and lines[i+1].strip()[0] in ("'", '"')):
                        break
                    
                    i += 1
                
                # Trim bitmap if necessary (remove empty leading/trailing rows and columns)
                if bitmap:
                    # Check if all rows are empty
                    all_empty = all(all(c == ' ' for c in row) for row in bitmap)
                    if all_empty:
                        bitmap = ["   " for _ in range(len(bitmap))]
                    else:
                        # Find leftmost and rightmost non-empty columns
                        min_col, max_col = 8, -1
                        for row in bitmap:
                            for col, pixel in enumerate(row):
                                if pixel == '#':
                                    min_col = min(min_col, col)
                                    max_col = max(max_col, col)
                        
                        if max_col >= min_col:
                            bitmap = [row[min_col:max_col+1] for row in bitmap]
                    
                    charset[char] = bitmap
            
            i += 1
        
        if DEBUG:
            print("Loaded charset:")
            for char, bitmap in charset.items():
                print(f"'{char}':")
                for row in bitmap:
                    print(f"  \"{row}\",")
        
        return charset
        
    except Exception as e:
        print(f"Error loading charset from {filepath}: {e}")
        return None

def writer_function(text, charset):
    """Convert text to pixel matrix using character bitmaps
    Supports newlines (\\n) to create multi-line text"""
    if not text:
        return []
    
    # Split text into lines
    lines = text.split('\\n')
    
    # Process each line separately
    line_matrices = []
    max_width = 0
    
    for line in lines:
        if not line:
            # Empty line - create a small gap
            line_matrix = []
        else:
            # Get maximum height among all characters in the line
            max_height = 0
            for char in line:
                if char in charset:
                    max_height = max(max_height, len(charset[char]))
            
            if max_height == 0:
                line_matrix = []
            else:
                # Build the pixel matrix for this line
                line_matrix = [[] for _ in range(max_height)]
                
                for i, char in enumerate(line):
                    if char in charset:
                        char_bitmap = charset[char]
                        char_height = len(char_bitmap)
                        
                        # Add character to matrix
                        for row in range(max_height):
                            if row < char_height:
                                # Add character row
                                for pixel in char_bitmap[row]:
                                    line_matrix[row].append(pixel == '#')
                            else:
                                # Pad with empty pixels if character is shorter
                                char_width = len(char_bitmap[0]) if char_bitmap else 0
                                for _ in range(char_width):
                                    line_matrix[row].append(False)
                    else:
                        # Unknown character, use space
                        if ' ' in charset:
                            char_bitmap = charset[' ']
                            char_height = len(char_bitmap)
                            
                            for row in range(max_height):
                                if row < char_height:
                                    for pixel in char_bitmap[row]:
                                        line_matrix[row].append(pixel == '#')
                                else:
                                    char_width = len(char_bitmap[0]) if char_bitmap else 3
                                    for _ in range(char_width):
                                        line_matrix[row].append(False)
                    
                    # Add spacing between characters (except after last character)
                    if i < len(line) - 1:
                        for row in range(max_height):
                            line_matrix[row].append(False)  # One column spacing
        
        if line_matrix:
            line_matrices.append(line_matrix)
            max_width = max(max_width, len(line_matrix[0]) if line_matrix else 0)
    
    if not line_matrices:
        return []
    
    # Combine all lines into a single matrix
    # Pad each line to max_width and stack vertically with spacing
    combined_matrix = []
    
    for i, line_matrix in enumerate(line_matrices):
        # Add the line rows
        for row in line_matrix:
            # Pad row to max_width
            padded_row = row + [False] * (max_width - len(row))
            combined_matrix.append(padded_row)
        
        # Add spacing between lines (except after last line)
        if i < len(line_matrices) - 1:
            # Add one empty row as spacing
            combined_matrix.append([False] * max_width)
    
    if DEBUG:
        print("Pixel matrix:")
        for row in combined_matrix:
            print(''.join('#' if pixel else ' ' for pixel in row))
    
    return combined_matrix

def pad_matrix(matrix, width, height, align, valign):
    """Pad matrix to specified dimensions with alignment"""
    if not matrix:
        return [[False] * width for _ in range(height)]
    
    current_height = len(matrix)
    current_width = len(matrix[0]) if matrix else 0
    
    # Create new matrix filled with False
    padded = [[False] * width for _ in range(height)]
    
    # Calculate horizontal offset
    if align == 'left':
        x_offset = 0
    elif align == 'right':
        x_offset = max(0, width - current_width)
    else:  # center
        x_offset = max(0, (width - current_width) // 2)
    
    # Calculate vertical offset
    if valign == 'top':
        y_offset = 0
    elif valign == 'bottom':
        y_offset = max(0, height - current_height)
    else:  # middle
        y_offset = max(0, (height - current_height) // 2)
    
    # Copy original matrix to padded matrix
    for y in range(min(current_height, height - y_offset)):
        for x in range(min(current_width, width - x_offset)):
            if y_offset + y < height and x_offset + x < width:
                padded[y_offset + y][x_offset + x] = matrix[y][x]
    
    return padded

def calculate_smart_scale(pixel_matrix, width, height, scale_func):
    """Find the largest scale factor where text still fits in the target dimensions"""
    if not pixel_matrix:
        return 1.0
    
    current_height = len(pixel_matrix)
    current_width = len(pixel_matrix[0]) if pixel_matrix else 0
    
    # Start with a reasonable maximum scale
    max_scale = min(width / current_width, height / current_height) if current_width > 0 and current_height > 0 else 1.0
    
    # Binary search for the optimal scale
    low = 0.1
    high = max_scale * 1.1  # Add a bit of margin
    best_scale = 1.0
    
    # Use binary search with precision of 0.01
    while high - low > 0.01:
        mid = (low + high) / 2
        
        # Try scaling with this factor
        scaled = scale_func(pixel_matrix, mid)
        scaled_height = len(scaled)
        scaled_width = len(scaled[0]) if scaled else 0
        
        # Check if it fits
        if scaled_width <= width and scaled_height <= height:
            best_scale = mid
            low = mid
        else:
            high = mid
    
    if DEBUG:
        print(f"Smart scale found: {best_scale:.2f}")
    
    return best_scale

def scale_matrix_nearestneighbour(matrix, scale_factor):
    """Scale matrix by given factor using nearest neighbor"""
    if not matrix or scale_factor <= 0:
        return matrix
    
    original_height = len(matrix)
    original_width = len(matrix[0]) if matrix else 0
    
    new_height = max(1, int(original_height * scale_factor))
    new_width = max(1, int(original_width * scale_factor))
    
    scaled = [[False] * new_width for _ in range(new_height)]
    
    for y in range(new_height):
        for x in range(new_width):
            orig_y = int(y / scale_factor)
            orig_x = int(x / scale_factor)
            if orig_y < original_height and orig_x < original_width:
                scaled[y][x] = matrix[orig_y][orig_x]
    
    return scaled
    
def scale_matrix_smallspot_bilinear(matrix, scale_factor):
    """Scale matrix using bilinear interpolation for smoother results"""
    if not matrix or scale_factor <= 0:
        return matrix
    
    original_height = len(matrix)
    original_width = len(matrix[0]) if matrix else 0
    
    new_height = max(1, int(original_height * scale_factor))
    new_width = max(1, int(original_width * scale_factor))
    
    # Handle boolean matrices by converting to float temporarily
    is_boolean = isinstance(matrix[0][0], bool)
    
    scaled = [[0.0] * new_width for _ in range(new_height)]
    
    for y in range(new_height):
        for x in range(new_width):
            # Map back to original coordinates (floating point)
            orig_y = y / scale_factor
            orig_x = x / scale_factor
            
            # Get the four surrounding pixels
            y1 = int(orig_y)
            x1 = int(orig_x)
            y2 = min(int((y+.5) / scale_factor),original_height - 1)
            x2 = min(int((x+.5) / scale_factor),original_width - 1)
            
            # Calculate interpolation weights
            wy = orig_y - y1
            wx = orig_x - x1
            
            # Get values (convert bool to float for interpolation)
            v11 = float(matrix[y1][x1])
            v12 = float(matrix[y1][x2])
            v21 = float(matrix[y2][x1])
            v22 = float(matrix[y2][x2])
            
            # Bilinear interpolation
            top = v11 * (1 - wx) + v12 * wx
            bottom = v21 * (1 - wx) + v22 * wx
            result = top * (1 - wy) + bottom * wy
            
            # Convert back to appropriate type
            if is_boolean:
                scaled[y][x] = result > 0.5
            else:
                scaled[y][x] = result
    
    return scaled    
    
def scale_matrix_bilinear(matrix, scale_factor):
    """Scale matrix using bilinear interpolation for smoother results"""
    if not matrix or scale_factor <= 0:
        return matrix
    
    original_height = len(matrix)
    original_width = len(matrix[0]) if matrix else 0
    
    new_height = max(1, int(original_height * scale_factor))
    new_width = max(1, int(original_width * scale_factor))
    
    # Handle boolean matrices by converting to float temporarily
    is_boolean = isinstance(matrix[0][0], bool)
    
    scaled = [[0.0] * new_width for _ in range(new_height)]
    
    for y in range(new_height):
        for x in range(new_width):
            # Map back to original coordinates (floating point)
            orig_y = y / scale_factor
            orig_x = x / scale_factor
            
            # Get the four surrounding pixels
            y1 = int(orig_y)
            x1 = int(orig_x)
            y2 = min(y1 + 1, original_height - 1)
            x2 = min(x1 + 1, original_width - 1)
            
            # Calculate interpolation weights
            wy = orig_y - y1
            wx = orig_x - x1
            
            # Get values (convert bool to float for interpolation)
            v11 = float(matrix[y1][x1])
            v12 = float(matrix[y1][x2])
            v21 = float(matrix[y2][x1])
            v22 = float(matrix[y2][x2])
            
            # Bilinear interpolation
            top = v11 * (1 - wx) + v12 * wx
            bottom = v21 * (1 - wx) + v22 * wx
            result = top * (1 - wy) + bottom * wy
            
            # Convert back to appropriate type
            if is_boolean:
                scaled[y][x] = result > 0.5
            else:
                scaled[y][x] = result
    
    return scaled
    
def scale_matrix_lanczos(matrix, scale_factor, a=3):
    """Scale matrix using Lanczos resampling for high-quality results"""
    import math
    
    if not matrix or scale_factor <= 0:
        return matrix
    
    original_height = len(matrix)
    original_width = len(matrix[0]) if matrix else 0
    
    new_height = max(1, int(original_height * scale_factor))
    new_width = max(1, int(original_width * scale_factor))
    
    def lanczos_kernel(x, a):
        """Lanczos kernel function"""
        if x == 0:
            return 1.0
        elif abs(x) < a:
            return a * math.sin(math.pi * x) * math.sin(math.pi * x / a) / (math.pi * math.pi * x * x)
        else:
            return 0.0
    
    is_boolean = isinstance(matrix[0][0], bool)
    scaled = [[0.0] * new_width for _ in range(new_height)]
    
    for y in range(new_height):
        for x in range(new_width):
            orig_y = y / scale_factor
            orig_x = x / scale_factor
            
            # Calculate the range of source pixels to consider
            y_start = max(0, int(orig_y - a + 1))
            y_end = min(original_height, int(orig_y + a + 1))
            x_start = max(0, int(orig_x - a + 1))
            x_end = min(original_width, int(orig_x + a + 1))
            
            total_weight = 0.0
            weighted_sum = 0.0
            
            # Apply Lanczos filter
            for sy in range(y_start, y_end):
                for sx in range(x_start, x_end):
                    weight = lanczos_kernel(orig_y - sy, a) * lanczos_kernel(orig_x - sx, a)
                    if weight != 0:
                        total_weight += weight
                        weighted_sum += weight * float(matrix[sy][sx])
            
            if total_weight > 0:
                result = weighted_sum / total_weight
            else:
                result = 0.0
            
            if is_boolean:
                scaled[y][x] = result > 0.5
            else:
                scaled[y][x] = max(0.0, min(1.0, result)) if 0 <= result <= 1 else result
    
    return scaled    

def apply_trapezoid_transform(matrix, top_width, bottom_width):
    """
    Apply trapezoid/perspective transformation to the matrix.
    The matrix width remains constant, but content is horizontally scaled
    based on vertical position - narrower at top, wider at bottom.
    
    Args:
        matrix: Input pixel matrix (height x width)
        top_width: Effective width at the top of trapezoid
        bottom_width: Effective width at the bottom of trapezoid
    
    Returns:
        Transformed matrix with same dimensions
    """
    if not matrix:
        return matrix
    
    height = len(matrix)
    width = len(matrix[0]) if matrix else 0
    
    if height == 0 or width == 0:
        return matrix
    
    # Create output matrix
    transformed = [[False] * width for _ in range(height)]
    
    # For each row, calculate its effective width based on trapezoid interpolation
    for y in range(height):
        # Linear interpolation: progress from 0 (top) to 1 (bottom)
        progress = y / (height - 1) if height > 1 else 0.5
        
        # Calculate effective width for this row
        row_width = top_width + (bottom_width - top_width) * progress
        
        # Calculate scaling factor for this row
        scale_x = row_width / width
        
        # Calculate horizontal offset to center the scaled content
        offset = (width - row_width) / 2
        
        # Transform this row
        for x in range(width):
            # Map destination x to source x position
            # Account for centering offset and scaling
            src_x = (x - offset) / scale_x
            
            if 0 <= src_x < width:
                # Use nearest neighbor for boolean matrix
                src_x_int = int(src_x + 0.5)
                if 0 <= src_x_int < width:
                    transformed[y][x] = matrix[y][src_x_int]
    
    return transformed

def encoder_function(matrix, width, height, nsprites):
    """Encode pixel matrix into sprite data"""
    assert len(matrix) == height, f"Matrix height {len(matrix)} != expected {height}"
    assert len(matrix[0]) == width, f"Matrix width {len(matrix[0])} != expected {width}"
    
    # Initialize sprite data (nsprites * 64 bytes each)
    img = [0] * (nsprites * 64)
    
    for x in range(width):
        for y in range(height):
            if matrix[y][x]:  # If pixel is set
                sprnum = x // 24  # Which sprite (0-based)
                sprx = (x // 8) % 3  # Which column within sprite (0-2)
                xbit = 2 ** (7 - (x & 7))  # Which bit within byte
                spry = y  # Which row
                
                if sprnum < nsprites:
                    byte_index = sprnum * 64 + sprx + spry * 3
                    if byte_index < len(img):
                        img[byte_index] |= xbit
    
    return bytes(img)

def main():
    parser = argparse.ArgumentParser(description='Text scaling tool with sprite encoding')
    parser.add_argument('-t', '--text', help='Text line to print (supports \\\\n for multi-line).', required=True)
    parser.add_argument('-n', '--nsprites', help='Number of sprites to use.', type=int, default=8)
    parser.add_argument('-a', '--align', help='Text alignment: left, center, right.', default='center')
    parser.add_argument('-v', '--valign', help='Vertical alignment: top, middle, bottom.', default='middle')
    parser.add_argument('-c', '--charset', help='Use character set from a specified file.')
    parser.add_argument('-sf', '--scalingfactor', help='Scaling factor per iteration.', type=float, default=0.8)
    parser.add_argument('-i', '--iterations', help='Number of scaling iterations.', type=int, default=8)
    parser.add_argument('-o', '--output', help='File path to save the output image.', required=True)
    parser.add_argument('-r', '--reverse', help='Reverse the order of sprite data iterations.', action='store_true')
    parser.add_argument('--debug', help='Enable debug output.', action='store_true')
    parser.add_argument('--scale', help='Scale factor for the initial text. Use "smart" for automatic fitting, or a number (default 2 if no argument).', nargs='?', default=None, const='2')
    parser.add_argument('--trapezoid', help='Apply trapezoid/perspective effect. Format: "top_width:bottom_width" (default: 184:192). Use without argument for default ratio.', nargs='?', default=None, const='184:192')

    args = parser.parse_args()
    
    global DEBUG
    DEBUG = args.debug
    
    # Calculate dimensions
    WIDTH = args.nsprites * 24
    HEIGHT = 21
    
    #scale_matrix_bilinear scale_matrix_smallspot_bilinear scale_matrix_lanczos scale_matrix_nearestneighbour
    scale_matrix = scale_matrix_smallspot_bilinear
    
    # Select charset
    if args.charset:
        charset = load_charset_from_file(args.charset)
        if charset is None:
            print("Failed to load charset, using default")
            charset = character_bitmaps
    else:
        charset = character_bitmaps
    
    # Convert text to pixel matrix
    pixel_matrix = writer_function(args.text, charset)
    
    # Determine initial scale
    initial_scale = None
    if args.scale is not None:
        if args.scale.lower() == 'smart':
            initial_scale = calculate_smart_scale(pixel_matrix, WIDTH, HEIGHT, scale_matrix)
            if DEBUG:
                print(f"Using smart scale: {initial_scale:.2f}")
        else:
            try:
                initial_scale = float(args.scale)
            except ValueError:
                print(f"Error: Invalid scale value '{args.scale}'. Using default (no scaling).")
                initial_scale = None
    
    if initial_scale is not None:
        pixel_matrix = scale_matrix(pixel_matrix, initial_scale)
    
    # Parse trapezoid settings
    trapezoid_enabled = False
    trapezoid_top_width = 184
    trapezoid_bottom_width = 192
    
    if args.trapezoid is not None:
        trapezoid_enabled = True
        try:
            parts = args.trapezoid.split(':')
            if len(parts) == 2:
                trapezoid_top_width = int(parts[0])
                trapezoid_bottom_width = int(parts[1])
            else:
                print(f"Warning: Invalid trapezoid format '{args.trapezoid}'. Using default 184:192")
        except ValueError:
            print(f"Warning: Invalid trapezoid values '{args.trapezoid}'. Using default 184:192")
        
        if DEBUG:
            print(f"Trapezoid mode enabled: {trapezoid_top_width}:{trapezoid_bottom_width}")
    
    # Open output file
    try:
        scale=1.0
        sprite_data_list = []  # Collect all sprite data iterations
        
        # Repeat for specified iterations
        for iteration in range(args.iterations):
            if DEBUG:
                print(f"\nIteration {iteration + 1}:")
            
            if scale==1.0:
                current_matrix=pixel_matrix
            else:
                current_matrix = scale_matrix(pixel_matrix, scale)
            # Pad matrix to target dimensions
            padded_matrix = pad_matrix(current_matrix, WIDTH, HEIGHT, args.align, args.valign)
            
            # Apply trapezoid transformation if enabled
            if trapezoid_enabled:
                padded_matrix = apply_trapezoid_transform(padded_matrix, trapezoid_top_width, trapezoid_bottom_width)
            
            # Encode sprite data
            encoded_data = encoder_function(padded_matrix, WIDTH, HEIGHT, args.nsprites)
            sprite_data_list.append(encoded_data)
            
            if DEBUG:
                print(f"Encoded {len(encoded_data)} bytes")
            
            # Scale matrix for next iteration
            scale *= args.scalingfactor
        
        # Reverse order if requested
        if args.reverse:
            sprite_data_list.reverse()
            if DEBUG:
                print("\nReversed sprite data order")
        
        # Write all sprite data to file
        with open(args.output, 'wb') as outfile:
            for encoded_data in sprite_data_list:
                outfile.write(encoded_data)
                
        if DEBUG:
            print(f"\nWrote total of {sum(len(data) for data in sprite_data_list)} bytes")
    
    except Exception as e:
        print(f"Error writing to output file: {e}")
        sys.exit(1)
    
    print(f"Successfully generated {args.output}")

if __name__ == "__main__":
    main()