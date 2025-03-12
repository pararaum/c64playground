import random

# Sample input list of strings
string_list = [
    "abyss connection", "atlantis", "blazon", "classic videogames radio",
    "commodore treffen graz", "cosmos designs", "cpc user club", "delysid",
    "digital talk team", "excess", "quantum", "trex", "fairlight", "finnish gold",
    "genesis project", "haujobb", "hokuto force", "laxity", "nodepond", "onslaught",
    "padua", "rabenauge", "rebels", "the solution", "triad", "coyhot", "gloegg",
    "harekiet", "jasmin68k", "joe", "phiwa", "sissim", "wizball6502", "xxx", "gorgh"
]

# Initialize 8 empty strings
output_strings = ['' for _ in range(8)]
used_indices = set()

# Process until all lines are used
while len(used_indices) < len(string_list):
    # Select a random unused element from the string list
    unused_indices = [i for i in range(len(string_list)) if i not in used_indices]
    if not unused_indices:
        break  # Break if there are no unused indices

    random_index = random.choice(unused_indices)
    used_indices.add(random_index)

    # Find the shortest string
    shortest_index = min(range(len(output_strings)), key=lambda i: len(output_strings[i]))

    # Append the selected item to the shortest string with 10 spaces in between
    if output_strings[shortest_index]:  # If not the first item
        output_strings[shortest_index] += ' ' * 10
    output_strings[shortest_index] += string_list[random_index]

# Prepare the final output format
final_output = []
for i, s in enumerate(output_strings):
    # Format each string according to the specified output
    formatted_string = f'NextText " \\x1e {s}{" " * 25}", {3 + i * 5}, 6'
    final_output.append(formatted_string)

# Print each formatted string
for line in final_output:
    print(line)
