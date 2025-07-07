#!/usr/bin/env sh

###############################################################
# Use this to get a rough idea of any unexpectedly fat files.
#
# Else consider using ncdu for an ncurses interface:
# ncdu --enable-delete --enable-refresh -2 --show-graph --show-percent --confirm-quit
###############################################################

# Define threshold size (e.g., 1GB)
THRESHOLD=1G

# Find large files
echo "Finding files larger than $THRESHOLD..."
find ~ -type f -size +$THRESHOLD >large_files.txt

# Display the results
echo "Large files found:"
cat large_files.txt

# Optional: Prompt to delete files
read -p "Do you want to delete these files? (y/n): " choice
if [[ $choice == "y" ]]; then
    xargs rm <large_files.txt
    echo "Files deleted."
else
    echo "No files were deleted."
fi
