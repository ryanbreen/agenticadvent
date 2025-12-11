package main

import (
	"fmt"
	"os"
	"strings"
)

// parseDiskMap parses the disk map into an expanded block representation.
// Returns a slice where each element is file ID or -1 for free space.
func parseDiskMap(filename string) ([]int, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	diskMap := strings.TrimSpace(string(data))
	blocks := []int{}
	fileID := 0
	isFile := true

	for _, ch := range diskMap {
		length := int(ch - '0')
		if isFile {
			for i := 0; i < length; i++ {
				blocks = append(blocks, fileID)
			}
			fileID++
		} else {
			for i := 0; i < length; i++ {
				blocks = append(blocks, -1) // -1 represents free space
			}
		}
		isFile = !isFile
	}

	return blocks, nil
}

// copyBlocks creates a copy of the blocks slice
func copyBlocks(blocks []int) []int {
	result := make([]int, len(blocks))
	copy(result, blocks)
	return result
}

// compactBlocks compacts disk by moving blocks one at a time from end to leftmost free space
func compactBlocks(blocks []int) []int {
	blocks = copyBlocks(blocks)
	left := 0
	right := len(blocks) - 1

	for left < right {
		// Find leftmost free space
		for left < right && blocks[left] != -1 {
			left++
		}
		// Find rightmost file block
		for left < right && blocks[right] == -1 {
			right--
		}

		if left < right {
			// Swap
			blocks[left] = blocks[right]
			blocks[right] = -1
			left++
			right--
		}
	}

	return blocks
}

// calculateChecksum calculates filesystem checksum: sum of position * file_id for each block
func calculateChecksum(blocks []int) int {
	checksum := 0
	for pos, fileID := range blocks {
		if fileID != -1 {
			checksum += pos * fileID
		}
	}
	return checksum
}

// part1 compacts by moving individual blocks, returns checksum
func part1() (int, error) {
	blocks, err := parseDiskMap("../input.txt")
	if err != nil {
		return 0, err
	}
	compacted := compactBlocks(blocks)
	return calculateChecksum(compacted), nil
}

// part2 compacts by moving whole files (highest ID first), returns checksum
func part2() (int, error) {
	blocks, err := parseDiskMap("../input.txt")
	if err != nil {
		return 0, err
	}

	// Find all files: file_id -> (start_pos, length)
	type FileInfo struct {
		start  int
		length int
	}
	files := make(map[int]FileInfo)
	i := 0
	for i < len(blocks) {
		if blocks[i] != -1 {
			fileID := blocks[i]
			start := i
			for i < len(blocks) && blocks[i] == fileID {
				i++
			}
			files[fileID] = FileInfo{start: start, length: i - start}
		} else {
			i++
		}
	}

	// Find max file ID
	maxFileID := 0
	for fileID := range files {
		if fileID > maxFileID {
			maxFileID = fileID
		}
	}

	// Process files in decreasing order of file ID
	for fileID := maxFileID; fileID >= 0; fileID-- {
		info, exists := files[fileID]
		if !exists {
			continue
		}
		start := info.start
		length := info.length

		// Find leftmost span of free space that fits this file
		// Must be to the left of current position
		freeStart := -1
		i := 0
		for i < start {
			if blocks[i] == -1 {
				// Count consecutive free blocks
				spanStart := i
				spanLength := 0
				for i < start && blocks[i] == -1 {
					spanLength++
					i++
				}
				if spanLength >= length {
					freeStart = spanStart
					break
				}
			} else {
				i++
			}
		}

		// Move file if we found a suitable span
		if freeStart != -1 {
			// Clear old position
			for j := start; j < start+length; j++ {
				blocks[j] = -1
			}
			// Write to new position
			for j := freeStart; j < freeStart+length; j++ {
				blocks[j] = fileID
			}
			// Update file position
			files[fileID] = FileInfo{start: freeStart, length: length}
		}
	}

	return calculateChecksum(blocks), nil
}

func main() {
	result1, err := part1()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error in part1: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 1: %d\n", result1)

	result2, err := part2()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error in part2: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 2: %d\n", result2)
}
