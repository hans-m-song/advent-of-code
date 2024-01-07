package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/hans-m-song/advent-of-code/2023/02/lib"
)

const (
	MAX_CUBE_RED   = 12
	MAX_CUBE_GREEN = 13
	MAX_CUBE_BLUE  = 14
)

func readGames() ([]lib.Game, error) {
	var games []lib.Game

	input := bufio.NewReader(os.Stdin)
	for readable := true; readable; {
		line, err := input.ReadString('\n')
		line = strings.TrimSuffix(line, "\n")
		if err != nil && err != io.EOF {
			return nil, fmt.Errorf("failed to read line: %s", err)
		}

		if err == io.EOF {
			readable = false
		}

		if len(line) < 1 {
			continue
		}

		var game lib.Game
		if err := game.UnmarshalText([]byte(line)); err != nil {
			return nil, fmt.Errorf("invalid game '%s': %s\n", line, err)
		}

		fmt.Printf("game: %+v, err: %+v\n", game, err)
		games = append(games, game)
	}

	return games, nil
}

func main() {
	games, err := readGames()
	if err != nil {
		fmt.Printf("failed to read games: %s\n", err)
		os.Exit(1)
	}

	count := 0
	sum := 0
	contents := lib.Set{MAX_CUBE_RED, MAX_CUBE_GREEN, MAX_CUBE_BLUE}
	for _, game := range games {
		if game.Satisfies(contents) {
			count += 1
			sum += int(game.ID)
		}
	}

	fmt.Printf("part 1 - %d/%d games, total %d\n", count, len(games), sum)

	var total int64
	for _, game := range games {
		total += game.Guess().Pow()
	}

	fmt.Printf("part 2 - total: %d\n", total)
}
