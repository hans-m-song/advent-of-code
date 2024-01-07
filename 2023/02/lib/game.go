package lib

import (
	"bytes"
	"encoding"
	"fmt"
	"strconv"
)

const (
	INDEX_RED   = 0
	INDEX_GREEN = 1
	INDEX_BLUE  = 2

	KEY_HEADER = "Game "
	KEY_RED    = "red"
	KEY_GREEN  = "green"
	KEY_BLUE   = "blue"
)

var (
	_ encoding.TextUnmarshaler = (*Set)(nil)
	_ encoding.TextUnmarshaler = (*Game)(nil)
)

type Set [3]int64

func (s *Set) UnmarshalText(raw []byte) error {
	if s == nil {
		s = &Set{}
	}

	for _, cube := range bytes.Split(raw, []byte(", ")) {
		count, color, ok := bytes.Cut(cube, []byte(" "))
		if !ok {
			return fmt.Errorf("invalid cube %s", cube)
		}

		value, err := strconv.ParseInt(string(count), 10, 64)
		if err != nil {
			return fmt.Errorf("invalid count '%s': %s", count, err)
		}

		switch string(color) {
		case KEY_RED:
			s[INDEX_RED] = value
		case KEY_GREEN:
			s[INDEX_GREEN] = value
		case KEY_BLUE:
			s[INDEX_BLUE] = value
		default:
			return fmt.Errorf("invalid color %s", color)
		}
	}

	return nil
}

// Satisfies returns true if the current set can satisfy the given set
func (current Set) Satisfies(given Set) bool {
	for i, v := range current {
		if v > given[i] {
			return false
		}
	}

	return true
}

func (s Set) Pow() int64 {
	return s[INDEX_RED] * s[INDEX_GREEN] * s[INDEX_BLUE]
}

type Game struct {
	ID   int64
	Sets []Set
}

func (g *Game) UnmarshalText(raw []byte) error {
	if g == nil {
		g = &Game{}
	}

	raw, ok := bytes.CutPrefix(raw, []byte(KEY_HEADER))
	if !ok {
		return fmt.Errorf("invalid header %s", raw)
	}

	rawID, raw, ok := bytes.Cut(raw, []byte(": "))
	if !ok {
		return fmt.Errorf("invalid id %s", raw)
	}

	id, err := strconv.ParseInt(string(rawID), 10, 64)
	if err != nil {
		return fmt.Errorf("invalid id '%s': %s", rawID, err)
	}

	g.ID = id
	for _, rawSet := range bytes.Split(raw, []byte("; ")) {
		var set Set
		if err := set.UnmarshalText(rawSet); err != nil {
			return fmt.Errorf("invalid set '%s': %s", rawSet, err)
		}

		g.Sets = append(g.Sets, set)
	}

	return nil
}

// Satisfies returns true if all recorded revealed sets can satisfy the given set.
func (g *Game) Satisfies(given Set) bool {
	for _, v := range g.Sets {
		if !v.Satisfies(given) {
			return false
		}
	}

	return true
}

// Guess determines the minimum number of cubes of each color needed to satisfy
// the revealed sets, i.e. the maximum of each color in the revealed sets.
func (g *Game) Guess() Set {
	results := Set{0, 0, 0}
	for _, v := range g.Sets {
		for i, c := range v {
			if c > results[i] {
				results[i] = c
			}
		}
	}

	return results
}
