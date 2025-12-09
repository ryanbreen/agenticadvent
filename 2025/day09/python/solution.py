from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input - each line is "x,y"
points = []
for line in input_text.split("\n"):
    x, y = map(int, line.split(","))
    points.append((x, y))


def part1():
    """Find the largest rectangle area using two red tiles as opposite corners."""
    max_area = 0
    n = len(points)

    # Check all pairs of points as opposite corners
    for i in range(n):
        x1, y1 = points[i]
        for j in range(i + 1, n):
            x2, y2 = points[j]
            # Rectangle area = width * height (inclusive of both corners)
            # Width includes both endpoints: |x2-x1| + 1
            # Height includes both endpoints: |y2-y1| + 1
            width = abs(x2 - x1) + 1
            height = abs(y2 - y1) + 1
            area = width * height
            max_area = max(max_area, area)

    return max_area


def part2():
    """Find the largest rectangle using only red and green tiles."""
    from collections import defaultdict

    # Build edges - list of (y, x_start, x_end, is_horizontal) or (x, y_start, y_end, is_vertical)
    # The polygon consists of horizontal and vertical edges connecting consecutive points

    n = len(points)
    horizontal_edges = []  # (y, x_min, x_max)
    vertical_edges = []    # (x, y_min, y_max)

    for i in range(n):
        x1, y1 = points[i]
        x2, y2 = points[(i + 1) % n]

        if y1 == y2:  # Horizontal edge
            horizontal_edges.append((y1, min(x1, x2), max(x1, x2)))
        else:  # Vertical edge
            vertical_edges.append((x1, min(y1, y2), max(y1, y2)))

    # For each row y, compute which x ranges are inside the polygon
    # Using scanline algorithm with vertical edges
    # Sort vertical edges by x coordinate
    vertical_edges.sort()

    # Get all unique y coordinates that matter (red point y values and edge endpoints)
    y_coords = set()
    for x, y in points:
        y_coords.add(y)

    # For efficient rectangle checking, we need to know for each rectangle
    # if it's entirely inside the polygon.
    #
    # Key insight: The polygon boundary forms a simple rectilinear polygon.
    # A rectangle with corners at (x1,y1) and (x2,y2) is valid if and only if
    # the entire rectangle is inside the polygon.
    #
    # For a rectilinear polygon, we can check this by verifying:
    # 1. No vertical edge crosses through the rectangle's interior
    # 2. No horizontal edge crosses through the rectangle's interior
    # 3. The rectangle is inside (not outside) the polygon

    # Build a map of vertical edges by x-coordinate for efficient lookup
    vert_by_x = defaultdict(list)
    for x, y_min, y_max in vertical_edges:
        vert_by_x[x].append((y_min, y_max))

    # Build a map of horizontal edges by y-coordinate
    horiz_by_y = defaultdict(list)
    for y, x_min, x_max in horizontal_edges:
        horiz_by_y[y].append((x_min, x_max))

    def is_inside_polygon(x, y):
        """Check if point (x, y) is inside or on the polygon using ray casting."""
        crossings = 0
        # Cast ray to the right
        for vx in sorted(vert_by_x.keys()):
            if vx <= x:
                continue
            for y_min, y_max in vert_by_x[vx]:
                if y_min < y < y_max:  # Strict inequality - ray crosses edge
                    crossings += 1
                elif y == y_min or y == y_max:
                    # On corner - count as 0.5 crossing
                    crossings += 0.5
        return crossings % 2 == 1

    def rectangle_valid(x1, y1, x2, y2):
        """Check if rectangle from (x1,y1) to (x2,y2) is entirely inside polygon."""
        min_x, max_x = min(x1, x2), max(x1, x2)
        min_y, max_y = min(y1, y2), max(y1, y2)

        # Check if any vertical edge crosses through the rectangle interior
        for vx, edges in vert_by_x.items():
            if min_x < vx < max_x:  # Vertical edge x is inside rectangle's x range
                for y_min, y_max in edges:
                    # Check if this edge segment overlaps with rectangle's y range
                    if not (y_max <= min_y or y_min >= max_y):
                        return False

        # Check if any horizontal edge crosses through the rectangle interior
        for hy, edges in horiz_by_y.items():
            if min_y < hy < max_y:  # Horizontal edge y is inside rectangle's y range
                for x_min, x_max in edges:
                    # Check if this edge segment overlaps with rectangle's x range
                    if not (x_max <= min_x or x_min >= max_x):
                        return False

        # Finally, check that we're inside the polygon (not outside)
        # Check center point
        center_x = (min_x + max_x) / 2
        center_y = (min_y + max_y) / 2
        return is_inside_polygon(center_x, center_y)

    # Find largest valid rectangle with red corners
    max_area = 0

    for i in range(len(points)):
        x1, y1 = points[i]
        for j in range(i + 1, len(points)):
            x2, y2 = points[j]

            if rectangle_valid(x1, y1, x2, y2):
                width = abs(x2 - x1) + 1
                height = abs(y2 - y1) + 1
                area = width * height
                max_area = max(max_area, area)

    return max_area


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
