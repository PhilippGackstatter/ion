struct Vector
    x: i32
    y: i32
    z: i32

enum Result<T, E>
    Ok(T)
    Err(E)

enum Option<T>
    Some(T)
    None

enum Type
    Integer
    Bool
    String(str)

impl Vector

    dot_product(other: Vector) -> i32
        self.x * other.x + self.y * other.y + self.z * other.z

    add(other: Vector) -> Vector
        x = self.x + other.x
        y = self.y + other.y
        z = self.z + other.z

        Vector { x, y, z }



z_unit_vector = Vector {
    x: 0
    y: 0
    z: 1
}

y_unit_vector = Vector { x: 0, y: 1, z: 0 }


if z_unit_vector.dot_product(y_unit_vector) == 0
    print "Vectors are orthogonal!"
else
    print "Vectors are not orthogonal!";

trait Iterator

    type Item

    next() -> Option<Self/Item>

trait Collection<T>

    add(item: T)
    remove(item: T)
    get(index: int) -> Option<T>
    contains(item: T) -> bool


impl std/iter/Iterator for Vector

    type Item = i32

    next() -> Option<T>
        self.index += 1
        if self.index < self.len
            self[self.index]
        else
            None

while year < 2020
    year += 1
    print year


for x in 0..1
    print x
