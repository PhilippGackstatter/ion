struct Vector
    x: i32
    y: i32
    z: i32


impl Vector

    dot_product(other: Vector) -> i32
        return self.x * other.x + self.y * other.y + self.z * other.z


var z_unit_vector = Vector {
    x: 0,
    y: 0,
    z: 1,
}

var y_unit_vector = Vector {
    x: 0,
    y: 1,
    z: 0,
}

if z_unit_vector.dot_product(y_unit_vector) == 0
    print "Vectors are orthogonal!"
else
    print "Vectors are not orthogonal!"
