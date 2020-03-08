struct Rectangle {
    width: i32,
    height: i32,
}

impl Rectangle {
    fn area() -> i32 {
        return self.width * self.height;
    }
}

var rect = Rectangle {
    width: 4,
    height: 3,
};

print rect.area();
