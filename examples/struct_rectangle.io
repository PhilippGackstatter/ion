struct Rectangle {
    width: i32,
    height: i32,
}

fn area(rect: Rectangle) -> i32 {
    return rect.width * rect.height;
}

var rect = Rectangle {
    width: 4,
    height: 3,
};

print area(rect);