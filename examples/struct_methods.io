struct Dog {
    name: str,
    age: i32,
}

impl Dog {

    fn set_name(name: str) {
        self.name = name;
    }

    fn get_name() -> str {
        return self.name;
    }
}

fn main() {
    var dog = Dog {
        name: "Jonas",
        age: 23,
    };

    dog.set_name("Johnny");

    print dog.get_name();
}

main();
