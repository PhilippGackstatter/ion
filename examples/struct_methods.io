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

struct Human {
    name: str,
    pet: Dog,
}

impl Human {

    fn pet_name() -> str {
        return self.pet.get_name();
    }

}

var human = Human {
    name: "Johnny",
    pet: Dog {
        name: "Jonas",
        age: 23,
    },
};

human.pet.name = "Puppy";

print human.pet_name();
