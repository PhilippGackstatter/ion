struct Dog
    name: str
    age: i32


impl Dog

    set_name(self, name: str)
        self.name = name

    // It is legal to specify the type of self.
    get_name(self: Dog) -> str
        return self.name


struct Human
    name: str
    pet: Dog


impl Human

    pet_name(self) -> str
        return self.pet.get_name()


var human = Human {
    name: "Johnny",
    pet: Dog {
        name: "Jonas",
        age: 23,
    },
}

human.pet.name = "Puppy"

print human.pet_name()
