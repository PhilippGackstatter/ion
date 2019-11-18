struct Dog {
    name: str,
    age: i32,
}

struct Owner {
    name: str,
    dog: Dog,
}

fn create_owner(owner_name: str) -> Owner {
    return Owner {
        name: owner_name,
        dog: Dog {
            name: "Nemo",
            age: 2,
        },
    };
}

fn execute() {
    var owner = create_owner("John");

    owner.dog.name = "Finding me, is finding ...";

    print owner.name;
    print owner.dog.name;
}

execute();
