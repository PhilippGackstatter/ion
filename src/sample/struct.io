struct MyType {}

struct MyType2 {
    field1: MyType
}

struct MyType3 {
    field1: MyType,
}

struct MyType4 {
    field1: MyType,
    field2: MyType2,
    field3: MyType3,
}