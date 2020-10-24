# Error Handling

Use an approach like [this](https://youtu.be/N6b44kMS6OM?t=3117), where there is a sentinel value that represents errors in the AST. If upstream code sees this value, it can just propagate that value upwards.

# Data

Data object like

```
data String
    buf: [u8]
```

should be copy-on-write. Doing

```
str = "test"
copy = str

copy.append("postfix")
println(f"{str} - {copy}")
```

should print `test - testpostfix`.

For a function like `std/sync/mpsc/Sender/send`

```
send<T: Sync>(t: T)
```

the function needs to be marked `Sync` so that the compiler will emit instructions such that `t` is deep-copied,
such that no underlying pointer is shared across threads.

# Resources

A resource object like

```
resource UdpSocket
    sock: std/os/Socket
```

should follow ownership-rules. Doing

```
sock = UdpSocket/new("127.0.0.1")!
copy = sock
buf = sock.receive(512)
```

should fail, since `sock` has been invalidated when it was _moved_ into `copy`.

# Resources inside data objects

What if we put resources inside of data objects? A `List` is `data` (probably), but `std/fs/File` was declared as `resource`.

Options

- It's an error to put resources in data objects. resources cannot be copied, just owned, so when `FileWrapper` is copied the resource can only belong to one of the copies.
- As soon as a data objects contain a resource, it must follow ownership rules. Otherwise it might be difficult to implement collection types as non-resources.

```
read_stuff(map: {String: File})
    for (k, v) in map
        println(v.read_string())

file = File/open("text.txt")!
file2 = File/open("text2.txt")!

map = {
    "f1": file,
    "f2": file2
}

// A map is data, but it contains resources
// so now it follows ownership rules.
// read_stuff takes ownership of the map
read_stuff(map)

// consequently, this is an error
content = map["f1"]!.read_string()
```

This is bad. Now the semantics of `data` and `resources` are the same, so the distinction was moot in the first place.

Does this only affect collection types? Then we might say a `collection List` follows whatever rules its contents follow.
Or the "third" type is `generic List` with the same idea, but the keyword is not limited to collections.

The fundamental problem is that we try to avoid references/pointers by copying, but that probably tanks performance so hard that it's not worth it.
So maybe we do need references in the language...

# Back to the drawing board

## Implicit References

### Mutable References

Do we need references to be explicit? Essentially: Can we do what Rust does, but hide references?

```
impl UdpSocket

    receive(buf: [u8])
        for (i, byte) in rcvd_bytes.enumerate()
            buf[i] = byte

sock = UdpSocket/new("127.0.0.1")!

buf = []
sock.receive(buf)

str = String/from_bytes(buf)
println(str)
```

Here I would like to give receive a `&mut` to the buffer, so that it can write to it.
We know that the assign operation in `UdpSocket/receive` mutates something, so
the method needs to i) own the buf or ii) have a mutable reference to it.

ion can conceivably determine that `receive` takes a reference, since
the buf is used later in the program, so `receive` cannot take ownership.
Now ion checks whether `receive` mutates `buf` or not, and since it does
it must be a mutable reference.
So ion might try different variants, i.e. `mutable reference` and `transfering ownership`
to see if one of those is valid.

### Immutable References

```
str = "immutable"
println(str)
```

Here `println` would simply take a reference to `str`, or alternatively
transfer ownership. The only difference would be at which point the `str` is
cleaned up, which, can be left as a decision to the compiler like it's left as a decision to the garbage collector in other langs.

## Ownership

```
sock = UdpSocket/new("127.0.0.1")!
copy = sock
sock.receive(...)
```

`sock` is a resource and used later, but `copy = sock` is transferring ownership, so this is an error.

## Side Note: Mutable or Immutable by default?

Maybe the programmer doesn't need to answer this question by declaring variables as one or the other.
Because most often the programmer is actually asking the question of "Is this variable being mutated?",
to get a better understanding of the program.
In this system it may be possible to let the compiler figure out if a variable is mutated or not, and
provide it as a inlay hint in the IDE, for instance.

## Lifetimes?

```
data Person
    first_name: String
    last_name: String

make_effective_copy() -> String
    person = Person {
                first_name: "Jon",
                last_name: "Snow"
            }
    // This would need to copy since person is dropped
    person.first_name
    // person is dropped here

copy = make_effective_copy()

make_effective_ref() -> String
    person = Person {
                first_name: "Robert",
                last_name: "Baratheon"
            }

    // This would be a reference
    ref = person.first_name
    println(ref)

    // This would need to copy since the
    // referenced value is dropped
    return ref

longer_part(person: Person, str: String) -> String
    // It cannot be determined which one will be returned
    // at runtime, so if
    // longest of both lifetimes is valid then use references, otherwise
    // copy anyway
    if str.len() < 5
        str
    else
        person.first_name

person = Person {
            first_name: "Eddard",
            last_name: "Stark"
        }

longer = longer_part(person, person.first_name)


```
