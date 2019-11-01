// Should fail since one of the return types is incompatible
fn bar(mycondition: bool) -> i32 {
    if (mycondition) {
        return "str";
    }
    return 4;
}
