// Should fail since str is not allowed in if
fn bar(first: str) {
    if (first) {
        print first;
    }
}
