struct NumWrap {
    integer: i32,
}

{
    var wrapper = NumWrap { integer: 234, };
    var y = !wrapper.integer;
}