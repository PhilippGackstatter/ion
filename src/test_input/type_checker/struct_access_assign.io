struct NumWrap
    integer: i32

main()
  let wrapper = NumWrap { integer: 234, }
  wrapper.integer = "asdf"
