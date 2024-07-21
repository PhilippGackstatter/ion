struct HTTPStatus
    code: i32
    msg: str

main()
  let x = HTTPStatus {
      code: 404,
      msg: "NotFound",
      another_field: "too many",
  }
