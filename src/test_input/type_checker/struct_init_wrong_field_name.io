struct HTTPStatus
    code: i32
    msg: str

main()
  let x = HTTPStatus {
      code: 404,
      message: "NotFound",
  }
