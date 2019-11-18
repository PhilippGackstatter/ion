struct HTTPStatus {
    code: i32,
    msg: str,
}

{
    var x = HTTPStatus {
        code: 404,
        message: "NotFound",
    };
}