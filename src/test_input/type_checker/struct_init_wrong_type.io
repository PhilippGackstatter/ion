// Should fail since HTTPStatus' msg field has type str
struct HTTPStatus
    code: i32
    msg: str

var x = HTTPStatus {
    code: 404,
    msg: 404,
}
