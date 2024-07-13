// TODO Should fail since the print statement uses book but it was partially moved into y.

struct Page
  text: str

struct Book
  length: i32
  page: Page

move()
  var book = Book {
    length: 1,
    page: Page {
      text: "text",
    },
  }
  var y = book.length
  print book

move()
