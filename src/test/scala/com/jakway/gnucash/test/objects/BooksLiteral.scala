package com.jakway.gnucash.test.objects

object BooksLiteral {
  val donQuixoteId = "b1615"
  val warAndPeaceId = "b1867"

  val ids: Set[String] = Set(donQuixoteId, warAndPeaceId)

  val numBooks = 2

  //verbatim from https://github.com/scala/scala-xml/wiki/Getting-started
  val books = <books>
    <book id="b1615">Don Quixote</book>
    <book id="b1867">War and Peace</book>
  </books>
}
