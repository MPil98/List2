import io.Source
import io.Codec
import java.nio.charset.CodingErrorAction
import scala.collection.mutable.Set




object Task2{
  def main(args: Array[String]) = {
    val books = List("C:/Users/Marcin/Desktop/Ognem_i_mieczem.txt","C:/Users/Marcin/Desktop/potop1.txt","C:/Users/Marcin/Desktop/potop2.txt","C:/Users/Marcin/Desktop/wolodyjowski.txt","C:/Users/Marcin/Desktop/dante.txt","C:/Users/Marcin/Desktop/proces.txt")
    val booksAsString = books.map(readBook)
    (0 to booksAsString.length-1) foreach{i=>
      (0 to booksAsString.length-1) foreach{j=>
        (4 to 13) foreach{k=>
          print(books(i))
          print(" ")
          print(books(j))
          print(" ")
          print(k)
          print(" ")
          print(jacc_Similarity(booksAsString(i),booksAsString(j),k))
          println()
        }
      }
    }
  }
  def readBook(filename:String):String={
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    return Source.fromFile(filename)(decoder).getLines.mkString.replace(" ","")
  }

  def jacc_Similarity(bookA:String, bookB:String,k:Int):Float={
    val bookAShingles = shingle(bookA,k)
    val bookBShingles = shingle(bookB,k)
    return jaccard(bookAShingles,bookBShingles)
  }

  def shingle(book:String,k:Int):Set[String]={
    var shingles = Set[String]()
    (0 to (book.length-k)) foreach {i=>
      val shing = book.substring(i,i+k)
      shingles+=shing
    }
    return shingles
  }

  def jaccard(bookAShingles:Set[String],bookBShingles:Set[String]):Float={
    val intersection = bookAShingles.intersect(bookBShingles)
    val union = bookAShingles.union(bookBShingles)
    return intersection.size.toFloat/union.size.toFloat
  }
}