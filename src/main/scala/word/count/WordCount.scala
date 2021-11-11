/*
Data: 10/11/2021

Técnicas de Programação 2 - Prof Rodrigo Bonifácio
Universidade de Brasília
2/2021

Grupo 10 - The One

Aluno             Matrícula
Lucas Cardoso     211001852
Jefte Batista     180057570

Aplicação do estilo de Programação The One para Algoritmo de Contagem de Palavras
 */

package word.count

import scala.io.Source._
import java.io.FileNotFoundException

object WordCount {

  case class TheOne[+T](private val internalValue: T) {

    type M[A] = A

    def pure[A](a: A): M[A] = a

    // get function to unwrap value from abstraction
    def get: T = synchronized {
      internalValue
    }

    // bind
    def bind[S](transformer: T => TheOne[S]): TheOne[S] = synchronized{
      return transformer(internalValue)
    }

  }

  // Read text file
  def read_file (fileName : String) :  Array[String] = {
    val words : Iterator[String] = try {
      fromFile(fileName).getLines
    } catch {
      case e: FileNotFoundException =>
        sys.error("No file named %s found".format(fileName))
    }
    return words.toArray
  }

  // Remove numbers, special characters and single letter words
  def filter_chars (words :  Array[String]) : Array[String] = {
    val result = words.map(s => s.filter(s => s.isLetter || s.isWhitespace)).filter(_.nonEmpty)
    return result.map(s => s.replaceAll("(\\s.\\s)|(\\s.$)", " "))
  }

  // Normalize text to lowercase
  def normalize(words: Array[String]): Array[String] = {
    return words.map(s => s.toLowerCase())
  }

  // Turn lines into words
  def scan(words: Array[String]): Array[String] ={
    val trimmed = words.map(s => s.replaceAll("""\s+""", " ").trim)
    return trimmed.flatMap(_.split("\\s"))
  }

  // Remove stop words from Strings
  def remove_stop_words(words: Array[String]): Array[String] = {
    val stopWords = try {
      fromFile("src/main/resources/stopwords.txt").getLines.mkString("\\b(", "|", ")\\b").r
    } catch {
      case e: FileNotFoundException =>
        sys.error("No file named stopwords.txt found")
    }
    return words.map(s => stopWords.replaceAllIn(s, ""))
  }

  // Generate map with words and their frequencies
  def frequencies(words: Array[String]): Map[String, Int] = {
    return words.foldLeft(Map.empty[String, Int]){(count, word) => count + (word -> (count.getOrElse(word, 0) + 1))}
  }

  // Sort words by their frequency value
  def sort(words: Map[String,Int]): List[(String, Int)] = {
    return words.toList.sortWith{_._2 > _._2}
  }

  // Print 25 most frequent words from sorted word map
  def top25_frequencies(words: List[(String, Int)]): Unit  = {
    val top25 = words.take(25)
    var counter = 1
    top25.foreach{
      p => println("Rank = "+ counter + ", Word = " + p._1 + ", Occurrences = " + p._2)
        counter+=1
    }
  }

    def main (args: Array[String]): Unit = {

      val filename : String = "src/main/resources/scala.txt"

      //top25_frequencies(sort(frequencies(scan(remove_stop_words(filter_chars(normalize(read_file(filename))))))))

      val words = new TheOne(filename)

      words.bind(s => TheOne(read_file(s))).bind(s => TheOne(normalize(s))).bind(s => TheOne(filter_chars(s))).bind(s => TheOne(remove_stop_words(s))).bind(s => TheOne(scan(s))).bind(s => TheOne(frequencies(s))).bind(s => TheOne(sort(s))).bind(s => TheOne(top25_frequencies(s)))


    }
}
