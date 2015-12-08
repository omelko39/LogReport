import java.io.{PrintWriter, File}

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.xml.Elem

/**
 * Created by Admin on 08.12.2015.
 */


object LogReport {

  case class Log(time: String, thread: String, level: String, who: String, msg: String)

  case class Render(docId: Int, page: Int, uuid: String, thread: ListBuffer[String], start: ListBuffer[String], get: ListBuffer[String])

  val regexp = """(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3}) (\[\w*-\d*\]) (\w*)  (\[\w*\]): (.*)""".r
  val argumentsRegExp = """arguments \[(\d*), (\d*)\]""".r
  val uuidRegExp = """returned (\d*-\d*)""".r

  def getRenderList(filePath: String): ListBuffer[Render] = {

    val renderList = ListBuffer.empty[Render]

    Source.fromFile(filePath).getLines()
      .filter(line => line.contains("[ServiceProvider]:") && (
        line.contains("Executing request startRendering") ||
          line.contains("Service startRendering returned") ||
          line.contains("Service getRendering returned")))
      .map { case regexp(time, thread, level, who, msg) =>
        Log(time, thread, level, who, msg)
      }
      .foreach(log =>
        log.msg match {
          case s: String if s.contains("Executing request startRendering") =>
            argumentsRegExp.findAllIn(s).matchData.foreach(args =>
              renderList.find(render =>
                render.docId == args.group(1).toInt && render.page == args.group(2).toInt
              ).map { el =>
                renderList.-=(el)
                renderList.+=(el.copy(thread = el.thread.+=(log.thread), start = el.start.+=(log.time)))
              }.getOrElse(
                renderList.+=(
                  Render(
                    args.group(1).toInt,
                    args.group(2).toInt,
                    "",
                    ListBuffer(log.thread),
                    ListBuffer(log.time),
                    ListBuffer.empty[String])
                )
              )
            )
          case s: String if s.contains("Service startRendering returned") =>
            renderList.find(_.thread.contains(log.thread)).foreach { a =>
              uuidRegExp.findAllIn(s).matchData.foreach { uuid =>
                renderList.-=(a)
                if(uuid.group(1) != "")
                renderList.+=(a.copy(uuid = uuid.group(1)))
              }
            }
          case s: String if s.contains("Service getRendering returned") =>
            renderList.find(_.thread.contains(log.thread)).foreach { el =>
              renderList.-=(el)
              renderList.+=(el.copy(get = el.get.+=(log.time), thread = el.thread.-=(log.thread)))
            }
        }
      )
    renderList
  }


  def generateXml(filePath: String) = {
    val list = getRenderList(filePath)
    val xml: Elem = <report>
      {list.map { el =>
        <rendering>
          <document>
            {el.docId}
          </document>
          <page>
            {el.page}
          </page>
          <uuid>
            {el.uuid}
          </uuid>{el.start.map(time => <start>
          {time}
        </start>)}{el.get.map(time => <get>
          {time}
        </get>)}
        </rendering>
      }}
      <summary>
        <count>
          {list.map(_.start).flatMap(el => el).size}
        </count>
        <duplicates>
          {list.count(el => list.find(a => el.uuid == a.uuid) match {
          case Some(a) => true
          case None => false
        })}
        </duplicates>
        <unnecessary>
          {list.count(el => el.start.size > el.get.size)}
        </unnecessary>
      </summary>
    </report>
    scala.xml.XML.save("report.xml", xml)
  }


}
