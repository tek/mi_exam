package tryp
package mi
package viz

import scalaz.stream.Process

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import fs2._
import fs2.util._
import Step._
import Stream.Handle

import monocle.macros._

import org.http4s._
import org.http4s.circe._
import org.http4s.util._
import MediaType._
import headers._
import client.blaze._
import client.{Client, middleware}

object Local
{
  implicit def strat = Strategy.sequential

  implicit def anyEntityDecoder[A: Decoder]: EntityDecoder[A] = jsonOf[A]

  implicit def zTaskToFs[A](z: ZTask[A]): Task[A] = {
    Task(z.unsafePerformSync)
  }

  implicit class ZTaskOps[A](self: ZTask[A])
  {
    def fs: Task[A] = self
  }
}

import Local._

case class Requester[A: EntityDecoder](api: Api, request: Request)
extends Logging
{
  override def loggerName = List("req")

  def reqLens = GenLens[Requester[A]](_.request)

  def bodyLens = reqLens ^|-> GenLens[Request](_.body)

  def methodLens = reqLens ^|-> GenLens[Request](_.method)

  def uriLens = reqLens ^|-> GenLens[Request](_.uri)

  def headersLens = reqLens ^|-> GenLens[Request](_.headers)

  def encodeBody[B: Encoder](data: B) = {
    implicit val enc = jsonEncoderOf[B]
    Process.eval(EntityEncoder[B].toEntity(data)).flatMap(_.body)
  }

  // def apply[B: Encoder](dat: B) = data(dat).run

  val jsonHeaders = Headers(`Content-Type`(`application/json`))

  def data[B: Encoder](dat: B) =
    bodyLens.set(encodeBody(dat))(headersLens.set(jsonHeaders)(this))

  def run = {
    log.debug(s"run $request")
    api.client.fetchAs[A](request).fs
  }

  def runIgnoreResponse = {
    log.debug(s"run $request")
    api.client.open.run(request).fs
  }

  def at(path: String) =
    uriLens.modify(_ / path)(this)

  def fetch(path: String) = at(path).run

  def method(meth: Method) =
    methodLens.set(meth)(this)

  def fetchMethod(meth: Method) =
    method(meth).run

  def asPost =
    method(Method.POST)

  def asPut =
    method(Method.PUT)

  def get =
    fetchMethod(Method.GET)

  def post =
    asPost.run

  def put =
    asPut.run

  def delete =
    fetchMethod(Method.DELETE)

  def /(path: String) =
    at(path)

  def map(f: Request => Request) = reqLens.modify(f)(this)

  def mapUri(f: Uri => Uri) = uriLens.modify(f)(this)
}

case class Api(base: Uri)
{
  lazy val client = middleware.FollowRedirect(3)(PooledHttp1Client())

  def uri(path: String) = base / path

  def req[A: EntityDecoder] =
    Requester[A](this, Request(uri = base))

  def /(path: String) = Api(base / path)
}

case class VisualizationUpdateCodec(series: List[List[Double]])

case class VisualizationCreateCodec
(`type`: String, description: String, series: List[List[Double]],
  label: List[Int], size: List[Double], alpha: List[Double], xaxis: String,
  yaxis: String)

case class Visualization
(id: String, name: Option[String], description: Option[String])
{
  // def plot[A: Plot](a: A): VisualizationUpdateCodec = {
  // }
}

object Names
{
  def v = "visualizations"
}

case class Visualizations(session: Session)
{
  import Names._

  def req[A: EntityDecoder] = session.req[A] / v

  def rootReq[A: EntityDecoder] = session.api.req[A] / v

  def all = req[List[Visualization]].get

  def create(tpe: String, name: String) = {
    val data = VisualizationCreateCodec(tpe, name, List(List(1d)), Nil, Nil,
      Nil, "x", "y")
    req[Visualization].data(data).post
  }

  def find(pred: Visualization => Boolean) =
    all.map(_.find(pred))

  def findOrCreate(tpe: String, name: String) = {
    all
      .flatMap { vs =>
        vs.find(_.description.contains(name)) match {
          case None =>
            create(tpe, name)
          case Some(v) => Task(v)
        }
      }
  }

  def delete(id: String) =
    (rootReq[Visualization] / id).delete

  def update[A: Plot](viz: Visualization, a: A) = {
    val data = VisualizationUpdateCodec(a.data)
    (rootReq[Visualization] / viz.id)
      .data(data)
      .put
  }
}

case class Session(api: Api, sessionId: String)
{
  def req[A: EntityDecoder] = api.req[A] / "sessions" / sessionId

  def vizReq[A: EntityDecoder] = req[A] / Names.v

  def viz = Visualizations(this)
}

case class SessionCodec(id: String)

class Lightning(base: Uri)
{
  lazy val api = Api(base)

  def createSession(name: String) = {
    implicit val d1: EntityDecoder[SessionCodec] = jsonOf[SessionCodec]
    (api.req[SessionCodec] / "sessions").data(Map("name" -> name)).post
  }

  def session(id: String) = Session(api, id)
}

object Lightning
{
  def defaultUri =
    Uri(
      scheme = Some(CaseInsensitiveString("http")),
      authority = Some(Uri.Authority(port = Some(3000)))
    )

  def default = apply(None)

  def apply(host: Option[String]) = {
    val h = host
      .flatMap(Uri.fromString(_).toOption)
      .getOrElse(defaultUri)
    new Lightning(h)
  }

  def createSession(name: String, host: Option[String]) =
    apply(host).createSession(name)
}

// case class Lightning2(host: String)
// extends Plots
// with Three
// with Linked
// {
//   var session: String = ""
//   var auth: Option[(String, String)] = None
//   var isNotebook: Boolean = false
//   def this() = this("http://localhost:3000")
//   def createSession(sessionName: String = "") {
//     val url = host + "/sessions/"
//     implicit val formats = DefaultFormats
//     val payload = sessionName match {
//       case "" => "{}"
//       case _ => Serialization.write( Map("name" -> sessionName) )
//     }
//     val id = post(url, payload)
//     session = id
//   }
//   def plot(name: String, data: Map[String, Any]): Visualization = {
//     this.checkSession()
//     val url = host + "/sessions/" + session + "/visualizations/"
//     val id = postData(url, data, name)
//     new Visualization(this, id, name)
//   }
//   def useSession(id: String): this.type = {
//     this.session = id
//     this
//   }
//   def useHost(host: String): this.type = {
//     this.host = host
//     this
//   }
//   def checkSession() {
//     if (session == "") {
//       this.createSession()
//     }
//   }
//   def post(url: String, payload: String, method: String = "POST"): String = {
//     var request = Http(url).postData(payload).method(method)
//       .header("content-type", "application/json")
//       .header("accept", "text/plain")
//     if (auth.nonEmpty) {
//       request = request.auth(auth.get._1, auth.get._2)
//     }
//     implicit val formats = DefaultFormats
//     val response = request.asString
//     response.body.toLowerCase match {
//       case "unauthorized" => throw new Exception("Unauthorized. Check username and/or password.")
//       case _ => {
//         val json = parse(response.body)
//         (json \ "id").extract[String]
//       }
//     }
//   }
//   def postData(url: String, data: Map[String, Any], name: String, method: String = "POST"): String = {
//     implicit val formats = DefaultFormats
//     val blob = Map("data" -> data, "type" -> name)
//     val payload = Serialization.write(blob)
//     post(url, payload, method)
//   }
// }
