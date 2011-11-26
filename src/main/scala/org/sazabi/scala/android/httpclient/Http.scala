package org.sazabi.scala.android.httpclient

import scala.collection.JavaConverters._
import scala.util.control.Exception._

import java.io.{BufferedReader, InputStream, InputStreamReader}

import android.content.Context
import android.net.{http, Uri}
import http.AndroidHttpClient

import org.apache.http.{client, message, util, HttpResponse, HttpStatus}
import client.{entity, methods, ResponseHandler}
import message.BasicNameValuePair
import util.EntityUtils
import entity.UrlEncodedFormEntity
import methods.{HttpGet, HttpPost, HttpUriRequest}

import scalaz._
import Scalaz._

import org.sazabi.scala.Log._

object Http extends Loggable {
  def apply(url: String, c: Context = null) = Request(url, c, Request.get _)
  def post(url: String, c: Context = null) = Request(url, c, Request.post _)

  val UserAgent = "sazabi-httpclient"

  type Errors = NonEmptyList[Throwable]
  type Result[A] = Validation[Errors, A]

  type HttpRequest = (Request => HttpUriRequest)

  case class Request(val url: String, private val c: Context = null,
      request: HttpRequest,
      params: List[(String, String)] = List(),
      headers: List[(String, String)] = List()) {
    lazy private val client = AndroidHttpClient.newInstance(UserAgent, c)

    def param(k: String, v: String) =
      Request(url, c, request, (k -> v) :: params, headers)
    def param(p: (String, String)) =
      Request(url, c, request, p :: params, headers)
    def params(p: (String, String)*): Request =
      Request(url, c, request, p.toList ::: this.params, headers)
    def params(p: List[(String, String)]): Request =
      Request(url, c, request, p ::: this.params, headers)

    def apply[A](f: (InputStream => A)): Result[A] = {
      try {
        val req = request(this)
        logRequest(req)
        client.execute(req, handler(f))
      } catch {
        case e => e.fail.liftFailNel
      } finally {
        client.close()
      }
    }

    private def handler[A](f: InputStream => A): ResponseHandler[Result[A]] =
      new ResponseHandler[Result[A]] {
        override def handleResponse(r: HttpResponse): Result[A] =
          try {
            val st = r.getStatusLine()
            val is = r.getEntity().getContent()
            st.getStatusCode() match {
              case code if code > HttpStatus.SC_BAD_REQUEST =>
                HttpException(code, st.getReasonPhrase(),
                  EntityUtils.toString(r.getEntity())).fail.liftFailNel
              case _ => f(r.getEntity().getContent()).success
            }
          } catch {
            case e => e.fail.liftFailNel
          }
      }

    private def logRequest(r: HttpUriRequest) =
      if (Http.hasLogger) r match {
        case get: HttpGet => Http.log(LogTypes.Debug,
          new StringBuilder().append("GET ")
            .append(get.getURI().toString()).toString)
        case post: HttpPost => Http.log(LogTypes.Debug,
          new StringBuilder().append("POST ")
            .append(post.getURI().toString())
            .append(" [")
            .append(EntityUtils.toString(post.getEntity()))
            .append("]")
            .toString)
      }
  }

  object Request {
    def get(r: Request): HttpUriRequest = {
      val builder = Uri.parse(r.url).buildUpon()
      r.params foreach (p => builder.appendQueryParameter(p._1, p._2))
      val req = new HttpGet(builder.build.toString)
      req
    }

    def post(r: Request): HttpUriRequest = {
      val req = new HttpPost(r.url)
      req.setEntity(new UrlEncodedFormEntity(r.params map toPair asJava, "UTF-8"))
      req
    }

    def multipart(r: Request): HttpUriRequest = {

      null
    }

    private def toPair(param: (String, String)) =
      new BasicNameValuePair(param._1, param._2)
  }

  case class HttpException(code: Int, message: String, body: String)
      extends RuntimeException(code + ":" + message)
}
