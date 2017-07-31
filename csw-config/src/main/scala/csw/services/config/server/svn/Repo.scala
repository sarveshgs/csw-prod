package csw.services.config.server.svn

import java.io.{InputStream, OutputStream}
import java.nio.file.Path

import akka.dispatch.MessageDispatcher
import org.tmatesoft.svn.core.wc.SVNRevision
import org.tmatesoft.svn.core.{SVNCommitInfo, SVNDirEntry, SVNLogEntry}

import scala.concurrent.Future

abstract class Repo(blockingIoDispatcher: MessageDispatcher) {

  private implicit val _blockingIoDispatcher = blockingIoDispatcher

  def getFile(path: Path, revision: Long, outputStream: OutputStream): Future[Unit]

  def addFile(path: Path, comment: String, data: InputStream): Future[SVNCommitInfo]

  def modifyFile(path: Path, comment: String, data: InputStream): Future[SVNCommitInfo]

  def delete(path: Path, comment: String): Future[SVNCommitInfo]

  def list(): Future[List[SVNDirEntry]]

  def pathExists(path: Path, id: Option[Long]): Future[Boolean]

  def hist(path: Path, maxResults: Int): Future[List[SVNLogEntry]]

  def update(): Unit

  def svnRevision(id: Option[Long] = None): Future[SVNRevision] = Future {
    id match {
      case Some(value) => SVNRevision.create(value)
      case None => SVNRevision.HEAD
    }
  }
}
