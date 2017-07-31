package csw.services.config.server.svn

import java.io.{File, InputStream, OutputStream}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import akka.dispatch.MessageDispatcher
import csw.services.config.server.Settings
import org.tmatesoft.svn.core._
import org.tmatesoft.svn.core.internal.io.fs.FSRepositoryFactory
import org.tmatesoft.svn.core.io.diff.SVNDeltaGenerator
import org.tmatesoft.svn.core.io.{ISVNEditor, SVNRepository, SVNRepositoryFactory}
import org.tmatesoft.svn.core.wc.{SVNClientManager, SVNRevision, SVNWCUtil}
import org.tmatesoft.svn.core.wc2.{SvnOperationFactory, SvnTarget}

import scala.concurrent.Future

class SvnWCRepo(settings: Settings, blockingIoDispatcher: MessageDispatcher) extends Repo(blockingIoDispatcher) {
  private implicit val _blockingIoDispatcher = blockingIoDispatcher

  val svnClientManager: SVNClientManager = SVNClientManager.newInstance()
  val remoteRepo: SVNRepository = SVNRepositoryFactory.create(SVNURL.parseURIDecoded("file:///tmp/csw-config-svn"))

  def initSvnRepo(): Unit = try {
    // Create the new main repo
    FSRepositoryFactory.setup()
    val svnURL: SVNURL = SVNRepositoryFactory.createLocalRepository(settings.repositoryFile, false, false)
    // Checkout a working copy
    val options = SVNWCUtil.createDefaultOptions(true)
    val authManager = SVNWCUtil.createDefaultAuthenticationManager()
    SVNClientManager.newInstance(options, authManager).getUpdateClient.doCheckout(svnURL, Paths.get("/tmp/wc").toFile, SVNRevision.HEAD, SVNRevision.HEAD, SVNDepth.INFINITY, true)
    println(s"New Repository created at ${settings.svnUrl}")
  } catch {
    //If the repo already exists, print stracktrace and continue to boot
    case ex: SVNException if ex.getErrorMessage.getErrorCode == SVNErrorCode.IO_ERROR ⇒
      println(s"Repository already exists at ${settings.svnUrl}")
  }

  def getFile(path: Path, revision: Long, outputStream: OutputStream): Future[Unit] = Future {
    val checkedOutFile = pathFromWC(path)
    outputStream.write(Files.readAllBytes(Paths.get(checkedOutFile.toString)))
    outputStream.flush()
    outputStream.close()
  }

  private def pathFromWC(path: Path): Path = {
    Paths.get("/tmp/wc", path.toString)
  }

  def addFile(path: Path, comment: String, data: InputStream): Future[SVNCommitInfo] = Future {

    val editor: ISVNEditor = remoteRepo.getCommitEditor("Log message", null, true, null)
    editor.openRoot(-1)
    editor.addFile(path.toString, null, -1)

    var deltaGenerator: SVNDeltaGenerator = new SVNDeltaGenerator()
    editor.applyTextDelta(path.toString, null)
    val checksum = deltaGenerator.sendDelta(path.toString, data, editor, true)
    editor.closeFile(path.toString, checksum)
    editor.closeDir()
    editor.closeEdit()

  }

  def modifyFile(path: Path, comment: String, data: InputStream): Future[SVNCommitInfo] = Future {
    val svnClientManager: SVNClientManager = SVNClientManager.newInstance()
    try {
      Files.copy(data, pathFromWC(path), StandardCopyOption.REPLACE_EXISTING)
      svnClientManager.getCommitClient.doCommit(Array(pathFromWC(path).toFile), false, comment, null, null, false, false, SVNDepth.INFINITY)
    } finally {
      svnClientManager.dispose()
    }
  }

  def delete(path: Path, comment: String): Future[SVNCommitInfo] = Future {
    val svnClientManager: SVNClientManager = SVNClientManager.newInstance()
    try {
      svnClientManager.getWCClient.doDelete(pathFromWC(path).toFile, true, true, false)
      svnClientManager.getCommitClient.doCommit(Array(pathFromWC(path).toFile), false, comment, null, null, false, false, SVNDepth.INFINITY)
    } finally {
      svnClientManager.dispose()
    }
  }

  def list(): Future[List[SVNDirEntry]] = Future {
    var entries = List[SVNDirEntry]()
    val svnOperationFactory = new SvnOperationFactory()
    try {
      val svnList = svnOperationFactory.createList()
      svnList.setSingleTarget(SvnTarget.fromURL(settings.svnUrl, SVNRevision.HEAD))
      svnList.setRevision(SVNRevision.HEAD)
      svnList.setDepth(SVNDepth.INFINITY)
      svnList.setReceiver((_, e: SVNDirEntry) => entries = e :: entries)
      svnList.run()
    } finally {
      svnOperationFactory.dispose()
    }
    entries
      .filter(_.getKind == SVNNodeKind.FILE)
      .sortWith(_.getRelativePath < _.getRelativePath)
  }

  def pathExists(path: Path, id: Option[Long]): Future[Boolean] = Future {
    val svnClientManager: SVNClientManager = SVNClientManager.newInstance()
    try {
      val svnurl: File = pathFromWC(path).toFile
      val revision: SVNRevision = id.fold(SVNRevision.WORKING) {
        SVNRevision.create
      }
      svnClientManager.getWCClient.doInfo(svnurl, revision)
      true
    } catch {
      case ex: SVNException => false
    } finally {
      svnClientManager.dispose()
    }
  }

  def update(): Unit = {

    svnClientManager.getUpdateClient.doUpdate(
      Array(Paths.get("/tmp/wc").toFile),
      SVNRevision.HEAD,
      SVNDepth.INFINITY, true, true)
  }

  def hist(path: Path, maxResults: Int): Future[List[SVNLogEntry]] = Future {
    val svnClientManager: SVNClientManager = SVNClientManager.newInstance()
    var logEntries = List[SVNLogEntry]()
    try {
      val logClient = svnClientManager.getLogClient
      val handler: ISVNLogEntryHandler = logEntry => logEntries = logEntry :: logEntries
      logClient.doLog(settings.svnUrl, Array(path.toString), SVNRevision.HEAD, null, null, true, true, maxResults, handler)
      logEntries.sortWith(_.getRevision > _.getRevision)
    } finally {
      svnClientManager.dispose()
    }
  } recover {
    case ex: SVNException => Nil
  }

}
