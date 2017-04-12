package csw.services.config.server.repo

import java.io._
import java.nio.file.{Path, Paths}
import java.time.Instant

import akka.stream.scaladsl.StreamConverters
import csw.services.config.api.models.{ConfigData, ConfigFileHistory, ConfigFileInfo, ConfigId}
import csw.services.config.api.scaladsl.ConfigManager
import csw.services.config.server.{ActorRuntime, Settings}
import csw.services.location.internal.StreamExt.RichSource
import org.tmatesoft.svn.core._
import org.tmatesoft.svn.core.auth.BasicAuthenticationManager
import org.tmatesoft.svn.core.io.diff.SVNDeltaGenerator
import org.tmatesoft.svn.core.io.{SVNRepository, SVNRepositoryFactory}
import org.tmatesoft.svn.core.wc.{SVNClientManager, SVNRevision}
import org.tmatesoft.svn.core.wc2.{SvnOperationFactory, SvnTarget}

import scala.async.Async._
import scala.concurrent.Future
import scala.util.control.NonFatal

class SvnConfigManager(settings: Settings, oversizeFileManager: OversizeFileManager, actorRuntime: ActorRuntime, svnOps: SvnOps) extends ConfigManager {

  import actorRuntime._

  override def name: String = "my name is missing"

  override def create(path: Path, configData: ConfigData, oversize: Boolean, comment: String): Future[ConfigId] = {

    def createOversize(): Future[ConfigId] = async {
      val sha1 = await(oversizeFileManager.post(configData))
      await(create(shaFile(path), ConfigData.fromString(sha1), oversize = false, comment))
    }

    // If the file does not already exists in the repo, create it
    def createImpl(present: Boolean): Future[ConfigId] = {
      if (present) {
        Future.failed(new IOException("File already exists in repository: " + path))
      } else if (oversize) {
        createOversize()
      } else {
        put(path, configData, update = false, comment)
      }
    }

    async {
      val present = await(exists(path))
      await(createImpl(present))
    }
  }

  override def update(path: Path, configData: ConfigData, comment: String): Future[ConfigId] = {

    def updateOversize(): Future[ConfigId] = async {
      val sha1 = await(oversizeFileManager.post(configData))
      await(update(shaFile(path), ConfigData.fromString(sha1), comment))
    }

    // If the file already exists in the repo, update it
    def updateImpl(present: Boolean): Future[ConfigId] = async {
      if (!present) {
        throw new FileNotFoundException("File not found: " + path)
      } else if (await(isOversize(path))) {
        await(updateOversize())
      } else {
        await(put(path, configData, update = true, comment))
      }
    }

    async {
      val present = await(exists(path))
      await(updateImpl(present))
    }
  }

  override def get(path: Path, id: Option[ConfigId]): Future[Option[ConfigData]] = {
    // Get oversize files that are stored in the annex server
    def getOversize: Future[Option[ConfigData]] = async {
      val opt = await(get(shaFile(path), id))
      await(getData(opt))
    }

    // Gets the actual file data using the SHA-1 value contained in the checked in file
    def getData(opt: Option[ConfigData]): Future[Option[ConfigData]] = async {
      opt match {
        case None             =>
          None
        case Some(configData) =>
          val sha1 = await(configData.toStringF)
          await(oversizeFileManager.get(sha1))
      }
    }

    // Returns the contents of the given version of the file, if found
    def getConfigData: Future[Option[ConfigData]] = {
      val svn = getSvn
      val outputStream = StreamConverters.asOutputStream().cancellableMat
      val source = outputStream.mapMaterializedValue { case (out, switch) ⇒
        svnOps.getFile(path, svnRevision(id).getNumber, out, ex ⇒ switch.abort(ex))
      }
      Future(Some(ConfigData.fromSource(source)))
    }

    // If the file exists in the repo, get its data
    def getImpl(present: Boolean): Future[Option[ConfigData]] = async {
      if (!present) {
        None
      } else if (await(isOversize(path))) {
        await(getOversize)
      } else {
        await(getConfigData)
      }
    }

    async {
      val present = await(exists(path))
      await(getImpl(present))
    }
  }

  override def get(path: Path, time: Instant): Future[Option[ConfigData]] = {

    // Gets the ConfigFileHistory matching the date
    def getHist: Future[Option[ConfigFileHistory]] = async {
      val h = await(history(path))
      val found = h.find(t => t.time.isBefore(time) || t.time.equals(time))
      if (found.nonEmpty) found
      else if (h.isEmpty) None
      else Some(if (time.isAfter(h.head.time)) h.head else h.last)
    }

    async {
      val hist = await(getHist)
      if (hist.isEmpty) None else await(get(path, hist.map(_.id)))
    }
  }

  override def exists(path: Path): Future[Boolean] = pathExists(path)

  //TODO: This implementation deletes all versions of a file. This is different than the expecations
  override def delete(path: Path, comment: String = "deleted"): Future[Unit] = async {
    if (await(isOversize(path))) {
      await(delete(shaFile(path), comment))
    } else {
      if (!await(pathExists(path))) {
        throw new FileNotFoundException("Can't delete " + path + " because it does not exist")
      }

      val svnOperationFactory = new SvnOperationFactory()
      try {
        val remoteDelete = svnOperationFactory.createRemoteDelete()
        remoteDelete.setSingleTarget(SvnTarget.fromURL(settings.svnUrl.appendPath(path.toString, false)))
        remoteDelete.setCommitMessage(comment)
        remoteDelete.run()
      } finally {
        svnOperationFactory.dispose()
      }
    }
  }

  override def list(): Future[List[ConfigFileInfo]] = Future {
    // XXX Should .sha1 files have the .sha1 suffix removed in the result?
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
    entries.filter(_.getKind == SVNNodeKind.FILE).sortWith(_.getRelativePath < _.getRelativePath)
      .map(e => ConfigFileInfo(Paths.get(e.getRelativePath), ConfigId(e.getRevision), e.getCommitMessage))
  }

  override def history(path: Path, maxResults: Int): Future[List[ConfigFileHistory]] = async {
    // XXX Should .sha1 files have the .sha1 suffix removed in the result?
    if (await(isOversize(path))) {
      hist(shaFile(path), maxResults)
    }
    else {
      hist(path, maxResults)
    }
  }

  override def setDefault(path: Path, id: Option[ConfigId] = None): Future[Unit] = async {
    val maybeConfigId1 = if (id.isDefined) id else await(getCurrentVersion(path))

    maybeConfigId1 match {
      case Some(configId) => await(create(defaultFile(path), ConfigData.fromString(configId.id)))
      case None           => throw new FileNotFoundException(s"Unknown path $path")
    }
  }

  override def resetDefault(path: Path): Future[Unit] = delete(defaultFile(path))

  override def getDefault(path: Path): Future[Option[ConfigData]] = async {
    await(getCurrentVersion(path)) match {
      case None           ⇒
        None
      case Some(configId) ⇒
        val d = await(get(defaultFile(path)))
        val id = if (d.isDefined) await(d.get.toStringF) else configId.id
        await(get(path, Some(ConfigId(id))))
    }
  }

  /**
    * Creates or updates a config file with the given path and data and optional comment.
    *
    * @param path       the config file path
    * @param configData the contents of the file
    * @param comment    an optional comment to associate with this file
    * @return a future unique id that can be used to refer to the file
    */
  private def put(path: Path, configData: ConfigData, update: Boolean, comment: String = ""): Future[ConfigId] = async {
    val inputStream = configData.toInputStream
    val commitInfo = if (update) {
      modifyFile(comment, path, inputStream)
    } else {
      await(addFile(comment, path, inputStream))
    }
    ConfigId(commitInfo.getNewRevision)
  }

  // Modifies the contents of the given file in the repository.
  // See http://svn.svnkit.com/repos/svnkit/tags/1.3.5/doc/examples/src/org/tmatesoft/svn/examples/repository/Commit.java.
  private def modifyFile(comment: String, path: Path, data: InputStream): SVNCommitInfo = {
    val svn = getSvn
    try {
      val editor = svn.getCommitEditor(comment, null)
      editor.openRoot(SVNRepository.INVALID_REVISION)
      val filePath = path.toString
      editor.openFile(filePath, SVNRepository.INVALID_REVISION)
      editor.applyTextDelta(filePath, null)
      val deltaGenerator = new SVNDeltaGenerator
      val checksum = deltaGenerator.sendDelta(filePath, data, editor, true)
      editor.closeFile(filePath, checksum)
      editor.closeDir()
      editor.closeEdit
    } finally {
      svn.closeSession()
    }
  }

  // Gets an object for accessing the svn repository (not reusing a single instance since not thread safe)
  private def getSvn: SVNRepository = {
    val svn = SVNRepositoryFactory.create(settings.svnUrl)
    val authManager = BasicAuthenticationManager.newInstance(settings.`svn-user-name`, Array[Char]())
    svn.setAuthenticationManager(authManager)
    svn
  }

  // Adds the given file (and dir if needed) to svn.
  // See http://svn.svnkit.com/repos/svnkit/tags/1.3.5/doc/examples/src/org/tmatesoft/svn/examples/repository/Commit.java.
  private def addFile(comment: String, path: Path, data: InputStream): Future[SVNCommitInfo] = {
    val svn = getSvn
    val editor = svn.getCommitEditor(comment, null)
    editor.openRoot(SVNRepository.INVALID_REVISION)
    val dirPath = path.getParent
    // Recursively add any missing directories leading to the file
    def addDir(dir: Path): Future[Unit] = async {
      if (dir != null) {
        await(addDir(dir.getParent))
        if (!await(dirExists(dir))) {
          editor.addDir(dir.toString, null, SVNRepository.INVALID_REVISION)
        }
      }
    }

    async {
      await(addDir(dirPath))
      val filePath = path.toString
      editor.addFile(filePath, null, SVNRepository.INVALID_REVISION)
      editor.applyTextDelta(filePath, null)
      val deltaGenerator = new SVNDeltaGenerator
      val checksum = deltaGenerator.sendDelta(filePath, data, editor, true)
      editor.closeFile(filePath, checksum)
      editor.closeDir() // XXX TODO I think all added parent dirs need to be closed also
      val commitInfo = editor.closeEdit()
      svn.closeSession()
      commitInfo
    } recover {
      case NonFatal(ex) ⇒
        svn.closeSession()
        throw ex
    }
  }

  // True if the directory path exists in the repository
  private def dirExists(path: Path): Future[Boolean] = {
    svnOps.checkDirPath(path)
  }

  // True if the path exists in the repository
  private def pathExists(path: Path): Future[Boolean] = async {
    val dd = await(svnOps.checkFilePath(path))
    if(!dd) await(isOversize(path)) else dd
  }

  // True if the .sha1 file exists, meaning the file needs special oversize handling.
  private def isOversize(path: Path): Future[Boolean] = {
    svnOps.checkFilePath(shaFile(path))
  }

  // Gets the svn revision from the given id, defaulting to HEAD
  private def svnRevision(id: Option[ConfigId] = None): SVNRevision = {
    id match {
      case Some(configId) => SVNRevision.create(configId.id.toLong)
      case None           => SVNRevision.HEAD
    }
  }

  // File used to store the SHA-1 of the actual file, if oversized.
  private def shaFile(path: Path): Path = Paths.get(s"${path.toString}${settings.`sha1-suffix`}")

  // Returns the current version of the file, if known
  private def getCurrentVersion(path: Path): Future[Option[ConfigId]] = async {
    if (await(isOversize(path))) {
      hist(shaFile(path), 1).headOption.map(_.id)
    }
    else {
      hist(path, 1).headOption.map(_.id)
    }
  }

  private def hist(path: Path, maxResults: Int = Int.MaxValue): List[ConfigFileHistory] = {
    val clientManager = SVNClientManager.newInstance()
    var logEntries = List[SVNLogEntry]()
    try {
      val logClient = clientManager.getLogClient
      logClient.doLog(settings.svnUrl, Array(path.toString), SVNRevision.HEAD, null, null, true, true, maxResults,
        new ISVNLogEntryHandler() {
          override def handleLogEntry(logEntry: SVNLogEntry): Unit = logEntries = logEntry :: logEntries
        })
      logEntries.sortWith(_.getRevision > _.getRevision)
        .map(e => ConfigFileHistory(ConfigId(e.getRevision), e.getMessage, e.getDate.toInstant))
    } catch {
      case ex: SVNException => Nil
    } finally {
      clientManager.dispose()
    }
  }

  // File used to store the id of the default version of the file.
  private def defaultFile(path: Path): Path =
    Paths.get(s"${path.toString}${settings.`default-suffix`}")
}
