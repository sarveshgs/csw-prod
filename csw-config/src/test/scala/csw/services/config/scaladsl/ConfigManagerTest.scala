package csw.services.config.scaladsl

import java.io._
import java.nio.file.Paths
import java.util.Date

import csw.services.config.commons.TestFileUtils
import csw.services.config.commons.TestFutureExtension.RichFuture
import csw.services.config.models.{ConfigFileHistory, ConfigFileInfo, ConfigString}
import csw.services.config.server.ServerWiring
import net.codejava.security.HashGeneratorUtils
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

abstract class ConfigManagerTest extends FunSuite with Matchers with BeforeAndAfterEach {

  private val serverWiring = new ServerWiring()

  private val testFileUtils = new TestFileUtils(serverWiring.settings)

  import serverWiring.actorRuntime._

  def configManager: ConfigManager

  override protected def beforeEach(): Unit = {
    serverWiring.svnAdmin.initSvnRepo()
  }

  override protected def afterEach(): Unit = {
    testFileUtils.deleteServerFiles()
  }

  test("should able to create a file and retrieve the same") {
    val configValue = "axisName = tromboneAxis"

    val file = Paths.get("test.conf").toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit test file").await
    configManager.get(file).await.get.toFutureString.await shouldBe configValue
  }

  test("should ignore '/' at the beginning of file path and create a file") {
    val configValue = "axisName = tromboneAxis"

    val fileName = "csw.conf"
    val file = Paths.get(s"/$fileName").toFile
    val fileWithoutBackslash = Paths.get(fileName).toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit csw file").await

    intercept[IOException] {
      configManager.create(fileWithoutBackslash, ConfigString(configValue), oversize = false, "commit without '/'").await
    }

    configManager.get(fileWithoutBackslash).await.get.toFutureString.await shouldBe configValue
  }

  test("should throw IOException while creating a file if it already exists in repository") {
    val configValue = "axisName = tromboneAxis"
    val file = Paths.get("/test.conf").toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit test conf for first time").await

    intercept[IOException] {
      configManager.create(file, ConfigString(configValue), oversize = false, "commit test conf again").await
    }
  }

  test("should able to update existing file and get the file with updated content") {
    val assemblyConfigValue = "axisName = tromboneAxis"
    val file = Paths.get("/assembly.conf").toFile
    configManager.create(file, ConfigString(assemblyConfigValue), oversize = false, "commit assembly conf").await
    configManager.get(file).await.get.toFutureString.await shouldBe assemblyConfigValue

    val updatedAssemblyConfigValue = "assemblyHCDCount = 3"
    configManager.update(file, ConfigString(updatedAssemblyConfigValue), "commit updated assembly conf").await
    configManager.get(file).await.get.toFutureString.await shouldBe updatedAssemblyConfigValue
  }

  test("update should throw FileNotFoundException if a file does not exists in repository") {
    val file = Paths.get("/assembly.conf").toFile

    intercept[FileNotFoundException] {
      configManager.update(file, ConfigString("assemblyHCDCount = 3"), "commit updated assembly conf").await
    }
  }

  test("get call should return `None` if a file does not exists in repository") {
    val file = Paths.get("/test.conf").toFile

    configManager.get(file).await shouldBe None
  }

  test("should able to retrieve the specific version of file by config ID") {
    val configValue = "axisName = tromboneAxis"
    val assemblyConfigValue = "assemblyHCDCount = 3"
    val newAssemblyConfigValue = "assemblyHCDCount = 5"
    val file = Paths.get("/a/b/csw.conf").toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit csw conf file").await
    configManager.get(file).await.get.toFutureString.await shouldBe configValue

    val configId = configManager.update(file, ConfigString(assemblyConfigValue), "commit updated conf file").await

    configManager.update(file, ConfigString(newAssemblyConfigValue), "updated config to assembly").await
    configManager.get(file).await.get.toFutureString.await shouldBe newAssemblyConfigValue

    configManager.get(file, Some(configId)).await.get.toFutureString.await shouldBe assemblyConfigValue
  }

  test("should get the correct version of file based on date") {
    val configValue = "axisName = tromboneAxis"
    val assemblyConfigValue = "assemblyHCDCount = 3"
    val newAssemblyConfigValue = "assemblyHCDCount = 5"

    val file = Paths.get("/test.conf").toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit initial configuration").await
    configManager.get(file).await.get.toFutureString.await shouldBe configValue

    configManager.update(file, ConfigString(assemblyConfigValue), "updated config to assembly").await
    val date = new Date()
    configManager.update(file, ConfigString(newAssemblyConfigValue), "updated config to assembly").await

    configManager.get(file).await.get.toFutureString.await shouldBe newAssemblyConfigValue
    configManager.get(file, date).await.get.toFutureString.await shouldBe assemblyConfigValue
  }

  test("should get the initial version of file if date provided is before the creation date") {
    val initialConfigValue = "axisName = tromboneAxis"
    val assemblyConfigValue = "assemblyHCDCount = 3"
    val newAssemblyConfigValue = "assemblyHCDCount = 5"

    val date = new Date(0L)
    val file = Paths.get("/test.conf").toFile
    configManager.create(file, ConfigString(initialConfigValue), oversize = false, "commit initial configuration").await
    configManager.get(file).await.get.toFutureString.await shouldBe initialConfigValue

    configManager.update(file, ConfigString(assemblyConfigValue), "updated config to assembly").await

    configManager.update(file, ConfigString(newAssemblyConfigValue), "updated config to assembly").await
    configManager.get(file).await.get.toFutureString.await shouldBe newAssemblyConfigValue

    configManager.get(file, date).await.get.toFutureString.await shouldBe initialConfigValue
  }

  test("should get the history of a file") {
    val configValue = "axisName = tromboneAxis"
    val assemblyConfigValue = "assemblyHCDCount = 3"
    val newAssemblyConfigValue = "assemblyHCDCount = 5"

    val file = Paths.get("/test.conf").toFile
    val configIdCreate = configManager.create(file, ConfigString(configValue), oversize = false, "commit initial configuration").await
    configManager.get(file).await.get.toFutureString.await shouldBe ConfigString(configValue).str

    val configIdUpdate1 = configManager.update(file, ConfigString(assemblyConfigValue), "updated config to assembly").await
    val configIdUpdate2 = configManager.update(file, ConfigString(newAssemblyConfigValue), "updated config to assembly").await

    configManager.history(file).await.size shouldBe 3
    configManager.history(file).await.map(_.id) shouldBe List(configIdUpdate2, configIdUpdate1, configIdCreate)

    configManager.history(file, 2).await.size shouldBe 2
    configManager.history(file, 2).await.map(_.id) shouldBe List(configIdUpdate2, configIdUpdate1)
  }

  test("should list all the available config files") {
    val tromboneConfig = Paths.get("trombone.conf").toFile
    val assemblyConfig = Paths.get("a/b/assembly/assembly.conf").toFile

    val tromboneConfigComment = "hello trombone"
    val assemblyConfigComment = "hello assembly"

    val tromboneConfigId = configManager.create(tromboneConfig, ConfigString("axisName = tromboneAxis"), oversize = false, tromboneConfigComment).await
    val assemblyConfigId = configManager.create(assemblyConfig, ConfigString("assemblyHCDCount = 3"), oversize = false, assemblyConfigComment).await

    val tromboneConfigInfo: ConfigFileInfo = ConfigFileInfo(tromboneConfig, tromboneConfigId, tromboneConfigComment)
    val assemblyConfigInfo: ConfigFileInfo = ConfigFileInfo(assemblyConfig, assemblyConfigId, assemblyConfigComment)

    configManager.list().await shouldBe List(assemblyConfigInfo, tromboneConfigInfo)
  }

  test("exists should return false if file does not exist") {
    val file = Paths.get("/test.conf").toFile

    configManager.exists(file).await shouldBe false
  }

  test("exists should return true if file exist") {
    val configValue = "axisName = tromboneAxis"

    val file = Paths.get("a/test.csw.conf").toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit config file").await

    configManager.exists(file).await shouldBe true
  }

  test("should able to delete existing file") {
    val configValue = "axisName = tromboneAxis"

    val file = Paths.get("tromboneHCD.conf").toFile
    configManager.create(file, ConfigString(configValue), oversize = false, "commit trombone config file").await

    configManager.get(file).await.get.toFutureString.await shouldBe configValue

    configManager.delete(file).await
    configManager.get(file).await shouldBe None
  }

  test("deleting non existing file should throw FileNotFoundException") {
    val file = Paths.get("tromboneHCD.conf").toFile
    intercept[FileNotFoundException] {
      configManager.delete(file).await
    }
  }

  //  TODO Implementation of delete() needs to be fixed
  //  This is not a valid test. Delete should just remove latest version and keep older ones.
  test("delete removes all versions of a file") {
    val configValue = "axisName = tromboneAxis"
    val assemblyConfigValue = "assemblyHCDCount = 3"
    val newAssemblyConfigValue = "assemblyHCDCount = 5"
    val file = Paths.get("/a/b/csw.conf").toFile

    configManager.create(file, ConfigString(configValue), oversize = false, "commit config file").await
    configManager.get(file).await.get.toFutureString.await shouldBe configValue

    val configId = configManager.update(file, ConfigString(assemblyConfigValue), "updated config to assembly").await
    configManager.update(file, ConfigString(newAssemblyConfigValue), "updated config to assembly").await

    configManager.history(file).await.size shouldBe 3
    configManager.delete(file).await
    configManager.history(file).await.size shouldBe 0
    configManager.get(file, Some(configId)).await shouldBe None
  }

  test("should able to get and set the default config file") {
    val configValue = "axisName = tromboneAxis"
    val assemblyConfigValue = "assemblyHCDCount = 3"
    val newAssemblyConfigValue = "assemblyHCDCount = 5"

    val file = Paths.get("/test.conf").toFile
    val configIdCreate = configManager.create(file, ConfigString(configValue), oversize = false, "hello world").await
    configManager.get(file).await.get.toFutureString.await shouldBe ConfigString(configValue).str

    val configIdUpdate1 = configManager.update(file, ConfigString(assemblyConfigValue), "Updated config to assembly").await
    val configIdUpdate2 = configManager.update(file, ConfigString(newAssemblyConfigValue), "Updated config to assembly").await

    configManager.getDefault(file).await.get.toFutureString.await shouldBe newAssemblyConfigValue
    configManager.setDefault(file, Some(configIdUpdate1)).await
    configManager.getDefault(file).await.get.toFutureString.await shouldBe assemblyConfigValue
    configManager.resetDefault(file).await
    configManager.getDefault(file).await.get.toFutureString.await shouldBe newAssemblyConfigValue
  }

  test("should able to store and retrieve oversize file") {
    val file = Paths.get("SomeOversizeFile.txt").toFile
    val content = "testing oversize file"

    val configId = configManager.create(file, ConfigString(content), true, "committing oversize file").await
    val fileContent = configManager.get(file, Some(configId)).await.get
    fileContent.toString shouldBe content

    val svnConfigData = configManager.get(new File(s"${file.getPath}${serverWiring.settings.`sha1-suffix`}"), Some(configId)).await.get
    svnConfigData.toString shouldBe HashGeneratorUtils.generateSHA1(content)
  }

  test("should list oversize files") {
    val file1 = Paths.get("OversizeFile1.txt").toFile
    val comment1  = "committing oversize file"

    val file2 = Paths.get("OversizeFile2.txt").toFile
    val comment2 = "committing one more oversize file"

    val configId1 = configManager.create(file1, ConfigString("testing oversize file"), true, comment1).await
    val configId2 = configManager.create(file2, ConfigString("testing oversize file"), true, comment2).await

    val listOfFileInfo: List[ConfigFileInfo] = configManager.list().await

    listOfFileInfo.toSet shouldBe Set(
      ConfigFileInfo(new File(s"${file1.toPath}${serverWiring.settings.`sha1-suffix`}"), configId1, comment1),
      ConfigFileInfo(new File(s"${file2.toPath}${serverWiring.settings.`sha1-suffix`}"), configId2, comment2)
    )
  }

  test("should able to update oversize file and retrieve the history") {
    val file = Paths.get("SomeOversizeFile.txt").toFile
    val creationContent = "testing oversize file"
    val creationComment = "initial commit"
    val creationConfigId = configManager.create(file, ConfigString(creationContent), true, creationComment).await

    val newContent = "testing oversize file, again"
    val newComment = "Updating file"
    val newConfigId = configManager.update(file, ConfigString(newContent), newComment).await

    val creationFileContent = configManager.get(file, Some(creationConfigId)).await.get
    creationFileContent.toString shouldBe creationContent

    val updatedFileContent = configManager.get(file, Some(newConfigId)).await.get
    updatedFileContent.toString shouldBe newContent

    val oldSvnConfigData = configManager.get(new File(s"${file.getPath}${serverWiring.settings.`sha1-suffix`}"), Some(creationConfigId)).await.get
    oldSvnConfigData.toString shouldBe HashGeneratorUtils.generateSHA1(creationContent)

    val newSvnConfigData = configManager.get(new File(s"${file.getPath}${serverWiring.settings.`sha1-suffix`}"), Some(newConfigId)).await.get
    newSvnConfigData.toString shouldBe HashGeneratorUtils.generateSHA1(newContent)

    val fileHistories: List[ConfigFileHistory] = configManager.history(file).await

    fileHistories.map(history => (history.id, history.comment)) shouldBe List(
      (newConfigId, newComment),
      (creationConfigId, creationComment)
    )
  }
}