package org.skyve.impl.create;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.create.MavenSkyveProject.MavenSkyveProjectCreator;

@SuppressWarnings("static-method")
class MavenSkyveProjectTest {

	@TempDir
	Path tempFolder;

	private static final String PROJECT_NAME = "myProject";
	private static final String PROJECT_DIR = "/tmp/projects/myProject";
	private static final String CUSTOMER_NAME = "myCustomer";
	private static final String SKYVE_DIR = "/tmp/skyve";

	private MavenSkyveProject createBasicProject() {
		return new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.initialise();
	}

	private MavenSkyveProject createProjectWithSkyve() {
		return new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveDirectory(SKYVE_DIR)
				.initialise();
	}

	// ---- Builder constructor tests ----

	@Test
	void defaultConstructorCreatesCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator);
	}

	@Test
	void constructorWithProjectNameSetsName() {
		MavenSkyveProject project = new MavenSkyveProjectCreator(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.initialise();
		assertEquals(PROJECT_NAME, project.getProjectName());
	}

	// ---- Builder fluent setter tests ----

	@Test
	void builderProjectNameFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.projectName(PROJECT_NAME));
	}

	@Test
	void builderProjectDescriptionFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.projectDescription("My Description"));
	}

	@Test
	void builderProjectDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.projectDirectory(PROJECT_DIR));
	}

	@Test
	void builderCustomerNameFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.customerName(CUSTOMER_NAME));
	}

	@Test
	void builderSkyveDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.skyveDirectory(SKYVE_DIR));
	}

	@Test
	void builderSrcDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.srcDirectory("custom/src"));
	}

	@Test
	void builderGeneratedDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.generatedDirectory("custom/gen"));
	}

	@Test
	void builderResourceDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.resourceDirectory("custom/res"));
	}

	@Test
	void builderWebDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.webDirectory("custom/web"));
	}

	@Test
	void builderTestDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.testDirectory("custom/test"));
	}

	@Test
	void builderGeneratedTestDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.generatedTestDirectory("custom/genTest"));
	}

	@Test
	void builderSkyveScriptFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.skyveScript("script content"));
	}

	@Test
	void builderCopyFromProjectFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.copyFromProject(true));
	}

	// ---- Project getter tests ----

	@Test
	void getProjectNameReturnsName() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(PROJECT_NAME, p.getProjectName());
	}

	@Test
	void getProjectDescriptionDefaultsToProjectName() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(PROJECT_NAME, p.getProjectDescription());
	}

	@Test
	void getProjectDescriptionUsesExplicitDescription() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDescription("My Custom Description")
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.initialise();
		assertEquals("My Custom Description", p.getProjectDescription());
	}

	@Test
	void getAbsoluteProjectPathReturnsDirectory() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(PROJECT_DIR), p.getAbsoluteProjectPath());
	}

	@Test
	void getCustomerNameReturnsCustomer() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(CUSTOMER_NAME, p.getCustomerName());
	}

	@Test
	void getAbsoluteSkyvePathNullWhenNotSet() {
		MavenSkyveProject p = createBasicProject();
		assertNull(p.getAbsoluteSkyvePath());
	}

	@Test
	void getAbsoluteSkyvePathReturnsSkyveDirectory() {
		MavenSkyveProject p = createProjectWithSkyve();
		assertEquals(Paths.get(SKYVE_DIR), p.getAbsoluteSkyvePath());
	}

	// ---- Relative path getters use builder defaults ----

	@Test
	void getRelativeSrcPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY), p.getRelativeSrcPath());
	}

	@Test
	void getRelativeGeneratedPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_GENERATED_DIRECTORY), p.getRelativeGeneratedPath());
	}

	@Test
	void getRelativeResourcePathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_RESOURCES_DIRECTORY), p.getRelativeResourcePath());
	}

	@Test
	void getRelativeWebPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_WEB_DIRECTORY), p.getRelativeWebPath());
	}

	@Test
	void getRelativeTestPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_TEST_DIRECTORY), p.getRelativeTestPath());
	}

	@Test
	void getRelativeGeneratedTestPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_GENERATED_TEST_DIRECTORY), p.getRelativeGeneratedTestPath());
	}

	// ---- Absolute path getters resolve relative to project directory ----

	@Test
	void getAbsoluteSrcPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY);
		assertEquals(expected, p.getAbsoluteSrcPath());
	}

	@Test
	void getAbsoluteCustomerPathResolvesUnderSrcDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR)
				.resolve(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY)
				.resolve("customers")
				.resolve(CUSTOMER_NAME);
		assertEquals(expected, p.getAbsoluteCustomerPath());
	}

	@Test
	void getAbsoluteCustomerResourcesPathResolvesUnderCustomerDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR)
				.resolve(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY)
				.resolve("customers")
				.resolve(CUSTOMER_NAME)
				.resolve("resources");
		assertEquals(expected, p.getAbsoluteCustomerResourcesPath());
	}

	@Test
	void getAbsoluteGeneratedPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_GENERATED_DIRECTORY);
		assertEquals(expected, p.getAbsoluteGeneratedPath());
	}

	@Test
	void getAbsoluteResourcePathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_RESOURCES_DIRECTORY);
		assertEquals(expected, p.getAbsoluteResourcePath());
	}

	@Test
	void getAbsoluteWebPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_WEB_DIRECTORY);
		assertEquals(expected, p.getAbsoluteWebPath());
	}

	@Test
	void getAbsoluteTestPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_TEST_DIRECTORY);
		assertEquals(expected, p.getAbsoluteTestPath());
	}

	@Test
	void getAbsoluteGeneratedTestPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_GENERATED_TEST_DIRECTORY);
		assertEquals(expected, p.getAbsoluteGeneratedTestPath());
	}

	@Test
	void getAbsoluteDockerPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve("docker");
		assertEquals(expected, p.getAbsoluteDockerPath());
	}

	@Test
	void getAbsoluteDeploymentsPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve("deployments");
		assertEquals(expected, p.getAbsoluteDeploymentsPath());
	}

	// ---- Skyve-directory-dependent paths ----

	@Test
	void getSkyveBasePathReturnsSkyveDirectoryAbsolutePath() {
		MavenSkyveProject p = createProjectWithSkyve();
		assertEquals(Paths.get(SKYVE_DIR).toAbsolutePath(), p.getSkyveBasePath());
	}

	@Test
	void getSkyveAbsoluteWarPathResolvesUnderSkyveDir() {
		MavenSkyveProject p = createProjectWithSkyve();
		Path expected = Paths.get(SKYVE_DIR).resolve(MavenSkyveProject.SKYVE_WEBAPP_PATH);
		assertEquals(expected, p.getSkyveAbsoluteWarPath());
	}

	// ---- Other property tests ----

	@Test
	void getSkyveScriptReturnsScript() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("module Contact { ... }")
				.initialise();
		assertEquals("module Contact { ... }", p.getSkyveScript());
	}

	@Test
	void getSkyveScriptNullByDefault() {
		MavenSkyveProject p = createBasicProject();
		assertNull(p.getSkyveScript());
	}

	@Test
	void getSkyveScriptPathResolvesScriptsFolder() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve("scripts").resolve("skyve.md");
		assertEquals(expected, p.getSkyveScriptPath());
	}

	@Test
	void getMetaDataInitiallyEmpty() {
		MavenSkyveProject p = createBasicProject();
		assertNotNull(p.getMetaData());
		assertTrue(p.getMetaData().isEmpty());
	}

	@Test
	void isCopyFromProjectDefaultFalse() {
		MavenSkyveProject p = createBasicProject();
		assertFalse(p.isCopyFromProject());
	}

	@Test
	void isCopyFromProjectTrueWhenSet() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.copyFromProject(true)
				.initialise();
		assertTrue(p.isCopyFromProject());
	}

	// ---- Custom directory overrides ----

	@Test
	void customSrcDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.srcDirectory("custom/src/")
				.initialise();
		assertEquals(Paths.get("custom/src/"), p.getRelativeSrcPath());
	}

	@Test
	void customGeneratedDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.generatedDirectory("custom/gen/")
				.initialise();
		assertEquals(Paths.get("custom/gen/"), p.getRelativeGeneratedPath());
	}

	@Test
	void customResourceDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.resourceDirectory("custom/resources/")
				.initialise();
		assertEquals(Paths.get("custom/resources/"), p.getRelativeResourcePath());
	}

	@Test
	void customWebDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.webDirectory("custom/webapp/")
				.initialise();
		assertEquals(Paths.get("custom/webapp/"), p.getRelativeWebPath());
	}

	@Test
	void customTestDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.testDirectory("custom/test/")
				.initialise();
		assertEquals(Paths.get("custom/test/"), p.getRelativeTestPath());
	}

	@Test
	void customGeneratedTestDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.generatedTestDirectory("custom/genTest/")
				.initialise();
		assertEquals(Paths.get("custom/genTest/"), p.getRelativeGeneratedTestPath());
	}

	// ---- Static constant tests ----

	@Test
	void staticSkyveWarPathEndsWithWebapp() {
		assertTrue(MavenSkyveProject.SKYVE_WEBAPP_PATH.toString().contains("webapp"));
	}

	@Test
	void staticSkyveTestPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_TEST_PATH.toString().contains("java"));
	}

	@Test
	void staticSkyveGeneratedPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_GENERATED_PATH.toString().contains("java"));
	}

	@Test
	void staticSkyveWebPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_WEB_PATH.toString().contains("java"));
	}

	@Test
	void staticSkyveSrcPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_SRC_PATH.toString().contains("java"));
	}

	@Test
	void staticSkyveSrcResourcesPathContainsResources() {
		assertTrue(MavenSkyveProject.SKYVE_SRC_RESOURCES_PATH.toString().contains("resources"));
	}

	@Test
	void staticSkyveGeneratedTestPathContainsGeneratedTest() {
		assertTrue(MavenSkyveProject.SKYVE_GENERATED_TEST_PATH.toString().contains("generatedTest"));
	}

	@Test
	void applyScriptWithValidScriptPopulatesMetaData() throws SkyveProjectCreationException {
		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("# Admin\n## Address\n- country Country")
				.initialise();
		// applyScript with writeToFile=false to avoid filesystem writes
		project.applyScript("# Admin\n## Address\n- country Country", false);
		assertFalse(project.getMetaData().isEmpty());
	}

	@Test
	void applyScriptWithNoModulesDetectedLeavesMetaDataEmpty() throws SkyveProjectCreationException {
		// A script with no module heading produces no modules
		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("just plain text with no headings")
				.initialise();
		project.applyScript("just plain text with no headings", false);
		assertTrue(project.getMetaData().isEmpty());
	}

	@Test
	void applyScriptWithValidModuleAndDocumentPopulatesMetaData() throws SkyveProjectCreationException {
		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("# testModule\n## TestDoc\n- name Name")
				.initialise();
		project.applyScript("# testModule\n## TestDoc\n- name Name", false);
		assertFalse(project.getMetaData().isEmpty());
	}

	@Test
	void getSkyveScriptReturnsScriptSetViaBuilder() {
		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("# MyModule")
				.initialise();
		assertEquals("# MyModule", project.getSkyveScript());
	}

	@Test
	void getSkyveScriptPathContainsScriptsDir() {
		MavenSkyveProject project = createBasicProject();
		assertTrue(project.getSkyveScriptPath().toString().contains("scripts"));
	}

	@Test
	void isCopyFromProjectDefaultIsFalse() {
		MavenSkyveProject project = createBasicProject();
		assertFalse(project.isCopyFromProject());
	}

	// ---- assemble() / clearBeforeAssemble() tests ----

	/**
	 * Builds the minimal skyve directory structure that assemble() needs:
	 * src/main/java/{modules,modules/admin,resources,schemas,router},
	 * src/generated/java/modules/admin, src/main/webapp/{desktop,external,pages,WEB-INF/resources/skyve},
	 * src/test/java/util, src/generatedTest/java/modules/admin
	 */
	private File createSkyveDirStructure(File base) throws IOException {
		for (String rel : new String[]{
				"skyve-war/src/main/java/modules",
				"skyve-war/src/main/java/modules/admin",
				"skyve-war/src/main/java/resources",
				"skyve-war/src/main/java/schemas",
				"skyve-war/src/main/java/router",
				"skyve-war/src/generated/java/modules/admin",
				"skyve-war/src/main/webapp/desktop",
				"skyve-war/src/main/webapp/external",
				"skyve-war/src/main/webapp/pages",
				"skyve-war/src/main/webapp/WEB-INF/resources/skyve",
				"skyve-war/src/test/java/util",
				"skyve-war/src/generatedTest/java/modules/admin",
				"skyve-war/src/main/resources"}) {
			new File(base, rel).mkdirs();
		}
		// Add a placeholder .java file in modules so listFiles returns something
		Files.createFile(new File(base, "skyve-war/src/main/java/modules/Placeholder.java").toPath());
		return base;
	}

	@Test
	void assembleWithAllFalseDoesNotCopyAnyFiles() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		// assemble(false,false,false,false) exercises all 4 false branch conditions
		project.assemble(false, false, false, false);

		// Nothing should be copied
		String[] contents = projectDir.list();
		assertTrue(contents == null || contents.length == 0);
	}

	@Test
	void assembleWithAppFilesOnlySucceeds() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		// Create the required target sub-dirs
		new File(projectDir, "src/main/java").mkdirs();
		new File(projectDir, "src/main/resources").mkdirs();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.assemble(true, false, false, false);
		// Verify assemble ran without exception
		assertNotNull(project);
	}

	@Test
	void assembleGeneratedFilesOnlySucceeds() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		new File(projectDir, "src/generated/java").mkdirs();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.assemble(false, true, false, false);
		assertNotNull(project);
	}

	@Disabled("copySkyveWebAppFiles adds hardcoded WEB-INF files (beans.xml etc.) that must exist - needs full skyve tree")
	@Test
	void assembleWebAppFilesOnlySucceeds() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		new File(projectDir, "src/main/webapp").mkdirs();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.assemble(false, false, true, false);
		assertNotNull(project);
	}

	@Test
	void assembleTestFilesOnlySucceeds() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		new File(projectDir, "src/test/java/util").mkdirs();
		new File(projectDir, "src/generatedTest/java").mkdirs();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.assemble(false, false, false, true);
		assertNotNull(project);
	}

	@Test
	void assembleTestFilesCopiesUtilTestResources() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		Path helper = skyveDir.toPath().resolve("skyve-war/src/test/resources/util/sail/smartclient-sail.js");
		Files.createDirectories(helper.getParent());
		Files.writeString(helper, "// helper");
		// Skyve's own test resources outside util are not part of an app
		Path skyveOnly = skyveDir.toPath().resolve("skyve-war/src/test/resources/sail/files/sail-image.png");
		Files.createDirectories(skyveOnly.getParent());
		Files.writeString(skyveOnly, "png");
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		new File(projectDir, "src/test/java/util").mkdirs();
		new File(projectDir, "src/generatedTest/java").mkdirs();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.assemble(false, false, false, true);

		Path copied = projectDir.toPath().resolve("src/test/resources/util/sail/smartclient-sail.js");
		assertEquals("// helper", Files.readString(copied));
		assertFalse(Files.exists(projectDir.toPath().resolve("src/test/resources/sail/files/sail-image.png")));
	}

	@Test
	void assembleTestFilesFromProjectCopiesUtilTestResources() throws IOException {
		File templateDir = Files.createDirectory(tempFolder.resolve("template")).toFile();
		new File(templateDir, "src/test/java/util").mkdirs();
		new File(templateDir, "src/generatedTest/java/modules").mkdirs();
		Path helper = templateDir.toPath().resolve("src/test/resources/util/sail/smartclient-sail.js");
		Files.createDirectories(helper.getParent());
		Files.writeString(helper, "// template helper");
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(templateDir.getAbsolutePath())
				.copyFromProject(true)
				.initialise();

		project.assemble(false, false, false, true);

		Path copied = projectDir.toPath().resolve("src/test/resources/util/sail/smartclient-sail.js");
		assertEquals("// template helper", Files.readString(copied));
	}

	@Test
	void clearBeforeAssembleRemovesOnlyUtilTestResources() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		for (String rel : new String[]{
				"src/main/java/modules/admin",
				"src/main/java/resources",
				"src/main/java/schemas",
				"src/main/java/router",
				"src/generated/java/modules/admin",
				"src/main/webapp/desktop",
				"src/main/webapp/external",
				"src/main/webapp/pages",
				"src/main/webapp/WEB-INF/resources/skyve",
				"src/test/java/util",
				"src/generatedTest/java/modules/admin"}) {
			new File(projectDir, rel).mkdirs();
		}
		Path stale = projectDir.toPath().resolve("src/test/resources/util/sail/smartclient-sail.js");
		Files.createDirectories(stale.getParent());
		Files.writeString(stale, "// old helper");
		Path appOwn = projectDir.toPath().resolve("src/test/resources/jasperreports.properties");
		Files.writeString(appOwn, "app");

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.clearBeforeAssemble();

		assertFalse(Files.exists(stale));
		assertTrue(Files.exists(appOwn));
	}

	@Test
	void testResourceDirectoryBuilderSetsTarget() {
		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.testResourceDirectory("custom/testResources")
				.initialise();

		assertEquals(Paths.get(PROJECT_DIR, "custom/testResources"), project.getAbsoluteTestResourcePath());
	}

	@Test
	void clearBeforeAssembleWithEmptyDirsDoesNotThrow() throws IOException {
		File skyveDir = createSkyveDirStructure(Files.createDirectory(tempFolder.resolve("skyve")).toFile());
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();
		// Create the directories that clearBeforeAssemble() tries to list
		new File(projectDir, "src/main/java/modules").mkdirs();
		new File(projectDir, "src/main/java/modules/admin").mkdirs();
		new File(projectDir, "src/main/java/resources").mkdirs();
		new File(projectDir, "src/main/java/schemas").mkdirs();
		new File(projectDir, "src/main/java/router").mkdirs();
		new File(projectDir, "src/generated/java/modules/admin").mkdirs();
		new File(projectDir, "src/main/webapp/desktop").mkdirs();
		new File(projectDir, "src/main/webapp/external").mkdirs();
		new File(projectDir, "src/main/webapp/pages").mkdirs();
		new File(projectDir, "src/main/webapp/WEB-INF/resources/skyve").mkdirs();
		new File(projectDir, "src/test/java/util").mkdirs();
		new File(projectDir, "src/generatedTest/java/modules/admin").mkdirs();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveDirectory(skyveDir.getAbsolutePath())
				.initialise();

		project.clearBeforeAssemble();
		assertNotNull(project);
	}

	@Test
	void applyScriptWithWriteToFileTrueWritesScriptFile() throws Exception {
		File projectDir = Files.createDirectory(tempFolder.resolve("project")).toFile();

		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName("test")
				.projectDirectory(projectDir.getAbsolutePath())
				.customerName("testCustomer")
				.skyveScript("# myModule\n## MyDoc\n- name Name")
				.initialise();

		// writeToFile=true writes to getSkyveScriptPath()
		project.applyScript("# myModule\n## MyDoc\n- name Name", true);

		File scriptFile = project.getSkyveScriptPath().toFile();
		assertTrue(scriptFile.exists());
		String content = new String(Files.readAllBytes(scriptFile.toPath()));
		assertTrue(content.contains("myModule"));
	}
}
