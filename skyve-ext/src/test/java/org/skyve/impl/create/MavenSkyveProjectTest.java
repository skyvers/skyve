package org.skyve.impl.create;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.Test;
import org.skyve.impl.create.MavenSkyveProject.MavenSkyveProjectCreator;

@SuppressWarnings("static-method")
public class MavenSkyveProjectTest {

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
	public void defaultConstructorCreatesCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator);
	}

	@Test
	public void constructorWithProjectNameSetsName() {
		MavenSkyveProject project = new MavenSkyveProjectCreator(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.initialise();
		assertEquals(PROJECT_NAME, project.getProjectName());
	}

	// ---- Builder fluent setter tests ----

	@Test
	public void builderProjectNameFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.projectName(PROJECT_NAME));
	}

	@Test
	public void builderProjectDescriptionFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.projectDescription("My Description"));
	}

	@Test
	public void builderProjectDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.projectDirectory(PROJECT_DIR));
	}

	@Test
	public void builderCustomerNameFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.customerName(CUSTOMER_NAME));
	}

	@Test
	public void builderSkyveDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.skyveDirectory(SKYVE_DIR));
	}

	@Test
	public void builderSrcDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.srcDirectory("custom/src"));
	}

	@Test
	public void builderGeneratedDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.generatedDirectory("custom/gen"));
	}

	@Test
	public void builderResourceDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.resourceDirectory("custom/res"));
	}

	@Test
	public void builderWebDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.webDirectory("custom/web"));
	}

	@Test
	public void builderTestDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.testDirectory("custom/test"));
	}

	@Test
	public void builderGeneratedTestDirectoryFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.generatedTestDirectory("custom/genTest"));
	}

	@Test
	public void builderSkyveScriptFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.skyveScript("script content"));
	}

	@Test
	public void builderCopyFromProjectFluentMethodReturnsCreator() {
		MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator();
		assertNotNull(creator.copyFromProject(true));
	}

	// ---- Project getter tests ----

	@Test
	public void getProjectNameReturnsName() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(PROJECT_NAME, p.getProjectName());
	}

	@Test
	public void getProjectDescriptionDefaultsToProjectName() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(PROJECT_NAME, p.getProjectDescription());
	}

	@Test
	public void getProjectDescriptionUsesExplicitDescription() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDescription("My Custom Description")
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.initialise();
		assertEquals("My Custom Description", p.getProjectDescription());
	}

	@Test
	public void getAbsoluteProjectPathReturnsDirectory() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(PROJECT_DIR), p.getAbsoluteProjectPath());
	}

	@Test
	public void getCustomerNameReturnsCustomer() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(CUSTOMER_NAME, p.getCustomerName());
	}

	@Test
	public void getAbsoluteSkyvePathNullWhenNotSet() {
		MavenSkyveProject p = createBasicProject();
		assertNull(p.getAbsoluteSkyvePath());
	}

	@Test
	public void getAbsoluteSkyvePathReturnsSkyveDirectory() {
		MavenSkyveProject p = createProjectWithSkyve();
		assertEquals(Paths.get(SKYVE_DIR), p.getAbsoluteSkyvePath());
	}

	// ---- Relative path getters use builder defaults ----

	@Test
	public void getRelativeSrcPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY), p.getRelativeSrcPath());
	}

	@Test
	public void getRelativeGeneratedPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_GENERATED_DIRECTORY), p.getRelativeGeneratedPath());
	}

	@Test
	public void getRelativeResourcePathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_RESOURCES_DIRECTORY), p.getRelativeResourcePath());
	}

	@Test
	public void getRelativeWebPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_WEB_DIRECTORY), p.getRelativeWebPath());
	}

	@Test
	public void getRelativeTestPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_TEST_DIRECTORY), p.getRelativeTestPath());
	}

	@Test
	public void getRelativeGeneratedTestPathUsesDefault() {
		MavenSkyveProject p = createBasicProject();
		assertEquals(Paths.get(MavenSkyveProjectCreator.DEFAULT_GENERATED_TEST_DIRECTORY), p.getRelativeGeneratedTestPath());
	}

	// ---- Absolute path getters resolve relative to project directory ----

	@Test
	public void getAbsoluteSrcPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY);
		assertEquals(expected, p.getAbsoluteSrcPath());
	}

	@Test
	public void getAbsoluteCustomerPathResolvesUnderSrcDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR)
				.resolve(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY)
				.resolve("customers")
				.resolve(CUSTOMER_NAME);
		assertEquals(expected, p.getAbsoluteCustomerPath());
	}

	@Test
	public void getAbsoluteCustomerResourcesPathResolvesUnderCustomerDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR)
				.resolve(MavenSkyveProjectCreator.DEFAULT_SRC_DIRECTORY)
				.resolve("customers")
				.resolve(CUSTOMER_NAME)
				.resolve("resources");
		assertEquals(expected, p.getAbsoluteCustomerResourcesPath());
	}

	@Test
	public void getAbsoluteGeneratedPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_GENERATED_DIRECTORY);
		assertEquals(expected, p.getAbsoluteGeneratedPath());
	}

	@Test
	public void getAbsoluteResourcePathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_RESOURCES_DIRECTORY);
		assertEquals(expected, p.getAbsoluteResourcePath());
	}

	@Test
	public void getAbsoluteWebPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_WEB_DIRECTORY);
		assertEquals(expected, p.getAbsoluteWebPath());
	}

	@Test
	public void getAbsoluteTestPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_TEST_DIRECTORY);
		assertEquals(expected, p.getAbsoluteTestPath());
	}

	@Test
	public void getAbsoluteGeneratedTestPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve(MavenSkyveProjectCreator.DEFAULT_GENERATED_TEST_DIRECTORY);
		assertEquals(expected, p.getAbsoluteGeneratedTestPath());
	}

	@Test
	public void getAbsoluteDockerPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve("docker");
		assertEquals(expected, p.getAbsoluteDockerPath());
	}

	@Test
	public void getAbsoluteDeploymentsPathResolvesUnderProjectDir() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve("deployments");
		assertEquals(expected, p.getAbsoluteDeploymentsPath());
	}

	// ---- Skyve-directory-dependent paths ----

	@Test
	public void getSkyveBasePathReturnsSkyveDirectoryAbsolutePath() {
		MavenSkyveProject p = createProjectWithSkyve();
		assertEquals(Paths.get(SKYVE_DIR).toAbsolutePath(), p.getSkyveBasePath());
	}

	@Test
	public void getSkyveAbsoluteWarPathResolvesUnderSkyveDir() {
		MavenSkyveProject p = createProjectWithSkyve();
		Path expected = Paths.get(SKYVE_DIR).resolve(MavenSkyveProject.SKYVE_WEBAPP_PATH);
		assertEquals(expected, p.getSkyveAbsoluteWarPath());
	}

	// ---- Other property tests ----

	@Test
	public void getSkyveScriptReturnsScript() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("module Contact { ... }")
				.initialise();
		assertEquals("module Contact { ... }", p.getSkyveScript());
	}

	@Test
	public void getSkyveScriptNullByDefault() {
		MavenSkyveProject p = createBasicProject();
		assertNull(p.getSkyveScript());
	}

	@Test
	public void getSkyveScriptPathResolvesScriptsFolder() {
		MavenSkyveProject p = createBasicProject();
		Path expected = Paths.get(PROJECT_DIR).resolve("scripts").resolve("skyve.md");
		assertEquals(expected, p.getSkyveScriptPath());
	}

	@Test
	public void getMetaDataInitiallyEmpty() {
		MavenSkyveProject p = createBasicProject();
		assertNotNull(p.getMetaData());
		assertTrue(p.getMetaData().isEmpty());
	}

	@Test
	public void isCopyFromProjectDefaultFalse() {
		MavenSkyveProject p = createBasicProject();
		assertFalse(p.isCopyFromProject());
	}

	@Test
	public void isCopyFromProjectTrueWhenSet() {
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
	public void customSrcDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.srcDirectory("custom/src/")
				.initialise();
		assertEquals(Paths.get("custom/src/"), p.getRelativeSrcPath());
	}

	@Test
	public void customGeneratedDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.generatedDirectory("custom/gen/")
				.initialise();
		assertEquals(Paths.get("custom/gen/"), p.getRelativeGeneratedPath());
	}

	@Test
	public void customResourceDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.resourceDirectory("custom/resources/")
				.initialise();
		assertEquals(Paths.get("custom/resources/"), p.getRelativeResourcePath());
	}

	@Test
	public void customWebDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.webDirectory("custom/webapp/")
				.initialise();
		assertEquals(Paths.get("custom/webapp/"), p.getRelativeWebPath());
	}

	@Test
	public void customTestDirectoryApplied() {
		MavenSkyveProject p = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.testDirectory("custom/test/")
				.initialise();
		assertEquals(Paths.get("custom/test/"), p.getRelativeTestPath());
	}

	@Test
	public void customGeneratedTestDirectoryApplied() {
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
	public void staticSkyveWarPathEndsWithWebapp() {
		assertTrue(MavenSkyveProject.SKYVE_WEBAPP_PATH.toString().contains("webapp"));
	}

	@Test
	public void staticSkyveTestPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_TEST_PATH.toString().contains("java"));
	}

	@Test
	public void staticSkyveGeneratedPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_GENERATED_PATH.toString().contains("java"));
	}

	@Test
	public void staticSkyveWebPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_WEB_PATH.toString().contains("java"));
	}

	@Test
	public void staticSkyveSrcPathEndsWithJava() {
		assertTrue(MavenSkyveProject.SKYVE_SRC_PATH.toString().contains("java"));
	}

	@Test
	public void staticSkyveSrcResourcesPathContainsResources() {
		assertTrue(MavenSkyveProject.SKYVE_SRC_RESOURCES_PATH.toString().contains("resources"));
	}

	@Test
	public void staticSkyveGeneratedTestPathContainsGeneratedTest() {
		assertTrue(MavenSkyveProject.SKYVE_GENERATED_TEST_PATH.toString().contains("generatedTest"));
	}

	@Test
	public void applyScriptWithValidScriptPopulatesMetaData() throws SkyveProjectCreationException {
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
	public void applyScriptWithNoModulesDetectedLeavesMetaDataEmpty() throws SkyveProjectCreationException {
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
	public void applyScriptWithValidModuleAndDocumentPopulatesMetaData() throws SkyveProjectCreationException {
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
	public void getSkyveScriptReturnsScriptSetViaBuilder() {
		MavenSkyveProject project = new MavenSkyveProjectCreator()
				.projectName(PROJECT_NAME)
				.projectDirectory(PROJECT_DIR)
				.customerName(CUSTOMER_NAME)
				.skyveScript("# MyModule")
				.initialise();
		assertEquals("# MyModule", project.getSkyveScript());
	}

	@Test
	public void getSkyveScriptPathContainsScriptsDir() {
		MavenSkyveProject project = createBasicProject();
		assertTrue(project.getSkyveScriptPath().toString().contains("scripts"));
	}

	@Test
	public void isCopyFromProjectDefaultIsFalse() {
		MavenSkyveProject project = createBasicProject();
		assertFalse(project.isCopyFromProject());
	}
}
