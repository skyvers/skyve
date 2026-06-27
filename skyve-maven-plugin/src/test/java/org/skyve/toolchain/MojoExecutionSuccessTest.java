package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;

import java.io.File;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.create.MavenSkyveProject;
import org.skyve.impl.create.MavenSkyveProject.MavenSkyveProjectCreator;
import org.skyve.impl.generate.DialectOptions;
import org.skyve.toolchain.config.GenerateDefaultQueriesConfig;
import org.skyve.toolchain.config.GenerateDomainConfig;
import org.skyve.toolchain.config.GenerateEditViewConfig;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("static-method")
class MojoExecutionSuccessTest {

	@TempDir
	Path tempDir;

	@Test
	void generateDomainUsesConfiguration() throws Exception {
		RecordingGenerateDomainMojo mojo = new RecordingGenerateDomainMojo();
		GenerateDomainConfig config = new GenerateDomainConfig();
		config.setDebug(true);
		config.setMultiTenant(true);
		config.setDialect("POSTGRESQL");
		config.setExcludedModules("admin,test");
		config.setCustomisationsClass("com.example.Customisations");
		ReflectionTestUtils.setField(mojo, "generateDomainConfig", config);
		ReflectionTestUtils.setField(mojo, "srcDir", "src");
		ReflectionTestUtils.setField(mojo, "generatedDir", "generated");
		ReflectionTestUtils.setField(mojo, "testDir", "test");
		ReflectionTestUtils.setField(mojo, "generatedTestDir", "generatedTest");

		mojo.execute();

		assertTrue(mojo.classpathConfigured);
		assertEquals("src", mojo.classpathSourceDirectory);
		assertEquals("com.example.Customisations", mojo.customisationsClassName);
		assertTrue(mojo.debug);
		assertTrue(mojo.multiTenant);
		assertEquals(DialectOptions.POSTGRESQL, mojo.dialect);
		assertEquals("src", mojo.sourceDirectory);
		assertEquals("generated", mojo.generatedDirectory);
		assertEquals("test", mojo.testDirectory);
		assertEquals("generatedTest", mojo.generatedTestDirectory);
		assertArrayEquals(new String[] {"admin", "test"}, mojo.excludedModules);
	}

	@Test
	void generateDefaultQueriesUsesConfiguredValues() throws Exception {
		RecordingGenerateDefaultQueriesMojo mojo = new RecordingGenerateDefaultQueriesMojo();
		GenerateDefaultQueriesConfig config = new GenerateDefaultQueriesConfig();
		config.setCustomer("demo");
		config.setModule("admin");
		config.setIncludeAssociationBizKeys(true);
		ReflectionTestUtils.setField(mojo, "generateDefaultQueriesConfig", config);

		mojo.execute();

		assertTrue(mojo.classpathConfigured);
		assertArrayEquals(new String[] {"demo", "admin", "true"}, mojo.arguments);
	}

	@Test
	void generateDefaultQueriesPromptsWhenConfigurationIsMissing() throws Exception {
		RecordingGenerateDefaultQueriesMojo mojo = new RecordingGenerateDefaultQueriesMojo();

		mojo.execute();

		assertTrue(mojo.classpathConfigured);
		assertArrayEquals(new String[] {"promptedCustomer", "promptedModule", "false"}, mojo.arguments);
	}

	@Test
	void generateEditViewUsesConfiguredValues() throws Exception {
		RecordingGenerateEditViewMojo mojo = new RecordingGenerateEditViewMojo();
		GenerateEditViewConfig config = new GenerateEditViewConfig();
		config.setCustomer("demo");
		config.setModule("admin");
		config.setDocument("User");
		config.setCustomerOverriden(true);
		config.setOverridenViewName("edit");
		ReflectionTestUtils.setField(mojo, "generateEditViewConfig", config);
		ReflectionTestUtils.setField(mojo, "srcDir", "src/main/java");

		mojo.execute();

		assertTrue(mojo.classpathConfigured);
		assertEquals("src/main/java", mojo.classpathSourceDirectory);
		assertTrue(mojo.repositorySet);
		assertArrayEquals(new String[] {"src/main/java", "demo", "admin", "User", "true", "edit"}, mojo.arguments);
	}

	@Test
	void generateEditViewPromptsWhenConfigurationIsMissing() throws Exception {
		RecordingGenerateEditViewMojo mojo = new RecordingGenerateEditViewMojo();
		ReflectionTestUtils.setField(mojo, "srcDir", "src/main/java");

		mojo.execute();

		assertArrayEquals(new String[] {"src/main/java", "promptedCustomer", "promptedModule", "promptedDocument", "false", null},
				mojo.arguments);
	}

	@Test
	void generateEditViewCanWireLocalRepository() {
		GenerateEditViewMojo mojo = new GenerateEditViewMojo();
		ReflectionTestUtils.setField(mojo, "srcDir", tempDir.toString());

		assertDoesNotThrow(mojo::setRepository);
	}

	@Test
	void compileJasperReportFindsNamedReportsWithOrWithoutExtension() throws Exception {
		CompileJasperReportMojo mojo = new CompileJasperReportMojo();
		MavenProject project = projectWithSourceRoot(tempDir);
		ReflectionTestUtils.setField(mojo, "project", project);
		Path modules = Files.createDirectories(tempDir.resolve("modules").resolve("admin").resolve("reports"));
		Path report = Files.writeString(modules.resolve("users.jrxml"), "<jasperReport/>");
		Files.writeString(modules.resolve("ignored.txt"), "");

		assertEquals(report.toFile(), mojo.getReports("users").get(0));
		assertEquals(report.toFile(), mojo.getReports("users.jrxml").get(0));
	}

	@Test
	void compileJasperReportCompilesEachMatchingReport() throws Exception {
		RecordingCompileJasperReportMojo mojo = new RecordingCompileJasperReportMojo();
		Prompter prompter = Mockito.mock(Prompter.class);
		Mockito.when(prompter.prompt(anyString())).thenReturn("users");
		ReflectionTestUtils.setField(mojo, "prompter", prompter);
		Path report = tempDir.resolve("users.jrxml");
		Files.writeString(report, "<jasperReport/>");
		mojo.reports = java.util.List.of(report.toFile());

		mojo.execute();

		assertTrue(mojo.classpathConfigured);
		assertEquals(report.toFile(), mojo.compiledReport);
		assertEquals(report.toString().replace("jrxml", "jasper"), mojo.compiledReportFilename);
	}

	@Test
	void compileJasperReportFailsWhenNoReportsMatch() throws Exception {
		RecordingCompileJasperReportMojo mojo = new RecordingCompileJasperReportMojo();
		Prompter prompter = Mockito.mock(Prompter.class);
		Mockito.when(prompter.prompt(anyString())).thenReturn("users");
		ReflectionTestUtils.setField(mojo, "prompter", prompter);
		mojo.reports = java.util.List.of();

		org.apache.maven.plugin.MojoExecutionException exception = org.junit.jupiter.api.Assertions.assertThrows(
				org.apache.maven.plugin.MojoExecutionException.class,
				mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to compile report"));
	}

	@Test
	void compileReportDelegatesToJasperCompiler() {
		CompileJasperReportMojo mojo = new CompileJasperReportMojo();

		org.junit.jupiter.api.Assertions.assertThrows(Exception.class,
				() -> mojo.compileReport(tempDir.resolve("invalid.jrxml").toFile(),
						tempDir.resolve("invalid.jasper").toString()));
	}

	@Test
	void generateDomainRegisterCustomisationsAcceptsBlankConfiguration() {
		GenerateDomainMojo mojo = new GenerateDomainMojo();

		assertDoesNotThrow(() -> mojo.registerCustomisations(null));
	}

	@Test
	void assembleUsesRelativeSkyveDirectoryWithoutTemplate() throws Exception {
		RecordingAssembleMojo mojo = new RecordingAssembleMojo();
		Path skyveDir = Files.createDirectory(tempDir.resolve("skyve"));
		ReflectionTestUtils.setField(mojo, "project", projectWithBaseDir(tempDir));
		ReflectionTestUtils.setField(mojo, "skyveDir", "skyve");
		ReflectionTestUtils.setField(mojo, "customer", "demo");

		mojo.execute();

		assertEquals("test-project", mojo.projectName);
		assertEquals(tempDir.toString(), mojo.projectDirectory);
		assertEquals("demo", mojo.customerName);
		assertEquals(skyveDir.toString(), mojo.skyveDirectory);
		assertFalse(mojo.copyFromProject);
		assertTrue(mojo.cleared);
		assertTrue(mojo.assembled);
	}

	@Test
	void assembleUsesAbsoluteSkyveDirectory() throws Exception {
		RecordingAssembleMojo mojo = new RecordingAssembleMojo();
		Path skyveDir = Files.createDirectory(tempDir.resolve("skyve"));
		ReflectionTestUtils.setField(mojo, "project", projectWithBaseDir(tempDir));
		ReflectionTestUtils.setField(mojo, "skyveDir", skyveDir.toString());
		ReflectionTestUtils.setField(mojo, "customer", "demo");

		mojo.execute();

		assertEquals(skyveDir.toString(), mojo.skyveDirectory);
	}

	@Test
	void assembleUsesExistingTemplateDirectoryWhenConfigured() throws Exception {
		RecordingAssembleMojo mojo = new RecordingAssembleMojo();
		Files.createDirectory(tempDir.resolve("skyve"));
		Path templateDir = Files.createDirectory(tempDir.resolve("template"));
		ReflectionTestUtils.setField(mojo, "project", projectWithBaseDir(tempDir));
		ReflectionTestUtils.setField(mojo, "skyveDir", "skyve");
		ReflectionTestUtils.setField(mojo, "templateDir", "template");
		ReflectionTestUtils.setField(mojo, "customer", "demo");

		mojo.execute();

		assertEquals(templateDir.toString(), mojo.skyveDirectory);
		assertTrue(mojo.copyFromProject);
	}

	private MavenProject projectWithSourceRoot(Path sourceRoot) {
		MavenProject project = projectWithBaseDir(tempDir);
		project.addCompileSourceRoot(sourceRoot.toString());
		return project;
	}

	private static MavenProject projectWithBaseDir(Path basedir) {
		MavenProject project = new MavenProject();
		project.setName("test-project");
		ReflectionTestUtils.setField(project, "basedir", basedir.toFile());
		return project;
	}

	private static final class RecordingGenerateDomainMojo extends GenerateDomainMojo {
		private boolean classpathConfigured;
		private String classpathSourceDirectory;
		private String customisationsClassName;
		private boolean debug;
		private boolean multiTenant;
		private DialectOptions dialect;
		private String sourceDirectory;
		private String generatedDirectory;
		private String testDirectory;
		private String generatedTestDirectory;
		private String[] excludedModules;

		@Override
		protected void configureClasspath(String srcDir) throws DependencyResolutionRequiredException, MalformedURLException {
			classpathConfigured = true;
			classpathSourceDirectory = srcDir;
		}

		@Override
		void registerCustomisations(String customisationsClassNameValue) {
			this.customisationsClassName = customisationsClassNameValue;
		}

		@Override
		void generateDomain(boolean debugValue,
								boolean multiTenantValue,
								DialectOptions dialectValue,
								String sourceDirectoryValue,
								String generatedDirectoryValue,
								String testDirectoryValue,
								String generatedTestDirectoryValue,
								String[] excludedModulesValue) {
			this.debug = debugValue;
			this.multiTenant = multiTenantValue;
			this.dialect = dialectValue;
			this.sourceDirectory = sourceDirectoryValue;
			this.generatedDirectory = generatedDirectoryValue;
			this.testDirectory = testDirectoryValue;
			this.generatedTestDirectory = generatedTestDirectoryValue;
			this.excludedModules = excludedModulesValue;
		}
	}

	private static final class RecordingGenerateDefaultQueriesMojo extends GenerateDefaultQueriesMojo {
		private boolean classpathConfigured;
		private String[] arguments;

		@Override
		protected void configureClasspath() throws DependencyResolutionRequiredException, MalformedURLException {
			classpathConfigured = true;
		}

		@Override
		void generateDefaultQueries(String[] argumentValues) {
			this.arguments = argumentValues;
		}

		@Override
		protected String getDefaultOrPromptCustomer(String defaultCustomer) {
			return (defaultCustomer == null) ? "promptedCustomer" : defaultCustomer;
		}

		@Override
		protected String getDefaultOrPrompt(String defaultValue, String promptMessage) {
			return (defaultValue == null) ? "promptedModule" : defaultValue;
		}
	}

	private static final class RecordingGenerateEditViewMojo extends GenerateEditViewMojo {
		private boolean classpathConfigured;
		private String classpathSourceDirectory;
		private boolean repositorySet;
		private String[] arguments;

		@Override
		protected void configureClasspath(String srcDir) throws DependencyResolutionRequiredException, MalformedURLException {
			classpathConfigured = true;
			classpathSourceDirectory = srcDir;
		}

		@Override
		void setRepository() {
			repositorySet = true;
		}

		@Override
		void generateEditView(String[] argumentValues) {
			this.arguments = argumentValues;
		}

		@Override
		protected String getDefaultOrPromptCustomer(String defaultCustomer) {
			return (defaultCustomer == null) ? "promptedCustomer" : defaultCustomer;
		}

		@Override
		protected String getDefaultOrPrompt(String defaultValue, String promptMessage) {
			if (defaultValue != null) {
				return defaultValue;
			}
			return promptMessage.contains("module") ? "promptedModule" : "promptedDocument";
		}
	}

	private static final class RecordingCompileJasperReportMojo extends CompileJasperReportMojo {
		private boolean classpathConfigured;
		private java.util.List<File> reports;
		private File compiledReport;
		private String compiledReportFilename;

		@Override
		protected void configureClasspath() throws DependencyResolutionRequiredException, MalformedURLException {
			classpathConfigured = true;
		}

		@Override
		java.util.List<File> getReports(String reportName) {
			return reports;
		}

		@Override
		void compileReport(File report, String compiledReportFileNameValue) {
			compiledReport = report;
			this.compiledReportFilename = compiledReportFileNameValue;
		}
	}

	private static final class RecordingAssembleMojo extends AssembleMojo {
		private String projectName;
		private String projectDirectory;
		private String customerName;
		private String skyveDirectory;
		private boolean copyFromProject;
		private boolean cleared;
		private boolean assembled;

		@Override
		MavenSkyveProjectCreator newProjectCreator() {
			return new RecordingCreator(this);
		}

		@Override
		MavenSkyveProject initialise(MavenSkyveProjectCreator creator) {
			return null;
		}

		@Override
		void clearBeforeAssemble(MavenSkyveProject projectToAssemble) {
			cleared = true;
		}

		@Override
		void assemble(MavenSkyveProject projectToAssemble) {
			assembled = true;
		}
	}

	private static final class RecordingCreator extends MavenSkyveProjectCreator {
		private final RecordingAssembleMojo mojo;

		private RecordingCreator(RecordingAssembleMojo mojo) {
			this.mojo = mojo;
		}

		@Override
		public MavenSkyveProjectCreator projectName(String projectName) {
			mojo.projectName = projectName;
			return this;
		}

		@Override
		public MavenSkyveProjectCreator projectDirectory(String projectDirectory) {
			mojo.projectDirectory = projectDirectory;
			return this;
		}

		@Override
		public MavenSkyveProjectCreator customerName(String customerName) {
			mojo.customerName = customerName;
			return this;
		}

		@Override
		public MavenSkyveProjectCreator skyveDirectory(String skyveDirectory) {
			mojo.skyveDirectory = skyveDirectory;
			return this;
		}

		@Override
		public MavenSkyveProjectCreator copyFromProject(boolean copyFromProject) {
			mojo.copyFromProject = copyFromProject;
			return this;
		}
	}
}
