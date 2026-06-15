package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Path;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.toolchain.config.GenerateDefaultQueriesConfig;
import org.skyve.toolchain.config.GenerateDomainConfig;
import org.skyve.toolchain.config.GenerateEditViewConfig;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("static-method")
class MojoExecutionFailureTest {

	@TempDir
	Path tempDir;

	@Test
	void generateDomainFailsWhenConfigurationMissing() {
		MojoExecutionException exception = assertThrows(MojoExecutionException.class,
				() -> new GenerateDomainMojo().execute());

		assertTrue(exception.getMessage().contains("Generate domain configuration not specified"));
	}

	@Test
	void generateDomainWrapsClasspathFailureAfterReadingConfiguration() {
		GenerateDomainMojo mojo = new GenerateDomainMojo();
		GenerateDomainConfig config = new GenerateDomainConfig();
		ReflectionTestUtils.setField(mojo, "generateDomainConfig", config);

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to generate domain"));
	}

	@Test
	void generateDefaultQueriesWrapsMissingCustomersDirectory() {
		GenerateDefaultQueriesMojo mojo = new GenerateDefaultQueriesMojo();
		ReflectionTestUtils.setField(mojo, "project", projectWithSourceRoot(tempDir));

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to generate default queries"));
	}

	@Test
	void generateDefaultQueriesWrapsClasspathFailureAfterReadingConfiguration() {
		GenerateDefaultQueriesMojo mojo = new GenerateDefaultQueriesMojo();
		GenerateDefaultQueriesConfig config = new GenerateDefaultQueriesConfig();
		config.setCustomer("demo");
		config.setModule("admin");
		config.setIncludeAssociationBizKeys(true);
		ReflectionTestUtils.setField(mojo, "generateDefaultQueriesConfig", config);

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to generate default queries"));
	}

	@Test
	void generateEditViewWrapsClasspathFailureAfterReadingConfiguration() {
		GenerateEditViewMojo mojo = new GenerateEditViewMojo();
		GenerateEditViewConfig config = new GenerateEditViewConfig();
		config.setCustomer("demo");
		config.setModule("admin");
		config.setDocument("User");
		config.setCustomerOverriden(true);
		config.setOverridenViewName("edit");
		ReflectionTestUtils.setField(mojo, "generateEditViewConfig", config);

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to generate edit view"));
	}

	@Test
	void compileJasperReportWrapsMissingModulesDirectory() throws Exception {
		CompileJasperReportMojo mojo = new CompileJasperReportMojo();
		Prompter prompter = mock(Prompter.class);
		when(prompter.prompt(anyString())).thenReturn("missingReport");
		ReflectionTestUtils.setField(mojo, "prompter", prompter);
		ReflectionTestUtils.setField(mojo, "project", projectWithSourceRoot(tempDir));

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to compile report"));
	}

	@Test
	void assembleWrapsMissingSkyveDirectory() {
		AssembleMojo mojo = new AssembleMojo();
		ReflectionTestUtils.setField(mojo, "project", projectWithBaseDir());
		ReflectionTestUtils.setField(mojo, "skyveDir", "missing-skyve");
		ReflectionTestUtils.setField(mojo, "customer", "demo");

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to assemble"));
	}

	@Test
	void scriptWrapsMissingScriptFile() {
		ScriptMojo mojo = new ScriptMojo();
		ReflectionTestUtils.setField(mojo, "project", projectWithTestClasspath());
		ReflectionTestUtils.setField(mojo, "skyveDir", tempDir.toString());
		ReflectionTestUtils.setField(mojo, "customer", "demo");
		ReflectionTestUtils.setField(mojo, "scriptPath", "missing.md");

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to apply Skyve script"));
	}

	@Test
	void clearBeforeAssembleCompletesForEmptyProject() {
		ClearBeforeAssembleMojo mojo = new ClearBeforeAssembleMojo();
		ReflectionTestUtils.setField(mojo, "project", projectWithBaseDir());
		ReflectionTestUtils.setField(mojo, "customer", "demo");

		assertDoesNotThrow(mojo::execute);
	}

	private MavenProject projectWithSourceRoot(Path sourceRoot) {
		MavenProject project = projectWithTestClasspath();
		project.addCompileSourceRoot(sourceRoot.toString());
		return project;
	}

	private MavenProject projectWithTestClasspath() {
		return projectWithBaseDir();
	}

	private MavenProject projectWithBaseDir() {
		MavenProject project = new MavenProject();
		project.setName("test-project");
		ReflectionTestUtils.setField(project, "basedir", tempDir.toFile());
		return project;
	}
}
