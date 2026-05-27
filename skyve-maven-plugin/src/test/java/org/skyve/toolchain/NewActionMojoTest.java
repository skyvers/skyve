package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

class NewActionMojoTest {

	@TempDir
	Path tempDir;

	private NewActionMojo mojo;
	private Prompter mockPrompter;

	@BeforeEach
	void setUp() throws Exception {
		mojo = new NewActionMojo();
		mockPrompter = mock(Prompter.class);

		org.apache.maven.project.MavenProject project = new org.apache.maven.project.MavenProject();
		Files.createDirectories(tempDir.resolve("modules/testModule/TestDocument"));
		project.addCompileSourceRoot(tempDir.toString());

		ReflectionTestUtils.setField(mojo, "project", project);
		ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);
		ReflectionTestUtils.setField(mojo, "srcDir", tempDir.toString());
	}

	@Test
	void executeCreatesActionClassFile() throws Exception {
		when(mockPrompter.prompt(anyString()))
				.thenReturn("testModule")
				.thenReturn("TestDocument")
				.thenReturn("SaveTestDocument");

		mojo.execute();

		assertTrue(tempDir.resolve("modules/testModule/TestDocument/actions").toFile().exists());
		assertTrue(tempDir.resolve("modules/testModule/TestDocument/actions/SaveTestDocument.java").toFile().exists());
	}

	@Test
	void executeThrowsWhenModuleDoesNotExist() throws Exception {
		when(mockPrompter.prompt(anyString()))
				.thenReturn("nonExistentModule")
				.thenReturn("TestDocument")
				.thenReturn("SomeAction");

		assertThrows(MojoExecutionException.class, () -> mojo.execute());
	}

	@Test
	void executeThrowsWhenDocumentDoesNotExist() throws Exception {
		when(mockPrompter.prompt(anyString()))
				.thenReturn("testModule")
				.thenReturn("NonExistentDocument")
				.thenReturn("SomeAction");

		assertThrows(MojoExecutionException.class, () -> mojo.execute());
	}

	@Test
	void executeUsesExtensionClassWhenPresent() throws Exception {
		Path docDir = tempDir.resolve("modules/testModule/TestDocument");
		Files.writeString(docDir.resolve("TestDocumentExtension.java"), "// extension");

		when(mockPrompter.prompt(anyString()))
				.thenReturn("testModule")
				.thenReturn("TestDocument")
				.thenReturn("SaveAction");

		mojo.execute();

		Path actionFile = tempDir.resolve("modules/testModule/TestDocument/actions/SaveAction.java");
		assertTrue(actionFile.toFile().exists());
		String content = Files.readString(actionFile);
		assertTrue(content.contains("TestDocumentExtension"));
	}
}
