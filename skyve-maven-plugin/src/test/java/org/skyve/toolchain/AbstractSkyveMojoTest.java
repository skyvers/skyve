package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

class AbstractSkyveMojoTest {

	/**
	 * Minimal concrete subclass for testing AbstractSkyveMojo.
	 */
	static class TestMojo extends AbstractSkyveMojo {
		@Override
		public void execute() throws MojoExecutionException {
			// no-op
		}
	}

	@TempDir
	Path tempDir;

	private TestMojo mojo;
	private org.apache.maven.project.MavenProject project;

	@BeforeEach
	void setUp() {
		mojo = new TestMojo();
		project = new org.apache.maven.project.MavenProject();
		ReflectionTestUtils.setField(mojo, "project", project);
	}

	@Test
	void getModulesDirectoryReturnsPathWhenModulesExists() throws Exception {
		Path src = tempDir.resolve("src");
		Files.createDirectories(src.resolve("modules"));
		project.addCompileSourceRoot(src.toString());

		Path result = mojo.getModulesDirectory();
		assertTrue(result.endsWith("modules"));
		assertTrue(result.toFile().exists());
	}

	@Test
	void getModulesDirectoryThrowsWhenModulesAbsent() {
		project.addCompileSourceRoot(tempDir.toString());
		assertThrows(FileNotFoundException.class, () -> mojo.getModulesDirectory());
	}

	@Test
	void getCustomersDirectoryReturnsPathWhenCustomersExists() throws Exception {
		Path src = tempDir.resolve("src");
		Files.createDirectories(src.resolve("customers"));
		project.addCompileSourceRoot(src.toString());

		Path result = mojo.getCustomersDirectory();
		assertTrue(result.endsWith("customers"));
	}

	@Test
	void getCustomerDirectoriesReturnsOnlyDirectories() throws Exception {
		Path customersDir = tempDir.resolve("customers");
		Files.createDirectories(customersDir.resolve("customerA"));
		Files.createDirectories(customersDir.resolve("customerB"));
		Files.createFile(customersDir.resolve("notADirectory.txt"));

		List<File> dirs = AbstractSkyveMojo.getCustomerDirectories(customersDir);
		assertEquals(2, dirs.size());
		assertTrue(dirs.stream().allMatch(File::isDirectory));
	}

	@Test
	void getDefaultOrPromptReturnsDefaultWhenNonBlank() throws Exception {
		assertEquals("myDefault", mojo.getDefaultOrPrompt("myDefault", "Enter value"));
	}

	@Test
	void getDefaultOrPromptCallsPrompterWhenDefaultBlank() throws Exception {
		Prompter mockPrompter = mock(Prompter.class);
		when(mockPrompter.prompt(anyString())).thenReturn("promptedValue");
		ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);

		assertEquals("promptedValue", mojo.getDefaultOrPrompt(null, "Enter value"));
		assertEquals("promptedValue", mojo.getDefaultOrPrompt("  ", "Enter value"));
	}

	@Test
	void getDefaultOrPromptCustomerReturnsSingleCustomerWithoutPrompt() throws Exception {
		Path src = tempDir.resolve("src");
		Files.createDirectories(src.resolve("customers").resolve("onlyCustomer"));
		project.addCompileSourceRoot(src.toString());

		String result = mojo.getDefaultOrPromptCustomer(null);
		assertEquals("onlyCustomer", result);
	}

	@Test
	void getDefaultOrPromptCustomerReturnsDefaultWhenNonBlank() throws Exception {
		String result = mojo.getDefaultOrPromptCustomer("fixedCustomer");
		assertEquals("fixedCustomer", result);
	}

	@Test
	void getDefaultOrPromptCustomerPromptsWhenMultipleCustomers() throws Exception {
		Path src = tempDir.resolve("src");
		Files.createDirectories(src.resolve("customers").resolve("alpha"));
		Files.createDirectories(src.resolve("customers").resolve("beta"));
		project.addCompileSourceRoot(src.toString());

		Prompter mockPrompter = mock(Prompter.class);
		when(mockPrompter.prompt(anyString())).thenReturn("alpha");
		ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);

		String result = mojo.getDefaultOrPromptCustomer(null);
		assertEquals("alpha", result);
	}

	@Test
	void getDefaultOrPromptCustomerThrowsWhenSelectedCustomerDoesNotExist() throws Exception {
		Path src = tempDir.resolve("src");
		Files.createDirectories(src.resolve("customers").resolve("alpha"));
		Files.createDirectories(src.resolve("customers").resolve("beta"));
		project.addCompileSourceRoot(src.toString());

		Prompter mockPrompter = mock(Prompter.class);
		when(mockPrompter.prompt(anyString())).thenReturn("nonexistent");
		ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);

		assertThrows(IllegalArgumentException.class, () -> mojo.getDefaultOrPromptCustomer(null));
	}

	@Test
	void getDefaultOrPromptCustomerPromptsWhenNoCustomersDir() throws Exception {
		Path src = tempDir.resolve("src");
		Files.createDirectories(src.resolve("customers"));
		// No subdirectories - empty customers directory
		project.addCompileSourceRoot(src.toString());

		Prompter mockPrompter = mock(Prompter.class);
		when(mockPrompter.prompt(anyString())).thenReturn("manualCustomer");
		ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);

		String result = mojo.getDefaultOrPromptCustomer(null);
		assertEquals("manualCustomer", result);
	}
}
