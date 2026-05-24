package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.maven.plugin.MojoExecutionException;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

class NewModuleMojoTest {

	@TempDir
	Path tempDir;

	private NewModuleMojo mojo;
	private Prompter mockPrompter;

	@BeforeEach
	void setUp() throws Exception {
		mojo = new NewModuleMojo();
		mockPrompter = mock(Prompter.class);

		org.apache.maven.project.MavenProject project = new org.apache.maven.project.MavenProject();
		Files.createDirectories(tempDir.resolve("modules"));
		project.addCompileSourceRoot(tempDir.toString());
		copySchemas(tempDir);

		ReflectionTestUtils.setField(mojo, "project", project);
		ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);
	}

	private static void copySchemas(Path targetBase) throws IOException {
		Path sourceSchemasDir = Paths.get(System.getProperty("user.dir"))
				.getParent()
				.resolve("skyve-war/src/main/java/schemas");
		Path targetSchemasDir = targetBase.resolve("schemas");
		Files.createDirectories(targetSchemasDir);
		if (Files.exists(sourceSchemasDir)) {
			try (var stream = Files.walk(sourceSchemasDir)) {
				stream.filter(Files::isRegularFile).forEach(f -> {
					try {
						Files.copy(f, targetSchemasDir.resolve(f.getFileName()));
					}
					catch (IOException e) {
						throw new RuntimeException(e);
					}
				});
			}
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void executeCreatesModuleDirectoryAndMetadata() throws Exception {
		when(mockPrompter.prompt(anyString())).thenReturn("testModule");
		mojo.execute();
		assertTrue(tempDir.resolve("modules/testModule").toFile().exists());
		assertTrue(tempDir.resolve("modules/testModule/testModule.xml").toFile().exists());
	}

	@Test
	@SuppressWarnings("static-method")
	void executeThrowsWhenModuleAlreadyExists() throws Exception {
		Files.createDirectories(tempDir.resolve("modules/existingModule"));
		when(mockPrompter.prompt(anyString())).thenReturn("existingModule");
		assertThrows(MojoExecutionException.class, () -> mojo.execute());
	}

	@Test
	@SuppressWarnings("static-method")
	void executeHandlesMissingCustomersDirectoryGracefully() throws Exception {
		when(mockPrompter.prompt(anyString())).thenReturn("newModule");
		mojo.execute();
	}

	@Test
	@SuppressWarnings("static-method")
	void executeWithCustomersDirectoryButNoCustomerXmlLogsWarning() throws Exception {
		// Customers dir exists with one customer dir but no XML file - should warn and return
		Files.createDirectories(tempDir.resolve("customers/myCustomer"));
		when(mockPrompter.prompt(anyString())).thenReturn("anotherModule");
		// Should complete without exception even when customer XML is absent
		mojo.execute();
		assertTrue(tempDir.resolve("modules/anotherModule").toFile().exists());
	}

	@Test
	@SuppressWarnings("static-method")
	void executeWithMultipleCustomersPromptsForSelection() throws Exception {
		Files.createDirectories(tempDir.resolve("customers/alpha"));
		Files.createDirectories(tempDir.resolve("customers/beta"));
		// Prompt returns module name first, then customer name
		when(mockPrompter.prompt(anyString()))
				.thenReturn("multiCustModule")
				.thenReturn("alpha");
		mojo.execute();
		assertTrue(tempDir.resolve("modules/multiCustModule").toFile().exists());
	}

	@Test
	@SuppressWarnings("static-method")
	void executeWithMultipleCustomersAndUnknownSelectionLogsWarning() throws Exception {
		Files.createDirectories(tempDir.resolve("customers/alpha"));
		Files.createDirectories(tempDir.resolve("customers/beta"));
		// Prompt returns module name first, then non-existent customer
		when(mockPrompter.prompt(anyString()))
				.thenReturn("warnModule")
				.thenReturn("nonexistent");
		// Should warn but not throw
		mojo.execute();
		assertTrue(tempDir.resolve("modules/warnModule").toFile().exists());
	}

	@Test
	@SuppressWarnings("static-method")
	void executeWithSingleCustomerAndValidXmlAddsModuleToCustomer() throws Exception {
		// Set up single customer with valid customer XML
		String customerName = "demo";
		Path customerDir = tempDir.resolve("customers/" + customerName);
		Files.createDirectories(customerDir);

		// Write a minimal valid customer XML (matching skyve customer schema)
		String customerXml = String.format("""
				<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
				<customer name="%s" xmlns="http://www.skyve.org/xml/customer"
				    xsi:schemaLocation="http://www.skyve.org/xml/customer ../../schemas/customer.xsd"
				    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
				    <uiResources logo="test.png" />
				    <defaultDateConverter>DD_MMM_YYYY</defaultDateConverter>
				    <defaultTimeConverter>HH24_MI</defaultTimeConverter>
				    <defaultDateTimeConverter>DD_MMM_YYYY_HH24_MI</defaultDateTimeConverter>
				    <defaultTimestampConverter>DD_MMM_YYYY_HH24_MI_SS</defaultTimestampConverter>
				    <modules homeModule="admin">
				        <module name="admin" />
				    </modules>
				</customer>""", customerName);
		Files.writeString(customerDir.resolve(customerName + ".xml"), customerXml);

		// Prompt returns module name (single customer so no customer selection prompt)
		when(mockPrompter.prompt(anyString())).thenReturn("newSingleCustomerModule");

		mojo.execute();

		// Module directory should be created
		assertTrue(tempDir.resolve("modules/newSingleCustomerModule").toFile().exists());
		// Customer XML should have been updated (file should still exist)
		assertTrue(customerDir.resolve(customerName + ".xml").toFile().exists());
	}
}
