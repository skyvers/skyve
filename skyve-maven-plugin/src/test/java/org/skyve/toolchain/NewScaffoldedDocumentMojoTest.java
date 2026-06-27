package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.generate.DialectOptions;
import org.skyve.toolchain.config.GenerateDomainConfig;
import org.skyve.toolchain.config.GenerateEditViewConfig;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("static-method")
class NewScaffoldedDocumentMojoTest {
    
    @TempDir
    Path tempDir;
    
    private NewScaffoldedDocumentMojo mojo;
    private String moduleName = "testModule";
    private String documentName = "TestDocument";
    
    @BeforeEach
	void setUp() {
        mojo = new NewScaffoldedDocumentMojo();
        
        // Set private fields using reflection
        ReflectionTestUtils.setField(mojo, "moduleName", moduleName);
        ReflectionTestUtils.setField(mojo, "documentName", documentName);
        ReflectionTestUtils.setField(mojo, "srcDir", tempDir.toString());
    }
    
    /**
	 * Tests the creation of a Bizlet class for a document.
	 * 
	 * Preconditions:
	 * - mojo is initialized with moduleName and documentName
	 * - srcDir is set to a temporary directory
	 * 
	 * Postconditions:
	 * - A Bizlet class file is created in the correct location
	 * - The class has the correct name and is public
	 * - The class extends Bizlet with the correct type parameter
	 */
    @Test
	void testCreateBizletClass() throws Exception {
        // Execute the method directly
		mojo.createBizletClass();
        
		// Verify the bizlet class file was created
		Path bizletFile = Paths.get(tempDir.toString(),
				"modules",
				moduleName,
				documentName,
				documentName + "Bizlet.java");
		assertTrue(Files.exists(bizletFile), "Bizlet class file should exist");
        
        // Read the file content
		String content = Files.readString(bizletFile);
        
        // Verify the class name
		assertTrue(content.contains("public class " + documentName + "Bizlet"),
                  "Class should have correct name");
        
		// Verify the superclass
		String expectedSuperclass = "Bizlet<" + documentName + "Extension>";
		assertTrue(normalizeWhitespace(content).contains(normalizeWhitespace(expectedSuperclass)),
				"Class should extend Bizlet with correct type parameter");
    }
    
    /**
     * Tests the creation of a factory class for a document.
     * 
     * Preconditions:
     * - mojo is initialized with moduleName and documentName
     * - srcDir is set to a temporary directory
     * 
     * Postconditions:
     * - A factory class file is created in the correct location
     * - The class has the correct name and is public
     * - The class has @SkyveFactory annotation
     * - The class has a crudInstance() method with @SkyveFixture annotation
     * - The crudInstance() method returns the correct type and has the correct implementation
     */
    @Test
    void testCreateFactoryClass() throws Exception {
        // Execute the method directly
        mojo.createFactoryClass();
        
        // Verify the factory class file was created
        Path factoryFile = Paths.get(tempDir.toString(), 
                                   "modules", 
                                   moduleName, 
                                   documentName, 
                                   documentName + "Factory.java");
        assertTrue(Files.exists(factoryFile), "Factory class file should exist");
        
        // Read the file content
        String content = Files.readString(factoryFile);
        
        // Verify the class name
        assertTrue(content.contains("public class " + documentName + "Factory"), 
                  "Class should have correct name");
        
        // Verify the @SkyveFactory annotation
        assertTrue(content.contains("@SkyveFactory"), 
                  "Class should have @SkyveFactory annotation");
        
        // Verify the crudInstance method and its annotation
		assertTrue(normalizeWhitespace(content).contains(normalizeWhitespace("@SkyveFixture(types = SkyveFixture.FixtureType.crud)")), 
                  "crudInstance method should have correct annotation");
        assertTrue(content.contains("public " + documentName + "Extension crudInstance()"), 
                  "Class should have crudInstance method");
        
        // Verify the method implementation
		assertTrue(content.contains("return new DataBuilder().fixture(SkyveFixture.FixtureType.crud)"),
                  "crudInstance method should have correct implementation");
    }
    
    /**
	 * Tests the creation of an extension class for a document.
	 * 
	 * Preconditions:
	 * - mojo is initialised with moduleName and documentName
	 * - srcDir is set to a temporary directory
	 * 
	 * Postconditions:
	 * - An extension class file is created in the correct location
	 * - The class has the correct name and is public
	 * - The class extends the base document class
	 */
	@Test
	void testCreateExtensionClass() throws Exception {
		// Execute the method directly
		mojo.createExtensionClass();

		// Verify the extension class file was created
		Path extensionFile = Paths.get(tempDir.toString(),
				"modules",
				moduleName,
				documentName,
				documentName + "Extension.java");
		assertTrue(Files.exists(extensionFile), "Extension class file should exist");

		// Read the file content
		String content = Files.readString(extensionFile);

		// Verify the class name
		assertTrue(content.contains("public class " + documentName + "Extension"),
				"Class should have correct name");

		// Verify the superclass
		String expectedSuperclass = "extends " + documentName;
		assertTrue(normalizeWhitespace(content).contains(normalizeWhitespace(expectedSuperclass)),
				"Class should extend the base document class");
	}

	/**
	 * Tests the creation of a service class for a document.
	 * 
	 * Preconditions:
	 * - mojo is initialized with moduleName and documentName
	 * - srcDir is set to a temporary directory
	 * 
	 * Postconditions:
	 * - A service class file is created in the correct location
	 * - The class has the correct name and is public
	 * - The class has @Default annotation
	 * - The class has an injected Persistence field
	 * - The class has get() and getAll() methods with correct signatures and implementations
	 */
    @Test
	void testCreateServiceClass() throws Exception {
        // Execute the method directly
		mojo.createServiceClass();
        
		// Verify the service class file was created
		Path serviceFile = Paths.get(tempDir.toString(),
				"modules",
				moduleName,
				documentName,
				documentName + "Service.java");
		assertTrue(Files.exists(serviceFile), "Service class file should exist");
        
        // Read the file content
		String content = Files.readString(serviceFile);
        
        // Verify the class name
		assertTrue(content.contains("public class " + documentName + "Service"),
                  "Class should have correct name");
        
		// Verify the @Default annotation
		assertTrue(content.contains("@Default"),
				"Class should have @Default annotation");

		// Verify the Persistence injection
		assertTrue(content.contains("@Inject"),
				"Class should have @Inject annotation");
		assertTrue(content.contains("private Persistence persistence"),
				"Class should have Persistence field");

		// Verify the get method and its javadoc
		String expectedGetMethodJavadoc = String.format("""
				/**
				 * Return the %s with the specified bizId.
				 *
				 * @param bizId The bizId of the %s to retrieve
				 * @return The %s, or null if one does not exist with the specified bizId
				 */""", documentName, documentName, documentName);
		assertTrue(normalizeWhitespace(content).contains(normalizeWhitespace(expectedGetMethodJavadoc)),
				"get method should have correct javadoc");
		assertTrue(content.contains("public " + documentName + "Extension get(String bizId)"),
				"Class should have get method");

		// Verify the getAll method and its javadoc
		String expectedGetAllMethodJavadoc = String.format("""
				/**
				 * Retrieves all %ss in the datastore.
				 *
				 * @return All %ss
				 */""", documentName, documentName);
		assertTrue(normalizeWhitespace(content).contains(normalizeWhitespace(expectedGetAllMethodJavadoc)),
				"getAll method should have correct javadoc");
		assertTrue(content.contains("public List<" + documentName + "Extension> getAll()"),
				"Class should have getAll method");

		// Verify the class javadoc
		assertTrue(content.contains("This class acts as a service layer to encapsulate domain logic"),
				"Class should have correct javadoc");
    }
    
	@Test
	void generateDomainThrowsWhenConfigIsNull() {
		org.apache.maven.plugin.MojoExecutionException ex = org.junit.jupiter.api.Assertions.assertThrows(
				org.apache.maven.plugin.MojoExecutionException.class,
				() -> mojo.generateDomain());
		org.junit.jupiter.api.Assertions.assertTrue(ex.getMessage().contains("Generate domain configuration not specified"));
	}

	@Test
	void generateDomainWrapsClasspathFailureWhenConfigIsPresent() {
		GenerateDomainConfig config = new GenerateDomainConfig();
		ReflectionTestUtils.setField(mojo, "generateDomainConfig", config);

		org.apache.maven.plugin.MojoExecutionException ex = org.junit.jupiter.api.Assertions.assertThrows(
				org.apache.maven.plugin.MojoExecutionException.class,
				() -> mojo.generateDomain());

		org.junit.jupiter.api.Assertions.assertTrue(ex.getMessage().contains("Failed to generate domain"));
	}

	@Test
	void generateEditViewWrapsClasspathFailureWhenConfigIsPresent() {
		GenerateEditViewConfig config = new GenerateEditViewConfig();
		config.setCustomer("demo");
		config.setCustomerOverriden(true);
		config.setOverridenViewName("edit");
		ReflectionTestUtils.setField(mojo, "generateEditViewConfig", config);

		org.apache.maven.plugin.MojoExecutionException ex = org.junit.jupiter.api.Assertions.assertThrows(
				org.apache.maven.plugin.MojoExecutionException.class,
				() -> mojo.generateEditView());

		org.junit.jupiter.api.Assertions.assertTrue(ex.getMessage().contains("Failed to generate edit view"));
	}

	@Test
	void executeCreatesDocumentThenRunsScaffoldSteps() throws Exception {
		RecordingNewScaffoldedDocumentMojo recordingMojo = new RecordingNewScaffoldedDocumentMojo();
		Path src = Files.createDirectories(tempDir.resolve("src/main/java"));
		Files.createDirectories(src.resolve("modules"));
		ReflectionTestUtils.setField(recordingMojo, "project", projectWithSourceRoot(src));
		ReflectionTestUtils.setField(recordingMojo, "srcDir", src.toString());
		ReflectionTestUtils.setField(recordingMojo, "generatedDir", tempDir.resolve("generated").toString());
		ReflectionTestUtils.setField(recordingMojo, "testDir", tempDir.resolve("test").toString());
		ReflectionTestUtils.setField(recordingMojo, "generatedTestDir", tempDir.resolve("generatedTest").toString());
		ReflectionTestUtils.setField(recordingMojo, "generateDomainConfig", new GenerateDomainConfig());
		GenerateEditViewConfig editViewConfig = new GenerateEditViewConfig();
		editViewConfig.setCustomer("demo");
		ReflectionTestUtils.setField(recordingMojo, "generateEditViewConfig", editViewConfig);

		recordingMojo.execute();

		org.junit.jupiter.api.Assertions.assertEquals(List.of("document", "extension", "bizlet", "factory", "service", "domain", "editView"),
				recordingMojo.steps);
		org.junit.jupiter.api.Assertions.assertEquals(moduleName, recordingMojo.moduleName);
		org.junit.jupiter.api.Assertions.assertEquals(documentName, recordingMojo.documentName);
	}

	@Test
	void generateDomainUsesConfiguredGeneratorValues() throws Exception {
		RecordingNewScaffoldedDocumentMojo recordingMojo = new RecordingNewScaffoldedDocumentMojo();
		GenerateDomainConfig config = new GenerateDomainConfig();
		config.setDebug(true);
		config.setMultiTenant(true);
		config.setDialect("MYSQL_8");
		config.setExcludedModules("admin,jobs");
		config.setCustomisationsClass("com.example.Customisations");
		ReflectionTestUtils.setField(recordingMojo, "generateDomainConfig", config);
		ReflectionTestUtils.setField(recordingMojo, "srcDir", "src");
		ReflectionTestUtils.setField(recordingMojo, "generatedDir", "generated");
		ReflectionTestUtils.setField(recordingMojo, "testDir", "test");
		ReflectionTestUtils.setField(recordingMojo, "generatedTestDir", "generatedTest");

		recordingMojo.generateDomain();

		assertTrue(recordingMojo.classpathConfigured);
		org.junit.jupiter.api.Assertions.assertEquals("src", recordingMojo.classpathSourceDirectory);
		org.junit.jupiter.api.Assertions.assertEquals("com.example.Customisations", recordingMojo.customisationsClassName);
		assertTrue(recordingMojo.repositorySet);
		assertTrue(recordingMojo.generateDomainClasses);
		assertTrue(recordingMojo.debug);
		assertTrue(recordingMojo.multiTenant);
		org.junit.jupiter.api.Assertions.assertEquals(DialectOptions.MYSQL_8, recordingMojo.dialect);
		org.junit.jupiter.api.Assertions.assertArrayEquals(new String[] {"admin", "jobs"}, recordingMojo.excludedModules);
	}

	@Test
	void generateEditViewUsesConfigAndMovesGeneratedView() throws Exception {
		RecordingNewScaffoldedDocumentMojo recordingMojo = new RecordingNewScaffoldedDocumentMojo();
		Path src = Files.createDirectories(tempDir.resolve("src/main/java"));
		Path modules = Files.createDirectories(src.resolve("modules"));
		GenerateEditViewConfig config = new GenerateEditViewConfig();
		config.setCustomer("demo");
		config.setCustomerOverriden(true);
		config.setOverridenViewName("edit");
		ReflectionTestUtils.setField(recordingMojo, "generateEditViewConfig", config);
		ReflectionTestUtils.setField(recordingMojo, "srcDir", src.toString());
		ReflectionTestUtils.setField(recordingMojo, "project", projectWithSourceRoot(src));
		recordingMojo.moduleName = moduleName;
		recordingMojo.documentName = documentName;

		recordingMojo.generateEditView();

		assertTrue(recordingMojo.classpathConfigured);
		org.junit.jupiter.api.Assertions.assertEquals(src.toString(), recordingMojo.classpathSourceDirectory);
		org.junit.jupiter.api.Assertions.assertArrayEquals(
				new String[] {src.toString(), "demo", moduleName, documentName, "true", "edit"},
				recordingMojo.editViewArguments);
		org.junit.jupiter.api.Assertions.assertEquals(
				modules.resolve(moduleName).resolve(documentName).resolve("views").resolve("generatedEdit.xml"),
				recordingMojo.moveSource);
		org.junit.jupiter.api.Assertions.assertEquals(
				modules.resolve(moduleName).resolve(documentName).resolve("views").resolve("edit.xml"),
				recordingMojo.moveDestination);
	}

    private static String normalizeWhitespace(String input) {
        return input.replaceAll("\\s+", "").trim();
    }

	private MavenProject projectWithSourceRoot(Path sourceRoot) {
		MavenProject project = new MavenProject();
		project.setName("test-project");
		project.addCompileSourceRoot(sourceRoot.toString());
		ReflectionTestUtils.setField(project, "basedir", tempDir.toFile());
		return project;
	}

	private static final class RecordingNewScaffoldedDocumentMojo extends NewScaffoldedDocumentMojo {
		private final List<String> steps = new ArrayList<>();
		private boolean classpathConfigured;
		private String classpathSourceDirectory;
		private String customisationsClassName;
		private boolean repositorySet;
		private boolean generateDomainClasses;
		private boolean debug;
		private boolean multiTenant;
		private DialectOptions dialect;
		private String[] excludedModules;
		private String[] editViewArguments;
		private Path moveSource;
		private Path moveDestination;

		@Override
		void createDocument() {
			steps.add("document");
			moduleName = "testModule";
			documentName = "TestDocument";
		}

		@Override
		void createExtensionClass() {
			steps.add("extension");
		}

		@Override
		void createBizletClass() {
			steps.add("bizlet");
		}

		@Override
		void createFactoryClass() {
			steps.add("factory");
		}

		@Override
		void createServiceClass() {
			steps.add("service");
		}

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
		void setRepository() {
			repositorySet = true;
		}

		@Override
		void generateDomain(boolean generateDomainClassesValue,
								boolean debugValue,
								boolean multiTenantValue,
								DialectOptions dialectValue,
								String sourceDirectory,
								String generatedDirectory,
								String testDirectory,
								String generatedTestDirectory,
								String[] excludedModulesValue) {
			steps.add("domain");
			this.generateDomainClasses = generateDomainClassesValue;
			this.debug = debugValue;
			this.multiTenant = multiTenantValue;
			this.dialect = dialectValue;
			this.excludedModules = excludedModulesValue;
		}

		@Override
		void generateEditView(String[] arguments) {
			steps.add("editView");
			editViewArguments = arguments;
		}

		@Override
		void move(Path source, Path destination) {
			moveSource = source;
			moveDestination = destination;
		}
	}
} 
