package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

class NewScaffoldedDocumentMojoTest {
    
    @TempDir
    Path tempDir;
    
    private NewScaffoldedDocumentMojo mojo;
    private String moduleName = "testModule";
    private String documentName = "TestDocument";
    
    @BeforeEach
    void setUp() throws Exception {
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
	 * - mojo is initialized with moduleName and documentName
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
    
    private static String normalizeWhitespace(String input) {
        return input.replaceAll("\\s+", "").trim();
    }
} 