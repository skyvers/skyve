package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.codehaus.plexus.components.interactivity.Prompter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

class NewServiceMojoTest {
    
    @TempDir
    Path tempDir;
    
    private NewServiceMojo mojo;
    private String moduleName = "testModule";
    private String documentName = "TestDocument";
    
    @BeforeEach
    void setUp() throws Exception {
        mojo = new NewServiceMojo();
        
        // Set private fields using reflection
        ReflectionTestUtils.setField(mojo, "moduleName", moduleName);
        ReflectionTestUtils.setField(mojo, "documentName", documentName);
        ReflectionTestUtils.setField(mojo, "srcDir", tempDir.toString());
        
        // Create module directory structure
        Path moduleDir = tempDir.resolve("modules").resolve(moduleName);
        Files.createDirectories(moduleDir);
        
        // Create schemas directory and copy schema files
        Path schemasDir = tempDir.resolve("schemas");
        Files.createDirectories(schemasDir);
        
        // Copy schema files from skyve-war
        Path sourceSchemasDir = Paths.get(System.getProperty("user.dir"))
                                   .getParent()  // Go up to workspace root
                                   .resolve("skyve-war")
                                   .resolve("src")
                                   .resolve("main")
                                   .resolve("java")
                                   .resolve("schemas");
        
        if (Files.exists(sourceSchemasDir)) {
            try (var files = Files.walk(sourceSchemasDir)) {
                files.filter(Files::isRegularFile)
                    .forEach(sourceFile -> {
                        try {
                            Path targetFile = schemasDir.resolve(sourceFile.getFileName());
                            Files.copy(sourceFile, targetFile);
                        } catch (Exception e) {
                            throw new RuntimeException("Failed to copy schema file: " + sourceFile, e);
                        }
                    });
            } catch (IOException e) {
                throw new RuntimeException("Failed to walk schema directory: " + sourceSchemasDir, e);
            }
        } else {
            System.out.println("Source schemas directory does not exist!");
        }
        
        // Create module metadata file
        String moduleXml = String.format("""
            <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
            <module xmlns="http://www.skyve.org/xml/module" name="%s" title="%s">
                <documentation>Test module documentation</documentation>
                <homeDocument>%s</homeDocument>
                <documents>
                    <document ref="%s"/>
                </documents>
                <menu/>
            </module>""", moduleName, moduleName, documentName, documentName);
        Files.write(moduleDir.resolve(moduleName + ".xml"), moduleXml.getBytes());
        
        // Set project and source directory
        org.apache.maven.project.MavenProject project = new org.apache.maven.project.MavenProject();
        project.addCompileSourceRoot(tempDir.toString());
        ReflectionTestUtils.setField(mojo, "project", project);
        
        // Set up NewDocumentConfig to avoid prompter for module name
        org.skyve.toolchain.config.NewDocumentConfig config = new org.skyve.toolchain.config.NewDocumentConfig();
        config.setDefaultModule(moduleName);
        ReflectionTestUtils.setField(mojo, "newDocumentConfig", config);
        
        // Set up mock prompter for document name
        Prompter mockPrompter = mock(Prompter.class);
        when(mockPrompter.prompt(anyString())).thenReturn(documentName);
        ReflectionTestUtils.setField(mojo, "prompter", mockPrompter);
    }
    
    /**
     * Tests the standalone execution of the NewServiceMojo.
     * 
     * Preconditions:
     * - mojo is initialized with moduleName and documentName
     * - Module directory and metadata exist
     * - srcDir is set to a temporary directory
     * 
     * Postconditions:
     * - Document directory is created (inherited from NewDocumentMojo)
     * - Document metadata file is created
     * - Service class file is created in the correct location
     * - Service class has correct structure and content
     */
    @Test
    void testExecute() throws Exception {
        // Execute the mojo
        mojo.execute();
        
        // Verify document directory was created (inherited functionality)
        Path documentDir = tempDir.resolve("modules")
                                .resolve(moduleName)
                                .resolve(documentName);
        assertTrue(Files.exists(documentDir), "Document directory should exist");
        
        // Verify document metadata file was created (inherited functionality)
        Path documentXml = documentDir.resolve(documentName + ".xml");
        assertTrue(Files.exists(documentXml), "Document metadata file should exist");
        
        // Verify service class file was created
        Path serviceFile = Paths.get(tempDir.toString(),
                                   "modules",
                                   moduleName,
                                   documentName,
                                   documentName + "Service.java");
        assertTrue(Files.exists(serviceFile), "Service class file should exist");
        
        // Read and verify service class content
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
        assertTrue(content.contains("@Inject private transient " + documentName + "Service service;"),
                  "Class javadoc should include injection example");
    }
    
    /**
     * Tests the createServiceClass method directly.
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
        assertTrue(content.contains("@Inject private transient " + documentName + "Service service;"),
                  "Class javadoc should include injection example");
    }
    
    /**
     * Tests the getExtensionName method.
     * 
     * Preconditions:
     * - mojo is initialized with documentName
     * 
     * Postconditions:
     * - Returns the correct extension name (documentName + "Extension")
     */
    @Test
    void testGetExtensionName() throws Exception {
        // Get the private method using reflection
        java.lang.reflect.Method getExtensionName = NewServiceMojo.class.getDeclaredMethod("getExtensionName");
        getExtensionName.setAccessible(true);
        
        // Test the method
        String result = (String) getExtensionName.invoke(mojo);
        assertEquals(documentName + "Extension", result, "Extension name should be correctly formatted");
        
        // Test with a different document name
        ReflectionTestUtils.setField(mojo, "documentName", "AnotherDocument");
        result = (String) getExtensionName.invoke(mojo);
        assertEquals("AnotherDocumentExtension", result, "Extension name should be correctly formatted for different document name");
    }
    
    /**
     * Tests error handling when service class creation fails.
     * 
     * Preconditions:
     * - mojo is initialized with invalid srcDir
     * 
     * Postconditions:
     * - Method should not throw exception but log a warning
     */
    @Test
    void testCreateServiceClassWithInvalidDirectory() throws Exception {
        // Set an invalid srcDir that doesn't exist
        ReflectionTestUtils.setField(mojo, "srcDir", "/nonexistent/directory");
        
        // This should not throw an exception but should log a warning
        mojo.createServiceClass();
        
        // The method should complete without throwing an exception
        // The actual file creation would fail, but the method handles the IOException gracefully
    }
    
    /**
     * Tests that the NewServiceMojo can be used by other Mojos.
     * This simulates how NewScaffoldedDocumentMojo uses NewServiceMojo.
     * 
     * Preconditions:
     * - NewServiceMojo is properly initialized
     * 
     * Postconditions:
     * - NewServiceMojo can be instantiated and used by other classes
     * - Service class creation works when called from another Mojo
     */
    @Test
    void testNewServiceMojoCanBeUsedByOtherMojos() throws Exception {
        // Create a new instance (simulating how NewScaffoldedDocumentMojo would use it)
        NewServiceMojo serviceMojo = new NewServiceMojo();
        
        // Set the required fields (simulating how NewScaffoldedDocumentMojo sets them)
        ReflectionTestUtils.setField(serviceMojo, "moduleName", moduleName);
        ReflectionTestUtils.setField(serviceMojo, "documentName", documentName);
        ReflectionTestUtils.setField(serviceMojo, "srcDir", tempDir.toString());
        
        // Execute the service class creation
        serviceMojo.createServiceClass();
        
        // Verify the service class file was created
        Path serviceFile = Paths.get(tempDir.toString(),
                                   "modules",
                                   moduleName,
                                   documentName,
                                   documentName + "Service.java");
        assertTrue(Files.exists(serviceFile), "Service class file should exist when called from another Mojo");
    }
    
    private static String normalizeWhitespace(String input) {
        return input.replaceAll("\\s+", "").trim();
    }
} 