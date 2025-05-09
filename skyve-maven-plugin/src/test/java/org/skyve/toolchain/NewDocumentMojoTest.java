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

class NewDocumentMojoTest {
    
    @TempDir
    Path tempDir;
    
    private NewDocumentMojo mojo;
    private String moduleName = "testModule";
    private String documentName = "TestDocument";
    
    @BeforeEach
    void setUp() throws Exception {
        mojo = new NewDocumentMojo();
        
        // Set private fields using reflection
        ReflectionTestUtils.setField(mojo, "moduleName", moduleName);
        ReflectionTestUtils.setField(mojo, "documentName", documentName);
        
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
     * Tests the creation of a new document.
     * 
     * Preconditions:
     * - mojo is initialized with moduleName and documentName
     * - Module directory and metadata exist
     * - srcDir is set to a temporary directory
     * 
     * Postconditions:
     * - Document directory is created
     * - Document metadata file is created with correct content
     * - Module metadata is updated to include the new document
     */
    @Test
    void testExecute() throws Exception {
        // Execute the mojo
        mojo.execute();
        
        // Verify document directory was created
        Path documentDir = tempDir.resolve("modules")
                                .resolve(moduleName)
                                .resolve(documentName);
        assertTrue(Files.exists(documentDir), "Document directory should exist");
        
        // Verify document metadata file was created
        Path documentXml = documentDir.resolve(documentName + ".xml");
        assertTrue(Files.exists(documentXml), "Document metadata file should exist");
        
        // Read and verify document metadata content
        String content = Files.readString(documentXml);
        assertTrue(content.contains("name=\"" + documentName + "\""), 
                  "Document metadata should have correct name");
        assertTrue(content.contains("<singularAlias>" + documentName + "</singularAlias>"), 
                  "Document metadata should have correct singular alias");
        assertTrue(content.contains("<pluralAlias>" + documentName.toLowerCase() + "s</pluralAlias>"), 
                  "Document metadata should have correct plural alias");
        
        // Verify module metadata was updated
        Path moduleXml = tempDir.resolve("modules")
                              .resolve(moduleName)
                              .resolve(moduleName + ".xml");
        String moduleContent = Files.readString(moduleXml);
        assertTrue(moduleContent.contains("ref=\"" + documentName + "\""), 
                  "Module metadata should include the new document");
    }
    
    /**
     * Tests the generation of a persistent name for a document.
     * 
     * Preconditions:
     * - mojo is initialized with moduleName and documentName
     * 
     * Postconditions:
     * - Generated persistent name follows the correct format (first 3 chars of module + _ + document name)
     */
	@Test
	@SuppressWarnings("static-method")
    void testGeneratePersistentName() throws Exception {
        // Get the private method using reflection
        java.lang.reflect.Method generatePersistentName = NewDocumentMojo.class.getDeclaredMethod("generatePersistentName", 
                                                                                                String.class, 
                                                                                                String.class);
        generatePersistentName.setAccessible(true);
        
        // Test with a module name longer than 3 characters
        String result = (String) generatePersistentName.invoke(null, "LongModule", "TestDoc");
        assertEquals("LON_TestDoc", result, "Persistent name should be correctly formatted");
        
        // Test with a module name exactly 3 characters
        result = (String) generatePersistentName.invoke(null, "Abc", "TestDoc");
        assertEquals("ABC_TestDoc", result, "Persistent name should be correctly formatted for 3-character module name");
        
        // Test with a module name shorter than 3 characters
        result = (String) generatePersistentName.invoke(null, "Ab", "TestDoc");
        assertNull(result, "Persistent name should be null for module name shorter than 3 characters");
        
        // Test with null inputs
        result = (String) generatePersistentName.invoke(null, null, null);
        assertNull(result, "Persistent name should be null for null inputs");
    }
} 