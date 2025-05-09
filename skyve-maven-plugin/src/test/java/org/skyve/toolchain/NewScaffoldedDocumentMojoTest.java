package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
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
    
    @Test
    void testCreateServiceClass() throws Exception {
        // Get the private method using reflection
		Method createServiceClass = NewScaffoldedDocumentMojo.class.getDeclaredMethod("createServiceClass");
		createServiceClass.setAccessible(true);
        
        // Execute the method
		createServiceClass.invoke(mojo);
        
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
        return input.replaceAll("\\s+", " ").trim();
    }
} 