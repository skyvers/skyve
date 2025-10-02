package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for ClearBeforeAssembleMojo parameters.
 * 
 * <p>This test verifies parameter configuration by parsing the ClearBeforeAssembleMojo.java source file
 * to verify @Parameter annotations. Since the plugin descriptor doesn't include property attributes
 * for this mojo, source parsing is the primary and only verification method used.
 */
class ClearBeforeAssembleMojoTest {

	@Test
	@SuppressWarnings("static-method")
    void testClearBeforeAssembleMojoParameterPropertyConfiguration() throws Exception {
        // Since plugin descriptor doesn't include property attributes, always use source parsing
        String source = loadMojoSource();
        assertTrue(source.contains("class ClearBeforeAssembleMojo"), "Mojo source should be readable");

        // Verify customer annotation has property="customer" and required=true
        Pattern customerPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*property\\s*=\\s*\\\"customer\\\"|property\\s*=\\s*\\\"customer\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
        assertTrue(customerPattern.matcher(source).find(), "customer should be @Parameter(required=true, property=\"customer\")");
    }


    private static String loadMojoSource() throws Exception {
        // Find source relative to test class location
        String testClassPath = ClearBeforeAssembleMojoTest.class.getProtectionDomain()
            .getCodeSource().getLocation().getPath();
        Path testDir = Path.of(testClassPath);
        
        // Navigate from target/test-classes back to src/main/java
        Path source = testDir.resolve("../../src/main/java/org/skyve/toolchain/ClearBeforeAssembleMojo.java")
            .normalize();
        
        // If not found, try alternative paths
        if (!Files.exists(source)) {
            source = testDir.resolve("../../../src/main/java/org/skyve/toolchain/ClearBeforeAssembleMojo.java")
                .normalize();
        }
        
        // Final fallback: search from current working directory
        if (!Files.exists(source)) {
            Path currentDir = Path.of(System.getProperty("user.dir"));
            source = currentDir.resolve("src/main/java/org/skyve/toolchain/ClearBeforeAssembleMojo.java");
            
            while (!Files.exists(source) && currentDir.getParent() != null) {
                currentDir = currentDir.getParent();
                source = currentDir.resolve("src/main/java/org/skyve/toolchain/ClearBeforeAssembleMojo.java");
            }
        }
        
        assertTrue(Files.exists(source), "Could not locate ClearBeforeAssembleMojo.java for source verification");
        return Files.readString(source);
    }
}
