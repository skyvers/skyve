package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for AssembleMojo parameters.
 * 
 * <p>This test uses multiple approaches to verify parameter configuration:
 * <ul>
 *   <li>Primary: Parses the generated plugin descriptor (plugin.xml) if available</li>
 *   <li>Fallback: Parses the AssembleMojo.java source file to verify @Parameter annotations</li>
 * </ul>
 * 
 * <p>The fallback approach is used when the plugin descriptor is not generated during the build,
 * ensuring tests remain reliable across different build configurations.
 */
class AssembleMojoTest {

	@Test
	@SuppressWarnings("static-method")
    void testAssembleMojoParameterPropertyConfiguration() throws Exception {
        // Since plugin descriptor doesn't include property attributes, always use source parsing
        String source = loadMojoSource();
        assertTrue(source.contains("class AssembleMojo"), "Mojo source should be readable");

        // Verify skyveDir annotation has property="skyveDir" and required=true
        Pattern skyveDirPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*property\\s*=\\s*\\\"skyveDir\\\"|property\\s*=\\s*\\\"skyveDir\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
        assertTrue(skyveDirPattern.matcher(source).find(), "skyveDir should be @Parameter(required=true, property=\"skyveDir\")");

        // Verify customer annotation has property="customer" and required=true
        Pattern customerPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*property\\s*=\\s*\\\"customer\\\"|property\\s*=\\s*\\\"customer\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
        assertTrue(customerPattern.matcher(source).find(), "customer should be @Parameter(required=true, property=\"customer\")");

        // Verify templateDir annotation has property="templateDir"
        Pattern templateDirPattern = Pattern.compile("@Parameter\\s*\\(\\s*property\\s*=\\s*\\\"templateDir\\\"[\\s,]*\\)");
        assertTrue(templateDirPattern.matcher(source).find(), "templateDir should be @Parameter(property=\"templateDir\")");
    }


    private static String loadMojoSource() throws Exception {
        // Find source relative to test class location
        String testClassPath = AssembleMojoTest.class.getProtectionDomain()
            .getCodeSource().getLocation().getPath();
        Path testDir = Path.of(testClassPath);
        
        // Navigate from target/test-classes back to src/main/java
        Path source = testDir.resolve("../../src/main/java/org/skyve/toolchain/AssembleMojo.java")
            .normalize();
        
        // If not found, try alternative paths
        if (!Files.exists(source)) {
            source = testDir.resolve("../../../src/main/java/org/skyve/toolchain/AssembleMojo.java")
                .normalize();
        }
        
        // Final fallback: search from current working directory
        if (!Files.exists(source)) {
            Path currentDir = Path.of(System.getProperty("user.dir"));
            source = currentDir.resolve("src/main/java/org/skyve/toolchain/AssembleMojo.java");
            
            while (!Files.exists(source) && currentDir.getParent() != null) {
                currentDir = currentDir.getParent();
                source = currentDir.resolve("src/main/java/org/skyve/toolchain/AssembleMojo.java");
            }
        }
        
        assertTrue(Files.exists(source), "Could not locate AssembleMojo.java for source verification");
        return Files.readString(source);
    }
}
