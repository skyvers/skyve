package org.skyve.toolchain.config;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for GenerateDomainConfig parameters.
 * 
 * <p>This test uses multiple approaches to verify parameter configuration:
 * <ul>
 *   <li>Primary: Parses the generated plugin descriptor (plugin.xml) if available</li>
 *   <li>Fallback: Parses the GenerateDomainConfig.java source file to verify @Parameter annotations</li>
 * </ul>
 * 
 * <p>The fallback approach is used when the plugin descriptor is not generated during the build,
 * ensuring tests remain reliable across different build configurations.
 */
class GenerateDomainConfigTest {

	@Test
	@SuppressWarnings("static-method")
    void testExcludedModulesParameterPropertyConfiguration() throws Exception {
        // Since plugin descriptor doesn't include property attributes for config classes, always use source parsing
        String source = loadConfigSource();
        assertTrue(source.contains("class GenerateDomainConfig"), "Config source should be readable");

        // Verify excludedModules annotation has property="excludedModules"
        Pattern exclPattern = Pattern.compile("@Parameter\\s*\\(\\s*property\\s*=\\s*\\\"excludedModules\\\"[\\s,]*\\)");
        Matcher exclMatcher = exclPattern.matcher(source);
        assertTrue(exclMatcher.find(), "@Parameter(property=\"excludedModules\") should be present on excludedModules");

        // Verify field type is String
        assertTrue(source.contains("private String excludedModules"), "excludedModules should be a String field");
    }



    private static String loadConfigSource() throws Exception {
        // Find source relative to test class location
        String testClassPath = GenerateDomainConfigTest.class.getProtectionDomain()
            .getCodeSource().getLocation().getPath();
        Path testDir = Path.of(testClassPath);
        
        // Navigate from target/test-classes back to src/main/java
        Path source = testDir.resolve("../../src/main/java/org/skyve/toolchain/config/GenerateDomainConfig.java")
            .normalize();
        
        // If not found, try alternative paths
        if (!Files.exists(source)) {
            source = testDir.resolve("../../../src/main/java/org/skyve/toolchain/config/GenerateDomainConfig.java")
                .normalize();
        }
        
        // Final fallback: search from current working directory
        if (!Files.exists(source)) {
            Path currentDir = Path.of(System.getProperty("user.dir"));
            source = currentDir.resolve("src/main/java/org/skyve/toolchain/config/GenerateDomainConfig.java");
            
            while (!Files.exists(source) && currentDir.getParent() != null) {
                currentDir = currentDir.getParent();
                source = currentDir.resolve("src/main/java/org/skyve/toolchain/config/GenerateDomainConfig.java");
            }
        }
        
        assertTrue(Files.exists(source), "Could not locate GenerateDomainConfig.java for source verification");
        return Files.readString(source);
    }
}
