package org.skyve.toolchain.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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
        Document plugin = loadPluginDescriptor();
        if (plugin != null) {
            // Look for generateDomain mojo and its config parameters
            Node mojo = selectMojo(plugin, "generateDomain");
            assertNotNull(mojo, "generateDomain mojo should exist in plugin.xml");

            // Check if there's a parameter for excludedModules in the config
            NodeList params = (NodeList) XPathFactory.newInstance().newXPath()
                .evaluate("parameters/parameter", mojo, XPathConstants.NODESET);
            
            boolean foundExcludedModules = false;
            for (int i = 0; i < params.getLength(); i++) {
                Node param = params.item(i);
                String name = getChildText(param, "name");
                if ("excludedModules".equals(name)) {
                    String property = getChildText(param, "property");
                    assertEquals("excludedModules", property, "excludedModules should have property='excludedModules'");
                    foundExcludedModules = true;
                    break;
                }
            }
            assertTrue(foundExcludedModules, "excludedModules parameter should be present in plugin descriptor");
        } else {
            // Fallback to source parsing when plugin.xml is not generated
            String source = loadConfigSource();
            assertTrue(source.contains("class GenerateDomainConfig"), "Config source should be readable");

            // Verify excludedModules annotation has property="excludedModules"
            Pattern exclPattern = Pattern.compile("@Parameter\\s*\\(\\s*property\\s*=\\s*\\\"excludedModules\\\"[\\s,]*\\)");
            Matcher exclMatcher = exclPattern.matcher(source);
            assertTrue(exclMatcher.find(), "@Parameter(property=\"excludedModules\") should be present on excludedModules");

            // Verify field type is String
            assertTrue(source.contains("private String excludedModules"), "excludedModules should be a String field");
        }
    }

    private static Document loadPluginDescriptor() throws Exception {
        // Prefer the assembled/classes location used at runtime
        Path primary = Path.of("target", "classes", "META-INF", "maven", "plugin.xml");
        Path fallback = Path.of("target", "plugin-descriptor", "plugin.xml");

        Path path = Files.exists(primary) ? primary : (Files.exists(fallback) ? fallback : null);
        if (path == null) {
            return null;
        }

        try (var is = Files.newInputStream(path)) {
            var db = DocumentBuilderFactory.newInstance();
            db.setNamespaceAware(false);
            return db.newDocumentBuilder().parse(is);
        }
    }

    private static Node selectMojo(Document doc, String goal) throws Exception {
        XPath xp = XPathFactory.newInstance().newXPath();
        NodeList mojos = (NodeList) xp.evaluate("/plugin/mojos/mojo", doc, XPathConstants.NODESET);
        for (int i = 0; i < mojos.getLength(); i++) {
            Node mojo = mojos.item(i);
            String g = getChildText(mojo, "goal");
            if (goal.equals(g)) {
                return mojo;
            }
        }
        return null;
    }

    private static String getChildText(Node node, String childName) {
        if (node == null || !node.hasChildNodes()) {
            return null;
        }
        NodeList children = node.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node c = children.item(i);
            if (childName.equals(c.getNodeName())) {
                return Optional.ofNullable(c.getTextContent()).map(String::trim).orElse(null);
            }
        }
        return null;
    }

    private static String loadConfigSource() throws Exception {
        // Find source relative to test location
        Path testDir = Path.of(System.getProperty("user.dir"));
        Path source = testDir.resolve("src/main/java/org/skyve/toolchain/config/GenerateDomainConfig.java");
        
        // If not found, try going up to parent directories (for different working directories)
        while (!Files.exists(source) && testDir.getParent() != null) {
            testDir = testDir.getParent();
            source = testDir.resolve("src/main/java/org/skyve/toolchain/config/GenerateDomainConfig.java");
        }
        
        assertTrue(Files.exists(source), "Could not locate GenerateDomainConfig.java for source verification");
        return Files.readString(source);
    }
}
