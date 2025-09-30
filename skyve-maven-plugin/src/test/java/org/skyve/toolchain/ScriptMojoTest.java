package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
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
 * Unit tests for ScriptMojo parameters.
 * 
 * <p>This test uses multiple approaches to verify parameter configuration:
 * <ul>
 *   <li>Primary: Parses the generated plugin descriptor (plugin.xml) if available</li>
 *   <li>Fallback: Parses the ScriptMojo.java source file to verify @Parameter annotations</li>
 * </ul>
 * 
 * <p>The fallback approach is used when the plugin descriptor is not generated during the build,
 * ensuring tests remain reliable across different build configurations.
 */
class ScriptMojoTest {

	@Test
	@SuppressWarnings("static-method")
    void testScriptMojoParameterPropertyConfiguration() throws Exception {
        Document plugin = loadPluginDescriptor();
        if (plugin != null) {
            Node mojo = selectMojo(plugin, "script");
            assertNotNull(mojo, "script mojo should exist in plugin.xml");

            // Test skyveDir parameter
            Node skyveDir = selectParameterByName(mojo, "skyveDir");
            assertNotNull(skyveDir, "skyveDir parameter should be present");
            assertEquals("true", getChildText(skyveDir, "required"), "skyveDir should be required");
            assertEquals("skyveDir", getChildText(skyveDir, "property"), "skyveDir should have property='skyveDir'");

            // Test customer parameter
            Node customer = selectParameterByName(mojo, "customer");
            assertNotNull(customer, "customer parameter should be present");
            assertEquals("true", getChildText(customer, "required"), "customer should be required");
            assertEquals("customer", getChildText(customer, "property"), "customer should have property='customer'");

            // Test scriptPath parameter
            Node scriptPath = selectParameterByName(mojo, "scriptPath");
            assertNotNull(scriptPath, "scriptPath parameter should be present");
            assertEquals("true", getChildText(scriptPath, "required"), "scriptPath should be required");
            assertEquals("script/skyve.md", getChildText(scriptPath, "default-value"), "scriptPath default value");
            assertEquals("scriptPath", getChildText(scriptPath, "property"), "scriptPath should have property='scriptPath'");
        } else {
            // Fallback to source parsing when plugin.xml is not generated
            String source = loadMojoSource();
            assertTrue(source.contains("class ScriptMojo"), "Mojo source should be readable");

            // Verify skyveDir annotation has property="skyveDir" and required=true
            Pattern skyveDirPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*property\\s*=\\s*\\\"skyveDir\\\"|property\\s*=\\s*\\\"skyveDir\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
            assertTrue(skyveDirPattern.matcher(source).find(), "skyveDir should be @Parameter(required=true, property=\"skyveDir\")");

            // Verify customer annotation has property="customer" and required=true
            Pattern customerPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*property\\s*=\\s*\\\"customer\\\"|property\\s*=\\s*\\\"customer\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
            assertTrue(customerPattern.matcher(source).find(), "customer should be @Parameter(required=true, property=\"customer\")");

            // Verify scriptPath annotation has property="scriptPath", required=true, and defaultValue
            Pattern scriptPathPattern = Pattern.compile("@Parameter\\s*\\([^)]*required\\s*=\\s*true[^)]*defaultValue\\s*=\\s*\\\"script/skyve\\.md\\\"[^)]*property\\s*=\\s*\\\"scriptPath\\\"[^)]*\\)");
            assertTrue(scriptPathPattern.matcher(source).find(), "scriptPath should be @Parameter(required=true, defaultValue=\"script/skyve.md\", property=\"scriptPath\")");
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

    private static Node selectParameterByName(Node mojo, String name) throws Exception {
        XPath xp = XPathFactory.newInstance().newXPath();
        NodeList params = (NodeList) xp.evaluate("parameters/parameter", mojo, XPathConstants.NODESET);
        for (int i = 0; i < params.getLength(); i++) {
            Node p = params.item(i);
            String n = getChildText(p, "name");
            if (name.equals(n)) {
                return p;
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

    private static String loadMojoSource() throws Exception {
        Path source = Path.of("src", "main", "java", "org", "skyve", "toolchain", "ScriptMojo.java");
        // Absolute fallback if tests run from project root
        if (!Files.exists(source)) {
            source = Path.of("/Users/benpetito/workspace/skyve/skyve-maven-plugin/src/main/java/org/skyve/toolchain/ScriptMojo.java");
        }
        assertTrue(Files.exists(source), "Could not locate ScriptMojo.java for source verification");
        return Files.readString(source);
    }
}
