package org.skyve.toolchain;

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
 * Unit tests for SystemDocumentationMojo parameters.
 * 
 * <p>This test uses multiple approaches to verify parameter configuration:
 * <ul>
 *   <li>Primary: Parses the generated plugin descriptor (plugin.xml) if available</li>
 *   <li>Fallback: Parses the SystemDocumentationMojo.java source file to verify @Parameter annotations</li>
 * </ul>
 * 
 * <p>The fallback approach is used when the plugin descriptor is not generated during the build,
 * ensuring tests remain reliable across different build configurations.
 */
class SystemDocumentationMojoTest {

	@Test
	@SuppressWarnings("static-method")
    void testExcludedModulesParameterPropertyConfiguration() throws Exception {
        Document plugin = loadPluginDescriptor();
        if (plugin != null) {
            Node mojo = selectMojo(plugin, "systemDocumentation");
            assertNotNull(mojo, "systemDocumentation mojo should exist in plugin.xml");

            Node parameter = selectParameterByName(mojo, "excludedModules");
            assertNotNull(parameter, "excludedModules parameter should be present");

            // Accept either <property>excludedModules</property> or legacy <expression>${excludedModules}</expression>
            String property = getChildText(parameter, "property");
            String expression = getChildText(parameter, "expression");

            boolean hasProperty = (property != null) && property.equals("excludedModules");
            boolean hasExpression = (expression != null) && expression.trim().equals("${excludedModules}");

            assertTrue(hasProperty || hasExpression,
                    "excludedModules should map to -DexcludedModules via <property> or <expression>");

            // Also ensure the type is String
            String type = getChildText(parameter, "type");
            assertEquals("java.lang.String", type, "excludedModules should be of type String");
        } else {
            // Fallback to source parsing when plugin.xml is not generated and annotations are CLASS-retention
            String source = loadMojoSource();
            assertTrue(source.contains("class SystemDocumentationMojo"), "Mojo source should be readable");

            // Verify excludedModules annotation has property="excludedModules"
            Pattern exclPattern = Pattern.compile("@Parameter\\s*\\(\\s*property\\s*=\\s*\\\"excludedModules\\\"[\\s,]*\\)");
            Matcher exclMatcher = exclPattern.matcher(source);
            assertTrue(exclMatcher.find(), "@Parameter(property=\"excludedModules\") should be present on excludedModules");

            // Verify field type is String
            assertTrue(source.contains("private String excludedModules;"), "excludedModules should be a String field");
        }
    }

	@Test
	@SuppressWarnings("static-method")
    void testOtherParametersConfiguration() throws Exception {
        Document plugin = loadPluginDescriptor();
        if (plugin != null) {
            Node mojo = selectMojo(plugin, "systemDocumentation");
            assertNotNull(mojo, "systemDocumentation mojo should exist in plugin.xml");

            // customer
            Node customer = selectParameterByName(mojo, "customer");
            assertNotNull(customer, "customer parameter should be present");
            assertEquals("true", getChildText(customer, "required"), "customer should be required");
            assertEquals("skyve", getChildText(customer, "default-value"), "customer default value");

            // srcDir
            Node srcDir = selectParameterByName(mojo, "srcDir");
            assertNotNull(srcDir, "srcDir parameter should be present");
            assertEquals("true", getChildText(srcDir, "required"), "srcDir should be required");
            assertEquals("src/main/java/", getChildText(srcDir, "default-value"), "srcDir default value");
        } else {
            // Fallback to source parsing when plugin.xml is not generated and annotations are CLASS-retention
            String source = loadMojoSource();

            // customer: required = true, defaultValue = "skyve"
            Pattern customerPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*defaultValue\\s*=\\s*\\\"skyve\\\"|defaultValue\\s*=\\s*\\\"skyve\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
            assertTrue(customerPattern.matcher(source).find(), "customer should be @Parameter(required=true, defaultValue=\"skyve\")");

            // srcDir: required = true, defaultValue = "src/main/java/"
            Pattern srcDirPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*defaultValue\\s*=\\s*\\\"src/main/java/\\\"|defaultValue\\s*=\\s*\\\"src/main/java/\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
            assertTrue(srcDirPattern.matcher(source).find(), "srcDir should be @Parameter(required=true, defaultValue=\"src/main/java/\")");
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

    private static String loadMojoSource() throws Exception {
        Path source = Path.of("src", "main", "java", "org", "skyve", "toolchain", "SystemDocumentationMojo.java");
        // Absolute fallback if tests run from project root
        if (!Files.exists(source)) {
            source = Path.of("/Users/benpetito/workspace/skyve/skyve-maven-plugin/src/main/java/org/skyve/toolchain/SystemDocumentationMojo.java");
        }
        assertTrue(Files.exists(source), "Could not locate SystemDocumentationMojo.java for source verification");
        return Files.readString(source);
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
}