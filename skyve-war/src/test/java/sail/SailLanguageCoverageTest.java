package sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Stream;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.XMLMetaData;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Checks that the shipped SAIL scripts and suites ({@code src/test/java/sail/**}{@code /*.xml}) are a complete example of
 * the language: every element that {@code sail.xsd} declares is used by at least one of them, and every attribute of
 * each element is used on it at least once, unless {@link #ALLOWED} gives the reason it cannot be.
 * <p>
 * An element's attributes are those of its type and the types it extends. A name on the allow-list that the scripts
 * do use, or that the schema no longer declares, fails too, so that the list stays true.
 */
class SailLanguageCoverageTest {
	private static final Path SCHEMA = Paths.get("src/main/java/schemas/sail.xsd");
	private static final Path SCRIPTS = Paths.get("src/test/java/sail");
	private static final String XS = XMLConstants.W3C_XML_SCHEMA_NS_URI;

	/**
	 * The elements ({@code name}) and attributes ({@code element@attribute}) that no shipped script uses, each with why.
	 */
	private static final Map<String, String> ALLOWED = Map.<String, String> ofEntries(
			// schema plumbing
			Map.entry("abstractAction", "the abstract base type of the action steps, not an element a script writes"),
			Map.entry("abstractAction@testSuccess", "the abstract base type's attribute; used on the concrete action steps"),
			Map.entry("procedure", "the type of <before> and <after>, not an element a script writes"),
			// suite features that the shipped suites do not need
			Map.entry("member@requires", "no shipped member depends on another, and a requirement must be genuine; SailSuiteTest shows it"),
			Map.entry("setUp@requires", "each set-up requires the earlier ones without naming them; no shipped set-up needs more"),
			// the test server's data and configuration
			Map.entry("login@customer", "the test server has a default customer, so its sign in page has no customer field"),
			// list models: no kitchen sink or admin menu item lists a model; ModelLookupFixture's view shows one
			Map.entry("listGridNew@model", "the kitchen sink's only list model grid (ModelLookupFixture) has showAdd=\"false\""),
			Map.entry("navigateCalendar@model", "the kitchen sink's calendar is of a document; no menu item lists a list model"),
			Map.entry("navigateMap@model", "the kitchen sink's map is of a document; no menu item lists a list model"),
			Map.entry("navigateTree@model", "the kitchen sink's tree is of a query; no menu item lists a list model"),
			Map.entry("navigateList@model", "no menu item of the kitchen sink or admin lists a list model"),
			Map.entry("pushListContext@model", "no menu item lists a list model"),
			// other ways of naming what the shipped scripts name another way
			Map.entry("navigateCalendar@query", "the kitchen sink's calendar menu item is of a document"),
			Map.entry("navigateMap@query", "the kitchen sink's map menu item is of a document"),
			Map.entry("navigateTree@document", "the kitchen sink's tree menu item is of a query"),
			Map.entry("pushEditContext@createView", "the context steps are written by the SAIL generator for compiled executors;"
													+ " a shipped script's contexts come from its navigate steps"),
			Map.entry("pushEditContext@uxui", "a pushed context takes the automation's UX/UI unless a generated script says otherwise"),
			Map.entry("pushEditContext@userAgentType", "a pushed context takes the automation's user agent unless a generated script says otherwise"),
			Map.entry("pushListContext@uxui", "a pushed context takes the automation's UX/UI unless a generated script says otherwise"),
			Map.entry("pushListContext@userAgentType", "a pushed context takes the automation's user agent unless a generated script says otherwise"),
			Map.entry("navigateMap@refreshTimeInSeconds", "no executor reads it; kept for older scripts"),
			Map.entry("navigateMap@showRefreshControls", "no executor reads it; kept for older scripts"),
			Map.entry("listGridSelect@model", "the kitchen sink's only list model grid has no selectedIdBinding, so a selected row has nothing to check"),
			Map.entry("listGridTool@model", "the kitchen sink's only list model grid lists values that do not change while a script runs, so a refresh has nothing to check"),
			Map.entry("dataEnter@navigateToWidget", "no executor reads it (the interpreted ones find the widget wherever it is); kept for older scripts"),
			// defaults that the kitchen sink has no reason to state
			Map.entry("save@confirm", "the kitchen sink's only confirming Save (EscapingFixture's) sits after the default Save that <defaults/> adds, and <save/> presses the first"),
			Map.entry("ok@confirm", "the kitchen sink's OK buttons ask for no confirmation"),
			Map.entry("zoomOut@confirm", "the kitchen sink's Zoom Out buttons ask for no confirmation"),
			Map.entry("cancel@testSuccess", "testSuccess=\"false\" is for a gesture that fails; no shipped Cancel does"),
			Map.entry("ok@testSuccess", "testSuccess=\"false\" is for a gesture that fails; no shipped OK does"),
			Map.entry("remove@testSuccess", "testSuccess=\"false\" is for a gesture that fails; no shipped Remove does"),
			Map.entry("zoomOut@testSuccess", "testSuccess=\"false\" is for a gesture that fails; no shipped Zoom Out does"),
			Map.entry("dataGridLink@testSuccess", "testSuccess=\"false\" is for a gesture that fails; no shipped row link does"));

	@Test
	@SuppressWarnings("static-method")
	void everyElementAndAttributeIsUsedOrAllowed() throws IOException, ParserConfigurationException, SAXException {
		Map<String, Set<String>> declared = declared();
		Map<String, Set<String>> used = used();
		Set<String> missing = new TreeSet<>();
		Set<String> known = new TreeSet<>();
		for (Map.Entry<String, Set<String>> element : declared.entrySet()) {
			String name = element.getKey();
			known.add(name);
			Set<String> attributes = used.get(name);
			if (attributes == null) {
				missing.add(name);
			}
			for (String attribute : element.getValue()) {
				String key = name + '@' + attribute;
				known.add(key);
				if ((attributes == null) || (! attributes.contains(attribute))) {
					missing.add(key);
				}
			}
		}
		List<Executable> checks = new ArrayList<>();
		for (String name : missing) {
			checks.add(() -> assertTrue(ALLOWED.containsKey(name), describe(name) + " is declared in sail.xsd but no shipped SAIL file uses it: " +
																		"use it in a script or suite under " + SCRIPTS + ", or allow it with a reason"));
		}
		for (String name : ALLOWED.keySet()) {
			checks.add(() -> assertTrue(known.contains(name), name + " is allowed but sail.xsd no longer declares it: remove it from the allow-list"));
			checks.add(() -> assertFalse(known.contains(name) && (! missing.contains(name)),
											name + " is allowed but a shipped SAIL file uses it: remove it from the allow-list"));
		}
		assertAll(checks);
	}

	private static String describe(String name) {
		int at = name.indexOf('@');
		return (at < 0) ? "Element <" + name + ">" : "Attribute " + name.substring(at + 1) + " of <" + name.substring(0, at) + ">";
	}

	/**
	 * Returns each element the schema declares (globally, or locally within a type) with its attributes, including those
	 * of the types its type extends.
	 */
	private static Map<String, Set<String>> declared() throws IOException, ParserConfigurationException, SAXException {
		Element schema = parse(SCHEMA).getDocumentElement();
		Map<String, Set<String>> attributesByType = new TreeMap<>();
		Map<String, String> baseByType = new TreeMap<>();
		Map<String, Set<String>> typesByElement = new TreeMap<>();
		NodeList types = schema.getElementsByTagNameNS(XS, "complexType");
		for (int i = 0, l = types.getLength(); i < l; i++) {
			Element type = (Element) types.item(i);
			String name = type.getAttribute("name");
			if (name.isEmpty()) {
				continue;
			}
			Set<String> attributes = new TreeSet<>();
			NodeList declaredAttributes = type.getElementsByTagNameNS(XS, "attribute");
			for (int j = 0, m = declaredAttributes.getLength(); j < m; j++) {
				attributes.add(((Element) declaredAttributes.item(j)).getAttribute("name"));
			}
			attributesByType.put(name, attributes);
			NodeList extensions = type.getElementsByTagNameNS(XS, "extension");
			if (extensions.getLength() > 0) {
				baseByType.put(name, localName(((Element) extensions.item(0)).getAttribute("base")));
			}
		}
		NodeList elements = schema.getElementsByTagNameNS(XS, "element");
		for (int i = 0, l = elements.getLength(); i < l; i++) {
			Element element = (Element) elements.item(i);
			String name = element.getAttribute("name");
			if (! name.isEmpty()) {
				typesByElement.computeIfAbsent(name, k -> new TreeSet<>()).add(localName(element.getAttribute("type")));
			}
		}
		Map<String, Set<String>> result = new TreeMap<>();
		for (Map.Entry<String, Set<String>> element : typesByElement.entrySet()) {
			Set<String> attributes = new TreeSet<>();
			for (String type : element.getValue()) {
				for (String t = type; t != null; t = baseByType.get(t)) {
					attributes.addAll(attributesByType.getOrDefault(t, Set.of()));
				}
			}
			result.put(element.getKey(), attributes);
		}
		return result;
	}

	/** Returns each SAIL element the shipped files use, with every attribute it has in any of them. */
	private static Map<String, Set<String>> used() throws IOException, ParserConfigurationException, SAXException {
		List<Path> files;
		try (Stream<Path> walk = Files.walk(SCRIPTS)) {
			files = walk.filter(p -> p.toString().endsWith(".xml")).sorted().toList();
		}
		assertFalse(files.isEmpty(), "No SAIL files under " + SCRIPTS.toAbsolutePath());
		Map<String, Set<String>> result = new TreeMap<>();
		for (Path file : files) {
			NodeList elements = parse(file).getElementsByTagNameNS(XMLMetaData.SAIL_NAMESPACE, "*");
			for (int i = 0, l = elements.getLength(); i < l; i++) {
				Element element = (Element) elements.item(i);
				Set<String> attributes = result.computeIfAbsent(element.getLocalName(), k -> new TreeSet<>());
				NamedNodeMap present = element.getAttributes();
				for (int j = 0, m = present.getLength(); j < m; j++) {
					Node attribute = present.item(j);
					if (attribute.getNamespaceURI() == null) { // not xmlns or xsi:schemaLocation
						attributes.add(attribute.getLocalName());
					}
				}
			}
		}
		return result;
	}

	private static String localName(String qualified) {
		return qualified.substring(qualified.indexOf(':') + 1);
	}

	private static org.w3c.dom.Document parse(Path file) throws IOException, ParserConfigurationException, SAXException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
		factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
		DocumentBuilder builder = factory.newDocumentBuilder();
		return builder.parse(file.toFile());
	}
}
