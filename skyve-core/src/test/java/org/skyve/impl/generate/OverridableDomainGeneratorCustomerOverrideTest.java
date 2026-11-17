package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Full {@link OverridableDomainGenerator#generate()} runs over a temporary directory for customer
 * document overrides that narrow a vanilla document or sit in an inheritance hierarchy, checking
 * the generated Java and that the generated vanilla and customer mappings hang together - every
 * subclass mapping is nested in or extends a class mapping, no entity name is defined twice and
 * every referenced entity name is defined.
 * <p/>
 * The disabled hierarchy tests reproduce bug 2: the customer mapping of an override inside a single or
 * joined hierarchy emits a top-level subclass mapping without <code>extends</code>, emits the subclass
 * twice when the base is overridden too, nests vanilla subclasses under a customer base class they do
 * not extend (mapped to customer classes that are never generated), and overriding a mapped base fails
 * generation outright. Fixing it needs a decision on how a customer hierarchy relates to the vanilla
 * one, because a generated customer subclass cannot extend both the vanilla subclass and the customer
 * base class.
 */
@SuppressWarnings("static-method")
class OverridableDomainGeneratorCustomerOverrideTest {
	private static final String MODULE = "sales";

	/**
	 * Vanilla documents of the sales module and the acme and beta customers' overrides of some of them.
	 */
	private static final class Fixture {
		private final Map<String, DocumentImpl> vanilla = new TreeMap<>();
		private final Map<String, Map<String, DocumentImpl>> overrides = Map.of("acme", new TreeMap<>(), "beta", new TreeMap<>());

		Fixture vanilla(DocumentImpl document) {
			vanilla.put(document.getName(), document);
			return this;
		}

		Fixture acme(DocumentImpl document) {
			return override("acme", document);
		}

		Fixture beta(DocumentImpl document) {
			return override("beta", document);
		}

		private Fixture override(String customerName, DocumentImpl document) {
			overrides.get(customerName).put(document.getName(), document);
			return this;
		}

		boolean overrides(String customerName, String documentName) {
			Map<String, DocumentImpl> documents = overrides.get(customerName);
			return (documents != null) && documents.containsKey(documentName);
		}
	}

	private static DocumentImpl document(String name, String persistentName, ExtensionStrategy strategy, String extendsName, String... attributeNames) {
		DocumentImpl result = new DocumentImpl();
		result.setName(name);
		result.setOwningModuleName(MODULE);
		result.setSingularAlias(name);
		result.setPluralAlias(name + "s");
		if ((persistentName != null) || (strategy != null)) {
			Persistent persistent = new Persistent();
			persistent.setName(persistentName);
			persistent.setStrategy(strategy);
			result.setPersistent(persistent);
		}
		if (extendsName != null) {
			Extends inherits = new Extends();
			inherits.setDocumentName(extendsName);
			result.setExtends(inherits);
		}
		for (String attributeName : attributeNames) {
			Text text = new Text();
			text.setName(attributeName);
			text.setDisplayName(attributeName);
			result.putAttribute(text);
		}
		return result;
	}

	/** Runs the generator over the fixture into {@code root/src}, returning the generated files by relative path. */
	private static Map<String, String> generate(Fixture fixture) throws Exception {
		Path root = Files.createTempDirectory("odg-customer-override");

		CustomerImpl acme = new CustomerImpl();
		acme.setName("acme");
		acme.getModuleEntries().put(MODULE, null);
		CustomerImpl beta = new CustomerImpl();
		beta.setName("beta");
		beta.getModuleEntries().put(MODULE, null);

		Map<String, Module.DocumentRef> refs = new TreeMap<>();
		for (String documentName : fixture.vanilla.keySet()) {
			Module.DocumentRef ref = new Module.DocumentRef();
			ref.setOwningModuleName(MODULE);
			refs.put(documentName, ref);
		}
		Module module = mock(Module.class);
		when(module.getName()).thenReturn(MODULE);
		when(module.getDocumentRefs()).thenReturn(refs);
		when(module.getDocument(any(), anyString())).thenAnswer(invocation -> {
			Customer customer = invocation.getArgument(0);
			String documentName = invocation.getArgument(1);
			if ((customer != null) && fixture.overrides(customer.getName(), documentName)) {
				return fixture.overrides.get(customer.getName()).get(documentName);
			}
			return fixture.vanilla.get(documentName);
		});

		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.getAllVanillaModuleNames()).thenReturn(List.of(MODULE));
		when(repo.getModule(any(), anyString())).thenReturn(module);
		when(repo.getAllCustomerNames()).thenReturn(List.of("acme", "beta"));
		when(repo.getCustomer("acme")).thenReturn(acme);
		when(repo.getCustomer("beta")).thenReturn(beta);
		when(repo.vtable(anyString(), anyString())).thenAnswer(invocation -> {
			String customerName = invocation.getArgument(0);
			String key = invocation.getArgument(1);
			String documentName = key.substring(key.lastIndexOf('/') + 1);
			if (key.startsWith("modules/" + MODULE + '/') && fixture.overrides(customerName, documentName)) {
				return "customers/" + customerName + '/' + key;
			}
			return key;
		});
		when(repo.findNearestPersistentSingleOrJoinedSuperDocument(any(), any(), any())).thenAnswer(invocation -> {
			Customer customer = invocation.getArgument(0);
			Document document = invocation.getArgument(2);
			Document current = document;
			Extends inherits = current.getExtends();
			while (inherits != null) {
				current = module.getDocument(customer, inherits.getDocumentName());
				inherits = current.getExtends();
				Persistent persistent = current.getPersistent();
				ExtensionStrategy strategy = (persistent == null) ? null : persistent.getStrategy();
				if (! ExtensionStrategy.mapped.equals(strategy)) {
					return ((persistent != null) && (persistent.getName() != null)) ? current : null;
				}
			}
			return null;
		});

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepositoryFactory.set(repo);
		try {
			OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
					false,
					false,
					DialectOptions.H2_NO_INDEXES,
					"",
					root.resolve("src").toString() + '/',
					"",
					root.resolve("test").toString() + '/',
					new String[] {MODULE});
			gen.repository = repo;
			gen.generate();
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}

		Map<String, String> result = new TreeMap<>();
		Path src = root.resolve("src");
		try (Stream<Path> paths = Files.walk(src)) {
			for (Path path : paths.filter(Files::isRegularFile).toList()) {
				result.put(src.relativize(path).toString().replace(java.io.File.separatorChar, '/'), Files.readString(path));
			}
		}
		return result;
	}

	/**
	 * Checks that the vanilla and customer mappings, loaded together as Skyve loads them into one
	 * SessionFactory, hang together: subclass mappings are nested in or extend a class mapping, no
	 * entity name is defined twice, every referenced entity name is defined, every mapped class was
	 * generated, and a nested subclass mapping's class extends the enclosing mapping's class.
	 * Returns the problems found.
	 */
	private static List<String> mappingProblems(Map<String, String> files) throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
		DocumentBuilder builder = factory.newDocumentBuilder();

		List<String> problems = new ArrayList<>();
		Map<String, String> definitions = new TreeMap<>();
		Set<String> references = new TreeSet<>();
		for (Map.Entry<String, String> file : files.entrySet()) {
			if (! file.getKey().endsWith(".hbm.xml")) {
				continue;
			}
			org.w3c.dom.Document xml = builder.parse(new ByteArrayInputStream(file.getValue().getBytes(StandardCharsets.UTF_8)));
			Element mapping = xml.getDocumentElement();
			for (Node child = mapping.getFirstChild(); child != null; child = child.getNextSibling()) {
				if ((child instanceof Element element) &&
						(element.getTagName().equals("subclass") || element.getTagName().equals("joined-subclass")) &&
						element.getAttribute("extends").isEmpty()) {
					problems.add(file.getKey() + ": top-level <" + element.getTagName() + "> " +
									element.getAttribute("entity-name") + " neither nested in a class nor extending one");
				}
			}
			for (String tag : List.of("class", "subclass", "joined-subclass", "union-subclass")) {
				NodeList elements = xml.getElementsByTagName(tag);
				for (int i = 0, l = elements.getLength(); i < l; i++) {
					Element element = (Element) elements.item(i);
					String entityName = element.getAttribute("entity-name");
					String previous = definitions.put(entityName, file.getKey() + ' ' + element.getAttribute("name"));
					if (previous != null) {
						problems.add("entity " + entityName + " defined twice: " + previous + " and " + file.getKey());
					}
					String superEntity = element.getAttribute("extends");
					if (! superEntity.isEmpty()) {
						references.add(superEntity);
					}
					String className = element.getAttribute("name");
					String java = files.get(className.replace('.', '/') + ".java");
					if (java == null) {
						problems.add(file.getKey() + ": entity " + entityName + " maps class " + className + " which was not generated");
					}
					else if (element.getParentNode() instanceof Element parent && (! parent.getAttribute("name").isEmpty()) &&
								(! extendsTransitively(files, className, parent.getAttribute("name")))) {
						problems.add(file.getKey() + ": entity " + entityName + " maps class " + className +
										" which does not extend the enclosing mapping's class " + parent.getAttribute("name"));
					}
				}
			}
			for (String tag : List.of("many-to-one", "one-to-many", "many-to-many", "one-to-one")) {
				NodeList elements = xml.getElementsByTagName(tag);
				for (int i = 0, l = elements.getLength(); i < l; i++) {
					references.add(((Element) elements.item(i)).getAttribute("entity-name"));
				}
			}
		}
		for (String reference : references) {
			if (! definitions.containsKey(reference)) {
				problems.add("entity " + reference + " is referenced but not defined");
			}
		}
		return problems;
	}

	/** Whether the generated class extends the ancestor class, following the generated extends clauses. */
	private static boolean extendsTransitively(Map<String, String> files, String className, String ancestorClassName) {
		String current = className;
		while (current != null) {
			String java = files.get(current.replace('.', '/') + ".java");
			if (java == null) {
				return false;
			}
			Matcher m = Pattern.compile("public (?:abstract )?class \\w+ extends (\\w+)").matcher(java);
			if (! m.find()) {
				return false;
			}
			String superName = m.group(1);
			String packageName = current.substring(0, current.lastIndexOf('.'));
			Matcher i = Pattern.compile("import ([\\w.]+\\." + superName + ");").matcher(java);
			current = i.find() ? i.group(1) : packageName + '.' + superName;
			if (current.equals(ancestorClassName)) {
				return true;
			}
		}
		return false;
	}

	// ----- an override that narrows the vanilla document ----------------------

	@Test
	void anOverrideThatOnlyDropsAnAttributeGetsAConcreteCustomerClass() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "description"))
				.acme(document("Order", "SALES_Order", null, null, "code")));

		String vanillaOrder = files.get("modules/sales/domain/Order.java");
		String acmeOverride = files.get("customers/acme/modules/sales/domain/OrderOverride.java");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(vanillaOrder.contains("public abstract class Order"), "acme's narrowing makes the vanilla class abstract"),
				() -> assertTrue(acmeOverride != null, "acme needs a concrete class for its Order, got " + files.keySet()),
				() -> assertTrue((acmeOverride != null) && acmeOverride.contains("public class OrderOverride extends Order"), String.valueOf(acmeOverride)),
				() -> assertTrue((acmeOverride != null) && (! acmeOverride.contains("description")), "acme dropped description"),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.OrderOverride\""), acmeMapping),
				() -> assertFalse(acmeMapping.contains("<class name=\"modules.sales.domain.Order\""), "acme must not map the abstract class"),
				() -> assertEquals(List.of(), mappingProblems(files)));
	}

	@Test
	void aCustomerWithoutTheOverrideUsesTheVanillaExtensionWhenAnotherCustomerNarrows() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "description"))
				.acme(document("Order", "SALES_Order", null, null, "code")));

		String vanillaClass = files.get("modules/sales/domain/OrderVanilla.java");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		assertAll(
				() -> assertTrue(vanillaClass.contains("public class OrderVanilla extends Order"), vanillaClass),
				() -> assertTrue(vanillaClass.contains("description"), "the vanilla extension keeps what acme dropped"),
				() -> assertTrue(vanillaMapping.contains("<class name=\"modules.sales.domain.OrderVanilla\""), vanillaMapping),
				() -> assertTrue(vanillaMapping.contains("name=\"description\""), vanillaMapping),
				() -> assertFalse(files.containsKey("customers/beta/modules/orm.hbm.xml"), "beta overrides nothing"));
	}

	@Test
	void anUnchangedOverrideOfANarrowedDocumentGetsAConcreteCustomerClass() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "description"))
				.acme(document("Order", "SALES_Order", null, null, "code"))
				.beta(document("Order", "SALES_Order", null, null, "code", "description")));

		String betaOverride = files.get("customers/beta/modules/sales/domain/OrderOverride.java");
		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(betaOverride != null, "beta's override adds nothing, but the vanilla class is abstract, got " + files.keySet()),
				() -> assertTrue((betaOverride != null) && betaOverride.contains("public class OrderOverride extends Order"), String.valueOf(betaOverride)),
				() -> assertTrue((betaOverride != null) && betaOverride.contains("description"), "beta keeps what acme dropped"),
				() -> assertTrue(betaMapping.contains("<class name=\"customers.beta.modules.sales.domain.OrderOverride\""), betaMapping),
				() -> assertTrue(betaMapping.contains("name=\"description\""), betaMapping),
				() -> assertEquals(List.of(), mappingProblems(files)));
	}

	@Test
	void anUnchangedOverrideOfAConcreteDocumentKeepsTheVanillaClass() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "description"))
				.acme(document("Order", "SALES_Order", null, null, "code", "description")));

		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(files.get("modules/sales/domain/Order.java").contains("public class Order"), "nothing narrows Order"),
				() -> assertFalse(files.containsKey("customers/acme/modules/sales/domain/OrderOverride.java"), files.keySet().toString()),
				() -> assertTrue(acmeMapping.contains("<class name=\"modules.sales.domain.Order\""), acmeMapping),
				() -> assertEquals(List.of(), mappingProblems(files)));
	}

	@Test
	void aDocumentNamedLikeTheConcreteVanillaClassIsRefused() {
		Fixture fixture = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "description"))
				.vanilla(document("Ordervanilla", "SALES_OrderVanilla", null, null, "code"))
				.acme(document("Order", "SALES_Order", null, null, "code"));

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Document Ordervanilla in module sales has the name of the class OrderVanilla generated for document Order. Rename the document.",
						e.getMessage());
	}

	@Test
	void aDocumentNamedLikeTheOverrideClassIsRefused() {
		Fixture fixture = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.vanilla(document("OrderOverride", "SALES_OrderOverride", null, null, "code"))
				.acme(document("Order", "SALES_Order", null, null, "code", "notes"));

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Document OrderOverride in module sales has the name of the class OrderOverride generated for document Order. Rename the document.",
						e.getMessage());
	}

	@Test
	void aNarrowedEnumerationIsMappedToTheClassThatDeclaresIt() throws Exception {
		DocumentImpl vanilla = document("Order", "SALES_Order", null, null, "code");
		vanilla.putAttribute(enumeration(vanilla, "status"));
		DocumentImpl acme = document("Order", "SALES_Order", null, null, "code");
		DocumentImpl beta = document("Order", "SALES_Order", null, null, "code", "notes");
		beta.putAttribute(enumeration(beta, "status"));
		beta.putAttribute(enumeration(beta, "priority"));

		Map<String, String> files = generate(new Fixture().vanilla(vanilla).acme(acme).beta(beta));

		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(files.get("modules/sales/domain/OrderVanilla.java").contains("public enum Status implements Enumeration"), "the dropped enumeration moves to the vanilla class"),
				() -> assertTrue(vanillaMapping.contains("modules.sales.domain.OrderVanilla$Status"), vanillaMapping),
				() -> assertFalse(vanillaMapping.contains("customers."), vanillaMapping),
				() -> assertTrue(files.get("customers/beta/modules/sales/domain/OrderOverride.java").contains("public enum Priority implements Enumeration"), "beta's own enumeration"),
				() -> assertTrue(betaMapping.contains("customers.beta.modules.sales.domain.OrderOverride$Priority"), betaMapping),
				() -> assertTrue(betaMapping.contains("customers.beta.modules.sales.domain.OrderOverride$Status"), betaMapping));
	}

	private static Enumeration enumeration(Document owningDocument, String name) {
		Enumeration result = new Enumeration();
		result.setOwningDocument(owningDocument);
		result.setName(name);
		result.setDisplayName(name);
		for (String code : List.of("low", "high")) {
			EnumeratedValue value = new EnumeratedValue();
			value.setCode(code);
			value.setDescription(code);
			result.getXmlValues().add(value);
		}
		return result;
	}

	// ----- overrides in an inheritance hierarchy -------------------------------

	/**
	 * Vanilla Base and Sub extends Base for the strategy. A mapped Base has no table; its Sub is then the
	 * root of its own (joined) hierarchy. The joined Sub has its own table, the single Sub shares Base's.
	 */
	private static Fixture hierarchy(ExtensionStrategy strategy, boolean overrideBase, boolean overrideSub) {
		boolean mapped = ExtensionStrategy.mapped.equals(strategy);
		String baseTable = mapped ? null : "SALES_Base";
		ExtensionStrategy subStrategy = mapped ? ExtensionStrategy.joined : strategy;
		String subTable = ExtensionStrategy.single.equals(strategy) ? "SALES_Base" : "SALES_Sub";
		Fixture result = new Fixture()
				.vanilla(document("Base", baseTable, strategy, null, "code"))
				.vanilla(document("Sub", subTable, subStrategy, "Base", "sub"));
		if (overrideBase) {
			result.acme(document("Base", baseTable, strategy, null, "code", "baseNotes"));
		}
		if (overrideSub) {
			result.acme(document("Sub", subTable, subStrategy, "Base", "sub", "subNotes"));
		}
		return result;
	}

	private static void assertConsistentMappings(Fixture fixture) throws Exception {
		Map<String, String> files = generate(fixture);
		assertEquals(List.of(), mappingProblems(files), String.valueOf(files.get("customers/acme/modules/orm.hbm.xml")));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingAJoinedSubclassGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.joined, false, true));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingAJoinedBaseGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.joined, true, false));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingAJoinedBaseAndSubclassGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.joined, true, true));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingASingleSubclassGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.single, false, true));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingASingleBaseGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.single, true, false));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingASingleBaseAndSubclassGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.single, true, true));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void narrowingAJoinedBaseGivesConsistentVanillaMappings() throws Exception {
		assertConsistentMappings(new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code", "description"))
				.vanilla(document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub"))
				.acme(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code")));
	}

	@Test
	void overridingAMappedSubclassGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.mapped, false, true));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingAMappedBaseGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.mapped, true, false));
	}

	@Test
	@Disabled("Bug 2: customer overrides in a single or joined hierarchy - awaiting a design decision, see the class javadoc")
	void overridingAMappedBaseAndSubclassGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.mapped, true, true));
	}
}
