package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.skyve.impl.generate.CustomerGenerationTestSupport.snapshot;
import static org.skyve.impl.generate.CustomerGenerationTestSupport.text;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.net.URI;
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

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Image;
import org.skyve.impl.metadata.model.document.field.Markup;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
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
 * In an inheritance hierarchy the customer mapping copies the whole hierarchy from its top. Only a
 * leaf can get its own customer class, as a generated subclass cannot extend both the vanilla class
 * and a customer class of its base, so the generator refuses the cases that would need one.
 */
@SuppressWarnings("static-method")
class OverridableDomainGeneratorCustomerOverrideTest {
	private static final String MODULE = CustomerGenerationTestSupport.MODULE;

	/**
	 * Vanilla documents of the sales module and the acme and beta customers' overrides of some of them.
	 */
	private static final class Fixture {
		private final Map<String, DocumentImpl> vanilla = new TreeMap<>();
		private final Map<String, Map<String, DocumentImpl>> overrides = Map.of("acme", new TreeMap<>(), "beta", new TreeMap<>());
		/** Hand-written extension sources by their path below the source directory. */
		private final Map<String, byte[]> extended = new TreeMap<>();

		/** Gives the vanilla document a hand-written <code>&lt;Document&gt;Extension</code> extending its domain class. */
		Fixture extension(String documentName) {
			return extension(documentName, documentName);
		}

		/**
		 * Gives the vanilla document a hand-written <code>&lt;Document&gt;Extension</code> extending the named
		 * class - a simple name in the module's domain package, or a fully qualified name.
		 */
		Fixture extension(String documentName, String superclassName) {
			String superclassImport = superclassName.contains(".") ? superclassName : "modules." + MODULE + ".domain." + superclassName;
			return extensionSource(documentName, "package modules." + MODULE + '.' + documentName + ";\n\n" +
														"import " + superclassImport + ";\n\n" +
														"public class " + documentName + "Extension extends " +
														superclassImport.substring(superclassImport.lastIndexOf('.') + 1) + " {\n" +
														"\tprivate static final long serialVersionUID = 1L;\n}\n");
		}

		/** Gives the vanilla document a hand-written <code>&lt;Document&gt;Extension</code> with the given source. */
		Fixture extensionSource(String documentName, String source) {
			return extensionBytes(documentName, source.getBytes(StandardCharsets.UTF_8));
		}

		/** Gives the vanilla document a hand-written <code>&lt;Document&gt;Extension</code> with the given source bytes. */
		Fixture extensionBytes(String documentName, byte[] source) {
			extended.put("modules/" + MODULE + '/' + documentName + '/' + documentName + "Extension.java", source);
			return this;
		}

		/**
		 * Gives the acme customer's override of the document a hand-written <code>&lt;Document&gt;Extension</code>,
		 * which extends the customer's override class.
		 */
		Fixture acmeExtension(String documentName) {
			return acmeExtension(documentName, "customers.acme.modules." + MODULE + ".domain." + documentName + "Override");
		}

		/**
		 * Gives the acme customer's override of the document, or its new document, a hand-written
		 * <code>&lt;Document&gt;Extension</code>, which extends the class of the fully qualified name.
		 */
		Fixture acmeExtension(String documentName, String superclassName) {
			String packageName = "customers.acme.modules." + MODULE + '.' + documentName;
			extended.put(packageName.replace('.', '/') + '/' + documentName + "Extension.java",
							("package " + packageName + ";\n\n" +
								"import " + superclassName + ";\n\n" +
								"public class " + documentName + "Extension extends " +
								superclassName.substring(superclassName.lastIndexOf('.') + 1) + " {\n" +
								"\tprivate static final long serialVersionUID = 1L;\n}\n").getBytes(StandardCharsets.UTF_8));
			return this;
		}

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

		/** A beta override of Line kept aside until {@link #useBetaLine()}. */
		private DocumentImpl betaLine;

		/** Whether generate() makes the extension sources unreadable. */
		boolean unreadableExtensions = false;

		Fixture betaLine(DocumentImpl document) {
			betaLine = document;
			return this;
		}

		void useBetaLine() {
			beta(betaLine);
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
		result.setBizKeyMethodCode("\t\treturn \"" + name + "\";");
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
			result.putAttribute(text(attributeName));
		}
		return result;
	}

	/**
	 * Records the customer's exported references - the persistent references to each document from the
	 * customer's versions of the documents - as the repository does when it loads the customer.
	 */
	private static void exportReferences(Fixture fixture, CustomerImpl customer) throws Exception {
		Map<String, DocumentImpl> documents = new TreeMap<>(fixture.vanilla);
		documents.putAll(fixture.overrides.get(customer.getName()));
		java.lang.reflect.Field field = CustomerImpl.class.getDeclaredField("exportedReferences");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		Map<String, List<ExportedReference>> exportedReferences = (Map<String, List<ExportedReference>>) field.get(customer);
		for (DocumentImpl document : documents.values()) {
			if (document.getPersistent() == null) {
				continue;
			}
			for (String referenceName : document.getReferenceNames()) {
				Reference reference = document.getNullSafeReferenceByName(referenceName);
				ExportedReference ref = new ExportedReference();
				ref.setModuleName(MODULE);
				ref.setDocumentName(document.getName());
				ref.setPersistent(document.getPersistent());
				ref.setReferenceFieldName(referenceName);
				ref.setType(reference.getType());
				exportedReferences.computeIfAbsent(MODULE + '.' + reference.getDocumentName(), k -> new ArrayList<>()).add(ref);
			}
		}
	}

	private static AssociationImpl association(String name, String documentName) {
		AssociationImpl result = new AssociationImpl();
		result.setName(name);
		result.setDisplayName(name);
		result.setDocumentName(documentName);
		result.setType(AssociationType.aggregation);
		return result;
	}

	private static CollectionImpl aggregatedCollection(String name, String documentName) {
		CollectionImpl result = new CollectionImpl();
		result.setName(name);
		result.setDisplayName(name);
		result.setDocumentName(documentName);
		result.setType(CollectionType.aggregation);
		return result;
	}

	private static CollectionImpl childCollection(String name, String documentName) {
		CollectionImpl result = new CollectionImpl();
		result.setName(name);
		result.setDisplayName(name);
		result.setDocumentName(documentName);
		result.setType(CollectionType.child);
		return result;
	}

	/**
	 * The sales module for the customer, or the vanilla one for null: it lists the vanilla documents and
	 * the customer's new ones, and resolves a customer's document to its override.
	 */
	private static Module module(Fixture fixture, String customerName) {
		Map<String, Module.DocumentRef> refs = new TreeMap<>();
		Set<String> documentNames = new TreeSet<>(fixture.vanilla.keySet());
		if (customerName != null) {
			documentNames.addAll(fixture.overrides.get(customerName).keySet());
		}
		for (String documentName : documentNames) {
			Module.DocumentRef ref = new Module.DocumentRef();
			ref.setOwningModuleName(MODULE);
			refs.put(documentName, ref);
		}
		Module result = mock(Module.class);
		when(result.getName()).thenReturn(MODULE);
		when(result.getDocumentRefs()).thenReturn(refs);
		when(result.getDocument(any(), anyString())).thenAnswer(invocation -> {
			Customer customer = invocation.getArgument(0);
			String documentName = invocation.getArgument(1);
			if ((customer != null) && fixture.overrides(customer.getName(), documentName)) {
				return fixture.overrides.get(customer.getName()).get(documentName);
			}
			return fixture.vanilla.get(documentName);
		});
		return result;
	}

	/** Runs the generator over the fixture into {@code root/src}, returning the generated files by relative path. */
	private static Map<String, String> generate(Fixture fixture) throws Exception {
		return generate(fixture, true);
	}

	/**
	 * Runs the generator, with metadata validation on or off, over the fixture into {@code root/src},
	 * returning the generated files by relative path.
	 */
	private static Map<String, String> generate(Fixture fixture, boolean validate) throws Exception {
		Path root = Files.createTempDirectory("odg-customer-override");
		Path src = root.resolve("source");
		for (Map.Entry<String, byte[]> entry : fixture.extended.entrySet()) {
			Path extension = src.resolve(entry.getKey());
			Files.createDirectories(extension.getParent());
			Files.write(extension, entry.getValue());
			if (fixture.unreadableExtensions) {
				Files.setPosixFilePermissions(extension, Set.of());
			}
		}

		CustomerImpl acme = new CustomerImpl();
		acme.setName("acme");
		acme.getModuleEntries().put(MODULE, null);
		CustomerImpl beta = new CustomerImpl();
		beta.setName("beta");
		beta.getModuleEntries().put(MODULE, null);
		exportReferences(fixture, acme);
		exportReferences(fixture, beta);

		Module module = module(fixture, null);
		Module acmeModule = module(fixture, "acme");
		Module betaModule = module(fixture, "beta");

		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.getAllVanillaModuleNames()).thenReturn(List.of(MODULE));
		when(repo.getModule(any(), anyString())).thenAnswer(invocation -> {
			Customer customer = invocation.getArgument(0);
			if (customer == null) {
				return module;
			}
			return "acme".equals(customer.getName()) ? acmeModule : betaModule;
		});

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
			CustomerGenerationTestSupport.generate(src.toString() + '/', root.resolve("src"), root.resolve("test"), repo, validate);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}

		// the generated files, and the hand-written extension sources by their path below the source directory
		Map<String, String> result = new TreeMap<>();
		for (Map.Entry<String, byte[]> entry : fixture.extended.entrySet()) {
			result.put(entry.getKey(), new String(entry.getValue(), StandardCharsets.ISO_8859_1));
		}
		result.putAll(snapshot(root.resolve("src")));
		return result;
	}

	/**
	 * Checks that the vanilla and customer mappings, loaded together as Skyve loads them into one
	 * SessionFactory, hang together: subclass mappings are nested in or extend a class mapping, no
	 * entity name is defined twice, every referenced entity name is defined, every mapped class was
	 * generated, a nested subclass mapping's class extends the enclosing mapping's class, every mapped
	 * property is a field of the mapped class or a class it extends, and every enumeration type is
	 * declared in the class it names. Hand-written extension sources count as classes.
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
					else {
						if (element.getParentNode() instanceof Element parent && (! parent.getAttribute("name").isEmpty()) &&
								(! extendsTransitively(files, className, parent.getAttribute("name")))) {
							problems.add(file.getKey() + ": entity " + entityName + " maps class " + className +
											" which does not extend the enclosing mapping's class " + parent.getAttribute("name"));
						}
						for (Node child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
							if ((child instanceof Element property) && PROPERTY_TAGS.contains(property.getTagName())) {
								String propertyName = property.getAttribute("name");
								if ((! INHERITED_PROPERTIES.contains(propertyName)) && (! declaresField(files, className, propertyName))) {
									problems.add(file.getKey() + ": entity " + entityName + " maps property " + propertyName +
													" which " + className + " and the classes it extends do not declare");
								}
							}
						}
					}
				}
			}
			NodeList params = xml.getElementsByTagName("param");
			for (int i = 0, l = params.getLength(); i < l; i++) {
				Element param = (Element) params.item(i);
				if ("enumClass".equals(param.getAttribute("name"))) {
					String enumClassName = param.getTextContent().trim();
					int dollar = enumClassName.indexOf('$');
					String java = files.get(enumClassName.substring(0, dollar).replace('.', '/') + ".java");
					if ((java == null) || (! java.contains("public enum " + enumClassName.substring(dollar + 1) + ' '))) {
						problems.add(file.getKey() + ": enumeration type " + enumClassName + " is not declared");
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

	/** The mapping elements that map a property of the mapped class. */
	private static final Set<String> PROPERTY_TAGS = Set.of("property", "many-to-one", "one-to-one", "bag", "list", "set", "map", "component");

	/** The properties a generated class inherits from the Skyve bean classes. */
	private static final Set<String> INHERITED_PROPERTIES = Set.of("bizId", "bizVersion", "bizLock", "bizKey", "bizCustomer",
																	"bizFlagComment", "bizDataGroupId", "bizUserId", "bizOrdinal",
																	"bizParentId", "parent");

	/** The fully qualified superclass of a generated or extension class, or null if it is not one of them. */
	private static String superclassOf(Map<String, String> files, String className) {
		String java = files.get(className.replace('.', '/') + ".java");
		if (java == null) {
			return null;
		}
		Matcher m = Pattern.compile("public (?:abstract )?class \\w+ extends (\\w+)").matcher(java);
		if (! m.find()) {
			return null;
		}
		String superName = m.group(1);
		Matcher i = Pattern.compile("import ([\\w.]+\\." + superName + ");").matcher(java);
		return i.find() ? i.group(1) : className.substring(0, className.lastIndexOf('.')) + '.' + superName;
	}

	/** Whether the generated class extends the ancestor class, following the generated extends clauses. */
	private static boolean extendsTransitively(Map<String, String> files, String className, String ancestorClassName) {
		String current = superclassOf(files, className);
		while (current != null) {
			if (current.equals(ancestorClassName)) {
				return true;
			}
			current = superclassOf(files, current);
		}
		return false;
	}

	/** Whether the class or a generated or extension class it extends declares the field. */
	private static boolean declaresField(Map<String, String> files, String className, String fieldName) {
		Pattern field = Pattern.compile("(?m)^\\s*private [^;=(]*\\b" + Pattern.quote(fieldName) + "\\s*[=;]");
		String current = className;
		while (current != null) {
			String java = files.get(current.replace('.', '/') + ".java");
			if (java == null) {
				return false;
			}
			if (field.matcher(java).find()) {
				return true;
			}
			current = superclassOf(files, current);
		}
		return false;
	}

	/**
	 * Compiles the generated and hand-written extension sources together, returning the errors.
	 */
	private static List<String> compilationErrors(Map<String, String> files) throws Exception {
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
		List<JavaFileObject> sources = new ArrayList<>();
		for (Map.Entry<String, String> file : files.entrySet()) {
			if (file.getKey().endsWith(".java")) {
				sources.add(new SimpleJavaFileObject(URI.create("string:///" + file.getKey()), JavaFileObject.Kind.SOURCE) {
					@Override
					public CharSequence getCharContent(boolean ignoreEncodingErrors) {
						return file.getValue();
					}
				});
			}
		}
		Path classes = Files.createTempDirectory("odg-customer-override-classes");
		try (StandardJavaFileManager fileManager = compiler.getStandardFileManager(diagnostics, null, StandardCharsets.UTF_8)) {
			List<String> options = List.of("-proc:none", "-d", classes.toString(), "-classpath", System.getProperty("java.class.path"));
			compiler.getTask(null, fileManager, diagnostics, options, null, sources).call();
		}
		List<String> result = new ArrayList<>();
		for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
			if (Diagnostic.Kind.ERROR.equals(diagnostic.getKind())) {
				result.add(diagnostic.getSource().getName() + ':' + diagnostic.getLineNumber() + ' ' + diagnostic.getMessage(null));
			}
		}
		return result;
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
				() -> assertNotNull(acmeOverride, "acme needs a concrete class for its Order, got " + files.keySet()),
				() -> assertTrue(acmeOverride.contains("public class OrderOverride extends Order"), acmeOverride),
				() -> assertFalse(acmeOverride.contains("description"), "acme dropped description"),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.OrderOverride\""), acmeMapping),
				() -> assertFalse(acmeMapping.contains("<class name=\"modules.sales.domain.Order\""), "acme must not map the abstract class"),
				() -> assertEquals(List.of(), mappingProblems(files)));
	}

	@Test
	void aCustomerWithoutTheOverrideUsesTheVanillaClassWhenAnotherCustomerNarrows() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "description"))
				.acme(document("Order", "SALES_Order", null, null, "code")));

		String vanillaClass = files.get("modules/sales/domain/OrderVanilla.java");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		assertAll(
				() -> assertTrue(vanillaClass.contains("public class OrderVanilla extends Order"), vanillaClass),
				() -> assertTrue(vanillaClass.contains("description"), "the vanilla class keeps what acme dropped"),
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
				() -> assertNotNull(betaOverride, "beta's override adds nothing, but the vanilla class is abstract, got " + files.keySet()),
				() -> assertTrue(betaOverride.contains("public class OrderOverride extends Order"), betaOverride),
				() -> assertTrue(betaOverride.contains("description"), "beta keeps what acme dropped"),
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

	/**
	 * A customer's class inherits the vanilla property and its mapping shares the vanilla table, so an
	 * override cannot change a property's type, whether or not validation is on.
	 */
	@Test
	void anOverrideThatChangesAPropertysTypeIsRefused() {
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "notes");
		org.skyve.impl.metadata.model.document.field.Integer code = new org.skyve.impl.metadata.model.document.field.Integer();
		code.setName("code");
		code.setDisplayName("code");
		acmeOrder.putAttribute(code);
		Fixture fixture = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "notes"))
				.acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture, false));
		assertEquals("Customer acme's override of document sales.Order changes the type of code from text to integer, " +
						"but the customer's class inherits the vanilla property and its mapping shares the vanilla table. " +
						"Keep code as text in the override, or add an attribute of another name.",
						e.getMessage());
	}

	/**
	 * A customer's mapping shares the vanilla table, so an override cannot change the type of a property
	 * it declares itself, as beta's override of a document acme's narrows does.
	 */
	@Test
	void anOverrideThatChangesTheTypeOfAPropertyItsClassDeclaresIsRefused() {
		Fixture fixture = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "notes"))
				.acme(document("Order", "SALES_Order", null, null, "code"))
				.beta(withAttribute(document("Order", "SALES_Order", null, null, "code"), named(new org.skyve.impl.metadata.model.document.field.Integer(), "notes")));

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture, false));
		assertEquals("Customer beta's override of document sales.Order changes the type of notes from text to integer, " +
						"but the customer's mapping shares the vanilla table and its column. " +
						"Keep notes as text in the override, or add an attribute of another name.",
						e.getMessage());
	}

	/** A memo and markup are both strings mapped as text, so an override can change one to the other. */
	@Test
	void anOverrideCanChangeAMemoToMarkup() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(withAttribute(document("Order", "SALES_Order", null, null, "code"), named(new Memo(), "notes")))
				.acme(withAttribute(document("Order", "SALES_Order", null, null, "code"), named(new Markup(), "notes"))));

		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(() -> assertTrue(acmeMapping.contains("<property name=\"notes\" type=\"text\""), acmeMapping),
					() -> assertEquals(List.of(), mappingProblems(files)));
	}

	/** Content and an image are both strings mapped with a length of 36, so an override can change one to the other. */
	@Test
	void anOverrideCanChangeContentToAnImage() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(withAttribute(document("Order", "SALES_Order", null, null, "code"), named(new Content(), "photo")))
				.acme(withAttribute(document("Order", "SALES_Order", null, null, "code"), named(new Image(), "photo"))));

		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(() -> assertTrue(acmeMapping.contains("<property name=\"photo\" length=\"36\""), acmeMapping),
					() -> assertEquals(List.of(), mappingProblems(files)));
	}

	/** Text and a memo are both strings, but are mapped differently, so an override cannot change one to the other. */
	@Test
	void anOverrideThatChangesTextToAMemoIsRefused() {
		Fixture fixture = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code", "notes"))
				.acme(withAttribute(document("Order", "SALES_Order", null, null, "code"), named(new Memo(), "notes")));

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Customer acme's override of document sales.Order changes the type of notes from text to memo, " +
						"but the customer's class inherits the vanilla property and its mapping shares the vanilla table. " +
						"Keep notes as text in the override, or add an attribute of another name.",
						e.getMessage());
	}

	/** The customer's class inherits an association typed by the vanilla target's class. */
	@Test
	void anOverrideThatPointsAnAssociationAtAnotherDocumentIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putRelation(association("customer", "Customer"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putRelation(association("customer", "Account"));
		Fixture fixture = new Fixture()
				.vanilla(vanillaOrder)
				.vanilla(document("Customer", "SALES_Customer", null, null, "name"))
				.vanilla(document("Account", "SALES_Account", null, null, "name"))
				.acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture, false));
		assertEquals("Customer acme's override of document sales.Order changes the document of customer from sales.Customer to sales.Account, " +
						"but the customer's class inherits the vanilla property, typed by sales.Customer's class, and its mapping shares the vanilla table. " +
						"Keep customer referring to sales.Customer in the override, or add a relation of another name.",
						e.getMessage());
	}

	/** A customer's mapping shares the vanilla table, so a collection its class declares itself cannot point at another document either. */
	@Test
	void anOverrideThatPointsACollectionItsClassDeclaresAtAnotherDocumentIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		vanillaOrder.putRelation(aggregatedCollection("items", "Product"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code");
		betaOrder.putRelation(aggregatedCollection("items", "Service"));
		Fixture fixture = new Fixture()
				.vanilla(vanillaOrder)
				.vanilla(document("Product", "SALES_Product", null, null, "name"))
				.vanilla(document("Service", "SALES_Service", null, null, "name"))
				.acme(document("Order", "SALES_Order", null, null, "code", "notes"))
				.beta(betaOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Customer beta's override of document sales.Order changes the document of items from sales.Product to sales.Service, " +
						"but the customer's mapping shares the vanilla join table of references to sales.Product. " +
						"Keep items referring to sales.Product in the override, or add a relation of another name.",
						e.getMessage());
	}

	/** A customer's mapping shares the vanilla table, so an association its class declares itself cannot point at another document. */
	@Test
	void anOverrideThatPointsAnAssociationItsClassDeclaresAtAnotherDocumentIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		vanillaOrder.putRelation(association("customer", "Customer"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		betaOrder.putRelation(association("customer", "Account"));
		Fixture fixture = new Fixture()
				.vanilla(vanillaOrder)
				.vanilla(document("Customer", "SALES_Customer", null, null, "name"))
				.vanilla(document("Account", "SALES_Account", null, null, "name"))
				.acme(document("Order", "SALES_Order", null, null, "code", "notes"))
				.beta(betaOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Customer beta's override of document sales.Order changes the document of customer from sales.Customer to sales.Account, " +
						"but the customer's mapping shares the vanilla table and its column of references to sales.Customer. " +
						"Keep customer referring to sales.Customer in the override, or add a relation of another name.",
						e.getMessage());
	}

	/** The customer's class inherits an inverse typed by the vanilla target's class, as for any relation. */
	@Test
	void anOverrideThatPointsAnInheritedInverseAtAnotherDocumentIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putRelation(inverse(new InverseMany(), "lines", "Line", "order", InverseRelationship.oneToMany));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putRelation(inverse(new InverseMany(), "lines", "Shipment", "order", InverseRelationship.oneToMany));
		Fixture fixture = ordersLinesAndShipments(vanillaOrder).acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Customer acme's override of document sales.Order changes the document of lines from sales.Line to sales.Shipment, " +
						"but the customer's class inherits the vanilla property, typed by sales.Line's class, and its mapping shares the vanilla table. " +
						"Keep lines referring to sales.Line in the override, or add a relation of another name.",
						e.getMessage());
	}

	/**
	 * Beta's override of a document acme's narrows declares an inverse itself, and the other document's
	 * table holds its key, so it can point at another document. The classes are not compiled, as the
	 * generated accessors of an association maintain its inverse, which they cannot once it is narrowed
	 * away or pointed elsewhere.
	 */
	@Test
	void anOverrideCanPointAnInverseItsClassDeclaresAtAnotherDocument() throws Exception {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		vanillaOrder.putRelation(inverse(new InverseMany(), "lines", "Line", "order", InverseRelationship.oneToMany));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		betaOrder.putRelation(inverse(new InverseMany(), "lines", "Shipment", "order", InverseRelationship.oneToMany));

		Map<String, String> files = generate(ordersLinesAndShipments(vanillaOrder)
				.acme(document("Order", "SALES_Order", null, null, "code", "notes"))
				.beta(betaOrder));

		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(() -> assertTrue(betaMapping.contains("<one-to-many entity-name=\"betasalesShipment\""), betaMapping),
					() -> assertEquals(List.of(), mappingProblems(files)));
	}

	/**
	 * Beta's override of a document acme's narrows declares a child collection itself, and the child
	 * document's table holds its key, so it can point at another document.
	 */
	@Test
	void anOverrideCanPointAChildCollectionItsClassDeclaresAtAnotherDocument() throws Exception {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putRelation(childCollection("lines", "Line"));
		vanillaOrder.putRelation(childCollection("notes", "Note"));
		DocumentImpl line = document("Line", "SALES_Line", null, null, "quantity");
		line.setParentDocumentName("Order");
		DocumentImpl note = document("Note", "SALES_Note", null, null, "text");
		note.setParentDocumentName("Order");
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putRelation(childCollection("notes", "Note"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code");
		betaOrder.putRelation(childCollection("lines", "Note"));
		betaOrder.putRelation(childCollection("notes", "Note"));

		Map<String, String> files = generate(new Fixture()
				.vanilla(vanillaOrder)
				.vanilla(line)
				.vanilla(note)
				.acme(acmeOrder)
				.beta(betaOrder));

		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(() -> assertTrue(betaMapping.contains("<one-to-many entity-name=\"betasalesNote\""), betaMapping),
					() -> assertEquals(List.of(), mappingProblems(files)),
					() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/** A collection its class declares itself is stored in the shared join table, so it cannot become another type. */
	@Test
	void anOverrideThatChangesTheTypeOfACollectionItsClassDeclaresIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		vanillaOrder.putRelation(aggregatedCollection("items", "Product"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		betaOrder.putRelation(association("items", "Product"));
		Fixture fixture = new Fixture()
				.vanilla(vanillaOrder)
				.vanilla(document("Product", "SALES_Product", null, null, "name"))
				.acme(document("Order", "SALES_Order", null, null, "code", "notes"))
				.beta(betaOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("Customer beta's override of document sales.Order changes the type of items from collection to association, " +
						"but the customer's mapping shares the vanilla table and its join table. " +
						"Keep items as collection in the override, or add an attribute of another name.",
						e.getMessage());
	}

	/** The vanilla Order, with Line and Shipment, which both reference Order. */
	private static Fixture ordersLinesAndShipments(DocumentImpl vanillaOrder) {
		DocumentImpl line = document("Line", "SALES_Line", null, null, "quantity");
		line.putRelation(association("order", "Order"));
		DocumentImpl shipment = document("Shipment", "SALES_Shipment", null, null, "carrier");
		shipment.putRelation(association("order", "Order"));
		return new Fixture().vanilla(vanillaOrder).vanilla(line).vanilla(shipment);
	}

	/** The customer's class inherits the enumeration, with the enum generated from the vanilla values. */
	@Test
	void anOverrideThatChangesTheCodesOfAnInheritedEnumerationIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putAttribute(enumeration(acmeOrder, "status", "low", "high", "urgent"));
		Fixture fixture = new Fixture().vanilla(vanillaOrder).acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture, false));
		assertEquals("Customer acme's override of document sales.Order changes the value codes or names, or the enum, of enumeration status, " +
						"but the customer's class inherits the vanilla property, whose enum is generated from the vanilla document. " +
						"Declare status as the vanilla document does in the override, or add an enumeration of another name.",
						e.getMessage());
	}

	/**
	 * Beta's override of a document with a vanilla extension extends the extension, so it inherits an
	 * enumeration acme's override narrows away from the vanilla class too.
	 */
	@Test
	void anOverrideThatChangesTheValuesOfAnEnumerationItInheritsThroughTheVanillaExtensionIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code");
		betaOrder.putAttribute(enumeration(betaOrder, "status", "low", "high", "urgent"));
		Fixture fixture = new Fixture()
				.vanilla(vanillaOrder)
				.extension("Order", "OrderVanilla")
				.acme(document("Order", "SALES_Order", null, null, "code"))
				.beta(betaOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertTrue(e.getMessage().startsWith("Customer beta's override of document sales.Order changes the value codes or names, or the enum, of enumeration status"), e.getMessage());
	}

	/** A value's Java identifier comes from its description unless it is named, so a new description can change it. */
	@Test
	void anOverrideThatChangesTheJavaIdentifierOfAnInheritedEnumerationsValueIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		Enumeration acmeStatus = enumeration(acmeOrder, "status");
		acmeStatus.getXmlValues().get(1).setDescription("Urgent");
		acmeOrder.putAttribute(acmeStatus);
		Fixture fixture = new Fixture().vanilla(vanillaOrder).acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertTrue(e.getMessage().startsWith("Customer acme's override of document sales.Order changes the value codes or names, or the enum, of enumeration status"),
					e.getMessage());
	}

	/**
	 * The customer's mapping names the enum nested in the class of the document that declares the
	 * enumeration the override's refers to, which is not the enum the customer's class inherits.
	 */
	@Test
	void anOverrideThatPointsAnInheritedEnumerationAtAnotherDocumentsIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl quote = document("Quote", "SALES_Quote", null, null, "code");
		quote.putAttribute(enumeration(quote, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putAttribute(enumerationReference(acmeOrder, "status", "Quote", "status"));
		Fixture fixture = new Fixture().vanilla(vanillaOrder).vanilla(quote).acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture, false));
		assertEquals("Customer acme's override of document sales.Order changes the value codes or names, or the enum, of enumeration status, " +
						"but the customer's class inherits the vanilla property, whose enum is generated from the vanilla document. " +
						"Declare status as the vanilla document does in the override, or add an enumeration of another name.",
						e.getMessage());
	}

	/**
	 * The vanilla enumeration refers to another document's, so the enum the customer's class inherits
	 * is nested in that document's class, and the override's own enumeration's enum is generated nowhere.
	 */
	@Test
	void anOverrideThatDeclaresAnInheritedReferencingEnumerationItselfIsRefused() {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumerationReference(vanillaOrder, "status", "Quote", "status"));
		DocumentImpl quote = document("Quote", "SALES_Quote", null, null, "code");
		quote.putAttribute(enumeration(quote, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putAttribute(enumeration(acmeOrder, "status"));
		Fixture fixture = new Fixture().vanilla(vanillaOrder).vanilla(quote).acme(acmeOrder);

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertTrue(e.getMessage().startsWith("Customer acme's override of document sales.Order changes the value codes or names, or the enum, of enumeration status"),
					e.getMessage());
	}

	/**
	 * The customer's class inherits the vanilla enum, so an override's descriptions and order of the
	 * same values are ignored, which generation warns about, whether or not validation is on.
	 */
	@Test
	void anOverrideThatChangesOnlyTheDescriptionsOrOrderOfAnInheritedEnumerationWarns() throws Exception {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		Enumeration acmeStatus = enumeration(acmeOrder, "status");
		EnumeratedValue high = acmeStatus.getXmlValues().get(1);
		high.setName("high"); // the Java identifier the vanilla description gives
		high.setDescription("Urgent");
		acmeOrder.putAttribute(acmeStatus);
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		betaOrder.putAttribute(enumeration(betaOrder, "status", "high", "low"));

		PrintStream previousErr = System.err;
		ByteArrayOutputStream err = new ByteArrayOutputStream();
		Map<String, String> files;
		System.setErr(new PrintStream(err, true, StandardCharsets.UTF_8));
		try {
			files = generate(new Fixture().vanilla(vanillaOrder).acme(acmeOrder).beta(betaOrder));
		}
		finally {
			System.setErr(previousErr);
		}

		String warnings = err.toString(StandardCharsets.UTF_8);
		String acmeOverride = files.get("customers/acme/modules/sales/domain/OrderOverride.java");
		assertAll(() -> assertTrue(warnings.contains("WARNING: Customer acme's override of document sales.Order changes the descriptions or order of the values of enumeration status, " +
														"which the customer's class inherits with the vanilla enum. " +
														"The override's descriptions and order are ignored, and the vanilla enum's apply."), warnings),
					() -> assertTrue(warnings.contains("WARNING: Customer beta's override of document sales.Order changes the descriptions or order of the values of enumeration status"), warnings),
					() -> assertFalse(acmeOverride.contains("enum Status"), acmeOverride),
					() -> assertEquals("modules.sales.domain.Order$Status", enumClass(files.get("customers/acme/modules/orm.hbm.xml"), "acmesalesOrder", "status")),
					() -> assertEquals(List.of(), mappingProblems(files)));
	}

	/**
	 * The checks run once every customer's narrowing is known: beta's override, later in the customer
	 * order, narrows away the enumeration acme's override changes, so acme's class declares it itself
	 * and it can have other values.
	 */
	@Test
	void anOverrideCanChangeTheValuesOfAnEnumerationALaterCustomerNarrowsAway() throws Exception {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putAttribute(enumeration(acmeOrder, "status", "low", "high", "urgent"));

		Map<String, String> files = generate(new Fixture()
				.vanilla(vanillaOrder)
				.acme(acmeOrder)
				.beta(document("Order", "SALES_Order", null, null, "code")));

		String acmeOverride = files.get("customers/acme/modules/sales/domain/OrderOverride.java");
		assertAll(() -> assertTrue(acmeOverride.contains("public enum Status implements Enumeration"), acmeOverride),
					() -> assertTrue(acmeOverride.contains("urgent"), acmeOverride),
					() -> assertEquals("customers.acme.modules.sales.domain.OrderOverride$Status",
										enumClass(files.get("customers/acme/modules/orm.hbm.xml"), "acmesalesOrder", "status")),
					() -> assertEquals(List.of(), mappingProblems(files)),
					() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/** An override can declare an inherited enumeration again as the vanilla document does. */
	@Test
	void anOverrideCanKeepAnInheritedEnumeration() throws Exception {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code", "notes");
		acmeOrder.putAttribute(enumeration(acmeOrder, "status"));

		Map<String, String> files = generate(new Fixture().vanilla(vanillaOrder).acme(acmeOrder));

		String acmeOverride = files.get("customers/acme/modules/sales/domain/OrderOverride.java");
		assertAll(() -> assertFalse(acmeOverride.contains("enum Status"), acmeOverride),
					() -> assertEquals(List.of(), mappingProblems(files)));
	}

	/**
	 * Beta's override of a document acme's narrows declares the enumeration itself, so it can have
	 * other values.
	 */
	@Test
	void anOverrideCanChangeTheValuesOfAnEnumerationItsClassDeclares() throws Exception {
		DocumentImpl vanillaOrder = document("Order", "SALES_Order", null, null, "code");
		vanillaOrder.putAttribute(enumeration(vanillaOrder, "status"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code");
		betaOrder.putAttribute(enumeration(betaOrder, "status", "low", "high", "urgent"));

		Map<String, String> files = generate(new Fixture()
				.vanilla(vanillaOrder)
				.acme(document("Order", "SALES_Order", null, null, "code"))
				.beta(betaOrder));

		String betaOverride = files.get("customers/beta/modules/sales/domain/OrderOverride.java");
		assertAll(() -> assertTrue(betaOverride.contains("public enum Status implements Enumeration"), betaOverride),
					() -> assertTrue(betaOverride.contains("urgent"), betaOverride),
					() -> assertEquals(List.of(), mappingProblems(files)));
	}

	/** Names the field, with the name as its display name too. */
	private static <T extends org.skyve.impl.metadata.model.document.field.Field> T named(T field, String name) {
		field.setName(name);
		field.setDisplayName(name);
		return field;
	}

	/** Adds the attribute to the document. */
	private static DocumentImpl withAttribute(DocumentImpl document, org.skyve.impl.metadata.model.document.field.Field attribute) {
		document.putAttribute(attribute);
		return document;
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

	@Test
	void aNewCustomerDocumentsEnumerationIsMappedToItsClass() throws Exception {
		DocumentImpl quote = document("Quote", "SALES_Quote", null, null, "code");
		quote.putAttribute(enumeration(quote, "status"));

		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(quote));

		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(files.get("customers/acme/modules/sales/domain/Quote.java").contains("public enum Status implements Enumeration"), "the new document's class declares it"),
				() -> assertTrue(acmeMapping.contains("<param name=\"enumClass\">customers.acme.modules.sales.domain.Quote$Status</param>"), acmeMapping),
				() -> assertEquals(List.of(), mappingProblems(files)),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/**
	 * Line's status references Order's, which acme's override leaves out, so the vanilla OrderVanilla
	 * declares it, for Line's class and for acme's copy of Line, which refers to the overridden Order.
	 */
	@Test
	void anEnumerationReferencedFromAnotherDocumentIsImportedAndMappedFromTheClassThatDeclaresIt() throws Exception {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.putAttribute(enumeration(order, "status"));
		DocumentImpl line = document("Line", "SALES_Line", null, null, "quantity");
		line.putRelation(association("order", "Order"));
		Enumeration lineStatus = new Enumeration();
		lineStatus.setOwningDocument(line);
		lineStatus.setName("orderStatus");
		lineStatus.setDisplayName("orderStatus");
		lineStatus.setDocumentRef("Order");
		lineStatus.setAttributeRef("status");
		line.putAttribute(lineStatus);

		Map<String, String> files = generate(new Fixture()
				.vanilla(order)
				.vanilla(line)
				.acme(document("Order", "SALES_Order", null, null, "code")));

		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(files.get("modules/sales/domain/Line.java").contains("import modules.sales.domain.OrderVanilla.Status;"), files.get("modules/sales/domain/Line.java")),
				() -> assertTrue(vanillaMapping.contains("<param name=\"enumClass\">modules.sales.domain.OrderVanilla$Status</param>"), vanillaMapping),
				() -> assertTrue(acmeMapping.contains("entity-name=\"acmesalesLine\""), acmeMapping),
				() -> assertFalse(acmeMapping.contains("OrderOverride$Status"), acmeMapping),
				() -> assertEquals(List.of(), mappingProblems(files)),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/**
	 * Acme's override leaves out Order's status, and beta's keeps it, so beta's OrderOverride declares
	 * its own Status. A property referencing it has the type its declaring class imports: vanilla
	 * Line's, mapped in beta's copy of Line, and the one beta's override of Memo inherits, have
	 * OrderVanilla's Status, and the one beta's override of Memo adds has OrderOverride's.
	 */
	@Test
	void anEnumerationReferencedFromAnotherDocumentIsMappedFromTheClassesItsDeclaringClassImportsFrom() throws Exception {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.putAttribute(enumeration(order, "status"));
		DocumentImpl betaOrder = document("Order", "SALES_Order", null, null, "code");
		betaOrder.putAttribute(enumeration(betaOrder, "status"));
		DocumentImpl line = document("Line", "SALES_Line", null, null, "quantity");
		line.putRelation(association("order", "Order"));
		line.putAttribute(orderStatusReference(line, "orderStatus"));
		DocumentImpl memo = document("Memo", "SALES_Memo", null, null, "text");
		memo.putRelation(association("order", "Order"));
		memo.putAttribute(orderStatusReference(memo, "orderStatus"));
		DocumentImpl betaMemo = document("Memo", "SALES_Memo", null, null, "text");
		betaMemo.putRelation(association("order", "Order"));
		betaMemo.putAttribute(orderStatusReference(betaMemo, "orderStatus"));
		betaMemo.putAttribute(orderStatusReference(betaMemo, "priorStatus"));

		Map<String, String> files = generate(new Fixture()
				.vanilla(order)
				.vanilla(line)
				.vanilla(memo)
				.acme(document("Order", "SALES_Order", null, null, "code"))
				.beta(betaOrder)
				.beta(betaMemo));

		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		String betaMemoOverride = files.get("customers/beta/modules/sales/domain/MemoOverride.java");
		assertAll(
				() -> assertTrue(files.get("customers/beta/modules/sales/domain/OrderOverride.java").contains("public enum Status implements Enumeration"), "beta's override declares it again"),
				() -> assertEquals("modules.sales.domain.OrderVanilla$Status", enumClass(betaMapping, "betasalesLine", "orderStatus")),
				() -> assertEquals("modules.sales.domain.OrderVanilla$Status", enumClass(betaMapping, "betasalesMemo", "orderStatus")),
				() -> assertEquals("customers.beta.modules.sales.domain.OrderOverride$Status", enumClass(betaMapping, "betasalesMemo", "priorStatus")),
				() -> assertTrue(betaMemoOverride.contains("import customers.beta.modules.sales.domain.OrderOverride.Status;"), betaMemoOverride),
				() -> assertFalse(betaMemoOverride.contains("private Status orderStatus"), "inherited: " + betaMemoOverride),
				() -> assertEquals(List.of(), mappingProblems(files)),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/** An enumeration of the document that references Order's status. */
	private static Enumeration orderStatusReference(Document owningDocument, String name) {
		return enumerationReference(owningDocument, name, "Order", "status");
	}

	/** An enumeration of the document that references another document's enumeration. */
	private static Enumeration enumerationReference(Document owningDocument, String name, String documentName, String attributeName) {
		Enumeration result = new Enumeration();
		result.setOwningDocument(owningDocument);
		result.setName(name);
		result.setDisplayName(name);
		result.setDocumentRef(documentName);
		result.setAttributeRef(attributeName);
		return result;
	}

	/** The enum class a mapping's entity maps an enumeration property to. */
	private static String enumClass(String mapping, String entityName, String propertyName) throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
		org.w3c.dom.Document xml = factory.newDocumentBuilder().parse(new ByteArrayInputStream(mapping.getBytes(StandardCharsets.UTF_8)));
		NodeList classes = xml.getElementsByTagName("class");
		for (int i = 0, l = classes.getLength(); i < l; i++) {
			Element entity = (Element) classes.item(i);
			if (entityName.equals(entity.getAttribute("entity-name"))) {
				for (Node child = entity.getFirstChild(); child != null; child = child.getNextSibling()) {
					if ((child instanceof Element property) && propertyName.equals(property.getAttribute("name"))) {
						return property.getElementsByTagName("param").item(0).getTextContent().trim();
					}
				}
			}
		}
		return null;
	}

	private static Enumeration enumeration(Document owningDocument, String name) {
		return enumeration(owningDocument, name, "low", "high");
	}

	private static Enumeration enumeration(Document owningDocument, String name, String... codes) {
		Enumeration result = new Enumeration();
		result.setOwningDocument(owningDocument);
		result.setName(name);
		result.setDisplayName(name);
		for (String code : codes) {
			EnumeratedValue value = new EnumeratedValue();
			value.setCode(code);
			value.setDescription(code);
			result.getXmlValues().add(value);
		}
		return result;
	}

	// ----- documents with a hand-written extension -----------------------------

	@Test
	void anOverrideOfADocumentWithAnExtensionExtendsTheExtension() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Invoice", "SALES_Invoice", null, null, "code"))
				.extension("Invoice")
				.acme(document("Invoice", "SALES_Invoice", null, null, "code", "notes")));

		String override = files.get("customers/acme/modules/sales/domain/InvoiceOverride.java");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		assertAll(
				() -> assertTrue(override.contains("public class InvoiceOverride extends InvoiceExtension"), "concrete, as acme has no extension of its own: " + override),
				() -> assertTrue(override.contains("import modules.sales.Invoice.InvoiceExtension;"), override),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.InvoiceOverride\""), acmeMapping),
				() -> assertTrue(vanillaMapping.contains("<class name=\"modules.sales.Invoice.InvoiceExtension\""), vanillaMapping));
	}

	@Test
	void anOverrideWithItsOwnExtensionIsAbstractAndMapsTheExtension() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Invoice", "SALES_Invoice", null, null, "code"))
				.extension("Invoice")
				.acme(document("Invoice", "SALES_Invoice", null, null, "code", "notes"))
				.acmeExtension("Invoice"));

		String override = files.get("customers/acme/modules/sales/domain/InvoiceOverride.java");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(override.contains("public abstract class InvoiceOverride extends InvoiceExtension"), override),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.Invoice.InvoiceExtension\""), acmeMapping));
	}

	@Test
	void aNarrowedDocumentWithAnExtensionMapsItsVanillaClass() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "ReceiptVanilla")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code")));

		String domainClass = files.get("modules/sales/domain/Receipt.java");
		String vanillaClass = files.get("modules/sales/domain/ReceiptVanilla.java");
		String override = files.get("customers/acme/modules/sales/domain/ReceiptOverride.java");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				// Receipt <- ReceiptVanilla <- ReceiptExtension
				() -> assertTrue(domainClass.contains("public abstract class Receipt extends AbstractPersistentBean"), domainClass),
				() -> assertFalse(domainClass.contains("description"), domainClass),
				() -> assertTrue(vanillaClass.contains("public abstract class ReceiptVanilla extends Receipt "), "abstract, as Receipt has an extension: " + vanillaClass),
				() -> assertFalse(vanillaClass.contains("import modules.sales.Receipt.ReceiptExtension;"), vanillaClass),
				() -> assertTrue(vanillaClass.contains("private String description"), "ReceiptVanilla declares what acme leaves out: " + vanillaClass),
				// the vanilla mapping maps the extension, which inherits description from ReceiptVanilla
				() -> assertTrue(vanillaMapping.contains("<class name=\"modules.sales.Receipt.ReceiptExtension\""), vanillaMapping),
				() -> assertTrue(vanillaMapping.contains("<property name=\"description\""), vanillaMapping),
				// acme's override extends the extension, inheriting description unmapped
				() -> assertTrue(override.contains("public class ReceiptOverride extends ReceiptExtension"), override),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.ReceiptOverride\""), acmeMapping),
				() -> assertFalse(acmeMapping.contains("name=\"description\""), acmeMapping));
	}

	@Test
	void aNarrowedDocumentsExtensionMustExtendItsVanillaClass() {
		Fixture fixture = new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "Receipt")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code"));

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("ReceiptExtension must extend ReceiptVanilla because customer acme's override of Receipt leaves out attributes, which only ReceiptVanilla declares.",
						e.getMessage());
	}

	@Test
	void anExtensionMustNotExtendAVanillaClassThatIsNotGenerated() {
		Fixture fixture = new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "modules.sales.domain.ReceiptVanilla");

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		assertEquals("ReceiptExtension must extend Receipt because no customer's override of Receipt leaves out attributes, so ReceiptVanilla is not generated.",
						e.getMessage());
	}

	@Test
	void aWrongExtensionSuperclassOnlyWarnsWithValidationOff() throws Exception {
		Fixture narrowed = new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "Receipt")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code"));
		Fixture notNarrowed = new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "ReceiptVanilla");

		PrintStream previousErr = System.err;
		ByteArrayOutputStream err = new ByteArrayOutputStream();
		Map<String, String> narrowedFiles;
		Map<String, String> notNarrowedFiles;
		System.setErr(new PrintStream(err, true, StandardCharsets.UTF_8));
		try {
			narrowedFiles = generate(narrowed, false);
			notNarrowedFiles = generate(notNarrowed, false);
		}
		finally {
			System.setErr(previousErr);
		}

		String warnings = err.toString(StandardCharsets.UTF_8);
		assertAll(
				() -> assertTrue(warnings.contains("WARNING: ReceiptExtension must extend ReceiptVanilla because customer acme's override of Receipt leaves out attributes"), warnings),
				() -> assertTrue(warnings.contains("WARNING: ReceiptExtension must extend Receipt because no customer's override of Receipt leaves out attributes"), warnings),
				// generation carried on and wrote the output
				() -> assertTrue(narrowedFiles.containsKey("modules/sales/domain/ReceiptVanilla.java"), narrowedFiles.keySet().toString()),
				() -> assertTrue(narrowedFiles.containsKey("customers/acme/modules/orm.hbm.xml"), narrowedFiles.keySet().toString()),
				() -> assertTrue(notNarrowedFiles.containsKey("modules/sales/domain/sales_orm.hbm.xml"), notNarrowedFiles.keySet().toString()));
	}

	/**
	 * A customer's own extension must extend the class generated for the customer, which the customer's
	 * mapping maps through it. Extending the vanilla class or the vanilla extension compiles, but
	 * Hibernate fails at startup.
	 */
	@Test
	void aCustomersExtensionMustExtendItsOverrideClass() {
		Fixture extendingTheVanillaClass = new Fixture()
				.vanilla(document("Invoice", "SALES_Invoice", null, null, "code"))
				.acme(document("Invoice", "SALES_Invoice", null, null, "code", "notes"))
				.acmeExtension("Invoice", "modules.sales.domain.Invoice");
		Fixture extendingTheVanillaExtension = new Fixture()
				.vanilla(document("Invoice", "SALES_Invoice", null, null, "code"))
				.extension("Invoice")
				.acme(document("Invoice", "SALES_Invoice", null, null, "code"))
				.acmeExtension("Invoice", "modules.sales.Invoice.InvoiceExtension");

		MetaDataException vanillaClass = assertThrows(MetaDataException.class, () -> generate(extendingTheVanillaClass));
		MetaDataException vanillaExtension = assertThrows(MetaDataException.class, () -> generate(extendingTheVanillaExtension));
		assertAll(() -> assertEquals("Customer acme's InvoiceExtension must extend InvoiceOverride, the class generated for its override of sales.Invoice, not Invoice.",
										vanillaClass.getMessage()),
					() -> assertEquals("Customer acme's InvoiceExtension must extend InvoiceOverride, the class generated for its override of sales.Invoice, not InvoiceExtension.",
										vanillaExtension.getMessage()));
	}

	@Test
	void aCustomersExtensionOfANewDocumentMustExtendItsClass() throws Exception {
		Fixture wrong = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(document("Quote", "SALES_Quote", null, null, "code"))
				.acmeExtension("Quote", "customers.acme.modules.sales.domain.QuoteOverride");
		Fixture right = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(document("Quote", "SALES_Quote", null, null, "code"))
				.acmeExtension("Quote", "customers.acme.modules.sales.domain.Quote");

		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(wrong));
		Map<String, String> files = generate(right);
		assertAll(() -> assertEquals("Customer acme's QuoteExtension must extend Quote, the class generated for its new document sales.Quote, not QuoteOverride.",
										e.getMessage()),
					() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void aWrongCustomerExtensionSuperclassOnlyWarnsWithValidationOff() throws Exception {
		Fixture fixture = new Fixture()
				.vanilla(document("Invoice", "SALES_Invoice", null, null, "code"))
				.acme(document("Invoice", "SALES_Invoice", null, null, "code", "notes"))
				.acmeExtension("Invoice", "modules.sales.domain.Invoice");

		PrintStream previousErr = System.err;
		ByteArrayOutputStream err = new ByteArrayOutputStream();
		Map<String, String> files;
		System.setErr(new PrintStream(err, true, StandardCharsets.UTF_8));
		try {
			files = generate(fixture, false);
		}
		finally {
			System.setErr(previousErr);
		}

		String warnings = err.toString(StandardCharsets.UTF_8);
		assertAll(() -> assertTrue(warnings.contains("WARNING: Customer acme's InvoiceExtension must extend InvoiceOverride"), warnings),
					() -> assertTrue(files.containsKey("customers/acme/modules/orm.hbm.xml"), files.keySet().toString()));
	}

	@Test
	void hierarchyRefusalsIgnoreTheValidationFlag() {
		Fixture fixture = hierarchy(ExtensionStrategy.joined, true, true, false);
		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture, false));
		assertTrue(e.getMessage().contains("Customer acme overrides document sales.Base, which other documents extend"), e.getMessage());
	}

	/**
	 * Validation checks the metadata and the extensions' superclasses, so generating without it
	 * writes exactly the same files.
	 */
	@Test
	void validationOnAndOffGenerateTheSameFiles() throws Exception {
		Fixture fixture = narrowedOrder()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "ReceiptVanilla")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code"));
		fixture.useBetaLine();

		Map<String, String> validated = generate(fixture, true);
		assertEquals(validated, generate(fixture, false));
	}

	@Test
	void anExtensionExtendingAnIntermediateClassIsNotChecked() throws Exception {
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "ReceiptBehaviour")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code")));

		assertTrue(files.containsKey("modules/sales/domain/ReceiptVanilla.java"), files.keySet().toString());
	}

	// ----- overrides in an inheritance hierarchy -------------------------------

	/**
	 * Vanilla Base and Sub extends Base for the strategy. A mapped Base has no table; its Sub is then the
	 * root of its own (joined) hierarchy. The joined Sub has its own table, the single Sub shares Base's.
	 * acme's override of Base adds baseNotes when {@code changeBase}, and its override of Sub adds subNotes.
	 */
	private static Fixture hierarchy(ExtensionStrategy strategy, boolean overrideBase, boolean changeBase, boolean overrideSub) {
		boolean mapped = ExtensionStrategy.mapped.equals(strategy);
		String baseTable = mapped ? null : "SALES_Base";
		ExtensionStrategy subStrategy = mapped ? ExtensionStrategy.joined : strategy;
		String subTable = ExtensionStrategy.single.equals(strategy) ? "SALES_Base" : "SALES_Sub";
		Fixture result = new Fixture()
				.vanilla(document("Base", baseTable, strategy, null, "code"))
				.vanilla(document("Sub", subTable, subStrategy, "Base", "sub"));
		if (overrideBase) {
			result.acme(changeBase ?
							document("Base", baseTable, strategy, null, "code", "baseNotes") :
							document("Base", baseTable, strategy, null, "code"));
		}
		if (overrideSub) {
			result.acme(document("Sub", subTable, subStrategy, "Base", "sub", "subNotes"));
		}
		return result;
	}

	private static Map<String, String> assertConsistentMappings(Fixture fixture) throws Exception {
		Map<String, String> files = generate(fixture);
		assertEquals(List.of(), mappingProblems(files), String.valueOf(files.get("customers/acme/modules/orm.hbm.xml")));
		return files;
	}

	/**
	 * Asserts that overriding a subclass copies the hierarchy into acme's mapping from its top, with the
	 * subclass's override class nested in the vanilla base class's mapping.
	 */
	private static void assertSubclassOverrideMapped(ExtensionStrategy strategy) throws Exception {
		Map<String, String> files = assertConsistentMappings(hierarchy(strategy, false, false, true));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		String subOverride = files.get("customers/acme/modules/sales/domain/SubOverride.java");
		assertAll(
				() -> assertTrue(subOverride.contains("public class SubOverride extends Sub"), subOverride),
				() -> assertTrue(acmeMapping.contains("name=\"customers.acme.modules.sales.domain.SubOverride\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("entity-name=\"acmesalesSub\""), acmeMapping),
				() -> assertTrue(ExtensionStrategy.mapped.equals(strategy) ||
									acmeMapping.contains("<class name=\"modules.sales.domain.Base\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("name=\"subNotes\""), acmeMapping));
	}

	private static void assertRefused(Fixture fixture, String... messageParts) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> generate(fixture));
		for (String part : messageParts) {
			assertTrue(e.getMessage().contains(part), e.getMessage());
		}
	}

	@Test
	void overridingAJoinedSubclassGivesConsistentMappings() throws Exception {
		assertSubclassOverrideMapped(ExtensionStrategy.joined);
	}

	@Test
	void overridingASingleSubclassGivesConsistentMappings() throws Exception {
		assertSubclassOverrideMapped(ExtensionStrategy.single);
	}

	@Test
	void overridingAMappedSubclassGivesConsistentMappings() throws Exception {
		assertSubclassOverrideMapped(ExtensionStrategy.mapped);
	}

	@Test
	void overridingAJoinedBaseWithoutChangesGivesConsistentMappings() throws Exception {
		Map<String, String> files = assertConsistentMappings(hierarchy(ExtensionStrategy.joined, true, false, false));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(acmeMapping.contains("<class name=\"modules.sales.domain.Base\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("<joined-subclass name=\"modules.sales.domain.Sub\""), acmeMapping));
	}

	@Test
	void overridingASingleBaseWithoutChangesGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.single, true, false, false));
	}

	@Test
	void overridingAMappedBaseWithoutChangesGivesConsistentMappings() throws Exception {
		Map<String, String> files = assertConsistentMappings(hierarchy(ExtensionStrategy.mapped, true, false, false));
		assertTrue(files.get("customers/acme/modules/orm.hbm.xml").contains("<class name=\"modules.sales.domain.Sub\""));
	}

	@Test
	void overridingAJoinedBaseAndSubclassWithoutChangingTheBaseGivesConsistentMappings() throws Exception {
		assertConsistentMappings(hierarchy(ExtensionStrategy.joined, true, false, true));
	}

	@Test
	void overridingAJoinedBaseThatAddsPropertiesIsRefused() {
		assertRefused(hierarchy(ExtensionStrategy.joined, true, true, false), "Customer acme overrides document sales.Base, which other documents extend");
	}

	@Test
	void overridingAJoinedBaseAndSubclassThatAddsPropertiesIsRefused() {
		assertRefused(hierarchy(ExtensionStrategy.joined, true, true, true), "Customer acme overrides document sales.Base", "Add the properties to the vanilla document");
	}

	@Test
	void overridingASingleBaseThatAddsPropertiesIsRefused() {
		assertRefused(hierarchy(ExtensionStrategy.single, true, true, false), "Customer acme overrides document sales.Base, which other documents extend");
	}

	@Test
	void overridingASingleBaseAndSubclassThatAddsPropertiesIsRefused() {
		assertRefused(hierarchy(ExtensionStrategy.single, true, true, true), "Customer acme overrides document sales.Base");
	}

	@Test
	void overridingAMappedBaseThatAddsPropertiesIsRefused() {
		assertRefused(hierarchy(ExtensionStrategy.mapped, true, true, false), "Customer acme overrides document sales.Base, which other documents extend");
	}

	@Test
	void overridingAMappedBaseAndSubclassThatAddsPropertiesIsRefused() {
		assertRefused(hierarchy(ExtensionStrategy.mapped, true, true, true), "Customer acme overrides document sales.Base");
	}

	@Test
	void narrowingAJoinedBaseIsRefused() {
		assertRefused(new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code", "description"))
				.vanilla(document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub"))
				.acme(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code")),
				"Customer acme's override of document sales.Base leaves out description, but other documents extend Base",
				"Keep description in the override");
	}

	@Test
	void narrowingAJoinedSubclassGivesConsistentMappings() throws Exception {
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code"))
				.vanilla(document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub", "description"))
				.acme(document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub")));
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(vanillaMapping.contains("<joined-subclass name=\"modules.sales.domain.SubVanilla\""), vanillaMapping),
				() -> assertTrue(acmeMapping.contains("<joined-subclass name=\"customers.acme.modules.sales.domain.SubOverride\""), acmeMapping));
	}

	/**
	 * Narrows Sub of {@link #hierarchy} for the strategy and checks that Sub's domain class is abstract,
	 * so that customers without the override get the concrete <code>SubVanilla</code>, which the vanilla
	 * mapping maps, and that the generated sources compile.
	 */
	private static void assertNarrowedSubclassIsAbstract(ExtensionStrategy strategy) throws Exception {
		Fixture fixture = hierarchy(strategy, false, false, false);
		DocumentImpl sub = fixture.vanilla.get("Sub");
		Persistent persistent = sub.getPersistent();
		assertNotNull(persistent);
		fixture.vanilla(document("Sub", persistent.getName(), persistent.getStrategy(), "Base", "sub", "description"))
				.acme(sub);
		Map<String, String> files = assertConsistentMappings(fixture);
		String subClass = files.get("modules/sales/domain/Sub.java");
		String subVanilla = files.get("modules/sales/domain/SubVanilla.java");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		assertAll(
				() -> assertTrue(subClass.contains("public abstract class Sub extends Base"), subClass),
				() -> assertTrue(subVanilla.contains("public class SubVanilla extends Sub"), subVanilla),
				() -> assertTrue(vanillaMapping.contains("name=\"modules.sales.domain.SubVanilla\""), vanillaMapping),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void aNarrowedJoinedSubclassIsAbstract() throws Exception {
		assertNarrowedSubclassIsAbstract(ExtensionStrategy.joined);
	}

	@Test
	void aNarrowedSingleSubclassIsAbstract() throws Exception {
		assertNarrowedSubclassIsAbstract(ExtensionStrategy.single);
	}

	@Test
	void aNarrowedSubclassOfAMappedDocumentIsAbstract() throws Exception {
		assertNarrowedSubclassIsAbstract(ExtensionStrategy.mapped);
	}

	private static final String NEW_DOCUMENT_EXTENDS = "the class generated for a new customer document cannot extend another document's class";

	@Test
	void aNewCustomerDocumentExtendingAJoinedDocumentIsRefused() {
		Fixture fixture = new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code"))
				.acme(document("Extra", "SALES_Extra", ExtensionStrategy.joined, "Base", "extra"));
		assertRefused(fixture, "Customer acme's new document sales.Extra extends document Base", NEW_DOCUMENT_EXTENDS);
	}

	@Test
	void aNewCustomerDocumentExtendingAMappedDocumentIsRefused() {
		// its class would extend AbstractPersistentBean and lack the inherited code its mapping maps
		Fixture fixture = new Fixture()
				.vanilla(document("Base", null, ExtensionStrategy.mapped, null, "code"))
				.acme(document("Extra", "SALES_Extra", ExtensionStrategy.joined, "Base", "extra"));
		assertRefused(fixture, "Customer acme's new document sales.Extra extends document Base", NEW_DOCUMENT_EXTENDS);
	}

	@Test
	void aCustomersOwnNewHierarchyIsRefused() {
		Fixture fixture = new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(document("NewBase", "SALES_NewBase", ExtensionStrategy.joined, null, "code"))
				.acme(document("NewSub", "SALES_NewSub", ExtensionStrategy.joined, "NewBase", "sub"));
		assertRefused(fixture, "Customer acme's new document sales.NewSub extends document NewBase", NEW_DOCUMENT_EXTENDS);
	}

	@Test
	void overridingATransientDocumentThatExtendsAJoinedDocumentIsNotRefused() throws Exception {
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code"))
				.vanilla(document("Wizard", null, null, "Base", "step"))
				.acme(document("Wizard", null, null, "Base", "step", "notes")));
		assertTrue(files.get("customers/acme/modules/sales/domain/WizardOverride.java").contains("public class WizardOverride extends Wizard"),
					files.keySet().toString());
	}

	// ----- documents copied into a customer mapping ---------------------------------

	/**
	 * Vanilla Party extends the mapped Proxy with no strategy of its own - as admin.User extends
	 * admin.UserProxy - so it is mapped as its own class on Proxy's table.
	 */
	private static Fixture proxied() {
		return new Fixture()
				.vanilla(document("Proxy", "SALES_Party", ExtensionStrategy.mapped, null, "code"))
				.vanilla(document("Party", "SALES_Party", null, "Proxy", "name"));
	}

	@Test
	void overridingANoStrategySubclassOfAMappedDocumentMapsTheOverrideClass() throws Exception {
		Map<String, String> files = assertConsistentMappings(proxied()
				.acme(document("Party", "SALES_Party", null, "Proxy", "name", "employeeNo")));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.PartyOverride\" table=\"SALES_Party\" entity-name=\"acmesalesParty\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("<property name=\"employeeNo\""), acmeMapping));
	}

	@Test
	void overridingANoStrategySubclassOfAMappedDocumentWithAnExtensionMapsTheOverrideClass() throws Exception {
		Map<String, String> files = assertConsistentMappings(proxied()
				.extension("Party")
				.acme(document("Party", "SALES_Party", null, "Proxy", "name", "employeeNo")));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		assertAll(
				() -> assertTrue(files.get("customers/acme/modules/sales/domain/PartyOverride.java").contains("public class PartyOverride extends PartyExtension")),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.PartyOverride\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("<property name=\"employeeNo\""), acmeMapping),
				() -> assertTrue(vanillaMapping.contains("<class name=\"modules.sales.Party.PartyExtension\""), vanillaMapping));
	}

	@Test
	void narrowingANoStrategySubclassOfAMappedDocumentMapsTheGeneratedSubclasses() throws Exception {
		Map<String, String> files = assertConsistentMappings(proxied()
				.acme(document("Party", "SALES_Party", null, "Proxy")));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		String vanillaMapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		assertAll(
				() -> assertTrue(vanillaMapping.contains("<class name=\"modules.sales.domain.PartyVanilla\""), vanillaMapping),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.domain.PartyOverride\""), acmeMapping));
	}

	/** Order has description, which acme's override leaves out; Order references Client and has Lines. */
	private static Fixture narrowedOrder() {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code", "description");
		order.putRelation(association("client", "Client"));
		order.putRelation(childCollection("lines", "Line"));
		DocumentImpl acmeOrder = document("Order", "SALES_Order", null, null, "code");
		acmeOrder.putRelation(association("client", "Client"));
		acmeOrder.putRelation(childCollection("lines", "Line"));
		DocumentImpl line = document("Line", "SALES_Line", null, null, "quantity");
		line.setParentDocumentName("Order");
		DocumentImpl betaLine = document("Line", "SALES_Line", null, null, "quantity", "notes");
		betaLine.setParentDocumentName("Order");
		DocumentImpl header = document("Header", "SALES_Header", null, null, "title");
		header.putRelation(association("order", "Order"));
		return new Fixture()
				.vanilla(order)
				.vanilla(document("Client", "SALES_Client", null, null, "name"))
				.vanilla(line)
				.vanilla(header)
				.acme(acmeOrder)
				.betaLine(betaLine);
	}

	/** Asserts beta's copy of the narrowed Order - which beta does not override - maps OrderVanilla. */
	private static void assertCopiedNarrowedOrderMapsVanillaClass(Fixture fixture) throws Exception {
		Map<String, String> files = assertConsistentMappings(fixture);
		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(betaMapping.contains("<class name=\"modules.sales.domain.OrderVanilla\" table=\"SALES_Order\" entity-name=\"betasalesOrder\""), betaMapping),
				() -> assertTrue(betaMapping.contains("<property name=\"description\""), betaMapping));
	}

	@Test
	void aNarrowedDocumentCopiedThroughAnExportedReferenceMapsItsVanillaClass() throws Exception {
		Fixture fixture = narrowedOrder();
		fixture.beta(document("Client", "SALES_Client", null, null, "name", "notes"));
		assertCopiedNarrowedOrderMapsVanillaClass(fixture);
	}

	@Test
	void aNarrowedDocumentCopiedAsAParentMapsItsVanillaClass() throws Exception {
		Fixture fixture = narrowedOrder();
		fixture.useBetaLine();
		assertCopiedNarrowedOrderMapsVanillaClass(fixture);
	}

	@Test
	void aNarrowedDocumentCopiedAsAChildMapsItsVanillaClass() throws Exception {
		// beta overrides Header, whose reference to Order copies Order, then Order's children
		Fixture fixture = new Fixture();
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.setParentDocumentName("Header");
		DocumentImpl line = document("Line", "SALES_Line", null, null, "quantity", "description");
		line.setParentDocumentName("Order");
		order.putRelation(childCollection("lines", "Line"));
		DocumentImpl acmeLine = document("Line", "SALES_Line", null, null, "quantity");
		acmeLine.setParentDocumentName("Order");
		DocumentImpl header = document("Header", "SALES_Header", null, null, "title");
		header.putRelation(childCollection("orders", "Order"));
		DocumentImpl betaHeader = document("Header", "SALES_Header", null, null, "title", "notes");
		betaHeader.putRelation(childCollection("orders", "Order"));
		fixture.vanilla(header).vanilla(order).vanilla(line).acme(acmeLine).beta(betaHeader);

		Map<String, String> files = assertConsistentMappings(fixture);
		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(betaMapping.contains("<class name=\"modules.sales.domain.LineVanilla\" table=\"SALES_Line\" entity-name=\"betasalesLine\""), betaMapping),
				() -> assertTrue(betaMapping.contains("<property name=\"description\""), betaMapping));
	}

	@Test
	void aCopiedNarrowedSubclassMapsItsEnumerationsFromItsVanillaClass() throws Exception {
		DocumentImpl sub = document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub");
		sub.putAttribute(enumeration(sub, "status"));
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code"))
				.vanilla(sub)
				.beta(document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub"))
				.acme(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code")));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(acmeMapping.contains("<joined-subclass name=\"modules.sales.domain.SubVanilla\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("modules.sales.domain.SubVanilla$Status"), acmeMapping));
	}

	// ----- generated sources compile ------------------------------------------------

	@Test
	void anOverrideOfANarrowedDocumentWithAnExtensionDoesNotRedeclareTheNarrowedAwayProperties() throws Exception {
		DocumentImpl receipt = document("Receipt", "SALES_Receipt", null, null, "code", "description");
		receipt.putAttribute(enumeration(receipt, "status"));
		DocumentImpl betaReceipt = document("Receipt", "SALES_Receipt", null, null, "code", "description", "notes");
		betaReceipt.putAttribute(enumeration(betaReceipt, "status"));
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(receipt)
				.extension("Receipt", "ReceiptVanilla")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code"))
				.beta(betaReceipt));

		String betaOverride = files.get("customers/beta/modules/sales/domain/ReceiptOverride.java");
		String betaMapping = files.get("customers/beta/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(betaOverride.contains("public class ReceiptOverride extends ReceiptExtension"), betaOverride),
				() -> assertTrue(betaOverride.contains("private String notes"), betaOverride),
				() -> assertFalse(betaOverride.contains("enum Status"), "inherited from ReceiptVanilla: " + betaOverride),
				() -> assertFalse(betaOverride.contains("private String description"), "inherited from ReceiptVanilla: " + betaOverride),
				() -> assertTrue(betaMapping.contains("modules.sales.domain.ReceiptVanilla$Status"), betaMapping),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void theGeneratedSourcesOfOverridesInAHierarchyCompile() throws Exception {
		DocumentImpl sub = document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub", "description");
		sub.putAttribute(enumeration(sub, "status"));
		DocumentImpl acmeSub = document("Sub", "SALES_Sub", ExtensionStrategy.joined, "Base", "sub", "notes");
		acmeSub.putAttribute(enumeration(acmeSub, "priority"));
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code"))
				.vanilla(sub)
				.acme(acmeSub)
				.beta(document("Base", "SALES_Base", ExtensionStrategy.joined, null, "code")));
		assertEquals(List.of(), compilationErrors(files));
	}

	// ----- reading the extension source -----------------------------------------------

	@Test
	void anExtensionInLatin1IsChecked() {
		byte[] source = ("package modules.sales.Receipt;\n// Gr\u00fc\u00dfe\npublic class ReceiptExtension extends modules.sales.domain.Receipt {\n}\n")
							.getBytes(StandardCharsets.ISO_8859_1);
		Fixture fixture = new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extensionBytes("Receipt", source)
				.acme(document("Receipt", "SALES_Receipt", null, null, "code"));
		assertRefused(fixture, "ReceiptExtension must extend ReceiptVanilla");
	}

	@Test
	void anExtensionsCommentsAndLiteralsAreIgnored() throws Exception {
		String source = """
				package modules.sales.Receipt;

				import modules.sales.domain.ReceiptVanilla;

				/**
				 * Not: public class ReceiptExtension extends Receipt {
				 */
				public class ReceiptExtension /* extends Receipt */ // extends Receipt
				\t\textends ReceiptVanilla {
				\tprivate static final String TEXT = "class ReceiptExtension extends Receipt {";
				\tprivate static final long serialVersionUID = 1L;
				}
				""";
		Map<String, String> files = generate(new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extensionSource("Receipt", source)
				.acme(document("Receipt", "SALES_Receipt", null, null, "code")));
		assertTrue(files.containsKey("modules/sales/domain/ReceiptVanilla.java"), files.keySet().toString());
	}

	@Test
	void anUnreadableExtensionIsSkippedWithAWarning() throws Exception {
		Fixture fixture = new Fixture()
				.vanilla(document("Receipt", "SALES_Receipt", null, null, "code", "description"))
				.extension("Receipt", "Receipt")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code"));
		fixture.unreadableExtensions = true;

		PrintStream previousErr = System.err;
		ByteArrayOutputStream err = new ByteArrayOutputStream();
		Map<String, String> files;
		System.setErr(new PrintStream(err, true, StandardCharsets.UTF_8));
		try {
			files = generate(fixture);
		}
		finally {
			System.setErr(previousErr);
		}
		String warnings = err.toString(StandardCharsets.UTF_8);
		assertAll(
				() -> assertTrue(warnings.contains("WARNING: Could not read"), warnings),
				() -> assertTrue(files.containsKey("modules/sales/domain/ReceiptVanilla.java"), files.keySet().toString()));
	}

	// ----- a customer's own extension ---------------------------------------------

	@Test
	void anUnchangedOverrideWithItsOwnExtensionGetsAnOverrideClass() throws Exception {
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Invoice", "SALES_Invoice", null, null, "code"))
				.acme(document("Invoice", "SALES_Invoice", null, null, "code"))
				.acmeExtension("Invoice"));
		String override = files.get("customers/acme/modules/sales/domain/InvoiceOverride.java");
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertNotNull(override, files.keySet()::toString),
				() -> assertTrue(override.contains("public abstract class InvoiceOverride extends Invoice"), override),
				() -> assertTrue(acmeMapping.contains("<class name=\"customers.acme.modules.sales.Invoice.InvoiceExtension\""), acmeMapping),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	// ----- references to a new customer document ------------------------------------

	/**
	 * An association from a customer document to a new customer document: the association's document
	 * has to be resolved for the customer, as the vanilla module does not have it.
	 */
	@Test
	void anOverrideWithAnAssociationToANewCustomerDocumentGivesConsistentMappings() throws Exception {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.putAttribute(association("note", "Note"));
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(order)
				.acme(document("Note", "SALES_Note", null, null, "text")));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(acmeMapping.contains("<many-to-one name=\"note\" entity-name=\"acmesalesNote\""), acmeMapping),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/**
	 * An inverse with the relationship that metadata validation works out before generation, or none
	 * as with validation off.
	 */
	private static AbstractInverse inverse(AbstractInverse result, String name, String documentName, String referenceName, InverseRelationship relationship) {
		result.setName(name);
		result.setDisplayName(name);
		result.setDocumentName(documentName);
		result.setReferenceName(referenceName);
		result.setRelationship(relationship);
		return result;
	}

	/**
	 * Generates acme's override of Order with an inverse of a new customer document Note's association
	 * to Order, returning the generated files.
	 */
	private static Map<String, String> generateInverseToANewCustomerDocument(AbstractInverse inverse) throws Exception {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.putAttribute(inverse);
		DocumentImpl note = document("Note", "SALES_Note", null, null, "text");
		note.putAttribute(association("order", "Order"));
		return assertConsistentMappings(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(order)
				.acme(note));
	}

	@Test
	void anOverrideWithAnInverseManyOfANewCustomerDocumentGivesConsistentMappings() throws Exception {
		Map<String, String> files = generateInverseToANewCustomerDocument(inverse(new InverseMany(), "notes", "Note", "order", InverseRelationship.oneToMany));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		String note = files.get("customers/acme/modules/sales/domain/Note.java");
		assertAll(
				() -> assertTrue(acmeMapping.contains("<bag name=\"notes\""), acmeMapping),
				() -> assertTrue(acmeMapping.contains("<one-to-many entity-name=\"acmesalesNote\""), acmeMapping),
				() -> assertTrue(note.contains("private OrderOverride order"), note),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void anOverrideWithAnInverseOneOfANewCustomerDocumentGivesConsistentMappings() throws Exception {
		Map<String, String> files = generateInverseToANewCustomerDocument(inverse(new InverseOne(), "note", "Note", "order", InverseRelationship.oneToOne));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertAll(
				() -> assertTrue(acmeMapping.contains("<one-to-one name=\"note\" entity-name=\"acmesalesNote\" property-ref=\"order\""), acmeMapping),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	/**
	 * acme's override of Line adds an association to Order, whose override adds its inverse, so each
	 * override class types the other document as the other's override class, which has the property
	 * the generated accessors maintain.
	 */
	@Test
	void overridesWithAnAssociationAndItsInverseTypeEachOtherAsOverrideClasses() throws Exception {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.putRelation(inverse(new InverseMany(), "lines", "Line", "order", InverseRelationship.oneToMany));
		DocumentImpl line = document("Line", "SALES_Line", null, null, "text");
		line.putRelation(association("order", "Order"));
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.vanilla(document("Line", "SALES_Line", null, null, "text"))
				.acme(order)
				.acme(line));
		String orderOverride = files.get("customers/acme/modules/sales/domain/OrderOverride.java");
		String lineOverride = files.get("customers/acme/modules/sales/domain/LineOverride.java");
		assertAll(
				() -> assertTrue(orderOverride.contains("List<LineOverride> lines"), orderOverride),
				() -> assertTrue(lineOverride.contains("private OrderOverride order"), lineOverride),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void aNewCustomerChildOfANewCustomerDocumentMapsItsParent() throws Exception {
		DocumentImpl noteLine = document("NoteLine", "SALES_NoteLine", null, null, "text");
		noteLine.setParentDocumentName("Note");
		DocumentImpl note = document("Note", "SALES_Note", null, null, "text");
		note.putAttribute(childCollection("lines", "NoteLine"));
		Map<String, String> files = assertConsistentMappings(new Fixture()
				.vanilla(document("Order", "SALES_Order", null, null, "code"))
				.acme(note)
				.acme(noteLine));
		String acmeMapping = files.get("customers/acme/modules/orm.hbm.xml");
		assertTrue(acmeMapping.contains("<many-to-one name=\"parent\" entity-name=\"acmesalesNote\""), acmeMapping);
	}

	/**
	 * acme's override of R adds an association t to T, and acme's override of T adds its inverse rs.
	 * The document named extends a vanilla extension and acme's own extension, so its override class
	 * is abstract and the generated accessors keeping the other side up to date pass the override
	 * class itself, which the other side's accessors take.
	 */
	private static void assertAnInversePairWithACustomerExtensionCompiles(String extendedDocumentName, AbstractInverse rs) throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles(extendedDocumentName, rs, true);
	}

	/**
	 * As {@link #assertAnInversePairWithACustomerExtensionCompiles(String, AbstractInverse)}, with or
	 * without a vanilla extension of the document named. Without one, acme's extension is the only
	 * extension, so a cast of this to it would name a class the override does not import.
	 */
	private static void assertAnInversePairWithACustomerExtensionCompiles(String extendedDocumentName,
																			AbstractInverse rs,
																			boolean withVanillaExtension)
	throws Exception {
		DocumentImpl r = document("R", "SALES_R", null, null, "code");
		r.putRelation(association("t", "T"));
		DocumentImpl t = document("T", "SALES_T", null, null, "code");
		t.putRelation(inverse(rs, "rs", "R", "t", (rs instanceof InverseOne) ? InverseRelationship.oneToOne : InverseRelationship.oneToMany));
		Fixture fixture = new Fixture()
				.vanilla(document("R", "SALES_R", null, null, "code"))
				.vanilla(document("T", "SALES_T", null, null, "code"));
		if (withVanillaExtension) {
			fixture.extension(extendedDocumentName);
		}
		Map<String, String> files = assertConsistentMappings(fixture
				.acmeExtension(extendedDocumentName)
				.acme(r)
				.acme(t));
		String override = files.get("customers/acme/modules/sales/domain/" + extendedDocumentName + "Override.java");
		assertAll(
				() -> assertFalse(override.contains("Extension) this"), override),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void anAssociationOfAnOverrideWithACustomerExtensionToAnInverseManyCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("R", new InverseMany());
	}

	@Test
	void anAssociationOfAnOverrideWithACustomerExtensionToAnInverseOneCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("R", new InverseOne());
	}

	@Test
	void anInverseManyOfAnOverrideWithACustomerExtensionCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("T", new InverseMany());
	}

	@Test
	void anInverseOneOfAnOverrideWithACustomerExtensionCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("T", new InverseOne());
	}

	@Test
	void anAssociationOfAnOverrideWithOnlyACustomerExtensionToAnInverseManyCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("R", new InverseMany(), false);
	}

	@Test
	void anAssociationOfAnOverrideWithOnlyACustomerExtensionToAnInverseOneCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("R", new InverseOne(), false);
	}

	@Test
	void anInverseManyOfAnOverrideWithOnlyACustomerExtensionCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("T", new InverseMany(), false);
	}

	@Test
	void anInverseOneOfAnOverrideWithOnlyACustomerExtensionCompiles() throws Exception {
		assertAnInversePairWithACustomerExtensionCompiles("T", new InverseOne(), false);
	}

	/**
	 * acme's override of Receipt leaves out a reference or an inverse between Receipt and Order, so the
	 * generated ReceiptVanilla declares it, and the hand-written ReceiptExtension extends
	 * ReceiptVanilla. The accessors keeping the other side up to date pass ReceiptVanilla to Order as
	 * the ReceiptExtension that Order's vanilla accessors take, which is the class the vanilla mapping
	 * maps.
	 */
	private static void assertANarrowedAwayRelationshipWithAnExtensionCompiles(boolean receiptHoldsTheAssociation, AbstractInverse inverse) throws Exception {
		InverseRelationship relationship = (inverse instanceof InverseOne) ? InverseRelationship.oneToOne : InverseRelationship.oneToMany;
		DocumentImpl receipt = document("Receipt", "SALES_Receipt", null, null, "code");
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		if (receiptHoldsTheAssociation) {
			receipt.putRelation(association("order", "Order"));
			order.putRelation(inverse(inverse, "receipts", "Receipt", "order", relationship));
		}
		else {
			receipt.putRelation(inverse(inverse, "orders", "Order", "receipt", relationship));
			order.putRelation(association("receipt", "Receipt"));
		}
		Map<String, String> files = generate(new Fixture()
				.vanilla(receipt)
				.vanilla(order)
				.extension("Receipt", "ReceiptVanilla")
				.acme(document("Receipt", "SALES_Receipt", null, null, "code")));
		String receiptVanilla = files.get("modules/sales/domain/ReceiptVanilla.java");
		assertAll(
				() -> assertTrue(receiptVanilla.contains("(ReceiptExtension) this"), receiptVanilla),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}

	@Test
	void aNarrowedAwayAssociationWithAnInverseManyAndAnExtensionCompiles() throws Exception {
		assertANarrowedAwayRelationshipWithAnExtensionCompiles(true, new InverseMany());
	}

	@Test
	void aNarrowedAwayAssociationWithAnInverseOneAndAnExtensionCompiles() throws Exception {
		assertANarrowedAwayRelationshipWithAnExtensionCompiles(true, new InverseOne());
	}

	@Test
	void aNarrowedAwayInverseManyWithAnExtensionCompiles() throws Exception {
		assertANarrowedAwayRelationshipWithAnExtensionCompiles(false, new InverseMany());
	}

	@Test
	void aNarrowedAwayInverseOneWithAnExtensionCompiles() throws Exception {
		assertANarrowedAwayRelationshipWithAnExtensionCompiles(false, new InverseOne());
	}

	// ----- generation with metadata validation off ------------------------------------

	/**
	 * Metadata validation works out each inverse's relationship, so with validation off generation
	 * works it out, and maps and generates each inverse by it.
	 */
	@Test
	void inversesWithoutAWorkedOutRelationshipAreGeneratedWithValidationOff() throws Exception {
		DocumentImpl order = document("Order", "SALES_Order", null, null, "code");
		order.putRelation(inverse(new InverseOne(), "note", "Note", "order", null));
		order.putRelation(inverse(new InverseMany(), "lines", "Line", "order", null));
		order.putRelation(inverse(new InverseMany(), "tags", "Tag", "orders", null));
		DocumentImpl noteDocument = document("Note", "SALES_Note", null, null, "text");
		noteDocument.putRelation(association("order", "Order"));
		DocumentImpl line = document("Line", "SALES_Line", null, null, "text");
		line.putRelation(association("order", "Order"));
		DocumentImpl tag = document("Tag", "SALES_Tag", null, null, "text");
		CollectionImpl orders = childCollection("orders", "Order");
		orders.setType(CollectionType.aggregation);
		tag.putRelation(orders);

		Map<String, String> files = generate(new Fixture().vanilla(order).vanilla(noteDocument).vanilla(line).vanilla(tag), false);
		String mapping = files.get("modules/sales/domain/sales_orm.hbm.xml");
		String orderClass = files.get("modules/sales/domain/Order.java");
		assertAll(
				() -> assertEquals(List.of(), mappingProblems(files)),
				() -> assertTrue(mapping.contains("<one-to-one name=\"note\" entity-name=\"salesNote\" property-ref=\"order\""), mapping),
				() -> assertTrue(mapping.contains("<one-to-many entity-name=\"salesLine\""), mapping),
				() -> assertTrue(mapping.contains("table=\"SALES_Tag_orders\""), mapping),
				() -> assertTrue(orderClass.contains("private Note note"), orderClass),
				() -> assertTrue(orderClass.contains("private List<Line> lines"), orderClass),
				() -> assertEquals(List.of(), compilationErrors(files)));
	}
}
