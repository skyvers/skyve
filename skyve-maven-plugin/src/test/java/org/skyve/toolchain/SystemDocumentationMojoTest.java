package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.metadata.model.document.field.Text;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.Role;
import org.springframework.test.util.ReflectionTestUtils;
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
@SuppressWarnings({ "static-method", "boxing" })
class SystemDocumentationMojoTest {

	private Method generateDiagram;
	private Method outputDiagram;

	@BeforeEach
	void setUpReflection() throws Exception {
		generateDiagram = SystemDocumentationMojo.class.getDeclaredMethod("generateDiagram",
				org.skyve.metadata.model.document.Document.class,
				Customer.class);
		generateDiagram.setAccessible(true);

		outputDiagram = SystemDocumentationMojo.class.getDeclaredMethod("outputDiagram",
				org.skyve.metadata.model.document.Document.class,
				boolean.class,
				Set.class,
				String.class);
		outputDiagram.setAccessible(true);
	}

	@Test
    void testExcludedModulesParameterPropertyConfiguration() throws Exception {
        Document plugin = loadPluginDescriptor();
        if (plugin != null) {
            Node mojo = selectMojo(plugin, "systemDocumentation");
            assertNotNull(mojo, "systemDocumentation mojo should exist in plugin.xml");

            Node parameter = selectParameterByName(mojo, "excludedModules");
            if (parameter != null) {
                // Check if plugin descriptor has the expected elements
                String property = getChildText(parameter, "property");
                String expression = getChildText(parameter, "expression");
                String type = getChildText(parameter, "type");

                // If plugin descriptor has the expected elements, validate them
                if (property != null || expression != null) {
                    boolean hasProperty = (property != null) && property.equals("excludedModules");
                    boolean hasExpression = (expression != null) && expression.trim().equals("${excludedModules}");

                    assertTrue(hasProperty || hasExpression,
                            "excludedModules should map to -DexcludedModules via <property> or <expression>. Found property='" + property + "', expression='" + expression + "'");

                    assertEquals("java.lang.String", type, "excludedModules should be of type String");
                } else {
                    // Plugin descriptor doesn't have property/expression elements, fall back to source parsing
                    String source = loadMojoSource();
                    assertTrue(source.contains("class SystemDocumentationMojo"), "Mojo source should be readable");

                    // Verify excludedModules annotation has property="excludedModules"
                    Pattern exclPattern = Pattern.compile("@Parameter\\s*\\(\\s*property\\s*=\\s*\\\"excludedModules\\\"[\\s,]*\\)");
                    Matcher exclMatcher = exclPattern.matcher(source);
                    assertTrue(exclMatcher.find(), "@Parameter(property=\"excludedModules\") should be present on excludedModules");

                    // Verify field type is String
                    assertTrue(source.contains("private String excludedModules;"), "excludedModules should be a String field");
                }
            } else {
                // Parameter not found in descriptor, fall back to source parsing
                String source = loadMojoSource();
                assertTrue(source.contains("class SystemDocumentationMojo"), "Mojo source should be readable");

                // Verify excludedModules annotation has property="excludedModules"
                Pattern exclPattern = Pattern.compile("@Parameter\\s*\\(\\s*property\\s*=\\s*\\\"excludedModules\\\"[\\s,]*\\)");
                Matcher exclMatcher = exclPattern.matcher(source);
                assertTrue(exclMatcher.find(), "@Parameter(property=\"excludedModules\") should be present on excludedModules");

                // Verify field type is String
                assertTrue(source.contains("private String excludedModules;"), "excludedModules should be a String field");
            }
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
    void testOtherParametersConfiguration() throws Exception {
        Document plugin = loadPluginDescriptor();
        if (plugin != null) {
            Node mojo = selectMojo(plugin, "systemDocumentation");
            assertNotNull(mojo, "systemDocumentation mojo should exist in plugin.xml");

            // customer
            Node customer = selectParameterByName(mojo, "customer");
            if (customer != null) {
                String required = getChildText(customer, "required");
                String defaultValue = getChildText(customer, "default-value");

                // If plugin descriptor has the expected elements, validate them
                if (defaultValue != null) {
                    assertEquals("true", required, "customer should be required");
                    assertEquals("skyve", defaultValue, "customer default value");
                } else {
                    // Plugin descriptor doesn't have default-value element, fall back to source parsing
                    String source = loadMojoSource();
                    Pattern customerPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*defaultValue\\s*=\\s*\\\"skyve\\\"|defaultValue\\s*=\\s*\\\"skyve\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
                    assertTrue(customerPattern.matcher(source).find(), "customer should be @Parameter(required=true, defaultValue=\"skyve\")");
                }
            } else {
                // Parameter not found in descriptor, fall back to source parsing
                String source = loadMojoSource();
                Pattern customerPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*defaultValue\\s*=\\s*\\\"skyve\\\"|defaultValue\\s*=\\s*\\\"skyve\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
                assertTrue(customerPattern.matcher(source).find(), "customer should be @Parameter(required=true, defaultValue=\"skyve\")");
            }

            // srcDir
            Node srcDir = selectParameterByName(mojo, "srcDir");
            if (srcDir != null) {
                String required = getChildText(srcDir, "required");
                String defaultValue = getChildText(srcDir, "default-value");

                // If plugin descriptor has the expected elements, validate them
                if (defaultValue != null) {
                    assertEquals("true", required, "srcDir should be required");
                    assertEquals("src/main/java/", defaultValue, "srcDir default value");
                } else {
                    // Plugin descriptor doesn't have default-value element, fall back to source parsing
                    String source = loadMojoSource();
                    Pattern srcDirPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*defaultValue\\s*=\\s*\\\"src/main/java/\\\"|defaultValue\\s*=\\s*\\\"src/main/java/\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
                    assertTrue(srcDirPattern.matcher(source).find(), "srcDir should be @Parameter(required=true, defaultValue=\"src/main/java/\")");
                }
            } else {
                // Parameter not found in descriptor, fall back to source parsing
                String source = loadMojoSource();
                Pattern srcDirPattern = Pattern.compile("@Parameter\\s*\\(\\s*required\\s*=\\s*true[\\s,]*defaultValue\\s*=\\s*\\\"src/main/java/\\\"|defaultValue\\s*=\\s*\\\"src/main/java/\\\"[\\s,]*required\\s*=\\s*true[\\s,]*\\)");
                assertTrue(srcDirPattern.matcher(source).find(), "srcDir should be @Parameter(required=true, defaultValue=\"src/main/java/\")");
            }
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

	@Test
	void generateDiagramReturnsNullWhenDocumentHasNoRelationships() throws Exception {
		Customer customer = mock(Customer.class);
		org.skyve.metadata.model.document.Document document = mock(org.skyve.metadata.model.document.Document.class);
		when(document.getName()).thenReturn("PlainDocument");
		when(document.getAllAttributes(customer)).thenReturn(List.of());
		doReturn(List.of()).when(document).getInterfaces();

		String diagram = (String) generateDiagram.invoke(null, document, customer);

		assertNull(diagram);
	}

	@Test
	void generateDiagramIncludesAssociationsCollectionsParentExtendsAndInterfaces() throws Exception {
		Customer customer = mock(Customer.class);
		org.skyve.metadata.model.document.Document document = mock(org.skyve.metadata.model.document.Document.class);
		when(document.getName()).thenReturn("Invoice");
		when(document.isPersistable()).thenReturn(Boolean.TRUE);
		when(document.getParentDocumentName()).thenReturn("ParentInvoice");

		Association association = mock(Association.class);
		when(association.getDocumentName()).thenReturn("Account");
		when(association.getType()).thenReturn(AssociationType.aggregation);
		when(association.isRequired()).thenReturn(Boolean.TRUE);
		when(association.getName()).thenReturn("account");

		Collection collection = mock(Collection.class);
		when(collection.getDocumentName()).thenReturn("Line");
		when(collection.getType()).thenReturn(CollectionType.composition);
		when(collection.isRequired()).thenReturn(Boolean.FALSE);
		when(collection.getMaxCardinality()).thenReturn(Integer.valueOf(5));
		when(collection.getName()).thenReturn("lines");

		Attribute viewOnly = mock(Attribute.class);
		when(viewOnly.getUsage()).thenReturn(Attribute.UsageType.view);

		Extends inherits = new Extends();
		inherits.setDocumentName("BaseInvoice");
		when(document.getExtends()).thenReturn(inherits);

		Interface implement = mock(Interface.class);
		when(implement.getInterfaceName()).thenReturn("com.example.InvoiceContract");
		doReturn(List.of(implement)).when(document).getInterfaces();
		doReturn(List.of(viewOnly, association, collection)).when(document).getAllAttributes(customer);

		String diagram = (String) generateDiagram.invoke(null, document, customer);

		assertTrue(diagram.contains("@startuml"));
		assertTrue(diagram.contains("class Invoice<<Persistent Child>>"));
		assertTrue(diagram.contains("\"Invoice\" o--> \"1\" \"Account\" : account"));
		assertTrue(diagram.contains("\"Invoice\" *--> \"0..5\" \"Line\" : lines"));
		assertTrue(diagram.contains("\"Invoice\" o--> \"0..1\" \"ParentInvoice\" : parent"));
		assertTrue(diagram.contains("\"BaseInvoice\" <|-- \"Invoice\""));
		assertTrue(diagram.contains("\"com.example.InvoiceContract\" <|.. \"Invoice\""));
		assertTrue(diagram.contains("interface \"com.example.InvoiceContract\""));
	}

	@Test
	void outputDiagramMarksNonPersistentChildAndDeclaresRelatedNames() throws Exception {
		org.skyve.metadata.model.document.Document document = mock(org.skyve.metadata.model.document.Document.class);
		when(document.getName()).thenReturn("TransientChild");
		when(document.isPersistable()).thenReturn(Boolean.FALSE);

		String diagram = (String) outputDiagram.invoke(null,
				document,
				Boolean.TRUE,
				Set.of("\"Related\"", "\"com.example.RelatedInterface\""),
				"\"TransientChild\" *--> \"Related\"\n");

		assertTrue(diagram.contains("class TransientChild<<Non-Persistent Child>>"));
		assertTrue(diagram.contains("class \"Related\""));
		assertTrue(diagram.contains("interface \"com.example.RelatedInterface\""));
		assertTrue(diagram.endsWith("@enduml"));
	}

	@Test
	@SuppressWarnings("unchecked")
	void prepareParametersBuildsTemplateModelFromCustomerMetadata() throws Exception {
		Customer customer = mock(Customer.class);
		Module includedModule = mock(Module.class);
		Module excludedModule = mock(Module.class);
		org.skyve.metadata.model.document.Document document = mock(org.skyve.metadata.model.document.Document.class);

		when(customer.getModules()).thenReturn(List.of(includedModule, excludedModule));
		when(includedModule.getName()).thenReturn("admin");
		when(includedModule.getLocalisedTitle()).thenReturn("Administration");
		when(includedModule.getDocumentRefs()).thenReturn(Map.of("Invoice", new Module.DocumentRef()));
		when(includedModule.getDocument(customer, "Invoice")).thenReturn(document);
		when(excludedModule.getName()).thenReturn("skip");

		when(document.getName()).thenReturn("Invoice");
		when(document.getSingularAlias()).thenReturn("Invoice");
		when(document.getDocumentation()).thenReturn("Document documentation");
		when(document.getAllAttributes(customer)).thenReturn(List.of());
		when(document.getInterfaces()).thenReturn(List.of());

		Text name = mock(Text.class);
		when(name.getName()).thenReturn("name");
		when(name.getLocalisedDisplayName()).thenReturn("Name");
		when(name.getAttributeType()).thenReturn(Attribute.AttributeType.text);
		when(name.isRequired()).thenReturn(Boolean.TRUE);
		when(name.isPersistent()).thenReturn(Boolean.TRUE);
		when(name.getLocalisedDescription()).thenReturn("Customer name");
		when(name.getDocumentation()).thenReturn("Name docs");
		when(name.isDeprecated()).thenReturn(Boolean.FALSE);
		when(name.getLength()).thenReturn(80);
		doReturn(List.of(name)).when(document).getAttributes();

		Reference reference = mock(Reference.class);
		when(document.getReferenceNames()).thenReturn(Set.of("account"));
		when(document.getReferenceByName("account")).thenReturn(reference);
		when(reference.getType()).thenReturn(AssociationType.aggregation);
		when(reference.getDocumentName()).thenReturn("Account");
		when(reference.isRequired()).thenReturn(Boolean.TRUE);
		when(reference.getDescription()).thenReturn("Account reference");
		when(reference.getDocumentation()).thenReturn("Reference docs");
		when(reference.isDeprecated()).thenReturn(Boolean.FALSE);

		Condition condition = mock(Condition.class);
		when(document.getConditionNames()).thenReturn(Set.of("active"));
		when(document.getCondition("active")).thenReturn(condition);
		when(condition.getDocumentation()).thenReturn("Active condition docs");

		UniqueConstraint constraint = mock(UniqueConstraint.class);
		when(document.getUniqueConstraints()).thenReturn(List.of(constraint));
		when(constraint.getName()).thenReturn("uk_name");
		when(constraint.getDescription()).thenReturn("Unique name");
		when(constraint.getScope()).thenReturn(DocumentScope.customer);
		when(constraint.getFieldNames()).thenReturn(List.of("name", "missing"));
		when(document.getPolymorphicAttribute(customer, "name")).thenReturn(name);
		when(document.getPolymorphicAttribute(customer, "missing")).thenReturn(null);
		when(document.getDefinedActionNames()).thenReturn(Set.of("archive"));

		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getName()).thenReturn("qInvoices");
		when(query.getDocumentName()).thenReturn("Invoice");
		when(query.getLocalisedDescription()).thenReturn("Invoice query");
		when(query.getDocumentation()).thenReturn("Query docs");
		when(includedModule.getMetadataQueries()).thenReturn(List.of(query));

		Role role = mock(Role.class);
		when(role.getName()).thenReturn("adminRole");
		when(role.getLocalisedDescription()).thenReturn("Admin role");
		when(role.getDocumentation()).thenReturn("Role docs");
		when(includedModule.getRoles()).thenReturn(List.of(role));

		JobMetaData job = mock(JobMetaData.class);
		when(job.getName()).thenReturn("nightly");
		when(job.getLocalisedDisplayName()).thenReturn("Nightly");
		when(job.getClassName()).thenReturn("com.example.NightlyJob");
		when(includedModule.getJobs()).thenReturn(List.of(job));

		TestSystemDocumentationMojo mojo = new TestSystemDocumentationMojo(customer);
		ReflectionTestUtils.setField(mojo, "excludedModules", "skip");
		ReflectionTestUtils.setField(mojo, "project", project());

		Map<String, Object> parameters = prepareParameters(mojo);

		assertEquals("System Documentation", parameters.get("title"));
		assertEquals("10.0-test", parameters.get("pomVersion"));
		assertEquals("Demo System", parameters.get("projectName"));
		assertNotNull(parameters.get("logo"));

		Map<String, Object> data = (Map<String, Object>) parameters.get("data");
		List<Map<String, Object>> modules = (List<Map<String, Object>>) data.get("modules");
		assertEquals(1, modules.size());
		Map<String, Object> module = modules.get(0);
		assertEquals("Administration", module.get("name"));
		assertEquals(1, ((List<?>) module.get("documents")).size());
		assertEquals(2, ((List<?>) module.get("queries")).size());
		assertEquals(1, ((List<?>) module.get("jobs")).size());
	}

	@Test
	@SuppressWarnings("unchecked")
	void prepareParametersUsesFallbackLabelsWhenProjectMetadataIsBlank() throws Exception {
		Customer customer = mock(Customer.class);
		when(customer.getModules()).thenReturn(List.of());
		TestSystemDocumentationMojo mojo = new TestSystemDocumentationMojo(customer);
		MavenProject project = new MavenProject();
		project.setVersion(" ");
		project.setName(" ");
		project.setArtifactId(" ");
		ReflectionTestUtils.setField(mojo, "project", project);

		Map<String, Object> parameters = prepareParameters(mojo);

		assertEquals("Unknown", parameters.get("pomVersion"));
		assertEquals("Unnamed System", parameters.get("projectName"));
		Map<String, Object> data = (Map<String, Object>) parameters.get("data");
		assertTrue(((List<?>) data.get("modules")).isEmpty());
	}

	@Test
	void executeInitialisesEnvironmentAndCopiesGeneratedPdf() throws Exception {
		Customer customer = mock(Customer.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ExecuteSystemDocumentationMojo mojo = new ExecuteSystemDocumentationMojo(customer, persistence);
		ReflectionTestUtils.setField(mojo, "srcDir", "src/main/java");
		ReflectionTestUtils.setField(mojo, "customer", "demo");

		mojo.execute();

		assertEquals("src/main/java", mojo.classpathSourceDirectory);
		assertEquals("demo", mojo.customerName);
		verify(persistence).setUser(org.mockito.ArgumentMatchers.argThat(user ->
				"demo".equals(user.getCustomerName()) &&
				"DocoUser".equals(user.getName()) &&
				"DocoUser".equals(user.getId())));
		assertEquals(mojo.pdfFile, mojo.copySource);
		assertEquals(new File("./target/system-documentation.pdf"), mojo.copyDestination);
		assertEquals(new File("./target/temp"), mojo.deletedFile);
	}

	@Test
	void executeFailsWhenCustomerCannotBeFound() {
		ExecuteSystemDocumentationMojo mojo = new ExecuteSystemDocumentationMojo(null, mock(AbstractPersistence.class));
		ReflectionTestUtils.setField(mojo, "srcDir", "src/main/java");
		ReflectionTestUtils.setField(mojo, "customer", "missing");

		MojoExecutionException exception = org.junit.jupiter.api.Assertions.assertThrows(MojoExecutionException.class,
				mojo::execute);

		assertTrue(exception.getMessage().contains("Failed to generate system documentation"));
		assertEquals("missing", mojo.customerName);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> prepareParameters(SystemDocumentationMojo mojo) throws Exception {
		Method method = SystemDocumentationMojo.class.getDeclaredMethod("prepareParameters");
		method.setAccessible(true);
		return (Map<String, Object>) method.invoke(mojo);
	}

	private static MavenProject project() {
		MavenProject project = new MavenProject();
		project.setVersion("10.0-test");
		project.setDescription("Demo System");
		project.setName("demo");
		project.setArtifactId("demo-artifact");
		return project;
	}

	private static final class TestSystemDocumentationMojo extends SystemDocumentationMojo {
		private final Customer customer;

		private TestSystemDocumentationMojo(Customer customer) {
			this.customer = customer;
		}

		@Override
		Customer getCustomer() {
			return customer;
		}
	}

	private static final class ExecuteSystemDocumentationMojo extends SystemDocumentationMojo {
		private final Customer customerMetadata;
		private final AbstractPersistence persistence;
		private final File pdfFile = new File("target/temp/system-documentation.pdf");
		private String classpathSourceDirectory;
		private String customerName;
		private File copySource;
		private File copyDestination;
		private File deletedFile;

		private ExecuteSystemDocumentationMojo(Customer customer, AbstractPersistence persistence) {
			this.customerMetadata = customer;
			this.persistence = persistence;
		}

		@Override
		protected void configureClasspath(String srcDir) throws DependencyResolutionRequiredException, MalformedURLException {
			classpathSourceDirectory = srcDir;
		}

		@Override
		Customer getCustomer(String requestedCustomerName) {
			this.customerName = requestedCustomerName;
			return customerMetadata;
		}

		@Override
		ProvidedRepository newRepository() {
			return mock(ProvidedRepository.class);
		}

		@Override
		AbstractPersistence getPersistence() {
			return persistence;
		}

		@Override
		public File pdf() {
			return pdfFile;
		}

		@Override
		void copy(File source, File destination) {
			copySource = source;
			copyDestination = destination;
		}

		@Override
		void delete(File file) {
			deletedFile = file;
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
        // Find source relative to test class location
        Path testDir = Paths.get(
                SystemDocumentationMojoTest.class
                        .getProtectionDomain()
                        .getCodeSource()
                        .getLocation()
                        .toURI()
        );
        
        // Navigate from target/test-classes back to src/main/java
        Path source = testDir.resolve("../../src/main/java/org/skyve/toolchain/SystemDocumentationMojo.java")
            .normalize();
        
        // If not found, try alternative paths
        if (!Files.exists(source)) {
            source = testDir.resolve("../../../src/main/java/org/skyve/toolchain/SystemDocumentationMojo.java")
                .normalize();
        }
        
        // Final fallback: search from current working directory
        if (!Files.exists(source)) {
            Path currentDir = Path.of(System.getProperty("user.dir"));
            source = currentDir.resolve("src/main/java/org/skyve/toolchain/SystemDocumentationMojo.java");
            
            while (!Files.exists(source) && currentDir.getParent() != null) {
                currentDir = currentDir.getParent();
                source = currentDir.resolve("src/main/java/org/skyve/toolchain/SystemDocumentationMojo.java");
            }
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
