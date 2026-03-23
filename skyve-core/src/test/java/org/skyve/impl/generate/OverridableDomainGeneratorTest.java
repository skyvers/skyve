package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.OrderedAttribute;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.test.SkyveFactory;

class OverridableDomainGeneratorTest {

	@Test
	void shouldBuildMappingHeaderAndFooterWithFilters() throws Exception {
		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefinitions = new StringBuilder();

		declaredMethod("createMappingFileHeader", StringBuilder.class).invoke(null, contents);
		declaredMethod("generateFilterStuff", String.class, StringBuilder.class, StringBuilder.class, String.class)
				.invoke(null, "AdminUser", contents, filterDefinitions, "");
		declaredMethod("createMappingFileFooter", StringBuilder.class, StringBuilder.class)
				.invoke(null, contents, filterDefinitions);

		String xml = contents.toString();
		assertTrue(xml.startsWith("<?xml version=\"1.0\"?>"));
		assertTrue(xml.contains("<hibernate-mapping default-access=\"field\">"));
		assertTrue(xml.contains("AdminUserCustomerFilter"));
		assertTrue(xml.endsWith("</hibernate-mapping>"));
	}

	@Test
	void shouldCreateOrderByClauseForSimpleOrderingOnly() throws Exception {
		OrderedAttribute ordered = mock(OrderedAttribute.class);
		Document referencedDocument = mock(Document.class);

		OrderingImpl ascending = new OrderingImpl("title", SortDirection.ascending);
		OrderingImpl descendingAssociation = new OrderingImpl("owner", SortDirection.descending);

		when(ordered.isComplexOrdering()).thenReturn(false);
		when(ordered.getOrdering()).thenReturn(List.of(ascending, descendingAssociation));
		when(referencedDocument.getAttribute("title")).thenReturn(mock(Attribute.class));
		when(referencedDocument.getAttribute("owner")).thenReturn(mock(Association.class));

		String result = (String) declaredMethod("orderBy", OrderedAttribute.class, Document.class)
				.invoke(null, ordered, referencedDocument);
		assertEquals("title asc, owner_id desc", result);

		when(ordered.isComplexOrdering()).thenReturn(true);
		assertNull(declaredMethod("orderBy", OrderedAttribute.class, Document.class).invoke(null, ordered, referencedDocument));

		when(ordered.isComplexOrdering()).thenReturn(false);
		when(ordered.getOrdering()).thenReturn(Collections.emptyList());
		assertNull(declaredMethod("orderBy", OrderedAttribute.class, Document.class).invoke(null, ordered, referencedDocument));
	}

	@Test
	void shouldGenerateUniqueColumnNamesAndFailOnDuplicates() throws Exception {
		Set<String> columnNames = new TreeSet<>();
		Method columnName = declaredMethod("columnName",
											String.class,
											String.class,
											String.class,
											String.class,
											String.class,
											Set.class);

		String generated = (String) columnName.invoke(null, "admin", "User", "status", "embedded", "_id", columnNames);
		assertEquals("embedded_status_id", generated);
		assertEquals(Set.of("embedded_status_id"), columnNames);

		InvocationTargetException duplicate = assertThrows(InvocationTargetException.class,
				() -> columnName.invoke(null, "admin", "User", "status", "embedded", "_id", columnNames));
		assertInstanceOf(MetaDataException.class, duplicate.getCause());
		assertTrue(duplicate.getCause().getMessage().contains("duplicated"));
	}

	@Test
	void shouldGeneratePropertyNamesFromDocumentMetadata() throws Exception {
		DocumentImpl document = new DocumentImpl();
		document.setName("Sample");
		document.setOwningModuleName("admin");

		Text persistentText = new Text();
		persistentText.setName("name");
		persistentText.setPersistent(true);
		document.putAttribute(persistentText);

		Text dynamicText = new Text();
		dynamicText.setName("runtime");
		dynamicText.setDynamic(true);
		document.putAttribute(dynamicText);

		Text bizKey = new Text();
		bizKey.setName(Bean.BIZ_KEY);
		document.putAttribute(bizKey);

		Map<String, Condition> conditions = new TreeMap<>();
		conditions.put("isEditable", new ConditionImpl());
		java.lang.reflect.Field conditionsField = DocumentImpl.class.getDeclaredField("conditions");
		conditionsField.setAccessible(true);
		conditionsField.set(document, conditions);

		@SuppressWarnings("unchecked")
		TreeMap<String, Attribute.AttributeType> result = (TreeMap<String, Attribute.AttributeType>) declaredMethod(
				"generateDocumentPropertyNames", Document.class).invoke(null, document);

		assertEquals(Attribute.AttributeType.text, result.get("name"));
		assertEquals(Attribute.AttributeType.bool, result.get("isEditable"));
		assertFalse(result.containsKey("runtime"));
		assertFalse(result.containsKey(Bean.BIZ_KEY));
	}

	@Test
	void shouldGenerateDatastoreNamesAndRespectDialectLimits() throws Exception {
		OverridableDomainGenerator generator = new OverridableDomainGenerator(false,
																				false,
																				false,
																				DialectOptions.MYSQL_5,
																				"",
																				"",
																				"",
																				"",
																				null);

		Method identifierTooLong = declaredMethod("identifierIsTooLong", String.class);
		assertFalse((boolean) identifierTooLong.invoke(generator, "short_name"));
		assertTrue((boolean) identifierTooLong.invoke(generator, "x".repeat(65)));

		Method shouldIndex = declaredMethod("shouldIndex", Boolean.class);
		assertTrue((boolean) shouldIndex.invoke(generator, new Object[] {null}));
		assertFalse((boolean) shouldIndex.invoke(generator, Boolean.FALSE));
		assertTrue((boolean) shouldIndex.invoke(generator, Boolean.TRUE));

		Class<?> dataStoreTypeClass = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DataStoreType");
		Method generateDataStoreName = declaredMethod("generateDataStoreName", dataStoreTypeClass, String.class, String.class);

		Object fk = enumConstant(dataStoreTypeClass, "FK");
		Object idx = enumConstant(dataStoreTypeClass, "IDX");

		assertEquals("FK_ADM_USER_owner_id", generateDataStoreName.invoke(generator, fk, "ADM_USER", "owner_id"));
		assertEquals("IDX_owner_id", generateDataStoreName.invoke(generator, idx, "ADM_USER", "owner_id"));

		String hashed = (String) generateDataStoreName.invoke(generator, fk, "T".repeat(80), "C".repeat(80));
		assertNotNull(hashed);
		assertTrue(hashed.startsWith("FK_"));
		assertTrue(hashed.length() <= DialectOptions.MYSQL_5.getDataStoreIdentifierCharacterLimit());
	}

	@Test
	void shouldPopulateArcsRecursivelyForPersistableDerivations() throws Exception {
		OverridableDomainGenerator generator = new OverridableDomainGenerator(false,
																				false,
																				false,
																				DialectOptions.H2_NO_INDEXES,
																				"",
																				"",
																				"",
																				"",
																				null);

		Document base = mock(Document.class);
		when(base.getOwningModuleName()).thenReturn("admin");
		when(base.getName()).thenReturn("Base");

		Document child = mock(Document.class);
		when(child.getOwningModuleName()).thenReturn("admin");
		when(child.getName()).thenReturn("Child");
		when(child.isPersistable()).thenReturn(true);

		Document transientChild = mock(Document.class);
		when(transientChild.getOwningModuleName()).thenReturn("admin");
		when(transientChild.getName()).thenReturn("TransientChild");
		when(transientChild.isPersistable()).thenReturn(false);

		Document grandChild = mock(Document.class);
		when(grandChild.getOwningModuleName()).thenReturn("admin");
		when(grandChild.getName()).thenReturn("GrandChild");
		when(grandChild.isPersistable()).thenReturn(true);

		Method putModocDerivation = declaredMethod("putModocDerivation", Document.class, Document.class);
		putModocDerivation.invoke(generator, child, base);
		putModocDerivation.invoke(generator, transientChild, base);
		putModocDerivation.invoke(generator, grandChild, transientChild);

		Map<String, Document> arcs = new TreeMap<>();
		declaredMethod("populateArcs", Document.class, Map.class).invoke(generator, base, arcs);

		assertTrue(arcs.containsKey("admin.Child"));
		assertFalse(arcs.containsKey("admin.TransientChild"));
		assertTrue(arcs.containsKey("admin.GrandChild"));
	}

	@Test
	void shouldReturnFactoryAnnotationWhenFactoryClassExists() throws Exception {
		OverridableDomainGenerator generator = new OverridableDomainGenerator(false,
																				false,
																				false,
																				DialectOptions.H2_NO_INDEXES,
																				"",
																				"",
																				"",
																				"",
																				null);

		String resourcePath = AnnotatedFactory.class.getName().replace('.', '/') + ".class";
		URL classRoot = Thread.currentThread().getContextClassLoader().getResource("");
		URL classResource = Thread.currentThread().getContextClassLoader().getResource(resourcePath);
		assertNotNull(classRoot);
		assertNotNull(classResource);

		java.lang.reflect.Field srcPathField = DomainGenerator.class.getDeclaredField("srcPath");
		srcPathField.setAccessible(true);
		srcPathField.set(generator, new File(classRoot.toURI()).getAbsolutePath() + '/');

		Method retrieveFactoryAnnotation = declaredMethod("retrieveFactoryAnnotation", File.class);
		SkyveFactory annotation = (SkyveFactory) retrieveFactoryAnnotation.invoke(generator, new File(classResource.toURI()));
		assertNotNull(annotation);
		assertFalse(annotation.testDomain());
		assertFalse(annotation.testAction());

		SkyveFactory notFound = (SkyveFactory) retrieveFactoryAnnotation.invoke(generator, new File("/tmp/not/a/factory/DoesNotExistFactory.java"));
		assertNull(notFound);
	}

	@Test
	void shouldReturnNullWhenFactoryClassHasNoSkyveFactoryAnnotation() throws Exception {
		OverridableDomainGenerator generator = new OverridableDomainGenerator(false,
																				false,
																				false,
																				DialectOptions.H2_NO_INDEXES,
																				"",
																				"",
																				"",
																				"",
																				null);

		String resourcePath = PlainFactory.class.getName().replace('.', '/') + ".class";
		URL classRoot = Thread.currentThread().getContextClassLoader().getResource("");
		URL classResource = Thread.currentThread().getContextClassLoader().getResource(resourcePath);
		assertNotNull(classRoot);
		assertNotNull(classResource);

		java.lang.reflect.Field srcPathField = DomainGenerator.class.getDeclaredField("srcPath");
		srcPathField.setAccessible(true);
		srcPathField.set(generator, new File(classRoot.toURI()).getAbsolutePath() + '/');

		Method retrieveFactoryAnnotation = declaredMethod("retrieveFactoryAnnotation", File.class);
		SkyveFactory annotation = (SkyveFactory) retrieveFactoryAnnotation.invoke(generator, new File(classResource.toURI()));
		assertNull(annotation);
	}

	@Test
	void shouldReturnOnlyExtraPropertiesForOverriddenDocument() throws Exception {
		OverridableDomainGenerator generator = new OverridableDomainGenerator(false,
																				false,
																				false,
																				DialectOptions.H2_NO_INDEXES,
																				"",
																				"",
																				"",
																				"",
																				null);

		Class<?> domainClassType = Class.forName("org.skyve.impl.generate.OverridableDomainGenerator$DomainClass");
		java.lang.reflect.Constructor<?> domainClassConstructor = domainClassType.getDeclaredConstructor();
		domainClassConstructor.setAccessible(true);
		Object domainClass = domainClassConstructor.newInstance();
		java.lang.reflect.Field attributesField = domainClassType.getDeclaredField("attributes");
		attributesField.setAccessible(true);
		TreeMap<String, Attribute.AttributeType> vanillaAttributes = new TreeMap<>();
		vanillaAttributes.put("baseName", Attribute.AttributeType.text);
		vanillaAttributes.put("baseFlag", Attribute.AttributeType.bool);
		attributesField.set(domainClass, vanillaAttributes);

		java.lang.reflect.Field vanillaClassesField = OverridableDomainGenerator.class.getDeclaredField("moduleDocumentVanillaClasses");
		vanillaClassesField.setAccessible(true);
		TreeMap<String, TreeMap<String, Object>> moduleDocumentVanillaClasses = new TreeMap<>();
		TreeMap<String, Object> documentClasses = new TreeMap<>();
		documentClasses.put("CustomerDocument", domainClass);
		moduleDocumentVanillaClasses.put("admin", documentClasses);
		vanillaClassesField.set(generator, moduleDocumentVanillaClasses);

		DocumentImpl overriddenDocument = new DocumentImpl();
		overriddenDocument.setOwningModuleName("admin");
		overriddenDocument.setName("CustomerDocument");

		Text baseField = new Text();
		baseField.setName("baseName");
		overriddenDocument.putAttribute(baseField);

		Text extraField = new Text();
		extraField.setName("extraValue");
		overriddenDocument.putAttribute(extraField);

		@SuppressWarnings("unchecked")
		TreeMap<String, Attribute.AttributeType> extraProperties = (TreeMap<String, Attribute.AttributeType>) declaredMethod(
				"getOverriddenDocumentExtraProperties", Document.class).invoke(generator, overriddenDocument);

		assertEquals(1, extraProperties.size());
		assertEquals(Attribute.AttributeType.text, extraProperties.get("extraValue"));
	}

	private static Method declaredMethod(String name, Class<?>... parameterTypes) throws NoSuchMethodException {
		Method method = OverridableDomainGenerator.class.getDeclaredMethod(name, parameterTypes);
		method.setAccessible(true);
		return method;
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	private static Object enumConstant(Class<?> enumClass, String name) {
		return Enum.valueOf((Class<? extends Enum>) enumClass.asSubclass(Enum.class), name);
	}

	@SkyveFactory(testDomain = false, testAction = false)
	private static class AnnotatedFactory {
		// annotation-only test fixture
	}

	private static class PlainFactory {
		// no annotation by design
	}
}
