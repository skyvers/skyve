package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.Boolean;
import org.skyve.impl.metadata.model.document.field.Colour;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Markup;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.module.Module;

/**
 * Tests for {@link OverridableDomainGenerator} ORM-generation private methods:
 * {@code generateORM}, {@code generateAttributeMappings}.
 */
@SuppressWarnings("static-method")
class OverridableDomainGeneratorORMTest {

	/** Instantiate a generator with no extended-context requirements. */
	private static OverridableDomainGenerator generator() {
		return new OverridableDomainGenerator(false, false, false,
				DialectOptions.H2_NO_INDEXES, "", "", "", "", null);
	}

	/** Set the private {@code repository} field via reflection. */
	private static void setRepository(OverridableDomainGenerator gen,
			ProvidedRepository repo) throws Exception {
		Field f = org.skyve.impl.generate.DomainGenerator.class.getDeclaredField("repository");
		f.setAccessible(true);
		f.set(gen, repo);
	}

	/** Populate {@code persistentPropertyLengths} with one entry for a document. */
	@SuppressWarnings("unchecked")
	private static void seedPropertyLengths(OverridableDomainGenerator gen,
			Persistent persistent, String fieldName, int length) throws Exception {
		Field f = OverridableDomainGenerator.class.getDeclaredField("persistentPropertyLengths");
		f.setAccessible(true);
		TreeMap<String, TreeMap<String, java.lang.Integer>> ppl =
				(TreeMap<String, TreeMap<String, java.lang.Integer>>) f.get(gen);
		TreeMap<String, java.lang.Integer> docLengths = ppl.computeIfAbsent(
				persistent.getPersistentIdentifier(), k -> new TreeMap<>());
		docLengths.put(fieldName, java.lang.Integer.valueOf(length));
	}

	/** Get the private 9-arg {@code generateORM} method via reflection. */
	private static Method ormMethod() throws Exception {
		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"generateORM",
				StringBuilder.class,     // contents
				Module.class,            // module
				Document.class,          // document
				String.class,            // packagePathPrefix
				boolean.class,           // forExt
				boolean.class,           // recursive
				org.skyve.metadata.customer.Customer.class, // customer
				StringBuilder.class,     // filterDefinitions
				String.class);           // indentation
		m.setAccessible(true);
		return m;
	}

	/** Get the private {@code generateAttributeMappings} method via reflection. */
	private static Method attrMappingsMethod() throws Exception {
		Method m = OverridableDomainGenerator.class.getDeclaredMethod(
				"generateAttributeMappings",
				StringBuilder.class,     // contents
				org.skyve.metadata.customer.Customer.class, // customer
				Module.class,            // module
				Document.class,          // document
				Persistent.class,        // persistent
				String.class,            // columnPrefix
				Set.class,               // columnNames
				String.class,            // owningDocumentName
				boolean.class,           // forExt
				String.class);           // indentation
		m.setAccessible(true);
		return m;
	}

	// ---- generateAttributeMappings ----------------------------------------

	@Test
	void generateAttributeMappingsWithTextFieldProducesPropertyElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("AttrDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_AttrDoc");
		doc.setPersistent(p);

		Text textField = new Text();
		textField.setName("title");
		textField.setDisplayName("Title");
		textField.setLength(200);
		doc.putAttribute(textField);

		seedPropertyLengths(gen, p, "title", 200);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<property name=\"title\""), "Should have property element");
		assertTrue(result.contains("length=\"200\""), "Should have length");
	}

	@Test
	void generateAttributeMappingsWithTransientFieldSkipsProperty() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientAttrDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_TransientAttrDoc");
		doc.setPersistent(p);

		Text transientField = new Text();
		transientField.setName("computed");
		transientField.setDisplayName("Computed");
		transientField.setLength(100);
		transientField.setPersistent(false);
		doc.putAttribute(transientField);

		seedPropertyLengths(gen, p, "computed", 100);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertFalse(result.contains("<property name=\"computed\""), "Transient field should be skipped");
	}

	@Test
	void generateAttributeMappingsWithChildCollectionProducesBagElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Referenced child doc
		DocumentImpl childDoc = new DocumentImpl();
		childDoc.setName("ChildItem");
		childDoc.setOwningModuleName("myMod");
		Persistent childPersistent = new Persistent();
		childPersistent.setName("MOD_ChildItem");
		childDoc.setPersistent(childPersistent);

		// Owner doc
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("OwnerDoc");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_OwnerDoc");
		ownerDoc.setPersistent(p);

		CollectionImpl coll = new CollectionImpl();
		coll.setName("children");
		coll.setDisplayName("Children");
		coll.setDocumentName("ChildItem");
		coll.setType(CollectionType.child);
		ownerDoc.putRelation(coll);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "ChildItem")).thenReturn(childDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<bag name=\"children\""), "Should have bag element for child collection");
		assertTrue(result.contains("<key column=\"parent_id\""), "Should have parent_id key");
		assertTrue(result.contains("<one-to-many entity-name=\"myModChildItem\""), "Should have one-to-many");
	}

	@Test
	void generateAttributeMappingsWithAggregationCollectionProducesManyToManyElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Tag");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Tag");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("ArticleDoc");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_ArticleDoc");
		ownerDoc.setPersistent(p);

		CollectionImpl coll = new CollectionImpl();
		coll.setName("tags");
		coll.setDisplayName("Tags");
		coll.setDocumentName("Tag");
		coll.setType(CollectionType.aggregation);
		ownerDoc.putRelation(coll);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Tag")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("MOD_ArticleDoc_tags"), "Should have join table name for aggregation collection");
		assertTrue(result.contains("<many-to-many"), "Should have many-to-many element");
	}

	// ---- generateORM ------------------------------------------------------

	@Test
	void generateORMForPersistentDocProducesClassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("ORMTestDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_ORMTestDoc");
		doc.setPersistent(p);

		Text textField = new Text();
		textField.setName("description");
		textField.setDisplayName("Description");
		textField.setLength(500);
		doc.putAttribute(textField);

		seedPropertyLengths(gen, p, "description", 500);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false,
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("<class name="), "Should have class element");
		assertTrue(result.contains("table=\"MOD_ORMTestDoc\""), "Should have table name");
		assertTrue(result.contains("entity-name=\"myModORMTestDoc\""), "Should have entity name");
		assertTrue(result.contains("bizId"), "Should have bizId property");
		assertTrue(result.contains("</class>"), "Should close class element");
		assertTrue(result.contains("<property name=\"description\""), "Should have description property");
	}

	@Test
	void generateORMForTransientDocProducesNoClassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Transient doc (no persistent)
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TransientORM");
		doc.setOwningModuleName("myMod");
		// no persistent

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false,
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.isEmpty(), "Transient doc should produce no ORM output");
	}

	@Test
	void generateORMWithSchemaAndCatalogIncludesThem() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("SchemaORMDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_SchemaORMDoc");
		p.setSchema("mySchema");
		p.setCatalog("myCatalog");
		doc.setPersistent(p);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false,
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("schema=\"mySchema\""), "Should include schema in class definition");
		assertTrue(result.contains("catalog=\"myCatalog\""), "Should include catalog in class definition");
	}

	@Test
	void generateORMForSubDocWithSingleStrategyProducesSubclassElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Sub-document with single table strategy
		DocumentImpl subDoc = new DocumentImpl();
		subDoc.setName("SubDoc");
		subDoc.setOwningModuleName("myMod");
		Persistent subPersistent = new Persistent();
		subPersistent.setName("MOD_BaseDoc"); // uses base table
		subPersistent.setStrategy(ExtensionStrategy.single);
		subDoc.setPersistent(subPersistent);

		org.skyve.metadata.model.Extends inherits = new org.skyve.metadata.model.Extends();
		inherits.setDocumentName("BaseDoc");
		subDoc.setExtends(inherits);

		// Base document
		DocumentImpl baseDoc = new DocumentImpl();
		baseDoc.setName("BaseDoc");
		baseDoc.setOwningModuleName("myMod");
		Persistent basePersistent = new Persistent();
		basePersistent.setName("MOD_BaseDoc");
		// no strategy = default (not single)
		baseDoc.setPersistent(basePersistent);

		seedPropertyLengths(gen, subPersistent, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		// findNearest returns baseDoc to indicate SubDoc is a subclass
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(
				any(), any(), any())).thenReturn(baseDoc);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, subDoc, "modules.", false,
				true, // recursive=true to process subclass
				null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("<subclass name="), "Should have subclass element for single strategy");
		assertTrue(result.contains("</subclass>"), "Should close subclass element");
	}

	@Test
	void generateAttributeMappingsWithAssociationAggregationProducesManyToOneElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Referenced doc
		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Category");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Category");
		refDoc.setPersistent(refPersistent);

		// Owner doc
		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Product");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Product");
		ownerDoc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("category");
		assoc.setDisplayName("Category");
		assoc.setDocumentName("Category");
		assoc.setType(AssociationType.aggregation);
		ownerDoc.putRelation(assoc);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Category")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<many-to-one name=\"category\""), "Should have many-to-one for aggregation association");
		assertTrue(result.contains("entity-name=\"myModCategory\""), "Should reference entity name");
		assertTrue(result.contains("cascade=\"persist,save-update,refresh,merge\""), "Aggregation cascade");
	}

	@Test
	void generateAttributeMappingsWithAssociationCompositionProducesUniqueManyToOne() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Address");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Address");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Contact");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Contact");
		ownerDoc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("homeAddress");
		assoc.setDisplayName("Home Address");
		assoc.setDocumentName("Address");
		assoc.setType(AssociationType.composition);
		ownerDoc.putRelation(assoc);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Address")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<many-to-one name=\"homeAddress\""), "Should have many-to-one for composition association");
		assertTrue(result.contains("unique=\"true\""), "Composition should be unique");
		assertTrue(result.contains("cascade=\"persist,save-update,refresh,delete-orphan,merge\""), "Composition cascade");
	}

	@Test
	void generateAttributeMappingsWithCompositionCollectionProducesAllDeleteOrphanCascade() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("LineItem");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_LineItem");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Order");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Order");
		ownerDoc.setPersistent(p);

		CollectionImpl coll = new CollectionImpl();
		coll.setName("lineItems");
		coll.setDisplayName("Line Items");
		coll.setDocumentName("LineItem");
		coll.setType(CollectionType.composition);
		ownerDoc.putRelation(coll);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "LineItem")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("cascade=\"all-delete-orphan\""), "Composition collection uses all-delete-orphan");
		assertTrue(result.contains("MOD_Order_lineItems"), "Should have join table");
		assertTrue(result.contains("<many-to-many"), "Should have many-to-many element for composition collection");
	}

	@Test
	void generateAttributeMappingsWithEnumerationFieldProducesEnumTypeElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl doc = new DocumentImpl();
		doc.setName("StatusDoc");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_StatusDoc");
		doc.setPersistent(p);

		Enumeration enumField = new Enumeration();
		enumField.setName("status");
		enumField.setDisplayName("Status");
		enumField.setOwningDocument(doc);
		EnumeratedValue val1 = new EnumeratedValue();
		val1.setCode("active");
		EnumeratedValue val2 = new EnumeratedValue();
		val2.setCode("inactive");
		enumField.getValues().add(val1);
		enumField.getValues().add(val2);
		doc.putAttribute(enumField);

		seedPropertyLengths(gen, p, "status", 30);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<property name=\"status\""), "Should have property element for enumeration");
		assertTrue(result.contains("<type name=\"Enum\">"), "Should have Enum type element");
	}

	@Test
	void generateAttributeMappingsWithTransientAssociationIsSkipped() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Ref");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Ref");
		refDoc.setPersistent(refPersistent);

		DocumentImpl ownerDoc = new DocumentImpl();
		ownerDoc.setName("Owner");
		ownerDoc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Owner");
		ownerDoc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("transientRef");
		assoc.setDisplayName("Transient Ref");
		assoc.setDocumentName("Ref");
		assoc.setType(AssociationType.aggregation);
		assoc.setPersistent(false);
		ownerDoc.putRelation(assoc);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Ref")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertFalse(result.contains("<many-to-one"), "Transient association should be skipped");
	}

	@Test
	void generateORMForDocWithAssociationIncludesManyToOne() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl refDoc = new DocumentImpl();
		refDoc.setName("Dept");
		refDoc.setOwningModuleName("myMod");
		Persistent refPersistent = new Persistent();
		refPersistent.setName("MOD_Dept");
		refDoc.setPersistent(refPersistent);

		DocumentImpl doc = new DocumentImpl();
		doc.setName("Employee");
		doc.setOwningModuleName("myMod");
		Persistent p = new Persistent();
		p.setName("MOD_Employee");
		doc.setPersistent(p);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("department");
		assoc.setDisplayName("Department");
		assoc.setDocumentName("Dept");
		assoc.setType(AssociationType.aggregation);
		doc.putRelation(assoc);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("myMod");
		when(module.getDocument(null, "Dept")).thenReturn(refDoc);

		ProvidedRepository mockRepo = mock(ProvidedRepository.class);
		when(mockRepo.findNearestPersistentSingleOrJoinedSuperDocument(any(), any(), any())).thenReturn(null);
		setRepository(gen, mockRepo);

		StringBuilder contents = new StringBuilder();
		StringBuilder filterDefs = new StringBuilder();
		ormMethod().invoke(gen, contents, module, doc, "modules.", false, false, null, filterDefs, "");

		String result = contents.toString();
		assertTrue(result.contains("<class name="), "Should have class element");
		assertTrue(result.contains("<many-to-one name=\"department\""), "Should have many-to-one for association");
	}

	// ---- Field type tests -------------------------------------------------

	@Test
	void generateAttributeMappingsWithDateFieldProducesDateOnlyType() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("DateDoc", "myMod");
		Persistent p = simplePersistent("MOD_DateDoc", doc);

		Date dateField = new Date();
		dateField.setName("birthDate");
		dateField.setDisplayName("Birth Date");
		doc.putAttribute(dateField);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertTrue(contents.toString().contains("<property name=\"birthDate\""), "Should have property");
		assertTrue(contents.toString().contains("DateOnly"), "Should have DateOnly type");
	}

	@Test
	void generateAttributeMappingsWithDateTimeFieldProducesDateTimeType() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("DtDoc", "myMod");
		Persistent p = simplePersistent("MOD_DtDoc", doc);

		DateTime dtField = new DateTime();
		dtField.setName("createdAt");
		dtField.setDisplayName("Created At");
		doc.putAttribute(dtField);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertTrue(contents.toString().contains("DateTime"), "Should have DateTime type");
	}

	@Test
	void generateAttributeMappingsWithDecimal2FieldProducesPrecisionScale() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("Dec2Doc", "myMod");
		Persistent p = simplePersistent("MOD_Dec2Doc", doc);

		Decimal2 d2 = new Decimal2();
		d2.setName("price");
		d2.setDisplayName("Price");
		doc.putAttribute(d2);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("precision=\"20\" scale=\"2\""), "Should have precision/scale for decimal2");
	}

	@Test
	void generateAttributeMappingsWithDecimal5FieldProducesPrecisionScale() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("Dec5Doc", "myMod");
		Persistent p = simplePersistent("MOD_Dec5Doc", doc);

		Decimal5 d5 = new Decimal5();
		d5.setName("amount");
		d5.setDisplayName("Amount");
		doc.putAttribute(d5);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("precision=\"23\" scale=\"5\""), "Should have precision/scale for decimal5");
	}

	@Test
	void generateAttributeMappingsWithDecimal10FieldProducesPrecisionScale() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("Dec10Doc", "myMod");
		Persistent p = simplePersistent("MOD_Dec10Doc", doc);

		Decimal10 d10 = new Decimal10();
		d10.setName("total");
		d10.setDisplayName("Total");
		doc.putAttribute(d10);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("precision=\"28\" scale=\"10\""), "Should have precision/scale for decimal10");
	}

	@Test
	void generateAttributeMappingsWithTimestampFieldProducesTimestampType() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("TsDoc", "myMod");
		Persistent p = simplePersistent("MOD_TsDoc", doc);

		Timestamp ts = new Timestamp();
		ts.setName("updatedAt");
		ts.setDisplayName("Updated At");
		doc.putAttribute(ts);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<property name=\"updatedAt\""), "Should have property");
		assertTrue(result.contains("Timestamp"), "Should have Timestamp type");
	}

	@Test
	void generateAttributeMappingsWithMemoFieldProducesTextType() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("MemoDoc", "myMod");
		Persistent p = simplePersistent("MOD_MemoDoc", doc);

		Memo memo = new Memo();
		memo.setName("notes");
		memo.setDisplayName("Notes");
		doc.putAttribute(memo);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("type=\"text\""), "Should have text type for memo");
	}

	@Test
	void generateAttributeMappingsWithMarkupFieldProducesTextType() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("MarkupDoc", "myMod");
		Persistent p = simplePersistent("MOD_MarkupDoc", doc);

		Markup markup = new Markup();
		markup.setName("description");
		markup.setDisplayName("Description");
		doc.putAttribute(markup);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("type=\"text\""), "Should have text type for markup");
	}

	@Test
	void generateAttributeMappingsWithBooleanFieldProducesPropertyElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("BoolDoc", "myMod");
		Persistent p = simplePersistent("MOD_BoolDoc", doc);

		Boolean boolField = new Boolean();
		boolField.setName("active");
		boolField.setDisplayName("Active");
		doc.putAttribute(boolField);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertTrue(contents.toString().contains("<property name=\"active\""), "Should have property for boolean");
	}

	@Test
	void generateAttributeMappingsWithIntegerFieldProducesPropertyElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("IntDoc", "myMod");
		Persistent p = simplePersistent("MOD_IntDoc", doc);

		Integer intField = new Integer();
		intField.setName("count");
		intField.setDisplayName("Count");
		doc.putAttribute(intField);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertTrue(contents.toString().contains("<property name=\"count\""), "Should have property for integer");
	}

	@Test
	void generateAttributeMappingsWithLongIntegerFieldProducesPropertyElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("LongIntDoc", "myMod");
		Persistent p = simplePersistent("MOD_LongIntDoc", doc);

		LongInteger longIntField = new LongInteger();
		longIntField.setName("bigCount");
		longIntField.setDisplayName("Big Count");
		doc.putAttribute(longIntField);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertTrue(contents.toString().contains("<property name=\"bigCount\""), "Should have property for long integer");
	}

	@Test
	void generateAttributeMappingsWithColourFieldProducesPropertyElement() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("ColourDoc", "myMod");
		Persistent p = simplePersistent("MOD_ColourDoc", doc);

		Colour colourField = new Colour();
		colourField.setName("background");
		colourField.setDisplayName("Background");
		doc.putAttribute(colourField);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertTrue(contents.toString().contains("<property name=\"background\""), "Should have property for colour");
	}

	@Test
	void generateAttributeMappingsWithTransientFieldSkipsMemo() throws Exception {
		OverridableDomainGenerator gen = generator();
		DocumentImpl doc = simpleDoc("TransMemoDoc", "myMod");
		Persistent p = simplePersistent("MOD_TransMemoDoc", doc);

		Memo memo = new Memo();
		memo.setName("transientNotes");
		memo.setDisplayName("Transient Notes");
		memo.setPersistent(false);
		doc.putAttribute(memo);
		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, doc, p,
				null, new TreeSet<String>(), null, false, "");

		assertFalse(contents.toString().contains("transientNotes"), "Transient memo should be skipped");
	}

	// ---- Inverse attribute tests ------------------------------------------

	@Test
	void generateAttributeMappingsWithInverseOneToManyProducesBagElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		// The inverse references a document that has an association back to this doc
		DocumentImpl inverseDoc = new DocumentImpl();
		inverseDoc.setName("Order");
		inverseDoc.setOwningModuleName("myMod");
		Persistent inversePersistent = new Persistent();
		inversePersistent.setName("MOD_Order");
		inverseDoc.setPersistent(inversePersistent);

		DocumentImpl ownerDoc = simpleDoc("Customer", "myMod");
		Persistent p = simplePersistent("MOD_Customer", ownerDoc);

		InverseMany inverse = new InverseMany();
		inverse.setName("orders");
		inverse.setDisplayName("Orders");
		inverse.setDocumentName("Order");
		inverse.setReferenceName("customer");
		inverse.setRelationship(InverseRelationship.oneToMany);
		ownerDoc.putRelation(inverse);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		when(module.getDocument(null, "Order")).thenReturn(inverseDoc);

		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<bag name=\"orders\""), "Should have bag for inverse oneToMany");
		assertTrue(result.contains("<one-to-many entity-name=\"myModOrder\""), "Should have one-to-many");
		assertTrue(result.contains("key column=\"customer_id\""), "Should have key column with reference_id");
	}

	@Test
	void generateAttributeMappingsWithInverseOneToOneProducesOneToOneElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl inverseDoc = new DocumentImpl();
		inverseDoc.setName("Profile");
		inverseDoc.setOwningModuleName("myMod");
		Persistent inversePersistent = new Persistent();
		inversePersistent.setName("MOD_Profile");
		inverseDoc.setPersistent(inversePersistent);

		DocumentImpl ownerDoc = simpleDoc("UserDoc", "myMod");
		Persistent p = simplePersistent("MOD_UserDoc", ownerDoc);

		InverseOne inverse = new InverseOne();
		inverse.setName("profile");
		inverse.setDisplayName("Profile");
		inverse.setDocumentName("Profile");
		inverse.setReferenceName("user");
		inverse.setRelationship(InverseRelationship.oneToOne);
		ownerDoc.putRelation(inverse);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		when(module.getDocument(null, "Profile")).thenReturn(inverseDoc);

		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<one-to-one name=\"profile\""), "Should have one-to-one for inverse oneToOne");
		assertTrue(result.contains("property-ref=\"user\""), "Should have property-ref");
	}

	@Test
	void generateAttributeMappingsWithInverseManyToManyProducesManyToManyBagElement() throws Exception {
		OverridableDomainGenerator gen = generator();

		DocumentImpl inverseDoc = new DocumentImpl();
		inverseDoc.setName("Tag");
		inverseDoc.setOwningModuleName("myMod");
		Persistent inversePersistent = new Persistent();
		inversePersistent.setName("MOD_Tag");
		inverseDoc.setPersistent(inversePersistent);

		DocumentImpl ownerDoc = simpleDoc("Article", "myMod");
		Persistent p = simplePersistent("MOD_Article", ownerDoc);

		InverseMany inverse = new InverseMany();
		inverse.setName("tags");
		inverse.setDisplayName("Tags");
		inverse.setDocumentName("Tag");
		inverse.setReferenceName("articles");
		inverse.setRelationship(InverseRelationship.manyToMany);
		ownerDoc.putRelation(inverse);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		when(module.getDocument(null, "Tag")).thenReturn(inverseDoc);

		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		String result = contents.toString();
		assertTrue(result.contains("<bag name=\"tags\""), "Should have bag for manyToMany");
		assertTrue(result.contains("<many-to-many entity-name=\"myModTag\""), "Should have many-to-many");
	}

	@Test
	void generateAttributeMappingsWithNonPersistentDocInverseSkips() throws Exception {
		OverridableDomainGenerator gen = generator();

		// Referenced doc has no persistent table
		DocumentImpl inverseDoc = new DocumentImpl();
		inverseDoc.setName("Order");
		inverseDoc.setOwningModuleName("myMod");
		// No persistent set on inverseDoc

		DocumentImpl ownerDoc = simpleDoc("CustomerT", "myMod");
		Persistent p = simplePersistent("MOD_CustomerT", ownerDoc);

		InverseMany inverse = new InverseMany();
		inverse.setName("orders");
		inverse.setDisplayName("Orders");
		inverse.setDocumentName("Order");
		inverse.setReferenceName("customer");
		inverse.setRelationship(InverseRelationship.oneToMany);
		ownerDoc.putRelation(inverse);

		seedPropertyLengths(gen, p, "_dummy", 0);

		Module module = mockModule("myMod");
		when(module.getDocument(null, "Order")).thenReturn(inverseDoc);

		setRepository(gen, mock(ProvidedRepository.class));

		StringBuilder contents = new StringBuilder();
		attrMappingsMethod().invoke(gen, contents, null, module, ownerDoc, p,
				null, new TreeSet<String>(), null, false, "");

		assertFalse(contents.toString().contains("<bag name=\"orders\""), "Non-persistent referenced doc inverse should be skipped");
	}

	// ---- helper methods ---------------------------------------------------

	private static DocumentImpl simpleDoc(String name, String moduleName) {
		DocumentImpl doc = new DocumentImpl();
		doc.setName(name);
		doc.setOwningModuleName(moduleName);
		return doc;
	}

	private static Persistent simplePersistent(String tableName, DocumentImpl doc) {
		Persistent p = new Persistent();
		p.setName(tableName);
		doc.setPersistent(p);
		return p;
	}

	private static Module mockModule(String moduleName) {
		Module module = mock(Module.class);
		when(module.getName()).thenReturn(moduleName);
		return module;
	}
}
