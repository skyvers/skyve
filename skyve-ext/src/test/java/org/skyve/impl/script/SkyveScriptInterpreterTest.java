package org.skyve.impl.script;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.commonmark.node.Document;
import org.commonmark.node.Node;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.document.ParentDocument;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;

@SuppressWarnings("static-method")
class SkyveScriptInterpreterTest {

	SkyveScriptInterpreter i;

	@BeforeEach
	void resetStaticState() throws Exception {
		java.lang.reflect.Field moduleField = SkyveScriptInterpreter.class.getDeclaredField("currentModule");
		moduleField.setAccessible(true);
		moduleField.set(null, null);
		java.lang.reflect.Field docField = SkyveScriptInterpreter.class.getDeclaredField("currentDocument");
		docField.setAccessible(true);
		docField.set(null, null);
		java.lang.reflect.Field parentField = SkyveScriptInterpreter.class.getDeclaredField("parentDocuments");
		parentField.setAccessible(true);
		parentField.set(null, new java.util.HashMap<>());
	}

	@Test
	void testModuleHeading() {
		// setup the test data
		String script = "# admin";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getName(), is("admin"));
		assertThat(module.getTitle(), is("Admin"));
		assertThat(module.getHomeDocument(), is(nullValue()));
		assertThat(module.getHomeRef(), is(nullValue()));
	}

	@Test
	void testModuleHeadingDisplayName() {
		// setup the test data
		String script = "# 'admin'";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getName(), is("admin"));
		assertThat(module.getTitle(), is("admin"));
	}

	@Test
	void testModuleSetsPrototypeByDefault() {
		// setup the test data
		String script = "# admin";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getPrototype(), is(Boolean.TRUE));
	}

	@Test
	void testModuleDoesntSetPrototypeWhenRequested() {
		// setup the test data
		String script = "# admin";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.setPrototypeModules(false);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getPrototype(), is(Boolean.FALSE));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testDocumentHeading() {
		// setup the test data
		String script = "# Admin\n## Address";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		ModuleMetaData module = i.getModules().get(0);
		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("Address"));
		assertThat(document.getSingularAlias(), is("Address"));
		assertThat("Plural alias should be singluar for non-persistent documents", document.getPluralAlias(), is("Address"));
		assertThat(document.getPersistent(), is(nullValue()));
		assertThat(document.getBizKey(), is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address")));

		assertThat(module.getHomeDocument(), is(notNullValue()));
		assertThat(module.getHomeRef(), is(notNullValue()));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testDocumentHeadingWithPersistentName() {
		// setup the test data
		String script = "# Admin\n## Address `ADM_Address`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("Address"));
		assertThat(document.getSingularAlias(), is("Address"));
		assertThat(document.getPluralAlias(), is("Addresses"));
		assertThat(document.getPersistent(), is(notNullValue()));
		assertThat(document.getPersistent().getPersistentIdentifier(), is("ADM_Address"));
	}

	@Test
	void testDocumentHeadingWithDisplayName() {
		// setup the test data
		String script = "# Admin\n## 'Address Title'";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("AddressTitle"));
		assertThat(document.getSingularAlias(), is("Address Title"));
		assertThat(document.getPluralAlias(), is("Address Title"));
		assertThat(document.getPersistent(), is(nullValue()));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testDocumentHeadingWithDisplayNameAndPersistentName() {
		// setup the test data
		String script = "# Admin\n## 'Address Title' `ADM_Address`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("AddressTitle"));
		assertThat(document.getSingularAlias(), is("Address Title"));
		assertThat(document.getPluralAlias(), is("Address Titles"));
		assertThat(document.getPersistent(), is(notNullValue()));
		assertThat(document.getPersistent().getPersistentIdentifier(), is("ADM_Address"));
	}

	@Test
	void testAssociation() {
		// setup the test data
		String script = "# Admin\n## Address\n- country Country";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Association);

		Association a = (Association) document.getAttributes().get(0);
		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertThat(a.getDocumentName(), is("Country"));
	}

	@Test
	void testAssociationRequired() {
		// setup the test data
		String script = "# Admin\n## Address\n- *country* Country";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Association);

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertTrue(a.isRequired());
		assertThat(a.getDocumentName(), is("Country"));
	}

	@Test
	void testAssociationAggregation() {
		// setup the test data
		String script = "# Admin\n## Address\n- *country* Country `aggregation`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Association);

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertTrue(a.isRequired());
		assertThat(a.getDocumentName(), is("Country"));
		assertThat(a.getType(), is(AssociationType.aggregation));
	}

	@Test
	void testAssociationComposition() {
		// setup the test data
		String script = "# Admin\n## Address\n- country Country `composition`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Association);

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertFalse(a.isRequired());
		assertThat(a.getDocumentName(), is("Country"));
		assertThat(a.getType(), is(AssociationType.composition));
	}

	@Test
	void testAssociationRelatedDocument() {
		// setup the test data
		String script = "# Admin\n## Address\n- *user* admin.User";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Association);

		assertEquals(1, i.getModules().size());
		assertEquals(2, i.getModules().get(0).getDocuments().size());
		assertThat(i.getModules().get(0).getDocuments().get(0).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(0).getRef(), is("Address"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getModuleRef(), is("admin"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getRef(), is("User"));

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("user"));
		assertThat(a.getDisplayName(), is("User"));
		assertTrue(a.isRequired());
		assertThat(a.getDocumentName(), is("User"));
		assertThat(a.getType(), is(AssociationType.aggregation));
	}

	@Test
	@SuppressWarnings("java:S5961")
	void testAssociationRelatedDocumentMultipleReferences() {
		// setup the test data
		String script = "# Admin\n## Address\n- *user* admin.User\n## Contact\n- user admin.User";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(2, i.getDocuments().size());

		assertEquals(1, i.getModules().size());
		assertEquals(3, i.getModules().get(0).getDocuments().size());
		assertThat(i.getModules().get(0).getDocuments().get(0).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(0).getRef(), is("Address"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getModuleRef(), is("admin"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getRef(), is("User"));
		assertThat(i.getModules().get(0).getDocuments().get(2).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(2).getRef(), is("Contact"));

		DocumentMetaData document1 = i.getDocuments().get(0);
		DocumentMetaData document2 = i.getDocuments().get(1);

		assertThat(document1, is(notNullValue()));
		assertEquals(1, document1.getAttributes().size());
		assertTrue(document1.getAttributes().get(0) instanceof Association);

		assertThat(document2, is(notNullValue()));
		assertEquals(1, document2.getAttributes().size());
		assertTrue(document2.getAttributes().get(0) instanceof Association);

		Association a1 = (Association) document1.getAttributes().get(0);
		Association a2 = (Association) document2.getAttributes().get(0);

		assertThat(a1.getName(), is("user"));
		assertThat(a1.getDisplayName(), is("User"));
		assertTrue(a1.isRequired());
		assertThat(a1.getDocumentName(), is("User"));
		assertThat(a1.getType(), is(AssociationType.aggregation));

		assertThat(a2.getName(), is("user"));
		assertThat(a2.getDisplayName(), is("User"));
		assertFalse(a2.isRequired());
		assertThat(a2.getDocumentName(), is("User"));
		assertThat(a2.getType(), is(AssociationType.aggregation));
	}

	@Test
	void testAssociationRequiredComposition() {
		// setup the test data
		String script = "# Admin\n## Address\n- *country* Country `composition`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Association);

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertTrue(a.isRequired());
		assertThat(a.getDocumentName(), is("Country"));
		assertThat(a.getType(), is(AssociationType.composition));
	}

	@Test
	void testCollection() {
		// setup the test data
		String script = "# Admin\n## Address\n+ roles Role";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Collection);

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertEquals(0, collection.getMinCardinality());
		assertThat(collection.getDocumentName(), is("Role"));
	}

	@Test
	void testCollectionRequired() {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Collection);

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertEquals(1, collection.getMinCardinality());
		assertThat(collection.getDocumentName(), is("Role"));
	}

	@Test
	void testCollectionComposition() {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role `composition`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Collection);

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertEquals(1, collection.getMinCardinality());
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.composition));
	}

	@Test
	void testCollectionRelatedDocument() {
		// setup the test data
		String script = "# Admin\n## Address\n+ users admin.User";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Collection);

		assertEquals(1, i.getModules().size());
		assertEquals(2, i.getModules().get(0).getDocuments().size());
		assertThat(i.getModules().get(0).getDocuments().get(0).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(0).getRef(), is("Address"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getModuleRef(), is("admin"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getRef(), is("User"));

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("users"));
		assertThat(collection.getDisplayName(), is("Users"));
		assertEquals(0, collection.getMinCardinality());
		assertThat(collection.getDocumentName(), is("User"));
	}

	@Test
	void testCollectionRequiredChild() {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role `child`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof Collection);

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertEquals(1, collection.getMinCardinality());
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.child));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testCollectionChildUpdatesParentAfter() {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role `child`\n## Role\n- roleName text 50";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(2, i.getDocuments().size());

		DocumentMetaData address = i.getDocuments().get(0);
		DocumentMetaData role = i.getDocuments().get(1);

		assertThat(address, is(notNullValue()));
		assertEquals(1, address.getAttributes().size());

		assertTrue(address.getAttributes().get(0) instanceof Collection);

		Collection collection = (Collection) address.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.child));

		assertThat(role, is(notNullValue()));
		assertEquals(1, role.getAttributes().size());

		ParentDocument parent = role.getParentDocument();
		assertThat(parent, is(notNullValue()));
		assertThat(parent.getParentDocumentName(), is("Address"));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testCollectionChildUpdatesParentBefore() {
		// setup the test data
		String script = "# Admin\n## Role\n- roleName text 50\n## Address\n+ *roles* Role `child`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(2, i.getDocuments().size());

		DocumentMetaData role = i.getDocuments().get(0);
		DocumentMetaData address = i.getDocuments().get(1);

		assertThat(address, is(notNullValue()));
		assertEquals(1, address.getAttributes().size());

		assertTrue(address.getAttributes().get(0) instanceof Collection);

		Collection collection = (Collection) address.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.child));

		assertThat(role, is(notNullValue()));
		assertEquals(1, role.getAttributes().size());

		ParentDocument parent = role.getParentDocument();
		assertThat(parent, is(notNullValue()));
		assertThat(parent.getParentDocumentName(), is("Address"));
	}

	@Test
	void testParse() {
		// setup the test data
		String script = "# Admin\n## Address\n- active boolean";
		i = new SkyveScriptInterpreter(script);

		// call the method under test
		Node document = i.parse();

		// verify the result
		assertThat(document, is(notNullValue()));
		assertTrue(document instanceof Document);
	}

	@Test
	@SuppressWarnings("java:S5976")
	void testPreProcessAddsEnumDefinition() {
		// setup the test data
		String script = "# Admin\n## Address\n- state (QLD,NSW,WA,NT,ACT,SA,VIC,TAS)";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- state enum (QLD,NSW,WA,NT,ACT,SA,VIC,TAS)";
		assertThat(result, is(expected));
	}
	
	@Test
	void testPreProcessAddsMissingHeadingSpace() {
		// setup the test data
		String script = "#Admin\n##Address\n- active boolean\n- completionDate date";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- active boolean\n- completionDate date";
		assertThat(result, is(expected));
	}

	@Test
	void testPreProcessAddsMissingListItemSpace() {
		// setup the test data
		String script = "# Admin\n## Address\n-active boolean\n- completionDate date";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- active boolean\n- completionDate date";
		assertThat(result, is(expected));
	}

	@Test
	void testPreProcessAddsMissingRequiredDeclaration() {
		// setup the test data
		String script = "# Admin\n## Address\n- *active boolean";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- *active* boolean";
		assertThat(result, is(expected));
	}

	@Test
	void testPreProcessAddsMissingRequiredDeclarationDisplayName() {
		// setup the test data
		String script = "# Admin\n## Address\n- *'First Name' boolean";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- *'First Name'* boolean";
		assertThat(result, is(expected));
	}

	@Test
	void testPreProcessReplacesCollectionTypeBrackets() {
		// setup the test data
		String script = "# Admin\n## Address\n- country Country [composition]";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- country Country `composition`";
		assertThat(result, is(expected));
	}

	@Test
	void testPreProcessMultiple() {
		// setup the test data
		String script = "# Admin\n## Address\n-state (QLD,NSW,WA,NT,ACT,SA,VIC,TAS)\n- *completionDate date";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		String expected = "# Admin\n## Address\n- state enum (QLD,NSW,WA,NT,ACT,SA,VIC,TAS)\n- *completionDate* date";
		assertThat(result, is(expected));
	}

	@Test
	void testPreProcessValidMarkdownDoesNothing() {
		// setup the test data
		String script = "# Admin\n## Address\n- active boolean";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		assertThat(result, is(script));
	}

	@Test
	void testScalarAttributeDisplayName() {
		// setup the test data
		String script = "# Admin\n## Address\n- 'Yes/No' boolean";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Boolean);
		assertThat(document.getAttributes().get(0).getName(), is("yesNo"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Yes/No"));
	}

	@Test
	void testScalarBooleanAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- active boolean";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Boolean);
		assertThat(document.getAttributes().get(0).getName(), is("active"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Active"));
	}

	@Test
	void testScalarColourAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- selectedColour colour";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Colour);
		assertThat(document.getAttributes().get(0).getName(), is("selectedColour"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Selected Colour"));
	}
	
	@Test
	void testScalarContentAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- content content";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Content);
		assertThat(document.getAttributes().get(0).getName(), is("content"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Content"));
	}

	@Test
	void testScalarDateAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- completionDate date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());
		
		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date);
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
	}

	@Test
	void testScalarDateTimeAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- completionDate dateTime";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.DateTime);
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
	}

	@Test
	void testScalarDecimal2Attribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- amount decimal2";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Decimal2);
		assertThat(document.getAttributes().get(0).getName(), is("amount"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Amount"));
	}

	@Test
	void testScalarDecimal5Attribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- amount decimal5";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Decimal5);
		assertThat(document.getAttributes().get(0).getName(), is("amount"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Amount"));
	}

	@Test
	void testScalarDecimal10Attribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- amount decimal10";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Decimal10);
		assertThat(document.getAttributes().get(0).getName(), is("amount"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Amount"));
	}

	@Test
	void testScalarEnumAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- state enum (QLD, NSW, WA, NT, ACT, SA, VIC, TAS)";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Enumeration);
		Enumeration e = (Enumeration) document.getAttributes().get(0);
		assertThat(e.getName(), is("state"));
		assertThat(e.getDisplayName(), is("State"));
		assertEquals(8, e.getValues().size());
	}

	@Test
	void testScalarEnumWithSpacesAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- status enum (\"Not Started\", 'In Progress', Pending Review, Complete)";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Enumeration);
		Enumeration e = (Enumeration) document.getAttributes().get(0);
		assertThat(e.getName(), is("status"));
		assertThat(e.getDisplayName(), is("Status"));
		assertEquals(4, e.getValues().size());

		assertThat(e.getValues().get(0).getCode(), is("NotStarted"));
		assertThat(e.getValues().get(0).getDescription(), is("Not Started"));

		assertThat(e.getValues().get(1).getCode(), is("InProgress"));
		assertThat(e.getValues().get(1).getDescription(), is("In Progress"));

		assertThat(e.getValues().get(2).getCode(), is("PendingReview"));
		assertThat(e.getValues().get(2).getDescription(), is("Pending Review"));

		assertThat(e.getValues().get(3).getCode(), is("Complete"));
		assertThat(e.getValues().get(3).getDescription(), is(nullValue()));
	}

	@Test
	void testScalarIdAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- selectedAddressId id";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Id);
		assertThat(document.getAttributes().get(0).getName(), is("selectedAddressId"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Selected Address Id"));
	}

	@Test
	void testScalarIntegerAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- postCode integer";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Integer);
		assertThat(document.getAttributes().get(0).getName(), is("postCode"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Post Code"));
	}

	/**
	 * Tests that an invalid scalar attribute type definition is not added to the
	 * document.
	 */
	@Test
	void testScalarInvalidAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- amount unknown";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(0, document.getAttributes().size(), "No attribtes should be added to the document");
		assertEquals(1, i.getErrors().size(), "1 error should be returned by the interpreter");
		assertEquals(3, i.getErrors().get(0).getLineNumber());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testScalarMultipleReqiredAttributesSetsBizKey() {
		// setup the test data
		String script = "# Admin\n## Address\n- *addressLine1* text 50\n- *suburb* text 50";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);
		assertThat(document.getBizKey().getExpression(), is(("Address - {addressLine1}")));

		assertThat(document, is(notNullValue()));
		assertEquals(2, document.getAttributes().size());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testScalarRequiredAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- *completionDate* date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address - {completionDate}")));

		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date);
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
		assertTrue(document.getAttributes().get(0).isRequired());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testScalarRequiredAttributeDisplayName() {
		// setup the test data
		String script = "# Admin\n## Address\n- *\"Completion Date\"* date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address - {completionDate}")));

		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date);
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
		assertTrue(document.getAttributes().get(0).isRequired());
	}

	@Test
	void testScalarRequiredAttributeDisplayName2() {
		// setup the test data
		String script = "# Admin\n## Address\n- *'Completion Date'* date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.preProcess();
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date);
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
		assertTrue(document.getAttributes().get(0).isRequired());
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testScalarTextAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- postCode text 4";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address")));

		assertEquals(1, document.getAttributes().size());
		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Text);

		Text attribute = (Text) document.getAttributes().get(0);
		assertThat(attribute.getName(), is("postCode"));
		assertThat(attribute.getDisplayName(), is("Post Code"));
		assertEquals(4, attribute.getLength());
	}

	@Test
	void testScalarTextAttributeDisplayName() {
		// setup the test data
		String script = "# Admin\n## Address\n- 'Address Line 1' text 4";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Text);

		Text attribute = (Text) document.getAttributes().get(0);
		assertThat(attribute.getName(), is("addressLine1"));
		assertThat(attribute.getDisplayName(), is("Address Line 1"));
		assertEquals(4, attribute.getLength());
	}

	@Test
	void testSplitAttribute() {
		// setup the test data
		String input = "postCode text 100";

		// call the method under test
		String[] results = SkyveScriptInterpreter.splitAttribute(input);

		// verify the result
		assertThat(Integer.valueOf(results.length), is(Integer.valueOf(3)));
		assertThat(results[0], is("postCode"));
		assertThat(results[1], is("text"));
		assertThat(results[2], is("100"));
	}

	@Test
	void testSplitAttributeDisplayName() {
		// setup the test data
		String input = "'Yes/No' boolean";

		// call the method under test
		String[] results = SkyveScriptInterpreter.splitAttribute(input);

		// verify the result
		assertThat(Integer.valueOf(results.length), is(Integer.valueOf(2)));
		assertThat(results[0], is("'Yes/No'"));
		assertThat(results[1], is("boolean"));
	}

	@Test
	void testSplitAttributeDisplayNameSpaces() {
		// setup the test data
		String input = "'Post Code' text 100";

		// call the method under test
		String[] results = SkyveScriptInterpreter.splitAttribute(input);

		// verify the result
		assertThat(Integer.valueOf(results.length), is(Integer.valueOf(3)));
		assertThat(results[0], is("'Post Code'"));
		assertThat(results[1], is("text"));
		assertThat(results[2], is("100"));
	}

	@Test
	void testSplitAttributeEnumShorthand() {
		// setup the test data
		String input = "state (sa, vic)";

		// call the method under test
		String[] results = SkyveScriptInterpreter.splitAttribute(input);

		// verify the result
		assertThat(Integer.valueOf(results.length), is(Integer.valueOf(2)));
		assertThat(results[0], is("state"));
		assertThat(results[1], is("(sa, vic)"));
	}

	@Test
	void testSplitAttributeEnum() {
		// setup the test data
		String input = "state enum (sa, vic)";

		// call the method under test
		String[] results = SkyveScriptInterpreter.splitAttribute(input);

		// verify the result
		assertThat(Integer.valueOf(results.length), is(Integer.valueOf(3)));
		assertThat(results[0], is("state"));
		assertThat(results[1], is("enum"));
		assertThat(results[2], is("(sa, vic)"));
	}

	@Test
	void testScalarGeometryAttribute() {
		// setup the test data
		String script = "# Admin\n## Address\n- location geometry";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Geometry);
		assertThat(document.getAttributes().get(0).getName(), is("location"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Location"));
	}

	@Test
	void testScalarLongIntegerAttribute() {
		// setup the test data
		String script = "# Admin\n## Account\n- balance longInteger";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.LongInteger);
		assertThat(document.getAttributes().get(0).getName(), is("balance"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Balance"));
	}

	@Test
	void testScalarMemoAttribute() {
		// setup the test data
		String script = "# Admin\n## Contact\n- notes memo";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Memo);
		assertThat(document.getAttributes().get(0).getName(), is("notes"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Notes"));
	}

	@Test
	void testScalarMarkupAttribute() {
		// setup the test data
		String script = "# Admin\n## Article\n- body markup";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Markup);
		assertThat(document.getAttributes().get(0).getName(), is("body"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Body"));
	}

	@Test
	void testScalarTimeAttribute() {
		// setup the test data
		String script = "# Admin\n## Schedule\n- startTime time";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Time);
		assertThat(document.getAttributes().get(0).getName(), is("startTime"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Start Time"));
	}

	@Test
	void testScalarTimestampAttribute() {
		// setup the test data
		String script = "# Admin\n## Log\n- createdAt timestamp";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertEquals(1, i.getModules().size());
		assertEquals(1, i.getDocuments().size());

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertEquals(1, document.getAttributes().size());

		assertTrue(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Timestamp);
		assertThat(document.getAttributes().get(0).getName(), is("createdAt"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Created At"));
	}

        @SuppressWarnings("null") // passes null on purpose
        @Test
        void testPreProcessThrowsWhenScriptIsNull() {
                i = new SkyveScriptInterpreter(null);
                assertThrows(IllegalArgumentException.class, () -> i.preProcess());
        }

        @Test
        void testPreProcessThrowsWhenScriptIsEmpty() {
                i = new SkyveScriptInterpreter("");
                assertThrows(IllegalArgumentException.class, () -> i.preProcess());
        }

        @Test
        void testMultipleModulesAddsError() {
                String script = "# Admin\n## Document1\n- name text\n# Another\n## Document2\n- name text";
                i = new SkyveScriptInterpreter(script);
                i.process();
                // Multiple modules should produce a critical error
                assertTrue(i.getErrors().size() > 0);
        }

        @Test
        void testSetPrototypeModulesFalse() {
                String script = "# Admin\n## Customer\n- name text";
                i = new SkyveScriptInterpreter(script);
                i.setPrototypeModules(false);
                i.process();
                assertEquals(1, i.getModules().size());
                assertFalse(i.isPrototypeModules());
        }

        @Test
        void testGetDocumentsReturnsDocuments() {
                String script = "# Admin\n## Customer\n- name text\n## Order\n- total decimal10";
                i = new SkyveScriptInterpreter(script);
                i.process();
                assertEquals(2, i.getDocuments().size());
        }

        @Test
        void testPreProcessReturnsScript() {
                String script = "# Admin\n## Customer\n- name text";
                i = new SkyveScriptInterpreter(script);
                String result = i.preProcess();
                assertThat(result, is(notNullValue()));
        }

	@Test
	void testTwoArgConstructorCreatesInterpreter() {
		// covers <init>@141: the two-arg constructor (calls this(script) and sets defaultModule)
		i = new SkyveScriptInterpreter("# admin\n## Customer\n- name text\n", "admin");
		i.process();
		assertEquals(1, i.getModules().size());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void testParseNullScriptThrowsIllegalArgument() {
		// covers parse@163: the throw statement when script is null
		i = new SkyveScriptInterpreter(null);
		assertThrows(IllegalArgumentException.class, () -> i.parse());
	}

	@Test
	void testInvalidScriptsAddErrors() {
		String[][] cases = {
				{"# Admin\n## Customer\n* name text\n", "star bullet marker should add error"},
				{"# `code`\n## Customer\n- name text\n", "code-span heading should add critical error"},
				{"# Admin\n## Customer\n- state enum (QLD,NSW\n", "invalid enum bracket format should add error"},
				{"# Admin\n## Customer\n- state enum\n", "enum with no brackets should add warning or error"},
				{"# Admin\n## Customer\n- name text abc\n", "non-integer text length should add error"},
				{"# Admin\n## Customer\n- *name*\n", "required attribute with no type should add warning or error"},
				{"# Admin\n## Customer\n- name foobartype\n", "unrecognised attribute type should add a warning"}};

		for (String[] testCase : cases) {
			i = new SkyveScriptInterpreter(testCase[0]);
			i.process();
			assertFalse(i.getErrors().isEmpty(), testCase[1]);
		}
	}

        @Test
        void testDocumentWithoutModuleUsesDefaultModule() {
                // covers L372 (currentModule == null) and initialiseDefaultModule() L922-933
                i = new SkyveScriptInterpreter("## Customer\n- name text 50\n", "myapp");
                i.process();
                assertEquals(1, i.getModules().size());
                assertThat(i.getModules().get(0).getName(), is("myapp"));
        }

	@Test
	void testDocumentWithoutModuleAndNoDefaultAddsCritical() {
		// covers L372 and initialiseDefaultModule() L922, L935 (addCritical path)
		i = new SkyveScriptInterpreter("## Customer\n- name text 50\n");
		i.process();
		assertFalse(i.getErrors().isEmpty(), "no default module should add critical error");
	}

        @Test
        void testTwoDocumentsInModuleCoversAppendRoleElseBranch() {
                // covers appendRole L507-516 (else branch when module already has roles from first document)
                i = new SkyveScriptInterpreter("# Admin\n## FirstDoc\n- name text 50\n\n## SecondDoc\n- age integer\n");
                i.process();
                assertEquals(1, i.getModules().size());
                assertEquals(2, i.getDocuments().size());
                // module roles should have been populated from firstDoc and reused for secondDoc
                assertFalse(i.getModules().get(0).getRoles().isEmpty(), "module should have roles");
        }

}
