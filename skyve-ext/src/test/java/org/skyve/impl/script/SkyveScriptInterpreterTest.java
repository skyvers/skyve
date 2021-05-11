package org.skyve.impl.script;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import org.commonmark.node.Document;
import org.commonmark.node.Node;
import org.junit.Test;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.document.ParentDocument;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;

public class SkyveScriptInterpreterTest {

	// Parser parser = Parser.builder().build();
	SkyveScriptInterpreter i;

	@Test
	@SuppressWarnings("boxing")
	public void testModuleHeading() throws Exception {
		// setup the test data
		String script = "# admin";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getName(), is("admin"));
		assertThat(module.getTitle(), is("Admin"));
		assertThat(module.getHomeDocument(), is(nullValue()));
		assertThat(module.getHomeRef(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testModuleHeadingDisplayName() throws Exception {
		// setup the test data
		String script = "# 'admin'";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getName(), is("admin"));
		assertThat(module.getTitle(), is("admin"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testModuleSetsPrototypeByDefault() throws Exception {
		// setup the test data
		String script = "# admin";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getPrototype(), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testModuleDoesntSetPrototypeWhenRequested() throws Exception {
		// setup the test data
		String script = "# admin";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.setPrototypeModules(false);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));

		ModuleMetaData module = i.getModules().get(0);

		assertThat(module.getPrototype(), is(Boolean.FALSE));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDocumentHeading() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

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

	@Test
	@SuppressWarnings("boxing")
	public void testDocumentHeadingWithPersistentName() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address `ADM_Address`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("Address"));
		assertThat(document.getSingularAlias(), is("Address"));
		assertThat(document.getPluralAlias(), is("Addresses"));
		assertThat(document.getPersistent(), is(notNullValue()));
		assertThat(document.getPersistent().getPersistentIdentifier(), is("ADM_Address"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDocumentHeadingWithDisplayName() throws Exception {
		// setup the test data
		String script = "# Admin\n## 'Address Title'";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("AddressTitle"));
		assertThat(document.getSingularAlias(), is("Address Title"));
		assertThat(document.getPluralAlias(), is("Address Title"));
		assertThat(document.getPersistent(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testDocumentHeadingWithDisplayNameAndPersistentName() throws Exception {
		// setup the test data
		String script = "# Admin\n## 'Address Title' `ADM_Address`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document.getName(), is("AddressTitle"));
		assertThat(document.getSingularAlias(), is("Address Title"));
		assertThat(document.getPluralAlias(), is("Address Titles"));
		assertThat(document.getPersistent(), is(notNullValue()));
		assertThat(document.getPersistent().getPersistentIdentifier(), is("ADM_Address"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociation() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- country Country";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Association, is(true));

		Association a = (Association) document.getAttributes().get(0);
		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertThat(a.getDocumentName(), is("Country"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociationRequired() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *country* Country";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Association, is(true));

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertThat(a.isRequired(), is(true));
		assertThat(a.getDocumentName(), is("Country"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociationAggregation() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *country* Country `aggregation`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Association, is(true));

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertThat(a.isRequired(), is(true));
		assertThat(a.getDocumentName(), is("Country"));
		assertThat(a.getType(), is(AssociationType.aggregation));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociationComposition() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- country Country `composition`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Association, is(true));

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertThat(a.isRequired(), is(false));
		assertThat(a.getDocumentName(), is("Country"));
		assertThat(a.getType(), is(AssociationType.composition));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociationRelatedDocument() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *user* admin.User";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Association, is(true));

		assertThat(i.getModules().size(), is(1));
		assertThat(i.getModules().get(0).getDocuments().size(), is(2));
		assertThat(i.getModules().get(0).getDocuments().get(0).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(0).getRef(), is("Address"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getModuleRef(), is("admin"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getRef(), is("User"));

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("user"));
		assertThat(a.getDisplayName(), is("User"));
		assertThat(a.isRequired(), is(true));
		assertThat(a.getDocumentName(), is("User"));
		assertThat(a.getType(), is(AssociationType.aggregation));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociationRelatedDocumentMultipleReferences() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *user* admin.User\n## Contact\n- user admin.User";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(2));

		assertThat(i.getModules().size(), is(1));
		assertThat(i.getModules().get(0).getDocuments().size(), is(3));
		assertThat(i.getModules().get(0).getDocuments().get(0).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(0).getRef(), is("Address"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getModuleRef(), is("admin"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getRef(), is("User"));
		assertThat(i.getModules().get(0).getDocuments().get(2).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(2).getRef(), is("Contact"));

		DocumentMetaData document1 = i.getDocuments().get(0);
		DocumentMetaData document2 = i.getDocuments().get(1);

		assertThat(document1, is(notNullValue()));
		assertThat(document1.getAttributes().size(), is(1));
		assertThat(document1.getAttributes().get(0) instanceof Association, is(true));

		assertThat(document2, is(notNullValue()));
		assertThat(document2.getAttributes().size(), is(1));
		assertThat(document2.getAttributes().get(0) instanceof Association, is(true));

		Association a1 = (Association) document1.getAttributes().get(0);
		Association a2 = (Association) document2.getAttributes().get(0);

		assertThat(a1.getName(), is("user"));
		assertThat(a1.getDisplayName(), is("User"));
		assertThat(a1.isRequired(), is(true));
		assertThat(a1.getDocumentName(), is("User"));
		assertThat(a1.getType(), is(AssociationType.aggregation));

		assertThat(a2.getName(), is("user"));
		assertThat(a2.getDisplayName(), is("User"));
		assertThat(a2.isRequired(), is(false));
		assertThat(a2.getDocumentName(), is("User"));
		assertThat(a2.getType(), is(AssociationType.aggregation));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testAssociationRequiredComposition() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *country* Country `composition`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Association, is(true));

		Association a = (Association) document.getAttributes().get(0);

		assertThat(a.getName(), is("country"));
		assertThat(a.getDisplayName(), is("Country"));
		assertThat(a.isRequired(), is(true));
		assertThat(a.getDocumentName(), is("Country"));
		assertThat(a.getType(), is(AssociationType.composition));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollection() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n+ roles Role";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Collection, is(true));

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertThat(collection.getMinCardinality(), is(0));
		assertThat(collection.getDocumentName(), is("Role"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollectionRequired() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Collection, is(true));

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertThat(collection.getMinCardinality(), is(1));
		assertThat(collection.getDocumentName(), is("Role"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollectionComposition() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role `composition`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Collection, is(true));

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertThat(collection.getMinCardinality(), is(1));
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.composition));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollectionRelatedDocument() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n+ users admin.User";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Collection, is(true));

		assertThat(i.getModules().size(), is(1));
		assertThat(i.getModules().get(0).getDocuments().size(), is(2));
		assertThat(i.getModules().get(0).getDocuments().get(0).getModuleRef(), is(nullValue()));
		assertThat(i.getModules().get(0).getDocuments().get(0).getRef(), is("Address"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getModuleRef(), is("admin"));
		assertThat(i.getModules().get(0).getDocuments().get(1).getRef(), is("User"));

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("users"));
		assertThat(collection.getDisplayName(), is("Users"));
		assertThat(collection.getMinCardinality(), is(0));
		assertThat(collection.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollectionRequiredChild() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role `child`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof Collection, is(true));

		Collection collection = (Collection) document.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDisplayName(), is("Roles"));
		assertThat(collection.getMinCardinality(), is(1));
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.child));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollectionChildUpdatesParentAfter() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n+ *roles* Role `child`\n## Role\n- roleName text 50";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(2));

		DocumentMetaData address = i.getDocuments().get(0);
		DocumentMetaData role = i.getDocuments().get(1);

		assertThat(address, is(notNullValue()));
		assertThat(address.getAttributes().size(), is(1));

		assertThat(address.getAttributes().get(0) instanceof Collection, is(true));

		Collection collection = (Collection) address.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.child));

		assertThat(role, is(notNullValue()));
		assertThat(role.getAttributes().size(), is(1));

		ParentDocument parent = role.getParentDocument();
		assertThat(parent, is(notNullValue()));
		assertThat(parent.getParentDocumentName(), is("Address"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testCollectionChildUpdatesParentBefore() throws Exception {
		// setup the test data
		String script = "# Admin\n## Role\n- roleName text 50\n## Address\n+ *roles* Role `child`";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(2));

		DocumentMetaData role = i.getDocuments().get(0);
		DocumentMetaData address = i.getDocuments().get(1);

		assertThat(address, is(notNullValue()));
		assertThat(address.getAttributes().size(), is(1));

		assertThat(address.getAttributes().get(0) instanceof Collection, is(true));

		Collection collection = (Collection) address.getAttributes().get(0);
		assertThat(collection.getName(), is("roles"));
		assertThat(collection.getDocumentName(), is("Role"));
		assertThat(collection.getType(), is(CollectionType.child));

		assertThat(role, is(notNullValue()));
		assertThat(role.getAttributes().size(), is(1));

		ParentDocument parent = role.getParentDocument();
		assertThat(parent, is(notNullValue()));
		assertThat(parent.getParentDocumentName(), is("Address"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testParse() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- active boolean";
		i = new SkyveScriptInterpreter(script);

		// call the method under test
		Node document = i.parse();

		// verify the result
		assertThat(document, is(notNullValue()));
		assertThat(document instanceof Document, is(true));
	}

	@Test
	public void testPreProcessAddsEnumDefinition() throws Exception {
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
	public void testPreProcessAddsMissingHeadingSpace() throws Exception {
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
	public void testPreProcessAddsMissingListItemSpace() throws Exception {
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
	public void testPreProcessAddsMissingRequiredDeclaration() throws Exception {
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
	public void testPreProcessAddsMissingRequiredDeclarationDisplayName() throws Exception {
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
	public void testPreProcessReplacesCollectionTypeBrackets() throws Exception {
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
	public void testPreProcessMultiple() throws Exception {
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
	public void testPreProcessValidMarkdownDoesNothing() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- active boolean";
		i = new SkyveScriptInterpreter(script);

		// perform the method under test
		String result = i.preProcess();

		// verify the result
		assertThat(result, is(script));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarAttributeDisplayName() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- 'Yes/No' boolean";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Boolean, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("yesNo"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Yes/No"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarBooleanAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- active boolean";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Boolean, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("active"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Active"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarColourAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- selectedColour colour";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Colour, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("selectedColour"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Selected Colour"));
	}
	
	@Test
	@SuppressWarnings("boxing")
	public void testScalarContentAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- content content";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Content, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("content"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Content"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarDateAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- completionDate date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));
		
		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarDateTimeAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- completionDate dateTime";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.DateTime, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarDecimal2Attribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- amount decimal2";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Decimal2, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("amount"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Amount"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarDecimal5Attribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- amount decimal5";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Decimal5, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("amount"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Amount"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarDecimal10Attribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- amount decimal10";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Decimal10, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("amount"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Amount"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarEnumAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- state enum (QLD, NSW, WA, NT, ACT, SA, VIC, TAS)";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Enumeration, is(true));
		Enumeration e = (Enumeration) document.getAttributes().get(0);
		assertThat(e.getName(), is("state"));
		assertThat(e.getDisplayName(), is("State"));
		assertThat(e.getValues().size(), is(8));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarEnumWithSpacesAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- status enum (\"Not Started\", 'In Progress', Pending Review, Complete)";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Enumeration, is(true));
		Enumeration e = (Enumeration) document.getAttributes().get(0);
		assertThat(e.getName(), is("status"));
		assertThat(e.getDisplayName(), is("Status"));
		assertThat(e.getValues().size(), is(4));

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
	@SuppressWarnings("boxing")
	public void testScalarIdAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- selectedAddressId id";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Id, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("selectedAddressId"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Selected Address Id"));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarIntegerAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- postCode integer";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Integer, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("postCode"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Post Code"));
	}

	/**
	 * Tests that an invalid scalar attribute type definition is not added to the
	 * document.
	 */
	@Test
	@SuppressWarnings("boxing")
	public void testScalarInvalidAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- amount unknown";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat("No attribtes should be added to the document", document.getAttributes().size(), is(0));
		assertThat("1 error should be returned by the interpreter", i.getErrors().size(), is(1));
		assertThat(i.getErrors().get(0).getLineNumber(), is(3));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarMultipleReqiredAttributesSetsBizKey() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *addressLine1* text 50\n- *suburb* text 50";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);
		assertThat(document.getBizKey().getExpression(), is(("Address - {addressLine1}")));

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(2));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarRequiredAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *completionDate* date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address - {completionDate}")));

		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
		assertThat(document.getAttributes().get(0).isRequired(), is(true));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarRequiredAttributeDisplayName() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *\"Completion Date\"* date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address - {completionDate}")));

		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
		assertThat(document.getAttributes().get(0).isRequired(), is(true));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarRequiredAttributeDisplayName2() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- *'Completion Date'* date";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.preProcess();
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Date, is(true));
		assertThat(document.getAttributes().get(0).getName(), is("completionDate"));
		assertThat(document.getAttributes().get(0).getDisplayName(), is("Completion Date"));
		assertThat(document.getAttributes().get(0).isRequired(), is(true));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarTextAttribute() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- postCode text 4";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getBizKey().getExpression(), is(("Address")));

		assertThat(document.getAttributes().size(), is(1));
		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Text, is(true));

		Text attribute = (Text) document.getAttributes().get(0);
		assertThat(attribute.getName(), is("postCode"));
		assertThat(attribute.getDisplayName(), is("Post Code"));
		assertThat(attribute.getLength(), is(4));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testScalarTextAttributeDisplayName() throws Exception {
		// setup the test data
		String script = "# Admin\n## Address\n- 'Address Line 1' text 4";

		// perform the method under test
		i = new SkyveScriptInterpreter(script);
		i.process();

		// verify the result
		assertThat(i.getModules().size(), is(1));
		assertThat(i.getDocuments().size(), is(1));

		DocumentMetaData document = i.getDocuments().get(0);

		assertThat(document, is(notNullValue()));
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Text, is(true));

		Text attribute = (Text) document.getAttributes().get(0);
		assertThat(attribute.getName(), is("addressLine1"));
		assertThat(attribute.getDisplayName(), is("Address Line 1"));
		assertThat(attribute.getLength(), is(4));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testSplitAttribute() throws Exception {
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
	@SuppressWarnings("static-method")
	public void testSplitAttributeDisplayName() throws Exception {
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
	@SuppressWarnings("static-method")
	public void testSplitAttributeDisplayNameSpaces() throws Exception {
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
	@SuppressWarnings("static-method")
	public void testSplitAttributeEnumShorthand() throws Exception {
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
	@SuppressWarnings("static-method")
	public void testSplitAttributeEnum() throws Exception {
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

}
