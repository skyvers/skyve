package org.skyve.impl.script;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

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
		assertThat(document.getPluralAlias(), is("Addresses"));
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
		assertThat(document.getPluralAlias(), is("Address Titles"));
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
		String script = "# Admin\n## Address\n- state enum (QLD,NSW,WA,NT,ACT,SA,VIC,TAS)";

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
		assertThat(document.getAttributes().size(), is(1));

		assertThat(document.getAttributes().get(0) instanceof org.skyve.impl.metadata.model.document.field.Text, is(true));

		Text attribute = (Text) document.getAttributes().get(0);
		assertThat(attribute.getName(), is("postCode"));
		assertThat(attribute.getDisplayName(), is("Post Code"));
		assertThat(attribute.getLength(), is(4));
	}

}
