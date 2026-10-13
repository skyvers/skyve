package org.skyve.impl.metadata.view.widget.bound.input;

import java.io.StringWriter;
import java.io.StringReader;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings({"static-method", "java:S5960"}) // JUnit assertions belong in this test class.
class LookupDescriptionTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new LookupDescription());
	}

	@Test
	void showsLabelByDefaultReturnsTrue() {
		assertTrue(new LookupDescription().showsLabelByDefault());
	}

	@Test
	void descriptionBindingRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setDescriptionBinding("description");
		assertEquals("description", ld.getDescriptionBinding());
	}

	@Test
	void queryRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setQuery("qContacts");
		assertEquals("qContacts", ld.getQuery());
	}

	@Test
	void queryNullByDefault() {
		assertNull(new LookupDescription().getQuery());
	}

	@Test
	void disableEditConditionRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlDisableEditCondition("noEdit");
		assertEquals("noEdit", ld.getDisableEditCondition());
	}

	@Test
	void enableEditConditionNegatesToDisableEditCondition() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlEnableEditCondition("canEdit");
		assertEquals("notCanEdit", ld.getDisableEditCondition());
	}

	@Test
	void disableAddConditionRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlDisableAddCondition("noAdd");
		assertEquals("noAdd", ld.getDisableAddCondition());
	}

	@Test
	void enableAddConditionNegatesToDisableAddCondition() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlEnableAddCondition("canAdd");
		assertEquals("notCanAdd", ld.getDisableAddCondition());
	}

	@Test
	void disableClearConditionRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlDisableClearCondition("noClear");
		assertEquals("noClear", ld.getDisableClearCondition());
	}

	@Test
	void enableClearConditionNegatesToDisableClearCondition() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlEnableClearCondition("canClear");
		assertEquals("notCanClear", ld.getDisableClearCondition());
	}

	@Test
	void disablePickConditionRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlDisablePickCondition("noPick");
		assertEquals("noPick", ld.getDisablePickCondition());
	}

	@Test
	void enablePickConditionNegatesToDisablePickCondition() {
		LookupDescription ld = new LookupDescription();
		ld.setXmlEnablePickCondition("canPick");
		assertEquals("notCanPick", ld.getDisablePickCondition());
	}

	@Test
	void editableRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setEditable(Boolean.FALSE);
		assertEquals(Boolean.FALSE, ld.getEditable());
	}

	@Test
	void pixelWidthRoundTrip() {
		LookupDescription ld = new LookupDescription();
		ld.setPixelWidth(Integer.valueOf(300));
		assertEquals(Integer.valueOf(300), ld.getPixelWidth());
	}

	@Test
	void dropDownColumnsListNonNull() {
		assertNotNull(new LookupDescription().getDropDownColumns());
	}

	@Test
	void filterParametersListNonNull() {
		assertNotNull(new LookupDescription().getFilterParameters());
	}

	@Test
	void parametersListNonNull() {
		assertNotNull(new LookupDescription().getParameters());
	}

	@Test
	void pickedActionsListNonNull() {
		assertNotNull(new LookupDescription().getPickedActions());
	}

	@Test
	void clearedActionsListNonNull() {
		assertNotNull(new LookupDescription().getClearedActions());
	}

	@Test
	void editedActionsListNonNull() {
		assertNotNull(new LookupDescription().getEditedActions());
	}

	@Test
	void addedActionsListNonNull() {
		assertNotNull(new LookupDescription().getAddedActions());
	}

	@Test
	void propertiesMapNonNull() {
		assertNotNull(new LookupDescription().getProperties());
	}

	@Test
	void xmlEnableEditConditionIsNullByDefault() {
		assertNull(new LookupDescription().getXmlEnableEditCondition());
	}

	@Test
	void xmlEnableAddConditionIsNullByDefault() {
		assertNull(new LookupDescription().getXmlEnableAddCondition());
	}

	@Test
	void xmlEnableClearConditionIsNullByDefault() {
		assertNull(new LookupDescription().getXmlEnableClearCondition());
	}

	@Test
	void xmlEnablePickConditionIsNullByDefault() {
		assertNull(new LookupDescription().getXmlEnablePickCondition());
	}
	@Test
	void modelAttributeSurvivesXmlRoundTrip() throws JAXBException {
		JAXBContext context = JAXBContext.newInstance(LookupDescription.class);
		String xml = "<lookupDescription xmlns=\"http://www.skyve.org/xml/view\" binding=\"contact\" descriptionBinding=\"bizKey\" model=\" ContactsModel \"/>";
		LookupDescription lookup = (LookupDescription) context.createUnmarshaller().unmarshal(new StringReader(xml));
		assertEquals("ContactsModel", lookup.getModelName());
		StringWriter output = new StringWriter();
		context.createMarshaller().marshal(lookup, output);
		assertTrue(output.toString().contains("model=\"ContactsModel\""));
		lookup.setModelName("  ");
		assertNull(lookup.getModelName());
	}

}
