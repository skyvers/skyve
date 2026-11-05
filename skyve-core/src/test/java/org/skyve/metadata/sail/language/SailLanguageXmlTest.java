package org.skyve.metadata.sail.language;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestMessage.MessageType;
import org.skyve.metadata.sail.language.step.TestWidget;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.TestMenu;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;

/**
 * Unmarshals and marshals the SAIL language additions through the generated sail.xsd.
 * This depends on skyve-core/src/test/resources/schemas/sail.xsd being in sync with skyve-war/src/main/java/schemas/sail.xsd.
 */
@SuppressWarnings("static-method")
class SailLanguageXmlTest {
	private static final String HEAD = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
										"<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
										"<interaction name=\"i\"><method>";
	private static final String TAIL = "</method></interaction></automation>";

	private static final String ADDITIONS = HEAD +
			"<action name=\"Restore\" confirm=\"false\"/>" +
			"<delete confirm=\"false\"/>" +
			"<remove confirm=\"true\"/>" +
			"<ok confirm=\"false\"/>" +
			"<save confirm=\"false\"/>" +
			"<zoomOut confirm=\"false\"/>" +
			"<listGridSelect module=\"admin\" document=\"User\" rowContaining=\"jane{run}\"/>" +
			"<listGridZoom query=\"qUsers\" rowContaining=\"jane{run}\"/>" +
			"<dataGridSelect binding=\"roles\" rowContaining=\"Basic\"/>" +
			"<dataGridZoom binding=\"roles\" rowContaining=\"Basic\"/>" +
			"<dataGridEdit binding=\"roles\" rowContaining=\"Basic\"/>" +
			"<dataGridRemove binding=\"roles\" rowContaining=\"Basic\"/>" +
			"<upload binding=\"contact.image\" file=\"sail/files/logo.png\"/>" +
			"<upload action=\"ImportAction\" file=\"sail/files/data.csv\"/>" +
			"<testMessage contains=\"Backup started\" type=\"info\"/>" +
			"<testGridRows query=\"qJobs\" rowContaining=\"Backup\" minRows=\"1\" maxRows=\"2\" timeoutMillis=\"30000\"/>" +
			"<testGridRows binding=\"roles\"/>" +
			"<testWidget action=\"Restore\" visible=\"true\" enabled=\"false\"/>" +
			"<testWidget tab=\"Backup/Restore\" visible=\"false\"/>" +
			"<navigateMenu module=\"admin\" item=\"Contacts\"/>" +
			"<testMenu module=\"admin\" item=\"Users\" present=\"false\"/>" +
			TAIL;

	private static final String LANGUAGE_GAPS = HEAD +
			"<listGridFilter module=\"admin\" query=\"qContacts\" column=\"name\" value=\"Smith{run}\" operator=\"startsWith\"/>" +
			"<listGridFilter document=\"Contact\" column=\"name\"/>" +
			"<listGridClearFilter module=\"admin\" document=\"Contact\" model=\"m\"/>" +
			"<listGridSort query=\"qContacts\" column=\"name\" descending=\"true\"/>" +
			"<listGridSort query=\"qContacts\" column=\"email\"/>" +
			"<dataEnter binding=\"status\" value=\"Active\" match=\"code\"/>" +
			"<dataEnter binding=\"status\" value=\"Active Member\" match=\"label\"/>" +
			"<dataEnter binding=\"status\" value=\"2\"/>" +
			"<lookupDescriptionPick binding=\"contact\" rowContaining=\"Smith{run}\"/>" +
			"<lookupDescriptionAutoComplete binding=\"contact\" search=\"Smi\" pick=\"Smith{var:n}\"/>" +
			"<captureValue binding=\"invoiceNumber\" variable=\"invoice\"/>" +
			TAIL;

	@Test
	void unmarshalsTheLanguageGaps() {
		assertLanguageGaps(XMLMetaData.unmarshalSAILString(LANGUAGE_GAPS).getInteractions().get(0).getSteps());
	}

	@Test
	void marshalsAndUnmarshalsTheLanguageGaps() {
		String xml = XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(LANGUAGE_GAPS));
		assertLanguageGaps(XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps());
	}

	@Test
	void unmarshalsALegacyLookupDescriptionPickByRow() {
		String xml = HEAD + "<lookupDescriptionPick binding=\"contact\" row=\"1\"/><lookupDescriptionAutoComplete binding=\"contact\" search=\"S\"/>" + TAIL;
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		LookupDescriptionPick pick = (LookupDescriptionPick) steps.get(0);
		assertAll(() -> assertEquals(Integer.valueOf(1), pick.getRow()),
					() -> assertThat(pick.getRowContaining(), is(nullValue())),
					() -> assertThat(((LookupDescriptionAutoComplete) steps.get(1)).getPick(), is(nullValue())));
	}

	@Test
	void unknownFilterOperatorIsRejectedByTheSchema() {
		String xml = HEAD + "<listGridFilter query=\"q\" column=\"c\" value=\"v\" operator=\"like\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	void listGridFilterAndSortWithoutColumnAreRejectedByTheSchema() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<listGridFilter query=\"q\" value=\"v\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<listGridSort query=\"q\"/>" + TAIL)));
	}

	@Test
	void unknownMatchIsRejectedByTheSchema() {
		String xml = HEAD + "<dataEnter binding=\"status\" value=\"x\" match=\"description\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	void captureValueWithoutVariableIsRejectedByTheSchema() {
		String xml = HEAD + "<captureValue binding=\"invoiceNumber\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	void lookupDescriptionPickWithoutRowUnmarshalsButCannotExecute() {
		String xml = HEAD + "<lookupDescriptionPick binding=\"contact\"/>" + TAIL;
		Step step = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps().get(0);
		Executor executor = mock(Executor.class);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
	}

	@Test
	@SuppressWarnings("null") // before and after are present in the XML
	void proceduresAcceptTheLanguageGapSteps() {
		String steps = "<listGridFilter query=\"q\" column=\"c\"/><listGridClearFilter query=\"q\"/>" +
						"<listGridSort query=\"q\" column=\"c\"/><captureValue binding=\"b\" variable=\"v\"/>";
		String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
						"<automation uxui=\"desktop\" userAgentType=\"desktop\" xmlns=\"http://www.skyve.org/xml/sail\">" +
						"<before>" + steps + "</before>" +
						"<interaction name=\"i\"><before>" + steps + "</before><method/><after>" + steps + "</after></interaction>" +
						"<after>" + steps + "</after></automation>";
		Automation automation = XMLMetaData.unmarshalSAILString(xml);
		Interaction interaction = automation.getInteractions().get(0);
		assertAll(() -> assertEquals(4, automation.getBefore().getSteps().size()),
					() -> assertEquals(4, automation.getAfter().getSteps().size()),
					() -> assertEquals(4, interaction.getBefore().getSteps().size()),
					() -> assertEquals(4, interaction.getAfter().getSteps().size()),
					() -> assertThat(automation.getBefore().getSteps().get(3), instanceOf(CaptureValue.class)));
	}

	private static void assertLanguageGaps(List<Step> steps) {
		assertEquals(11, steps.size());
		ListGridFilter filter = (ListGridFilter) steps.get(0);
		ListGridFilter clearColumn = (ListGridFilter) steps.get(1);
		ListGridClearFilter clear = (ListGridClearFilter) steps.get(2);
		ListGridSort descending = (ListGridSort) steps.get(3);
		ListGridSort ascending = (ListGridSort) steps.get(4);
		DataEnter code = (DataEnter) steps.get(5);
		DataEnter label = (DataEnter) steps.get(6);
		DataEnter index = (DataEnter) steps.get(7);
		LookupDescriptionPick pick = (LookupDescriptionPick) steps.get(8);
		LookupDescriptionAutoComplete complete = (LookupDescriptionAutoComplete) steps.get(9);
		CaptureValue capture = (CaptureValue) steps.get(10);
		assertAll(() -> assertEquals("admin", filter.getModuleName()),
					() -> assertEquals("qContacts", filter.getQueryName()),
					() -> assertEquals("name", filter.getColumn()),
					() -> assertEquals("Smith{run}", filter.getValue()),
					() -> assertEquals(FilterOperator.startsWith, filter.getOperator()),
					() -> assertEquals("Contact", clearColumn.getDocumentName()),
					() -> assertThat(clearColumn.getValue(), is(nullValue())),
					() -> assertThat(clearColumn.getOperator(), is(nullValue())),
					() -> assertEquals(FilterOperator.contains, clearColumn.getEffectiveOperator()),
					() -> assertEquals("admin", clear.getModuleName()),
					() -> assertEquals("Contact", clear.getDocumentName()),
					() -> assertEquals("m", clear.getModelName()),
					() -> assertEquals("name", descending.getColumn()),
					() -> assertEquals(Boolean.TRUE, descending.getDescending()),
					() -> assertEquals("email", ascending.getColumn()),
					() -> assertThat(ascending.getDescending(), is(nullValue())),
					() -> assertEquals(Match.code, code.getMatch()),
					() -> assertEquals("Active", code.getValue()),
					() -> assertEquals(Match.label, label.getMatch()),
					() -> assertEquals("Active Member", label.getValue()),
					() -> assertThat(index.getMatch(), is(nullValue())),
					() -> assertEquals(Match.index, index.getEffectiveMatch()),
					() -> assertEquals("contact", pick.getBinding()),
					() -> assertEquals("Smith{run}", pick.getRowContaining()),
					() -> assertThat(pick.getRow(), is(nullValue())),
					() -> assertEquals("Smi", complete.getSearch()),
					() -> assertEquals("Smith{var:n}", complete.getPick()),
					() -> assertEquals("invoiceNumber", capture.getBinding()),
					() -> assertEquals("invoice", capture.getVariable()));
	}

	@Test
	void unmarshalsTheLanguageAdditions() {
		List<Step> steps = XMLMetaData.unmarshalSAILString(ADDITIONS).getInteractions().get(0).getSteps();
		assertAdditions(steps);
	}

	@Test
	void marshalsAndUnmarshalsTheLanguageAdditions() {
		Automation automation = XMLMetaData.unmarshalSAILString(ADDITIONS);
		String xml = XMLMetaData.marshalSAIL(automation);
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		assertAdditions(steps);
	}

	@Test
	void unmarshalsLegacyRowAndAbsentConfirm() {
		String xml = HEAD + "<listGridZoom document=\"User\" row=\"0\"/><delete/>" + TAIL;
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		ListGridZoom zoom = (ListGridZoom) steps.get(0);
		assertAll(() -> assertEquals(Integer.valueOf(0), zoom.getRow()),
					() -> assertThat(zoom.getRowContaining(), is(nullValue())),
					() -> assertThat(((Delete) steps.get(1)).getConfirm(), is(nullValue())));
	}

	@Test
	void gridStepWithoutRowUnmarshalsButCannotExecute() {
		String xml = HEAD + "<dataGridZoom binding=\"roles\"/>" + TAIL;
		Step step = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps().get(0);
		Executor executor = mock(Executor.class);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
	}

	@Test
	void switchModeRoundTripsAfterLogin() {
		String xml = HEAD + "<login user=\"admin\" password=\"admin\"/><switchMode/>" + TAIL;
		Automation automation = XMLMetaData.unmarshalSAILString(xml);
		List<Step> steps = XMLMetaData.unmarshalSAILString(XMLMetaData.marshalSAIL(automation)).getInteractions().get(0).getSteps();
		assertAll(() -> assertEquals(2, steps.size()),
					() -> assertThat(steps.get(0), instanceOf(Login.class)),
					() -> assertThat(steps.get(1), instanceOf(SwitchMode.class)));
	}

	@Test
	void switchModeWithAnAttributeIsRejectedByTheSchema() {
		String xml = HEAD + "<switchMode uxui=\"desktop\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	void unknownMessageTypeIsRejectedByTheSchema() {
		String xml = HEAD + "<testMessage contains=\"x\" type=\"fatal\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	void uploadWithoutFileIsRejectedByTheSchema() {
		String xml = HEAD + "<upload binding=\"content\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	void navigateMenuWithoutItemIsRejectedByTheSchema() {
		String xml = HEAD + "<navigateMenu module=\"admin\"/>" + TAIL;
		assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(xml));
	}

	@Test
	@SuppressWarnings("null") // before is present in the XML
	void procedureAcceptsTheNewSteps() {
		String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
						"<automation uxui=\"desktop\" userAgentType=\"desktop\" xmlns=\"http://www.skyve.org/xml/sail\">" +
						"<before><testMenu module=\"admin\"/><navigateMenu module=\"admin\" item=\"Users\"/></before>" +
						"<interaction name=\"i\"><method/></interaction></automation>";
		Procedure before = XMLMetaData.unmarshalSAILString(xml).getBefore();
		assertAll(() -> assertThat(before.getSteps().get(0), instanceOf(TestMenu.class)),
					() -> assertThat(before.getSteps().get(1), instanceOf(NavigateMenu.class)));
	}

	private static void assertAdditions(List<Step> steps) {
		assertEquals(21, steps.size());
		Upload bindingUpload = (Upload) steps.get(12);
		Upload actionUpload = (Upload) steps.get(13);
		TestMessage message = (TestMessage) steps.get(14);
		TestGridRows listRows = (TestGridRows) steps.get(15);
		TestGridRows dataRows = (TestGridRows) steps.get(16);
		TestWidget action = (TestWidget) steps.get(17);
		TestWidget tab = (TestWidget) steps.get(18);
		NavigateMenu navigate = (NavigateMenu) steps.get(19);
		TestMenu menu = (TestMenu) steps.get(20);
		assertAll(() -> assertEquals(Boolean.FALSE, ((Action) steps.get(0)).getConfirm()),
					() -> assertEquals(Boolean.FALSE, ((Delete) steps.get(1)).getConfirm()),
					() -> assertEquals(Boolean.TRUE, ((Remove) steps.get(2)).getConfirm()),
					() -> assertEquals(Boolean.FALSE, ((Ok) steps.get(3)).getConfirm()),
					() -> assertEquals(Boolean.FALSE, ((Save) steps.get(4)).getConfirm()),
					() -> assertEquals(Boolean.FALSE, ((ZoomOut) steps.get(5)).getConfirm()),
					() -> assertRowContaining(steps.get(6), ListGridSelect.class, "jane{run}"),
					() -> assertRowContaining(steps.get(7), ListGridZoom.class, "jane{run}"),
					() -> assertRowContaining(steps.get(8), DataGridSelect.class, "Basic"),
					() -> assertRowContaining(steps.get(9), DataGridZoom.class, "Basic"),
					() -> assertRowContaining(steps.get(10), DataGridEdit.class, "Basic"),
					() -> assertRowContaining(steps.get(11), DataGridRemove.class, "Basic"),
					() -> assertEquals("contact.image", bindingUpload.getBinding()),
					() -> assertThat(bindingUpload.getActionName(), is(nullValue())),
					() -> assertEquals("sail/files/logo.png", bindingUpload.getFile()),
					() -> assertEquals("ImportAction", actionUpload.getActionName()),
					() -> assertEquals("sail/files/data.csv", actionUpload.getFile()),
					() -> assertEquals("Backup started", message.getContains()),
					() -> assertEquals(MessageType.info, message.getType()),
					() -> assertEquals("qJobs", listRows.getQueryName()),
					() -> assertEquals("Backup", listRows.getRowContaining()),
					() -> assertEquals(Integer.valueOf(1), listRows.getMinRows()),
					() -> assertEquals(Integer.valueOf(2), listRows.getMaxRows()),
					() -> assertEquals(Integer.valueOf(30000), listRows.getTimeoutMillis()),
					() -> assertEquals("roles", dataRows.getBinding()),
					() -> assertEquals(1, dataRows.getEffectiveMinRows()),
					() -> assertEquals("Restore", action.getActionName()),
					() -> assertEquals(Boolean.TRUE, action.getVisible()),
					() -> assertEquals(Boolean.FALSE, action.getEnabled()),
					() -> assertEquals("Backup/Restore", tab.getTabPath()),
					() -> assertEquals(Boolean.FALSE, tab.getVisible()),
					() -> assertEquals("admin", navigate.getModuleName()),
					() -> assertEquals("Contacts", navigate.getItemName()),
					() -> assertEquals("admin", menu.getModuleName()),
					() -> assertEquals("Users", menu.getItemName()),
					() -> assertEquals(Boolean.FALSE, menu.getPresent()));
	}

	private static void assertRowContaining(Step step, Class<? extends GridRowStep> type, String expected) {
		assertThat(step, instanceOf(type));
		GridRowStep gridStep = (GridRowStep) step;
		assertAll(() -> assertEquals(expected, gridStep.getRowContaining()),
					() -> assertThat(gridStep.getRow(), is(nullValue())));
	}
}
