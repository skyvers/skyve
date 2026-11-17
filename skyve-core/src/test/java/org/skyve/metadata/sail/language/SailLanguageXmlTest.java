package org.skyve.metadata.sail.language;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
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
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridMove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool.Tool;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListMembershipMove;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.CloseWindow;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateBack;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.Refresh;
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

	private static final String APPS_GAPS = HEAD +
			"<upload action=\"ImportAction\" file=\"sail/files/data.csv\" testSuccess=\"false\"/>" +
			"<captureValue binding=\"reference\" variable=\"ref\" pattern=\"REF-(\\d+)\"/>" +
			"<captureCell query=\"qToDos\" rowContaining=\"SAIL {run}\" column=\"reference\" variable=\"cell\" pattern=\"\\d+\"/>" +
			"<captureCell binding=\"rows\" row=\"0\" column=\"name\" variable=\"row\"/>" +
			"<captureTitle variable=\"title\" pattern=\"Request (\\d+)\"/>" +
			"<dataGridLink binding=\"rows\" rowContaining=\"SAIL {run}\" text=\"Run From Row\" confirm=\"false\" testSuccess=\"false\"/>" +
			"<navigateLink text=\"Open\" exact=\"true\"/>" +
			"<comment>Made {run}</comment>" +
			"<login user=\"jane{run}\" password=\"x\" customer=\"{var:customer}\"/>" +
			TAIL;

	private static final String WORKFLOW = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
			"<workflow xmlns=\"http://www.skyve.org/xml/sail\">" +
			"<automation uxui=\"tablet\" userAgentType=\"tablet\"><interaction name=\"pf\"><method>" +
			"<captureTitle variable=\"title\"/></method></interaction></automation>" +
			"<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Verify\"><interaction name=\"sc\"><method>" +
			"<dataGridLink binding=\"rows\" row=\"0\" text=\"Go\"/></method></interaction></automation>" +
			"</workflow>";

	@Test
	void unmarshalsTheAppsGaps() {
		assertAppsGaps(XMLMetaData.unmarshalSAILString(APPS_GAPS).getInteractions().get(0).getSteps());
	}

	@Test
	void marshalsAndUnmarshalsTheAppsGaps() {
		String xml = XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(APPS_GAPS));
		assertAppsGaps(XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps());
	}

	@Test
	void unmarshalsAWorkflowOfAutomations() {
		assertWorkflow(XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW));
	}

	@Test
	void marshalsAndUnmarshalsAWorkflow() {
		String xml = XMLMetaData.marshalSAILWorkflow(XMLMetaData.unmarshalSAILWorkflowString(WORKFLOW));
		assertAll(() -> assertTrue(xml.contains(":workflow "), xml),
					() -> assertWorkflow(XMLMetaData.unmarshalSAILWorkflowString(xml)));
	}

	@Test
	void aSingleAutomationIsAWorkflowOfOne() {
		Workflow workflow = XMLMetaData.unmarshalSAILWorkflowString(LANGUAGE_GAPS);
		assertAll(() -> assertEquals(1, workflow.getAutomations().size()),
					() -> assertEquals(11, workflow.getAutomations().get(0).getInteractions().get(0).getSteps().size()));
	}

	@Test
	void aWorkflowIsNotAnAutomation() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(WORKFLOW)),
					() -> assertThrows(MetaDataException.class,
											() -> XMLMetaData.unmarshalSAILWorkflowString("<workflow xmlns=\"http://www.skyve.org/xml/sail\"/>")));
	}

	@Test
	void theNewStepsWithoutTheirRequiredAttributesAreRejectedByTheSchema() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<captureCell binding=\"rows\" row=\"0\" variable=\"v\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<captureCell binding=\"rows\" row=\"0\" column=\"c\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<captureTitle/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<dataGridLink binding=\"rows\" row=\"0\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<dataGridLink row=\"0\" text=\"Go\"/>" + TAIL)));
	}

	private static final String DATA_GRID_MOVE = HEAD +
			"<dataGridMove binding=\"rows\" rowContaining=\"Third {run}\" to=\"0\"/>" +
			"<dataGridMove binding=\"rows\" row=\"2\" to=\"1\"/>" +
			TAIL;

	@Test
	void marshalsAndUnmarshalsDataGridMove() {
		String xml = XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(DATA_GRID_MOVE));
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		DataGridMove byText = (DataGridMove) steps.get(0);
		DataGridMove byRow = (DataGridMove) steps.get(1);
		assertAll(() -> assertEquals(2, steps.size()),
					() -> assertEquals("rows", byText.getBinding()),
					() -> assertEquals("Third {run}", byText.getRowContaining()),
					() -> assertEquals(Integer.valueOf(0), byText.getTo()),
					() -> assertEquals(Integer.valueOf(2), byRow.getRow()),
					() -> assertEquals(Integer.valueOf(1), byRow.getTo()));
	}

	@Test
	void dataGridMoveWithoutItsRequiredAttributesIsRejectedByTheSchema() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<dataGridMove binding=\"rows\" row=\"0\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<dataGridMove row=\"0\" to=\"1\"/>" + TAIL)));
	}

	private static final String LIST_MEMBERSHIP_MOVE = HEAD +
			"<listMembershipMove binding=\"members\" rowContaining=\"Third {run}\" to=\"0\"/>" +
			"<listMembershipMove binding=\"members\" row=\"2\" to=\"1\"/>" +
			TAIL;

	@Test
	void marshalsAndUnmarshalsListMembershipMove() {
		String xml = XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(LIST_MEMBERSHIP_MOVE));
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		ListMembershipMove byText = (ListMembershipMove) steps.get(0);
		ListMembershipMove byRow = (ListMembershipMove) steps.get(1);
		assertAll(() -> assertEquals(2, steps.size()),
					() -> assertEquals("members", byText.getBinding()),
					() -> assertEquals("Third {run}", byText.getRowContaining()),
					() -> assertEquals(Integer.valueOf(0), byText.getTo()),
					() -> assertEquals(Integer.valueOf(2), byRow.getRow()),
					() -> assertEquals(Integer.valueOf(1), byRow.getTo()));
	}

	@Test
	void listMembershipMoveWithoutItsRequiredAttributesIsRejectedByTheSchema() {
		assertAll(() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<listMembershipMove binding=\"members\" row=\"0\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<listMembershipMove row=\"0\" to=\"1\"/>" + TAIL)));
	}

	private static void assertAppsGaps(List<Step> steps) {
		assertEquals(9, steps.size());
		Upload upload = (Upload) steps.get(0);
		CaptureValue value = (CaptureValue) steps.get(1);
		CaptureCell listCell = (CaptureCell) steps.get(2);
		CaptureCell gridCell = (CaptureCell) steps.get(3);
		CaptureTitle title = (CaptureTitle) steps.get(4);
		DataGridLink link = (DataGridLink) steps.get(5);
		NavigateLink exact = (NavigateLink) steps.get(6);
		Comment comment = (Comment) steps.get(7);
		Login login = (Login) steps.get(8);
		assertAll(() -> assertEquals(Boolean.FALSE, upload.getTestSuccess()),
					() -> assertEquals("REF-(\\d+)", value.getPattern()),
					() -> assertEquals("qToDos", listCell.getQueryName()),
					() -> assertEquals("SAIL {run}", listCell.getRowContaining()),
					() -> assertEquals("reference", listCell.getColumn()),
					() -> assertEquals("cell", listCell.getVariable()),
					() -> assertEquals("\\d+", listCell.getPattern()),
					() -> assertEquals("rows", gridCell.getBinding()),
					() -> assertEquals(Integer.valueOf(0), gridCell.getRow()),
					() -> assertThat(gridCell.getPattern(), is(nullValue())),
					() -> assertEquals("title", title.getVariable()),
					() -> assertEquals("Request (\\d+)", title.getPattern()),
					() -> assertEquals("rows", link.getBinding()),
					() -> assertEquals("SAIL {run}", link.getRowContaining()),
					() -> assertEquals("Run From Row", link.getText()),
					() -> assertEquals(Boolean.FALSE, link.getConfirm()),
					() -> assertEquals(Boolean.FALSE, link.getTestSuccess()),
					() -> assertEquals(Boolean.TRUE, exact.getExact()),
					() -> assertEquals("Made {run}", comment.getComment()),
					() -> assertEquals("{var:customer}", login.getCustomer()));
	}

	private static void assertWorkflow(Workflow workflow) {
		List<Automation> automations = workflow.getAutomations();
		assertEquals(2, automations.size());
		Automation pf = automations.get(0);
		Automation sc = automations.get(1);
		assertAll(() -> assertEquals("tablet", pf.getUxui()),
					() -> assertThat(pf.getInteractions().get(0).getSteps().get(0), instanceOf(CaptureTitle.class)),
					() -> assertEquals("desktop", sc.getUxui()),
					() -> assertEquals(Automation.TestStrategy.Verify, sc.getTestStrategy()),
					() -> assertThat(sc.getInteractions().get(0).getSteps().get(0), instanceOf(DataGridLink.class)));
	}

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
	@SuppressWarnings("java:S5976") // one named test per schema rule reads more clearly than a parameterized table of XML snippets
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

	@Test
	void aCharacterReferenceKeepsANewLineInAValue() {
		String xml = HEAD +
						"<dataEnter binding=\"memo\" value=\"line 1&#10;line 2&#10;&#10;line 4\"/>" +
						"<dataEnter binding=\"memo\" value=\"line 1\nline 2\"/>" +
						TAIL;
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		assertAll(() -> assertEquals("line 1\nline 2\n\nline 4", ((DataEnter) steps.get(0)).getValue()),
					// XML attribute-value normalisation makes a literal new line in an attribute a space
					() -> assertEquals("line 1 line 2", ((DataEnter) steps.get(1)).getValue()));
	}

	@Test
	void navigateLinkByHrefOrTextRoundTrips() {
		String xml = HEAD +
						"<navigateLink href=\"?a=e&amp;m=admin&amp;d=Contact&amp;i={var:id}\"/>" +
						"<navigateLink text=\"Open request {run}\"/>" +
						TAIL;
		Automation automation = XMLMetaData.unmarshalSAILString(xml);
		List<Step> steps = XMLMetaData.unmarshalSAILString(XMLMetaData.marshalSAIL(automation)).getInteractions().get(0).getSteps();
		NavigateLink byHref = (NavigateLink) steps.get(0);
		NavigateLink byText = (NavigateLink) steps.get(1);
		assertAll(() -> assertEquals("?a=e&m=admin&d=Contact&i={var:id}", byHref.getHref()),
					() -> assertThat(byHref.getText(), is(nullValue())),
					() -> assertEquals("Open request {run}", byText.getText()),
					() -> assertThat(byText.getHref(), is(nullValue())));
	}

	@Test
	void navigateLinkNeedsExactlyOneOfHrefAndText() {
		String xml = HEAD + "<navigateLink/><navigateLink href=\"?a=l\" text=\"Open\"/><navigateLink text=\"Open\"/>" + TAIL;
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		Step neither = steps.get(0);
		Step both = steps.get(1);
		Step textOnly = steps.get(2);
		Executor executor = mock(Executor.class);
		assertAll(() -> assertThrows(MetaDataException.class, () -> neither.execute(executor)),
					() -> assertThrows(MetaDataException.class, () -> both.execute(executor)),
					() -> assertDoesNotThrow(() -> textOnly.execute(executor)));
		verify(executor).executeNavigateLink((NavigateLink) textOnly);
	}

	private static final String WINDOWS_AND_TOOLS = HEAD +
			"<navigateLink text=\"Print Invoice\" window=\"new\"/>" +
			"<navigateLink href=\"?a=e&amp;m=admin&amp;d=Contact\" window=\"new\"/>" +
			"<navigateLink text=\"Print\" exact=\"true\" window=\"new\"/>" +
			"<closeWindow/>" +
			"<navigateBack/>" +
			"<refresh/>" +
			"<listGridTool query=\"qContacts\" tool=\"refresh\"/>" +
			"<listGridTool module=\"admin\" document=\"Contact\" model=\"m\" tool=\"clearSelection\"/>" +
			"<navigateLink text=\"Open\"/>" +
			TAIL;

	@Test
	void marshalsAndUnmarshalsTheWindowAndListGridToolSteps() {
		String xml = XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(WINDOWS_AND_TOOLS));
		List<Step> steps = XMLMetaData.unmarshalSAILString(xml).getInteractions().get(0).getSteps();
		NavigateLink byText = (NavigateLink) steps.get(0);
		NavigateLink byHref = (NavigateLink) steps.get(1);
		NavigateLink exact = (NavigateLink) steps.get(2);
		ListGridTool refresh = (ListGridTool) steps.get(6);
		ListGridTool clear = (ListGridTool) steps.get(7);
		NavigateLink sameWindow = (NavigateLink) steps.get(8);
		assertAll(() -> assertEquals(9, steps.size()),
					() -> assertTrue(xml.contains("window=\"new\""), xml),
					() -> assertEquals("Print Invoice", byText.getText()),
					() -> assertEquals(NavigateLink.Window.newWindow, byText.getWindow()),
					() -> assertEquals("?a=e&m=admin&d=Contact", byHref.getHref()),
					() -> assertEquals(NavigateLink.Window.newWindow, byHref.getWindow()),
					() -> assertEquals(Boolean.TRUE, exact.getExact()),
					() -> assertEquals(NavigateLink.Window.newWindow, exact.getWindow()),
					() -> assertThat(steps.get(3), instanceOf(CloseWindow.class)),
					() -> assertThat(steps.get(4), instanceOf(NavigateBack.class)),
					() -> assertThat(steps.get(5), instanceOf(Refresh.class)),
					() -> assertEquals("qContacts", refresh.getQueryName()),
					() -> assertEquals(Tool.refresh, refresh.getTool()),
					() -> assertEquals("admin", clear.getModuleName()),
					() -> assertEquals("Contact", clear.getDocumentName()),
					() -> assertEquals("m", clear.getModelName()),
					() -> assertEquals(Tool.clearSelection, clear.getTool()),
					() -> assertThat(sameWindow.getWindow(), is(nullValue())));
	}

	@Test
	void anUnknownWindowOrToolAndAToolStepWithoutAToolAreRejectedByTheSchema() {
		MetaDataException unknownTool = assertThrows(MetaDataException.class,
														() -> XMLMetaData.unmarshalSAILString(HEAD + "<listGridTool query=\"q\" tool=\"export\"/>" + TAIL));
		assertAll(() -> assertTrue(String.valueOf(unknownTool.getCause()).contains("export"), String.valueOf(unknownTool.getCause())),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<listGridTool query=\"q\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<navigateLink text=\"x\" window=\"same\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<closeWindow name=\"x\"/>" + TAIL)),
					() -> assertThrows(MetaDataException.class, () -> XMLMetaData.unmarshalSAILString(HEAD + "<refresh query=\"x\"/>" + TAIL)));
	}

	@Test
	@SuppressWarnings("null") // before and after are present in the XML
	void proceduresAcceptTheWindowAndListGridToolSteps() {
		String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
						"<automation uxui=\"desktop\" userAgentType=\"desktop\" xmlns=\"http://www.skyve.org/xml/sail\">" +
						"<before><navigateLink text=\"Print\" window=\"new\"/><closeWindow/><navigateBack/></before>" +
						"<interaction name=\"i\"><method/></interaction>" +
						"<after><refresh/><listGridTool query=\"q\" tool=\"clearSelection\"/></after></automation>";
		Automation automation = XMLMetaData.unmarshalSAILString(xml);
		List<Step> before = automation.getBefore().getSteps();
		List<Step> after = automation.getAfter().getSteps();
		assertAll(() -> assertThat(before.get(0), instanceOf(NavigateLink.class)),
					() -> assertThat(before.get(1), instanceOf(CloseWindow.class)),
					() -> assertThat(before.get(2), instanceOf(NavigateBack.class)),
					() -> assertThat(after.get(0), instanceOf(Refresh.class)),
					() -> assertThat(after.get(1), instanceOf(ListGridTool.class)));
	}

	@Test
	void newLinesTabsAndCarriageReturnsSurviveMarshalling() {
		String xml = HEAD +
						"<dataEnter binding=\"memo\" value=\"a&#10;&#10;b\"/>" +
						"<dataEnter binding=\"memo\" value=\"x&#9;y&#13;&#10;z\"/>" +
						"<dataEnter binding=\"name\" value=\"&lt;&amp;&quot; caf\u00e9&#10;2\"/>" +
						TAIL;
		String marshalled = XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(xml));
		List<Step> steps = XMLMetaData.unmarshalSAILString(marshalled).getInteractions().get(0).getSteps();
		assertAll(() -> assertTrue(marshalled.contains("value=\"a&#10;&#10;b\""), marshalled),
					() -> assertTrue(marshalled.contains("value=\"x&#9;y&#13;&#10;z\""), marshalled),
					() -> assertTrue(marshalled.contains("value=\"&lt;&amp;&quot; caf\u00e9&#10;2\""), marshalled),
					() -> assertEquals("a\n\nb", ((DataEnter) steps.get(0)).getValue()),
					() -> assertEquals("x\ty\r\nz", ((DataEnter) steps.get(1)).getValue()),
					() -> assertEquals("<&\" caf\u00e9\n2", ((DataEnter) steps.get(2)).getValue()));
	}

	/**
	 * The expected output was marshalled before tabs, new lines and carriage returns were written as character
	 * references: a script without them (but with markup characters, a non-ASCII character and a multi-line script
	 * element) marshals byte for byte as before.
	 */
	@Test
	void marshallingAScriptWithoutLineBreaksInAttributesIsUnchanged() throws Exception {
		String in = resource("marshal-unchanged-in.xml");
		String expected = resource("marshal-unchanged-out.xml");
		assertEquals(expected, XMLMetaData.marshalSAIL(XMLMetaData.unmarshalSAILString(in)));
	}

	private static String resource(String name) throws java.io.IOException {
		try (java.io.InputStream in = java.util.Objects.requireNonNull(SailLanguageXmlTest.class.getResourceAsStream(name), name)) {
			return new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
		}
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
