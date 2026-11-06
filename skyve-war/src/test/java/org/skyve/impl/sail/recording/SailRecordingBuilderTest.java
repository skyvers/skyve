package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Cancel;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;

/**
 * Tests that recorded steps become the SAIL steps they stand for, that gestures which are not steps become comments,
 * and that the XML is readable and reads back.
 */
@SuppressWarnings("static-method")
class SailRecordingBuilderTest {
	private static final long STARTED = 1_762_390_800_000L;
	private static final long STOPPED = STARTED + 15 * 60_000L;

	private static Map<String, String> step(String... keysAndValues) {
		Map<String, String> result = new LinkedHashMap<>();
		for (int i = 0; i < keysAndValues.length; i += 2) {
			result.put(keysAndValues[i], keysAndValues[i + 1]);
		}
		return result;
	}

	private static SailRecordingBuilder.Result build(List<Map<String, String>> steps) {
		return new SailRecordingBuilder(null, "admin", STARTED, STOPPED).build(steps);
	}

	private static List<Step> method(SailRecordingBuilder.Result result) {
		return result.automation().getInteractions().get(0).getSteps();
	}

	@Test
	void signsInAsTheRecordingUserWithThePasswordVariableThenSwitchesToSmartClient() {
		Automation automation = new SailRecordingBuilder("demo", "tester", STARTED, STOPPED).build(List.of()).automation();
		List<Step> before = java.util.Objects.requireNonNull(automation.getBefore()).getSteps();
		Login login = (Login) before.get(0);
		assertAll(() -> assertEquals("desktop", automation.getUxui()),
					() -> assertEquals("desktop", automation.getUserAgentType().name()),
					() -> assertNull(automation.getTestStrategy()),
					() -> assertEquals("demo", login.getCustomer()),
					() -> assertEquals("tester", login.getUser()),
					() -> assertEquals("{var:password}", login.getPassword()),
					() -> assertInstanceOf(SwitchMode.class, before.get(1)),
					() -> assertEquals(2, before.size()));
	}

	@Test
	void eachRecordedGestureBecomesItsStep() {
		List<Map<String, String>> steps = List.of(
				step("op", "navigateMenu", "module", "admin", "item", "Contacts", "view", ""),
				step("op", "listGridNew", "view", "list"),
				step("op", "dataEnter", "binding", "name", "value", "Fred", "view", "admin.Contact"),
				step("op", "dataEnter", "binding", "contactType", "value", "Person", "match", "code"),
				step("op", "tabSelect", "path", "Details::Address"),
				step("op", "action", "name", "Check", "confirm", "false"),
				step("op", "save"),
				step("op", "ok", "confirm", "false"),
				step("op", "listGridZoom", "rowContaining", "Fred"),
				step("op", "listGridZoom", "module", "admin", "query", "qContacts", "row", "3"),
				step("op", "listGridNew", "module", "admin", "document", "Contact", "model", "MyModel"),
				step("op", "cancel"),
				step("op", "delete"),
				step("op", "zoomOut"),
				step("op", "remove"),
				step("op", "navigateEdit", "module", "admin", "document", "Contact", "bizId", "123"),
				step("op", "navigateList", "module", "admin", "query", "qContacts"));
		List<Step> method = method(build(steps));
		NavigateMenu menu = (NavigateMenu) method.get(0);
		ListGridNew listNew = (ListGridNew) method.get(1);
		DataEnter text = (DataEnter) method.get(2);
		DataEnter code = (DataEnter) method.get(3);
		Action action = (Action) method.get(5);
		Ok ok = (Ok) method.get(7);
		ListGridZoom byText = (ListGridZoom) method.get(8);
		ListGridZoom byRow = (ListGridZoom) method.get(9);
		ListGridNew modelNew = (ListGridNew) method.get(10);
		NavigateEdit edit = (NavigateEdit) method.get(15);
		NavigateList list = (NavigateList) method.get(16);
		assertAll(() -> assertEquals(17, method.size()),
					() -> assertEquals("admin", menu.getModuleName()),
					() -> assertEquals("Contacts", menu.getItemName()),
					() -> assertNull(listNew.getModuleName()),
					() -> assertEquals("name", text.getBinding()),
					() -> assertEquals("Fred", text.getValue()),
					() -> assertNull(text.getMatch()),
					() -> assertEquals(Match.code, code.getMatch()),
					() -> assertEquals("Details::Address", ((TabSelect) method.get(4)).getTabPath()),
					() -> assertEquals("Check", action.getActionName()),
					() -> assertEquals(Boolean.FALSE, action.getConfirm()),
					() -> assertInstanceOf(Save.class, method.get(6)),
					() -> assertNull(((Save) method.get(6)).getConfirm()),
					() -> assertEquals(Boolean.FALSE, ok.getConfirm()),
					() -> assertEquals("Fred", byText.getRowContaining()),
					() -> assertNull(byText.getRow()),
					() -> assertEquals("qContacts", byRow.getQueryName()),
					() -> assertEquals(Integer.valueOf(3), byRow.getRow()),
					() -> assertEquals("Contact", modelNew.getDocumentName()),
					() -> assertEquals("MyModel", modelNew.getModelName()),
					() -> assertInstanceOf(Cancel.class, method.get(11)),
					() -> assertInstanceOf(Delete.class, method.get(12)),
					() -> assertInstanceOf(ZoomOut.class, method.get(13)),
					() -> assertInstanceOf(Remove.class, method.get(14)),
					() -> assertEquals("123", edit.getBizId()),
					() -> assertEquals("qContacts", list.getQueryName()));
	}

	/**
	 * A gesture is never dropped: an unmapped gesture, an unknown op-code and an incomplete step each become a comment
	 * before the next step, and those after the last step go at the end of the method.
	 */
	@Test
	void gesturesThatAreNotStepsBecomeComments() {
		List<Map<String, String>> steps = List.of(
				step("op", "navigateMenu", "module", "admin", "item", "Contacts"),
				step("op", "unmapped", "text", "click on ToolStripButton #isc_1 \"Export\""),
				step("op", "dataEnter", "binding", "name", "value", "  "),
				step("op", "listGridZoom", "row", "1", "rowContaining", "Fred"),
				step("op", "frobnicate", "x", "1", "id", "p-1", "view", "list"),
				step("op", "save"),
				step("op", "unmapped", "text", "click on link \"Help\""));
		SailRecordingBuilder.Result result = build(steps);
		List<Step> method = method(result);
		List<String> beforeSave = result.commentsFor(method.get(1));
		assertAll(() -> assertEquals(2, method.size()),
					() -> assertEquals(List.of("unmapped: click on ToolStripButton #isc_1 \"Export\"",
												"unmapped: dataEnter binding=\"name\" value=\"  \"",
												"unmapped: listGridZoom row=\"1\" rowContaining=\"Fred\"",
												"unmapped: frobnicate x=\"1\""), beforeSave),
					() -> assertEquals(List.of("unmapped: click on link \"Help\""),
										result.commentsFor(result.automation().getInteractions().get(0))));
	}

	/**
	 * Going back to the Control Panel to stop is not part of the workflow, unless something was done after it.
	 */
	@Test
	void stepsThatGoBackToTheControlPanelToStopAreLeftOut() {
		Map<String, String> work = step("op", "navigateMenu", "module", "admin", "item", "Contacts");
		Map<String, String> back = step("op", "navigateMenu", "module", "admin", "item", "DevOps/Control Panel",
											"target", "admin.ControlPanel", "view", "admin.Contact");
		Map<String, String> tab = step("op", "tabSelect", "path", "SAIL", "view", "admin.ControlPanel");
		Map<String, String> elsewhere = step("op", "tabSelect", "path", "Details", "view", "admin.Contact");
		assertAll(() -> assertEquals(1, method(build(List.of(work, back, tab))).size()),
					() -> assertEquals(1, method(build(List.of(work, back))).size()),
					() -> assertEquals(4, method(build(List.of(work, back, tab, elsewhere))).size()));
	}

	@Test
	void aRecordingThatDoesNotStartWithANavigationSaysWhere() {
		SailRecordingBuilder.Result result = build(List.of(step("op", "save")));
		String comment = result.commentsFor(method(result).get(0)).get(0);
		assertTrue(comment.startsWith("The recording started in the Control Panel."), comment);
	}

	/**
	 * The script is for people to read and edit: the SAIL default namespace, one step per line, attributes in a
	 * fixed order, comments as XML comments, and new lines kept as character references.
	 */
	@Test
	void theScriptIsReadableXml() {
		List<Map<String, String>> steps = new ArrayList<>();
		steps.add(step("op", "navigateMenu", "item", "Contacts", "module", "admin"));
		steps.add(step("op", "listGridZoom", "rowContaining", "Fred", "query", "qContacts", "module", "admin"));
		steps.add(step("op", "unmapped", "text", "a -- b-"));
		steps.add(step("op", "dataEnter", "value", "a\nb", "binding", "description"));
		steps.add(step("op", "action", "confirm", "false", "name", "Check"));
		String xml = SailRecordingBuilder.toXml(build(steps));
		SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm");
		String expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
							"<automation uxui=\"desktop\" userAgentType=\"desktop\" xmlns=\"http://www.skyve.org/xml/sail\">\n" +
							"\t<!-- Recorded in SmartClient by admin from " + format.format(new Date(STARTED)) + " to " +
								format.format(new Date(STOPPED)) + " (5 recorded gestures). -->\n" +
							"\t<!-- The password is never recorded: define the variable \"password\" before running this script, or replace {var:password}. -->\n" +
							"\t<before>\n" +
							"\t\t<login user=\"admin\" password=\"{var:password}\"/>\n" +
							"\t\t<switchMode/>\n" +
							"\t</before>\n" +
							"\t<interaction name=\"Recording\">\n" +
							"\t\t<method>\n" +
							"\t\t\t<navigateMenu module=\"admin\" item=\"Contacts\"/>\n" +
							"\t\t\t<listGridZoom module=\"admin\" query=\"qContacts\" rowContaining=\"Fred\"/>\n" +
							"\t\t\t<!-- unmapped: a - - b- -->\n" +
							"\t\t\t<dataEnter binding=\"description\" value=\"a&#10;b\"/>\n" +
							"\t\t\t<action name=\"Check\" confirm=\"false\"/>\n" +
							"\t\t</method>\n" +
							"\t</interaction>\n" +
							"</automation>\n";
		assertEquals(expected, xml);

		Automation read = XMLMetaData.unmarshalSAILString(xml);
		List<Step> method = read.getInteractions().get(0).getSteps();
		assertAll(() -> assertEquals(4, method.size()),
					() -> assertEquals("a\nb", ((DataEnter) method.get(2)).getValue()),
					() -> assertFalse(xml.contains("ns2:")));
	}

	/**
	 * Context for whoever edits the script: a heading per screen, a data entry's label and earlier value, a zoomed
	 * row's bizKey and values, and what responses showed and changed, each before the step it belongs with.
	 */
	@Test
	void contextIsWrittenAsComments() {
		List<Map<String, String>> steps = List.of(
				step("op", "navigateMenu", "module", "admin", "item", "Contacts", "screen", "admin.ControlPanel (Control Panel), edit view, SmartClient"),
				step("op", "listGridZoom", "rowContaining", "Fred", "rowBizKey", "Fred Smith", "rowTexts", "Fred Smith | Person",
						"screen", "list admin_qContacts, SmartClient"),
				step("op", "dataEnter", "binding", "mobile", "value", "0400", "label", "Mobile", "before", "",
						"screen", "admin.Contact (Contact), edit view, SmartClient"),
				step("op", "dataEnter", "binding", "name", "value", "Fred", "label", "Name", "before", "Frederick",
						"screen", "admin.Contact (Contact), edit view, SmartClient"),
				step("op", "save", "screen", "admin.Contact (Contact), edit view, SmartClient"),
				step("op", "context", "text", "changed on screen: email1 \"\" -> \"a@b\""),
				step("op", "context", "text", "shown: info growl \"Saved\""),
				step("op", "ok", "screen", "admin.Contact (Contact), edit view, SmartClient"));
		SailRecordingBuilder.Result result = build(steps);
		List<Step> method = method(result);
		assertAll(() -> assertEquals(6, method.size()),
					() -> assertEquals(List.of("screen: admin.ControlPanel (Control Panel), edit view, SmartClient"), result.commentsFor(method.get(0))),
					() -> assertEquals(List.of("screen: list admin_qContacts, SmartClient",
												"row: bizKey \"Fred Smith\", showing Fred Smith | Person"), result.commentsFor(method.get(1))),
					() -> assertEquals(List.of("screen: admin.Contact (Contact), edit view, SmartClient", "\"Mobile\" was empty"),
										result.commentsFor(method.get(2))),
					() -> assertEquals(List.of("\"Name\" was \"Frederick\""), result.commentsFor(method.get(3))),
					() -> assertEquals(List.of(), result.commentsFor(method.get(4))),
					() -> assertEquals(List.of("changed on screen: email1 \"\" -> \"a@b\"", "shown: info growl \"Saved\""),
										result.commentsFor(method.get(5))));
	}

	/**
	 * A step the check cannot confirm, or a row named by text it did not show, becomes a comment saying why; it is
	 * never dropped.
	 */
	@Test
	void unconfirmedStepsBecomeCommentsSayingWhy() {
		List<Map<String, String>> steps = List.of(
				step("op", "navigateMenu", "module", "admin", "item", "Contacts"),
				step("op", "listGridZoom", "rowContaining", "Fred", "rowTexts", "Jane | Person"),
				step("op", "dataEnter", "binding", "nope", "value", "x", "view", "admin.Contact"),
				step("op", "save"));
		SailRecordingBuilder.Result result = new SailRecordingBuilder(null, "admin", STARTED, STOPPED)
				.check(recorded -> "nope".equals(recorded.get("binding")) ? "the admin.Contact view has no a widget for the binding nope" : null)
				.build(steps);
		List<Step> method = method(result);
		assertAll(() -> assertEquals(2, method.size()),
					() -> assertEquals(List.of("unconfirmed: listGridZoom rowContaining=\"Fred\" - the row did not show \"Fred\"",
												"unconfirmed: dataEnter binding=\"nope\" value=\"x\" - the admin.Contact view has no a widget for the binding nope"),
										result.commentsFor(method.get(1))));
	}

	@Test
	void theHeaderHasTheRolesReadyForViewRolesAndTheUxUi() {
		SailRecordingBuilder.Result result = new SailRecordingBuilder(null, "jane", STARTED, STOPPED)
				.roles(List.of("admin.AppUser", "admin.BasicUser"))
				.uxui("desktop")
				.build(List.of());
		List<String> header = result.commentsFor(result.automation());
		assertAll(() -> assertEquals("The recorded user's roles, for the test: viewRoles(\"jane\", \"admin.AppUser\", \"admin.BasicUser\");", header.get(2)),
					() -> assertEquals("The recorded user got the UX/UI desktop.", header.get(3)));
	}

	@Test
	void commentTextCannotCloseTheComment() {
		assertEquals(" a - - - b - ", SailRecordingBuilder.commentText("a --- b -"));
	}
}
