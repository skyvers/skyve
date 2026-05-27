package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.Action.ActionShow;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("java:S5976") // separate tests preferred over parameterized here as junit-jupiter-params is not on the classpath
class ViewValidatorTest {

	@Mock
	private ProvidedRepository repository;

	@Mock
	private CustomerImpl customer;

	@Mock
	private ModuleImpl module;

	private DocumentImpl document;
	private ViewImpl view;

	@BeforeEach
	void setUp() {
		when(repository.getModule(any(), anyString())).thenReturn(module);

		document = new DocumentImpl();
		document.setOwningModuleName("testMod");
		document.setName("TestDoc");

		view = new ViewImpl();
		view.setName("edit");
	}

	private ViewValidator newValidator() {
		return new ViewValidator(view, repository, customer, document, "desktop");
	}

	// ---- empty view ----

	@Test
	void emptyView_visit_noException() {
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- visitView title / help ----

	@Test
	void viewWithTitle_noException() {
		view.setTitle("My Title");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void viewBothHelpUrlAndFileName_throws() {
		view.setHelpURL("http://example.com/help");
		view.setHelpRelativeFileName("help.html");
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void viewOnlyHelpUrl_noException() {
		view.setHelpURL("http://example.com/help");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void viewOnlyHelpRelativeFileName_noException() {
		view.setHelpRelativeFileName("help.html");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- visitSpacer — condition names ----

	@Test
	void spacer_noCondition_noException() {
		Spacer s = new Spacer();
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_trueCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("true");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_falseCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("false");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_persistedCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("persisted");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_createdCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("created");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_changedCondition_noException() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("changed");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_notCondition_validConditionInDocument_noException() {
		document.getConditions().put("myFlag", new ConditionImpl());
		Spacer s = new Spacer();
		s.setInvisibleConditionName("notMyFlag");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_validConditionInDocument_noException() {
		document.getConditions().put("myFlag", new ConditionImpl());
		Spacer s = new Spacer();
		s.setInvisibleConditionName("myFlag");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_unknownCondition_throws() {
		Spacer s = new Spacer();
		s.setInvisibleConditionName("noSuchCondition");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void spacer_notConditionBadCamelCase_throws() {
		// "notxxx" — 'x' is lower-case after "not" → must be MetaDataException
		Spacer s = new Spacer();
		s.setInvisibleConditionName("notxxx");
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- validateSize — Spacer pixel width ----

	@Test
	void spacer_positivePixelWidth_noException() {
		Spacer s = new Spacer();
		s.setPixelWidth(Integer.valueOf(100));
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void spacer_zeroPixelWidth_throws() {
		Spacer s = new Spacer();
		s.setPixelWidth(Integer.valueOf(0));
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void spacer_negativePixelWidth_throws() {
		Spacer s = new Spacer();
		s.setPixelWidth(Integer.valueOf(-5));
		view.getContained().add(s);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- visitBlurb ----

	@Test
	void blurb_withMarkup_noException() {
		Blurb b = new Blurb();
		b.setMarkup("<p>Hello</p>");
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void blurb_noMarkup_throws() {
		Blurb b = new Blurb();
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void blurb_withConditionInDocument_noException() {
		document.getConditions().put("showBlurb", new ConditionImpl());
		Blurb b = new Blurb();
		b.setMarkup("<p>Content</p>");
		b.setInvisibleConditionName("showBlurb");
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void blurb_unknownCondition_throws() {
		Blurb b = new Blurb();
		b.setMarkup("<p>Content</p>");
		b.setInvisibleConditionName("noSuchCond");
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- visitButton ----

	@Test
	void button_nullActionName_noException() {
		// null action name → validateActionName does nothing
		Button btn = new Button();
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void button_nonExistentAction_throws() {
		Button btn = new Button();
		btn.setActionName("notAnAction");
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void button_existingAction_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);

		Button btn = new Button();
		btn.setActionName("myAction");
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- HBox ----

	@Test
	void hbox_empty_noException() {
		HBox hbox = new HBox();
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void hbox_unknownCondition_throws() {
		HBox hbox = new HBox();
		hbox.setInvisibleConditionName("badCondition");
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void hbox_collapsible_noTitle_throws() {
		HBox hbox = new HBox();
		hbox.setCollapsible(Collapsible.open);
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void hbox_negativePixelWidth_throws() {
		HBox hbox = new HBox();
		hbox.setPixelWidth(Integer.valueOf(-1));
		view.getContained().add(hbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- VBox ----

	@Test
	void vbox_empty_noException() {
		VBox vbox = new VBox();
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void vbox_unknownCondition_throws() {
		VBox vbox = new VBox();
		vbox.setInvisibleConditionName("badCond");
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void vbox_withSpacerInside_noException() {
		VBox vbox = new VBox();
		Spacer s = new Spacer();
		vbox.getContained().add(s);
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- HBox inside VBox ----

	@Test
	void vbox_withHBoxInside_noException() {
		VBox vbox = new VBox();
		HBox hbox = new HBox();
		vbox.getContained().add(hbox);
		view.getContained().add(vbox);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- TabPane and Tab ----

	@Test
	void tabPane_empty_noException() {
		TabPane tabPane = new TabPane();
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void tabPane_unknownCondition_throws() {
		TabPane tabPane = new TabPane();
		tabPane.setInvisibleConditionName("badCondition");
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void tabPane_negativePixelWidth_throws() {
		TabPane tabPane = new TabPane();
		tabPane.setPixelWidth(Integer.valueOf(-1));
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void tab_withSpacerInside_noException() {
		Tab tab = new Tab();
		Spacer s = new Spacer();
		tab.getContained().add(s);
		TabPane tabPane = new TabPane();
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void tab_unknownDisabledCondition_throws() {
		Tab tab = new Tab();
		tab.setDisabledConditionName("badCond");
		TabPane tabPane = new TabPane();
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	// ---- Actions on the view ----

	@Test
	void action_saveImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Save");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_cancelImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Cancel");
		action.setImplicitName(ImplicitActionName.Cancel);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_okImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("OK");
		action.setImplicitName(ImplicitActionName.OK);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_deleteImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Delete");
		action.setImplicitName(ImplicitActionName.Delete);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_addImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Add");
		action.setImplicitName(ImplicitActionName.Add);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_editImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Edit");
		action.setImplicitName(ImplicitActionName.Edit);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_removeImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("Remove");
		action.setImplicitName(ImplicitActionName.Remove);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_newImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("New");
		action.setImplicitName(ImplicitActionName.New);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_zoomOutImplicit_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("ZoomOut");
		action.setImplicitName(ImplicitActionName.ZoomOut);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_iconShowWithIconStyleClass_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setShow(ActionShow.icon);
		action.setIconStyleClass("fa fa-save");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_iconShowWithNoIcon_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setShow(ActionShow.icon);
		view.putAction(action);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void action_iconShowWithRelativeIconFileName_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setShow(ActionShow.icon);
		action.setRelativeIconFileName("save.png");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_unknownConditionOnAction_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setDisabledConditionName("noSuchCondition");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void action_validConditionOnAction_noException() {
		document.getConditions().put("canSave", new ConditionImpl());
		ActionImpl action = new ActionImpl();
		action.setName("myAction");
		action.setImplicitName(ImplicitActionName.Save);
		action.setDisabledConditionName("canSave");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- DEFAULTS implicit action ----

	@Test
	void action_defaultsImplicit_editView_noException() {
		// DEFAULTS on an edit view expands to all edit-view implicit actions
		ActionImpl action = new ActionImpl();
		action.setName("defaults");
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		view.setName("edit");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void action_defaultsImplicit_listView_noException() {
		// DEFAULTS on a list view expands to New
		ActionImpl action = new ActionImpl();
		action.setName("defaults");
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		view.setName("list");
		view.putAction(action);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- Button icon show mode ----

	@Test
	void button_iconShowWithNoIcon_throws() {
		ActionImpl action = new ActionImpl();
		action.setName("doSomething");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);

		Button btn = new Button();
		btn.setActionName("doSomething");
		btn.setShow(ActionShow.icon);
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void button_iconShowWithIconStyleClass_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("doSomething");
		action.setImplicitName(ImplicitActionName.Save);
		action.setIconStyleClass("fa fa-check");
		view.putAction(action);

		Button btn = new Button();
		btn.setActionName("doSomething");
		btn.setShow(ActionShow.icon);
		view.getContained().add(btn);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- refreshConditionName on view ----

	@Test
	void view_refreshConditionName_unknown_throws() {
		view.setRefreshConditionName("noSuch");
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void view_refreshConditionName_valid_noException() {
		document.getConditions().put("refreshable", new ConditionImpl());
		view.setRefreshConditionName("refreshable");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- refreshActionName on view ----

	@Test
	void view_refreshActionName_nonExistent_throws() {
		view.setRefreshActionName("noSuchAction");
		ViewValidator v = newValidator();
		assertThrows(MetaDataException.class, v::visit);
	}

	@Test
	void view_refreshActionName_existingAction_noException() {
		ActionImpl action = new ActionImpl();
		action.setName("refreshNow");
		action.setImplicitName(ImplicitActionName.Save);
		view.putAction(action);
		view.setRefreshActionName("refreshNow");
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	// ---- nested containers with conditions ----

	@Test
	void hboxInsideTabWithCondition_noException() {
		document.getConditions().put("isVisible", new ConditionImpl());
		Tab tab = new Tab();
		HBox hbox = new HBox();
		hbox.setInvisibleConditionName("isVisible");
		tab.getContained().add(hbox);
		TabPane tabPane = new TabPane();
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}

	@Test
	void multipleWidgetsInView_noException() {
		document.getConditions().put("flag", new ConditionImpl());
		Spacer s1 = new Spacer();
		Spacer s2 = new Spacer();
		s2.setInvisibleConditionName("flag");
		Blurb b = new Blurb();
		b.setMarkup("<p>ok</p>");
		view.getContained().add(s1);
		view.getContained().add(s2);
		view.getContained().add(b);
		ViewValidator v = newValidator();
		assertDoesNotThrow(v::visit);
	}
}
