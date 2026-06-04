package org.skyve.impl.sail.mock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.Test;

import jakarta.faces.FacesException;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.behavior.Behavior;

@SuppressWarnings("static-method")
class MockApplicationTest {
	private static String componentType(String className) {
		try {
			Class<?> c = Class.forName(className);
			Field f = c.getField("COMPONENT_TYPE");
			return (String) f.get(null);
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to resolve COMPONENT_TYPE for " + className, e);
		}
	}

	private static String behaviorId(String className) {
		try {
			Class<?> c = Class.forName(className);
			Field f = c.getField("BEHAVIOR_ID");
			return (String) f.get(null);
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to resolve BEHAVIOR_ID for " + className, e);
		}
	}

	@Test
	void createComponentSupportsWidePrimeFacesAndSkyveSet() {
		MockApplication app = new MockApplication();
		List<String> classNames = List.of(
				"org.primefaces.component.autocomplete.AutoComplete",
				"org.primefaces.component.accordionpanel.AccordionPanel",
				"org.primefaces.component.barchart.BarChart",
				"org.primefaces.component.breadcrumb.BreadCrumb",
				"org.primefaces.component.button.Button",
				"org.primefaces.component.colorpicker.ColorPicker",
				"org.primefaces.component.column.Column",
				"org.primefaces.component.commandbutton.CommandButton",
				"org.primefaces.component.commandlink.CommandLink",
				"org.primefaces.component.contextmenu.ContextMenu",
				"org.primefaces.component.datalist.DataList",
				"org.primefaces.component.datatable.DataTable",
				"org.primefaces.component.datepicker.DatePicker",
				"org.primefaces.component.defaultcommand.DefaultCommand",
				"org.primefaces.component.dialog.Dialog",
				"org.primefaces.component.donutchart.DonutChart",
				"org.primefaces.component.fieldset.Fieldset",
				"org.primefaces.component.fileupload.FileUpload",
				"org.primefaces.component.graphicimage.GraphicImage",
				"org.primefaces.component.inputmask.InputMask",
				"org.primefaces.component.inputtext.InputText",
				"org.primefaces.component.inputtextarea.InputTextarea",
				"org.primefaces.component.linechart.LineChart",
				"org.primefaces.component.menubutton.MenuButton",
				"org.primefaces.component.menuitem.UIMenuItem",
				"org.primefaces.component.message.Message",
				"org.primefaces.component.outputpanel.OutputPanel",
				"org.primefaces.component.overlaypanel.OverlayPanel",
				"org.primefaces.component.panel.Panel",
				"org.primefaces.component.panelgrid.PanelGrid",
				"org.primefaces.component.password.Password",
				"org.primefaces.component.picklist.PickList",
				"org.primefaces.component.piechart.PieChart",
				"org.primefaces.component.polarareachart.PolarAreaChart",
				"org.primefaces.component.progressbar.ProgressBar",
				"org.primefaces.component.radarchart.RadarChart",
				"org.primefaces.component.remotecommand.RemoteCommand",
				"org.primefaces.component.row.Row",
				"org.primefaces.component.selectonemenu.SelectOneMenu",
				"org.primefaces.component.selectoneradio.SelectOneRadio",
				"org.primefaces.component.selectbooleancheckbox.SelectBooleanCheckbox",
				"org.primefaces.component.selectmanycheckbox.SelectManyCheckbox",
				"org.primefaces.component.slider.Slider",
				"org.primefaces.component.spacer.Spacer",
				"org.primefaces.component.spinner.Spinner",
				"org.primefaces.component.steps.Steps",
				"org.primefaces.component.sticky.Sticky",
				"org.primefaces.component.tabview.Tab",
				"org.primefaces.component.tabview.TabView",
				"org.primefaces.component.texteditor.TextEditor",
				"org.primefaces.component.toolbar.Toolbar",
				"org.primefaces.component.toolbar.ToolbarGroup",
				"org.primefaces.component.tristatecheckbox.TriStateCheckbox",
				"org.primefaces.component.signature.Signature",
				"org.skyve.impl.web.faces.components.Conversation",
				"org.skyve.impl.web.faces.components.ListGrid",
				"org.skyve.impl.web.faces.components.Map",
				"org.skyve.impl.web.faces.components.SetUxUi",
				"org.skyve.impl.web.faces.components.View",
				"jakarta.faces.component.UIForm",
				"jakarta.faces.component.UIOutput",
				"jakarta.faces.component.UIParameter",
				"jakarta.faces.component.UISelectItems",
				"jakarta.faces.component.html.HtmlForm",
				"jakarta.faces.component.html.HtmlInputHidden",
				"jakarta.faces.component.html.HtmlInputText",
				"jakarta.faces.component.html.HtmlOutputLink",
				"jakarta.faces.component.html.HtmlOutputText");

		for (String className : classNames) {
			UIComponent component = app.createComponent(componentType(className));
			assertNotNull(component, className);
		}
	}

	@Test
	void createComponentSpecialCasesOverrideClientId() {
		MockApplication app = new MockApplication();
		UIComponent panelGrid = app.createComponent(componentType("jakarta.faces.component.html.HtmlPanelGrid"));
		UIComponent panelGroup = app.createComponent(componentType("jakarta.faces.component.html.HtmlPanelGroup"));

		panelGrid.setId("pg1");
		panelGroup.setId("pg2");
		assertEquals("pg1", panelGrid.getClientId());
		assertTrue(panelGroup.getClientId().startsWith("MOCKED:pg2"));
	}

	@Test
	void createComponentThrowsForUnknownType() {
		MockApplication app = new MockApplication();
		assertThrows(FacesException.class, () -> app.createComponent("unknown.component.Type"));
	}

	@Test
	void createBehaviorSupportsAjaxAndConfirm() {
		MockApplication app = new MockApplication();
		Behavior ajax = app.createBehavior(behaviorId("org.primefaces.behavior.ajax.AjaxBehavior"));
		Behavior confirm = app.createBehavior(behaviorId("org.primefaces.behavior.confirm.ConfirmBehavior"));

		assertNotNull(ajax);
		assertNotNull(confirm);
	}

	@Test
	void createBehaviorThrowsForUnknownId() {
		MockApplication app = new MockApplication();
		assertThrows(FacesException.class, () -> app.createBehavior("missingBehavior"));
	}

	@Test
	void noOpAndNullReturningMethodsRemainStable() {
		MockApplication app = new MockApplication();
		assertNotNull(app.getExpressionFactory());
		assertNull(app.getActionListener());
		assertNull(app.getDefaultLocale());
		assertNull(app.getDefaultRenderKitId());
		assertNull(app.getMessageBundle());
		assertNull(app.getNavigationHandler());
		assertNull(app.getViewHandler());
		assertNull(app.getStateManager());
		assertNull(app.getComponentTypes());
		assertNull(app.createConverter("id"));
		assertNull(app.createConverter(String.class));
		assertNull(app.getConverterIds());
		assertNull(app.getConverterTypes());
		assertNull(app.getSupportedLocales());
		assertNull(app.createValidator("id"));
		assertNull(app.getValidatorIds());

		app.setActionListener(null);
		app.setDefaultLocale(Locale.ENGLISH);
		app.setDefaultRenderKitId("rk");
		app.setMessageBundle("bundle");
		app.setNavigationHandler(null);
		app.setViewHandler(null);
		app.setStateManager(null);
		app.addComponent("t", "c");
		app.addConverter("id", "clazz");
		app.addConverter(String.class, "clazz");
		app.setSupportedLocales(List.of(Locale.ENGLISH));
		app.addValidator("id", "clazz");
	}
}
