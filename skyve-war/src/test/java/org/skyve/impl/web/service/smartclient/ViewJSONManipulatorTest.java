package org.skyve.impl.web.service.smartclient;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.View;
import org.skyve.report.ReportFormat;
import org.skyve.util.Util;

import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ViewJSONManipulatorTest extends AbstractSkyveTest {
	private static final String TEST_UXUI = "external";
	
	@Test
	void testAllFormatsVisible()
			throws Exception {
		View view = createView();

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				json.contains("_0") && json.contains("_1"));
	}

	@Test
	void testFirstFormatVisible()
			throws Exception {
		ViewImpl view = createView();
		Blurb b2 = (Blurb) ((Form) view.getContained().get(0)).getRows().get(1).getItems().get(0).getWidget();
		b2.setInvisibleConditionName("true");

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				json.contains("_0") && (!json.contains("_1")));
	}

	@Test
	void testSecondFormatVisible()
			throws Exception {
		ViewImpl view = createView();
		Blurb b1 = (Blurb) ((Form) view.getContained().get(0)).getRows().get(0).getItems().get(0).getWidget();
		b1.setInvisibleConditionName("true");

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				(!json.contains("_0")) && json.contains("_1"));
	}

	@Test
	void testFirstFormFormatsInvisible()
			throws Exception {
		ViewImpl view = createView();
		((Form) view.getContained().get(0)).setInvisibleConditionName("true");
		view.getContained().add(createForm());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				(!json.contains("_0")) && (!json.contains("_1") &&
						json.contains("_2") && json.contains("_3")));
	}

	@Test
	void testViewWithTextField() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("text"));
	}

	@Test
	void testViewWithCheckBox() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		CheckBox cb = new CheckBox();
		cb.setBinding("booleanFlag");
		item.setWidget(cb);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("booleanFlag"));
	}

	@Test
	void testViewWithCombo() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Combo combo = new Combo();
		combo.setBinding("enum3");
		item.setWidget(combo);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("enum3"));
	}

	@Test
	void testViewWithTextArea() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextArea ta = new TextArea();
		ta.setBinding("memo");
		item.setWidget(ta);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("memo"));
	}

	@Test
	void testViewWithTabPane() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Details");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		tab.getContained().add(form);

		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDataGrid() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithZoomIn() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ZoomIn zi = new ZoomIn();
		zi.setBinding("aggregatedAssociation");
		item.setWidget(zi);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithSpacer() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Spacer spacer = new Spacer();
		spacer.setPixelWidth(Integer.valueOf(10));
		item.setWidget(spacer);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithColourPicker() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ColourPicker cp = new ColourPicker();
		cp.setBinding("colour");
		item.setWidget(cp);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("colour"));
	}

	@Test
	void testViewWithRichText() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		RichText rt = new RichText();
		rt.setBinding("markup");
		item.setWidget(rt);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("markup"));
	}

	@Test
	void testViewWithHTML() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		HTML html = new HTML();
		html.setBinding("markup");
		item.setWidget(html);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithPassword() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Password pw = new Password();
		pw.setBinding("text");
		item.setWidget(pw);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithRadio() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Radio radio = new Radio();
		radio.setBinding("enum3");
		item.setWidget(radio);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("enum3"));
	}

	@Test
	void testViewWithSlider() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Slider slider = new Slider();
		slider.setBinding("normalInteger");
		item.setWidget(slider);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithSpinner() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Spinner spinner = new Spinner();
		spinner.setBinding("normalInteger");
		item.setWidget(spinner);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLookupDescription() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("aggregatedAssociation"));
	}

	@Test
	void testViewWithVBox() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		VBox vbox = new VBox();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		vbox.getContained().add(form);
		view.getContained().add(vbox);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithHBox() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		HBox hbox = new HBox();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		hbox.getContained().add(form);
		view.getContained().add(hbox);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithGeometry() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Geometry g = new Geometry();
		g.setBinding("geometry");
		item.setWidget(g);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithGeometryMap() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		GeometryMap gm = new GeometryMap();
		gm.setBinding("geometry");
		item.setWidget(gm);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLabel() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Label label = new Label();
		label.setBinding("text");
		item.setWidget(label);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithProgressBar() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ProgressBar pb = new ProgressBar();
		pb.setBinding("normalInteger");
		item.setWidget(pb);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithMapDisplay() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		MapDisplay map = new MapDisplay();
		map.setModelName("testMap");
		view.getContained().add(map);

		// Need a form for the view to be valid
		view.getContained().add(createForm());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithChart() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Chart chart = new Chart();
		chart.setType(ChartType.bar);
		chart.setModelName("testChart");
		view.getContained().add(chart);

		view.getContained().add(createForm());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDynamicImage() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DynamicImage image = new DynamicImage();
		image.setName("testImage");
		view.getContained().add(image);

		view.getContained().add(createForm());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLink() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		link.setValue("Test Link");
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://example.com");
		link.setReference(ref);
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDialogButton() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		DialogButton db = new DialogButton();
		db.setDisplayName("Open Dialog");
		db.setDialogName("testDialog");
		item.setWidget(db);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithActionButtons() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		view.getContained().add(createForm());

		view.putAction(new SaveAction().toMetaDataAction());
		view.putAction(new DeleteAction().toMetaDataAction());
		view.putAction(new OKAction().toMetaDataAction());
		view.putAction(new CancelAction().toMetaDataAction());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithBlurb() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Blurb blurb = new Blurb();
		blurb.setMarkup("{bizKey}");
		item.setWidget(blurb);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDataRepeater() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		view.getContained().add(createForm());

		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		repeater.getColumns().add(col);
		view.getContained().add(repeater);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithListGrid() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		view.getContained().add(createForm());

		ListGrid listGrid = new ListGrid();
		listGrid.setQueryName("qExpressionQuery");
		view.getContained().add(listGrid);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithContentImage() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ContentImage ci = new ContentImage();
		ci.setBinding("text");
		item.setWidget(ci);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithContentLink() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ContentLink cl = new ContentLink();
		cl.setBinding("text");
		item.setWidget(cl);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithContentSignature() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ContentSignature cs = new ContentSignature();
		cs.setBinding("text");
		item.setWidget(cs);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithListMembership() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ListMembership lm = new ListMembership();
		lm.setBinding("aggregatedCollection");
		lm.setCandidatesHeading("Available");
		lm.setMembersHeading("Selected");
		item.setWidget(lm);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithCheckMembership() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		CheckMembership cm = new CheckMembership();
		cm.setBinding("aggregatedCollection");
		item.setWidget(cm);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithStaticImage() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		StaticImage si = new StaticImage();
		si.setRelativeFile("images/test.png");
		item.setWidget(si);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLinkExternalReference() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://skyve.org");
		link.setReference(ref);
		link.setValue("Skyve");
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLinkContentReference() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		ContentReference ref = new ContentReference();
		ref.setBinding("text");
		link.setReference(ref);
		link.setValue("Download");
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLinkReportReference() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		ReportReference ref = new ReportReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		ref.setReportName("TestReport");
		ref.setFormat(ReportFormat.pdf);
		link.setReference(ref);
		link.setValue("Report");
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLinkResourceReference() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		ResourceReference ref = new ResourceReference();
		ref.setRelativeFile("resources/test.pdf");
		link.setReference(ref);
		link.setValue("Resource");
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLinkEditViewReference() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		EditViewReference ref = new EditViewReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		link.setReference(ref);
		link.setValue("Edit");
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLinkActionReference() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("testAction");
		viewAction.setResourceName("TestAction");
		view.putAction(viewAction);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		ActionReference ref = new ActionReference();
		ref.setActionName("testAction");
		link.setReference(ref);
		link.setValue("Action");
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithButton() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("testAction");
		viewAction.setResourceName("TestAction");
		view.putAction(viewAction);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Button button = new Button();
		button.setActionName("testAction");
		item.setWidget(button);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldSetDisabledEventAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("text");
		setDisabled.setDisabledConditionName("condition");
		tf.getChangedActions().add(setDisabled);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldToggleDisabledEventAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ToggleDisabledEventAction toggle = new ToggleDisabledEventAction();
		toggle.setBinding("text");
		tf.getChangedActions().add(toggle);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldToggleVisibilityEventAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ToggleVisibilityEventAction toggleVis = new ToggleVisibilityEventAction();
		toggleVis.setBinding("text");
		tf.getChangedActions().add(toggleVis);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldSetInvisibleEventAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("text");
		setInvisible.setInvisibleConditionName("condition");
		tf.getChangedActions().add(setInvisible);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldServerSideActionEventAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("testAction");
		viewAction.setResourceName("TestAction");
		view.putAction(viewAction);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("testAction");
		tf.getChangedActions().add(server);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldRerenderEventAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		tf.getChangedActions().add(rerender);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testToJSONWithRedirectUrl() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");
		view.getContained().add(createForm());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, "https://skyve.org/redirect");
		assertNotNull(json);
		assertTrue(json.contains("_redirectUrl"));
	}

	@Test
	void testViewWithListGridWithConditions() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		ListGrid lg = new ListGrid();
		lg.setQueryName("qExpressionQuery");
		lg.setDisableAddConditionName("condition");
		lg.setDisableEditConditionName("condition");
		lg.setDisableZoomConditionName("condition");
		lg.setDisableRemoveConditionName("condition");
		lg.setSelectedIdBinding("text");
		view.getContained().add(lg);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDataGridEditable() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		grid.setEditable(Boolean.TRUE);
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDataGridInline() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		grid.setInline(Boolean.TRUE);
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDataGridWithConditions() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		grid.setDisableAddConditionName("condition");
		grid.setDisableEditConditionName("condition");
		grid.setDisableZoomConditionName("condition");
		grid.setDisableRemoveConditionName("condition");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldInvisibleCondition() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		tf.setInvisibleConditionName("condition");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTextFieldDisabledCondition() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		tf.setDisabledConditionName("condition");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLookupDescriptionWithConditions() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setDescriptionBinding("aggregatedAssociation_bizKey");
		ld.setDisabledConditionName("condition");
		ld.setInvisibleConditionName("notCondition");
		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithListGridForApply() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		ListGrid lg = new ListGrid();
		lg.setQueryName("qExpressionQuery");
		view.getContained().add(lg);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, true);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithDataGridForApply() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, true);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithTabPaneWithConditions() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		TabPane tabPane = new TabPane();
		tabPane.setInvisibleConditionName("condition");
		Tab tab = new Tab();
		tab.setTitle("Tab 1");
		tab.setInvisibleConditionName("notCondition");
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		tab.getContained().add(form);
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithListQuery() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		ListGrid lg = new ListGrid();
		lg.setQueryName("qExpressionQuery");
		view.getContained().add(lg);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithLookupDescriptionRerenderEventInChangedActions() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setDescriptionBinding("aggregatedAssociation_bizKey");
		RerenderEventAction rerender = new RerenderEventAction();
		ld.getPickedActions().add(rerender);
		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testViewWithFormItemVisibleConditionOnFormLevel() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		form.setInvisibleConditionName("condition");
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	@Test
	void testToJSONIncludesGrowlsWhenContextHasGrowls() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		// Add a growl to trigger the _growls path (line 330 in ViewJSONManipulator)
		((org.skyve.impl.web.service.smartclient.SmartClientWebContext) ctx).growl(MessageSeverity.info, "Test growl message");
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("_growls"), "JSON should contain _growls key when context has growls: " + json);
	}

	@Test
	void testToJSONIncludesMessagesWhenContextHasMessages() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		// Add a message to trigger the _messages path (line 334 in ViewJSONManipulator)
		((org.skyve.impl.web.service.smartclient.SmartClientWebContext) ctx).message(MessageSeverity.error, "Test error message");
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("_messages"), "JSON should contain _messages key when context has messages: " + json);
	}

	@Test
	void testViewWithLinkReportWithParameterBinding() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		link.setValue("Report with Param");
		ReportReference ref = new ReportReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		ref.setReportName("testReport");
		ref.setFormat(ReportFormat.pdf);
		// Add a parameter with value binding to test lines 204-207
		ParameterImpl param = new ParameterImpl();
		param.setName("paramName");
		param.setValueBinding("text");
		ref.getParameters().add(param);
		link.setReference(ref);
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("paramName"), "JSON should contain the parameter name: " + json);
	}

	@Test
	void testViewWithLinkReportWithParameterValue() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		link.setValue("Report with Param Value");
		ReportReference ref = new ReportReference();
		ref.setModuleName("test");
		ref.setDocumentName("AllAttributesPersistent");
		ref.setReportName("testReport2");
		ref.setFormat(ReportFormat.pdf);
		// Add a parameter with static value (not binding) to test lines 210-212
		ParameterImpl param = new ParameterImpl();
		param.setName("staticParam");
		param.setValue("staticValue");
		ref.getParameters().add(param);
		link.setReference(ref);
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
		assertTrue(json.contains("staticParam"), "JSON should contain the static parameter name: " + json);
	}

	@Test
	void testViewWithDataGridWithImplicitRemoveAction() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setTitle("TEST");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		grid.getColumns().add(col);

		// Add a link column using ImplicitActionReference with Remove to trigger lines 183-191
		Link link = new Link();
		link.setValue("Remove");
		ImplicitActionReference implicitRef = new ImplicitActionReference();
		implicitRef.setImplicitActionName(ImplicitActionName.Remove);
		link.setReference(implicitRef);

		view.getContained().add(grid);

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, TEST_UXUI, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = mockWebContext();
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		assertNotNull(json);
	}

	private static ViewImpl createView() {
		ViewImpl result = new ViewImpl();
		result.setTitle("TEST");

		result.getContained().add(createForm());

		return result;
	}

	private static Form createForm() {
		Form f = new Form();
		f.getColumns().add(new FormColumn());
		f.getColumns().add(new FormColumn());

		FormRow r = new FormRow();
		FormItem i = new FormItem();
		Blurb b = new Blurb();
		b.setMarkup("Test {bizKey}");
		i.setWidget(b);
		r.getItems().add(i);
		f.getRows().add(r);

		r = new FormRow();
		i = new FormItem();
		b = new Blurb();
		b.setMarkup("Test {bizId}");
		i.setWidget(b);
		r.getItems().add(i);
		f.getRows().add(r);

		return f;
	}
}
