package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.DownloadAction;
import org.skyve.impl.metadata.repository.view.actions.NewAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.PrintAction;
import org.skyve.impl.metadata.repository.view.actions.RemoveAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.metadata.view.View.ViewType;

import modules.test.AbstractSkyveTest;

class SmartClientViewRendererTest extends AbstractSkyveTest {

	private static final String UXUI = "external";

	@Test
	void renderEmptyEditView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithForm() {
		ViewImpl view = createFormView();
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithTextFieldGeneratesFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'text'"));
	}

	@Test
	void renderViewWithCheckBox() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("checkbox"));
	}

	@Test
	void renderViewWithCombo() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("select"));
	}

	@Test
	void renderViewWithTextArea() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("textArea"));
	}

	@Test
	void renderViewWithBlurb() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Blurb blurb = new Blurb();
		blurb.setMarkup("<b>Test blurb content</b>");
		view.getContained().add(blurb);

		// Need a form for viewHasAtLeastOneForm check
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithTabPane() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Tab 1");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizTabPane"));
	}

	@Test
	void renderViewWithDataGrid() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		// Need a form for viewHasAtLeastOneForm check
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderCreateView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.create.toString());
		view.setTitle("Create Test");
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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("create=isc.BizContainer"));
	}

	@Test
	void getCodeReturnsStringBuilder() {
		ViewImpl view = createFormView();
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, true);
		renderer.visit();
		assertNotNull(renderer.getCode());
		assertFalse(renderer.getCode().isEmpty());
	}

	@Test
	void renderViewWithPassword() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithRadio() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithRichText() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithHtml() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithSpinner() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithSlider() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithColourPicker() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLookupDescription() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithGeometry() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Geometry geo = new Geometry();
		geo.setBinding("geometry");
		item.setWidget(geo);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLabel() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithProgressBar() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ProgressBar pb = new ProgressBar();
		pb.setBinding("decimal2");
		item.setWidget(pb);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithZoomIn() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithSpacer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithStaticImage() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		StaticImage img = new StaticImage();
		img.setRelativeFile("images/test.png");
		view.getContained().add(img);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithHBox() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithMultipleFormsInTabPane() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		TabPane tabPane = new TabPane();

		Tab tab1 = new Tab();
		tab1.setTitle("Details");
		Form form1 = new Form();
		form1.getColumns().add(new FormColumn());
		FormRow row1 = new FormRow();
		form1.getRows().add(row1);
		FormItem item1 = new FormItem();
		TextField tf1 = new TextField();
		tf1.setBinding("text");
		item1.setWidget(tf1);
		row1.getItems().add(item1);
		tab1.getContained().add(form1);

		Tab tab2 = new Tab();
		tab2.setTitle("More");
		Form form2 = new Form();
		form2.getColumns().add(new FormColumn());
		FormRow row2 = new FormRow();
		form2.getRows().add(row2);
		FormItem item2 = new FormItem();
		TextArea ta = new TextArea();
		ta.setBinding("memo");
		item2.setWidget(ta);
		row2.getItems().add(item2);
		tab2.getContained().add(form2);

		tabPane.getTabs().add(tab1);
		tabPane.getTabs().add(tab2);
		view.getContained().add(tabPane);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizTabPane"));
	}

	@Test
	void renderViewWithVBox() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		org.skyve.impl.metadata.view.container.VBox vbox = new org.skyve.impl.metadata.view.container.VBox();
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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizVBox"));
	}

	@Test
	void renderViewWithContentImage() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ContentImage ci =
				new org.skyve.impl.metadata.view.widget.bound.input.ContentImage();
		ci.setBinding("text");
		item.setWidget(ci);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithContentSignature() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ContentSignature cs =
				new org.skyve.impl.metadata.view.widget.bound.input.ContentSignature();
		cs.setBinding("text");
		item.setWidget(cs);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataRepeater() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater repeater =
				new org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater();
		repeater.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		repeater.getColumns().add(col);
		view.getContained().add(repeater);

		// Need a form for viewHasAtLeastOneForm check
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithListMembership() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.ListMembership lm =
				new org.skyve.impl.metadata.view.widget.bound.input.ListMembership();
		lm.setBinding("aggregatedCollection");
		item.setWidget(lm);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithCheckMembership() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		org.skyve.impl.metadata.view.widget.bound.input.CheckMembership cm =
				new org.skyve.impl.metadata.view.widget.bound.input.CheckMembership();
		cm.setBinding("aggregatedCollection");
		item.setWidget(cm);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLink() {
		ViewImpl view = createFormView();

		Link link = new Link();
		link.setValue("External Site");
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://example.com");
		link.setReference(ref);
		view.getContained().add(link);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithMapDisplay() {
		ViewImpl view = createFormView();

		MapDisplay map = new MapDisplay();
		map.setModelName("testMap");
		map.setPixelWidth(Integer.valueOf(400));
		map.setPixelHeight(Integer.valueOf(300));
		view.getContained().add(map);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithChart() {
		ViewImpl view = createFormView();

		Chart chart = new Chart();
		chart.setType(ChartType.bar);
		chart.setModelName("testChart");
		view.getContained().add(chart);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithDynamicImage() {
		org.skyve.metadata.model.document.Document adpd = m.getDocument(c, "AllAttributesDynamicPersistent");
		ViewImpl view = createFormView();

		DynamicImage image = new DynamicImage();
		image.setName("TestDynamicImage");
		view.getContained().add(image);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, adpd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithDialogButton() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithListGrid() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListGrid listGrid = new ListGrid();
		listGrid.setQueryName("qExpressionQuery");
		view.getContained().add(listGrid);

		// Need a form for viewHasAtLeastOneForm check
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithTreeGrid() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		TreeGrid treeGrid = new TreeGrid();
		treeGrid.setQueryName("qExpressionQuery");
		view.getContained().add(treeGrid);

		// Need a form for viewHasAtLeastOneForm check
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithListRepeater() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListRepeater listRepeater = new ListRepeater();
		listRepeater.setQueryName("qExpressionQuery");
		view.getContained().add(listRepeater);

		// Need a form for viewHasAtLeastOneForm check
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithActions() {
		ViewImpl view = createFormView();

		view.putAction(new SaveAction().toMetaDataAction());
		view.putAction(new DeleteAction().toMetaDataAction());
		view.putAction(new OKAction().toMetaDataAction());
		view.putAction(new CancelAction().toMetaDataAction());

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithGeometryMap() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithContentLink() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithSidebar() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		view.getContained().add(createForm());

		Sidebar sidebar = new Sidebar();
		Form sidebarForm = new Form();
		sidebarForm.getColumns().add(new FormColumn());
		FormRow sidebarRow = new FormRow();
		sidebarForm.getRows().add(sidebarRow);
		FormItem sidebarItem = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		sidebarItem.setWidget(tf);
		sidebarRow.getItems().add(sidebarItem);
		sidebar.getContained().add(sidebarForm);
		view.setSidebar(sidebar);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithMoreActions() {
		ViewImpl view = createFormView();

		CustomAction customAction = new CustomAction();
		customAction.setClassName("modules.test.SomeAction");
		view.putAction(customAction.toMetaDataAction());
		view.putAction(new NewAction().toMetaDataAction());
		view.putAction(new ZoomOutAction().toMetaDataAction());

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithBizExportImportActions() {
		ViewImpl view = createFormView();

		BizExportAction bizExport = new BizExportAction();
		bizExport.setClassName("modules.test.Action");
		view.putAction(bizExport.toMetaDataAction());
		BizImportAction bizImport = new BizImportAction();
		bizImport.setClassName("modules.test.Action");
		view.putAction(bizImport.toMetaDataAction());
		DownloadAction download = new DownloadAction();
		download.setClassName("modules.test.Action");
		view.putAction(download.toMetaDataAction());
		UploadAction upload = new UploadAction();
		upload.setClassName("modules.test.Action");
		view.putAction(upload.toMetaDataAction());

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithRemoveAction() {
		ViewImpl view = createFormView();

		view.putAction(new RemoveAction().toMetaDataAction());

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithFormBorderTitle() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.setBorderTitle("Section Title");
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithVBoxBorderTitle() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setBorderTitle("VBox Section");
		Form form = createForm();
		vbox.getContained().add(form);
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	private static ViewImpl createFormView() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		view.getContained().add(createForm());

		return view;
	}

	private static Form createForm() {
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		return form;
	}

	@Test
	void renderViewWithServerSideActionEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("testAction");
		viewAction.setResourceName("TestAction");
		view.putAction(viewAction);
		ServerSideActionEventAction action = new ServerSideActionEventAction();
		action.setActionName("testAction");
		tf.getChangedActions().add(action);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithRerenderEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithSetDisabledEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("text");
		setDisabled.setDisabledConditionName("false");
		tf.getChangedActions().add(setDisabled);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithSetInvisibleEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("text");
		setInvisible.setInvisibleConditionName("false");
		tf.getChangedActions().add(setInvisible);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithToggleDisabledEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithToggleVisibilityEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	void renderViewWithMultipleEventHandlers() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		// Add all event action types
		ActionImpl viewAction = new ActionImpl();
		viewAction.setName("action1");
		viewAction.setResourceName("Action1");
		view.putAction(viewAction);
		ServerSideActionEventAction server = new ServerSideActionEventAction();
		server.setActionName("action1");
		tf.getChangedActions().add(server);
		RerenderEventAction rerender = new RerenderEventAction();
		tf.getChangedActions().add(rerender);
		SetDisabledEventAction setDisabled = new SetDisabledEventAction();
		setDisabled.setBinding("text");
		tf.getChangedActions().add(setDisabled);
		SetInvisibleEventAction setInvisible = new SetInvisibleEventAction();
		setInvisible.setBinding("text");
		tf.getChangedActions().add(setInvisible);
		ToggleDisabledEventAction toggleDisabled = new ToggleDisabledEventAction();
		toggleDisabled.setBinding("text");
		tf.getChangedActions().add(toggleDisabled);
		ToggleVisibilityEventAction toggleVis = new ToggleVisibilityEventAction();
		toggleVis.setBinding("text");
		tf.getChangedActions().add(toggleVis);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithFocusEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		RerenderEventAction rerender = new RerenderEventAction();
		tf.getFocusActions().add(rerender);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithBlurEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		RerenderEventAction rerender = new RerenderEventAction();
		tf.getBlurActions().add(rerender);
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridAddedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		RerenderEventAction rerender = new RerenderEventAction();
		grid.getAddedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridEditedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		RerenderEventAction rerender = new RerenderEventAction();
		grid.getEditedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridRemovedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		RerenderEventAction rerender = new RerenderEventAction();
		grid.getRemovedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridSelectedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		RerenderEventAction rerender = new RerenderEventAction();
		grid.getSelectedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLookupDescriptionPickedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLookupDescriptionClearedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setDescriptionBinding("aggregatedAssociation_bizKey");
		RerenderEventAction rerender = new RerenderEventAction();
		ld.getClearedActions().add(rerender);
		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDateTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("date");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'DD_MMM_YYYY'") || code.contains("type:'DD_MM_YYYY'") || code.contains("type:"));
	}

	@Test
	void renderViewWithDateTimeTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("dateTime");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		// dateTime maps to converter name or DD_MMM_YYYY when no converter
		assertTrue(code.contains("type:'DD_MMM_YYYY'") || code.contains("type:'DD_MMM_YYYY_HH_MM'") || code.contains("type:"));
	}

	@Test
	void renderViewWithTimeTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("time");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		// time type maps to converter name or HH24_MI when no converter
		assertTrue(code.contains("type:'HH24_MI'") || code.contains("type:'HH_MM_AM'") || code.contains("type:'HH_MM'"));
	}

	@Test
	void renderViewWithTimestampTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("timestamp");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		// timestamp maps to converter name or DD_MMM_YYYY when no converter
		assertTrue(code.contains("type:'DD_MMM_YYYY'") || code.contains("type:'DD_MMM_YYYY_HH_MM'") || code.contains("type:"));
	}

	@Test
	void renderViewWithDecimal2TextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("decimal2");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'bizDecimal2'") || code.contains("type:'bizDollarsAndCents'"));
	}

	@Test
	void renderViewWithDecimal5TextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("decimal5");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'bizDecimal5'") || code.contains("type:'bizDollarsAndCents'"));
	}

	@Test
	void renderViewWithDecimal10TextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("decimal10");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'bizDecimal10'") || code.contains("type:'bizDollarsAndCents'"));
	}

	@Test
	void renderViewWithNormalIntegerTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("normalInteger");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'integer'") || code.contains("type:'bizIntegerPercentage'") || code.contains("type:'bizIntegerSeparator'"));
	}

	@Test
	void renderViewWithLongIntegerTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("longInteger");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'integer'") || code.contains("type:'bizIntegerPercentage'") || code.contains("type:'bizIntegerSeparator'"));
	}

	@Test
	void renderViewWithMarkupTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("markup");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'richText'"));
	}

	@Test
	void renderViewWithMemoTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("memo");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'text'"));
	}

	@Test
	void renderViewWithColourPickerWithEnumBinding() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		ColourPicker cp = new ColourPicker();
		cp.setBinding("colour");
		cp.setDisabledConditionName("condition");
		item.setWidget(cp);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("colour"));
	}

	@Test
	void renderViewWithEnumCombo() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("valueMap"));
	}

	@Test
	void renderViewWithSpinnerForInteger() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Spinner sp = new Spinner();
		sp.setBinding("normalInteger");
		item.setWidget(sp);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("normalInteger"));
	}

	@Test
	void renderViewWithDataGridForCollection() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid dg = new DataGrid();
		dg.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		dg.getColumns().add(col);
		view.getContained().add(dg);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("aggregatedCollection"));
	}

	@Test
	void renderViewWithListGridInQueryMode() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListGrid lg = new ListGrid();
		lg.setQueryName("qExpressionQuery");
		view.getContained().add(lg);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithTextFieldWithLabel() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		item.setLabel("My Label");
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("My Label"));
	}

	@Test
	void renderViewWithTextFieldWithEnabledCondition() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		tf.setEnabledConditionName("condition");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithTextFieldWithVisibleCondition() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithTextFieldWithDisabledCondition() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithSidebarAndContent() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Sidebar sidebar = new Sidebar();
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		sidebar.getContained().add(form);
		view.setSidebar(sidebar);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithVBoxContainer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithVBoxVerticalAlignmentTop() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setVerticalAlignment(VerticalAlignment.top);
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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("align:'top'"), "VBox with top alignment should contain align:'top': " + code);
	}

	@Test
	void renderViewWithVBoxVerticalAlignmentMiddle() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setVerticalAlignment(VerticalAlignment.middle);
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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("align:'center'"), "VBox with middle alignment should contain align:'center': " + code);
	}

	@Test
	void renderViewWithVBoxVerticalAlignmentBottom() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setVerticalAlignment(VerticalAlignment.bottom);
		vbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("align:'bottom'"), "VBox with bottom alignment should contain align:'bottom': " + code);
	}

	@Test
	void renderViewWithVBoxHorizontalAlignmentLeft() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setHorizontalAlignment(HorizontalAlignment.left);
		vbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("defaultLayoutAlign:'left'"), "VBox with left alignment: " + code);
	}

	@Test
	void renderViewWithVBoxHorizontalAlignmentCentre() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setHorizontalAlignment(HorizontalAlignment.centre);
		vbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("defaultLayoutAlign:'center'"), "VBox with centre alignment: " + code);
	}

	@Test
	void renderViewWithVBoxHorizontalAlignmentRight() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setHorizontalAlignment(HorizontalAlignment.right);
		vbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("defaultLayoutAlign:'right'"), "VBox with right alignment: " + code);
	}

	@Test
	void renderViewWithHBoxHorizontalAlignmentLeft() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setHorizontalAlignment(HorizontalAlignment.left);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("align:'left'"), "HBox with left alignment: " + code);
	}

	@Test
	void renderViewWithHBoxHorizontalAlignmentCentre() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setHorizontalAlignment(HorizontalAlignment.centre);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("align:'center'"), "HBox with centre alignment: " + code);
	}

	@Test
	void renderViewWithHBoxHorizontalAlignmentRight() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setHorizontalAlignment(HorizontalAlignment.right);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("align:'right'"), "HBox with right alignment: " + code);
	}

	@Test
	void renderViewWithHBoxVerticalAlignmentTop() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setVerticalAlignment(VerticalAlignment.top);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("defaultLayoutAlign:'top'"), "HBox with top vertical alignment: " + code);
	}

	@Test
	void renderViewWithHBoxVerticalAlignmentMiddle() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setVerticalAlignment(VerticalAlignment.middle);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("defaultLayoutAlign:'center'"), "HBox with middle vertical alignment: " + code);
	}

	@Test
	void renderViewWithHBoxVerticalAlignmentBottom() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setVerticalAlignment(VerticalAlignment.bottom);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("defaultLayoutAlign:'bottom'"), "HBox with bottom vertical alignment: " + code);
	}

	@Test
	void renderViewWithVBoxPixelPaddingAndMemberPadding() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setPixelPadding(Integer.valueOf(8));
		vbox.setPixelMemberPadding(Integer.valueOf(4));
		vbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("layoutMargin:8"), "Should contain layoutMargin:8: " + code);
		assertTrue(code.contains("membersMargin:4"), "Should contain membersMargin:4: " + code);
	}

	@Test
	void renderViewWithTabPaneAndSelectedTabIndexBinding() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		TabPane tabPane = new TabPane();
		tabPane.setSelectedTabIndexBinding("normalInteger");
		Tab tab = new Tab();
		tab.setTitle("Tab 1");
		tab.getContained().add(createSimpleForm("text"));
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertTrue(code.contains("selectedTabIndexBinding:"), "Code should contain selectedTabIndexBinding: " + code);
	}

	@Test
	void renderViewWithVBoxCollapsibleOpen() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		VBox vbox = new VBox();
		vbox.setBorderTitle("Collapsible Section");
		vbox.setCollapsible(Collapsible.open);
		vbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(vbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizCollapsible"), "Collapsible VBox should use BizCollapsible: " + code);
	}

	@Test
	void renderViewWithTabIconStyleClass() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Home");
		tab.setIconStyleClass("fa fa-home");
		tab.getContained().add(createSimpleForm("text"));
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("bizhubFontIcon"), "Tab icon style class should generate bizhubFontIcon: " + code);
	}

	@Test
	void renderViewWithNoCreateViewAndSidebar() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		view.getContained().add(createSimpleForm("text"));

		Sidebar sidebar = new Sidebar();
		sidebar.getContained().add(createSimpleForm("memo"));
		view.setSidebar(sidebar);

		// noCreateView=true triggers rearrangeForSidebar(sidebar, null, "edit") path
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, true);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("sidebarPane"), "Sidebar should generate sidebarPane: " + code);
	}

	@Test
	void renderCreateViewWithSidebar() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.create.toString());
		view.setTitle("Test");

		view.getContained().add(createSimpleForm("text"));

		Sidebar sidebar = new Sidebar();
		sidebar.getContained().add(createSimpleForm("memo"));
		view.setSidebar(sidebar);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("sidebarPane"), "Create view with sidebar should generate sidebarPane: " + code);
	}

	@Test
	void renderViewWithFormWithBorder() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		form.setBorder(Boolean.TRUE);
		form.setBorderTitle("Bordered Form");
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithFormCollapsible() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		form.setCollapsible(Collapsible.open);
		form.setBorderTitle("Collapsible Form");
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizCollapsible"), "Collapsible form should use BizCollapsible: " + code);
	}

	@Test
	void renderViewWithLookupDescriptionWithExplicitQueryAndDropDownColumns() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setQuery("qExpressionQuery");

		// Add a dropDownColumn matching a projected column in qExpressionQuery
		LookupDescriptionColumn col = new LookupDescriptionColumn();
		col.setName("pu");
		ld.getDropDownColumns().add(col);

		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithTabWithIcon16x16RelativeFileName() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		TabPane tabPane = new TabPane();
		Tab tab = new Tab();
		tab.setTitle("Home");
		tab.setIcon16x16RelativeFileName("myIcon.png");
		// no iconStyleClass set — triggers the icon16x16Url branch
		tab.getContained().add(createSimpleForm("text"));
		tabPane.getTabs().add(tab);
		view.getContained().add(tabPane);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("icon:'../"), "Tab with icon16x16 should contain icon:'../ path: " + code);
	}

	@Test
	void renderViewWithHBoxCollapsible() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		hbox.setBorderTitle("Collapsible HBox");
		hbox.setCollapsible(Collapsible.open);
		hbox.getContained().add(createSimpleForm("text"));
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizCollapsible"), "Collapsible HBox should use BizCollapsible: " + code);
	}

	@Test
	void renderViewWithFormWithTopLabelLayout() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.setLabelLayout(FormLabelLayout.top);
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("titleOrientation:'top'"), "Form with top label layout should contain titleOrientation:'top': " + code);
	}

	@Test
	void renderViewWithFormWithLabelDefaultAlignment() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.setLabelDefaultHorizontalAlignment(HorizontalAlignment.right);
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("titleAlign:'"), "Form with label alignment should contain titleAlign: " + code);
	}

	@Test
	void renderViewWithFormColumnWithPixelWidth() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		FormColumn col = new FormColumn();
		col.setPixelWidth(Integer.valueOf(200));
		form.getColumns().add(col);
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("colWidths:[200]") || code.contains("200,"), "Form with pixel column width should contain pixel width 200: " + code);
	}

	@Test
	void renderViewWithFormColumnWithPercentageWidth() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		FormColumn col = new FormColumn();
		col.setPercentageWidth(Integer.valueOf(50));
		form.getColumns().add(col);
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("50%'"), "Form with percentage column width should contain '50%': " + code);
	}

	@Test
	void renderViewWithFormColumnWithResponsiveWidth() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		FormColumn col = new FormColumn();
		col.setResponsiveWidth(Integer.valueOf(6));
		form.getColumns().add(col);
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("%'"), "Form with responsive column width should contain percentage: " + code);
	}

	@Test
	void renderViewWithFormItemWithRowspan() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		item.setRowspan(Integer.valueOf(2));
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("rowSpan:2"), "Form item with rowspan should contain 'rowSpan:2': " + code);
	}

	@Test
	void renderViewWithFormItemWithHorizontalAlignment() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		item.setHorizontalAlignment(HorizontalAlignment.left);
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("align:'left'"), "Form item with horizontal alignment should contain 'align:left': " + code);
	}

	@Test
	void renderViewWithFormItemWithLabelHorizontalAlignment() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		item.setLabelHorizontalAlignment(HorizontalAlignment.centre);
		TextField tf = new TextField();
		tf.setBinding("text");
		item.setWidget(tf);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("titleAlign:'center'"), "Form item with label alignment should contain titleAlign:'center': " + code);
	}

	@Test
	void renderViewWithFormMultipleItemsPerRow() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		FormItem item1 = new FormItem();
		TextField tf1 = new TextField();
		tf1.setBinding("text");
		item1.setWidget(tf1);
		row.getItems().add(item1);
		FormItem item2 = new FormItem();
		TextField tf2 = new TextField();
		tf2.setBinding("normalInteger");
		item2.setWidget(tf2);
		row.getItems().add(item2);
		form.getRows().add(row);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("startRow:false"), "Form with multiple items per row should contain 'startRow:false': " + code);
	}

	@Test
	void renderViewWithButtonInFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		CustomAction customAction = new CustomAction();
		customAction.setName("MyAction");
		customAction.setClassName("modules.test.SomeAction");
		view.putAction(customAction.toMetaDataAction());

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Button button = new Button();
		button.setActionName("MyAction");
		item.setWidget(button);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'canvas'"), "Button in form item should render as canvas type: " + code);
	}

	@Test
	void renderViewWithButtonInContainer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		CustomAction customAction = new CustomAction();
		customAction.setName("MyContainerAction");
		customAction.setClassName("modules.test.SomeAction");
		view.putAction(customAction.toMetaDataAction());

		HBox hbox = new HBox();
		Button button = new Button();
		button.setActionName("MyContainerAction");
		hbox.getContained().add(button);
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithZoomInInContainer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		ZoomIn zi = new ZoomIn();
		zi.setBinding("aggregatedAssociation");
		hbox.getContained().add(zi);
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDialogButtonInContainer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		HBox hbox = new HBox();
		DialogButton db = new DialogButton();
		db.setDisplayName("OK");
		hbox.getContained().add(db);
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("BizLabel"), "DialogButton in container should render as BizLabel: " + code);
	}

	@Test
	void renderViewWithChartWithBuiltInModel() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Chart chart = new Chart();
		chart.setType(ChartType.bar);
		ChartBuilderMetaData model = new ChartBuilderMetaData();
		chart.setModel(model);
		view.getContained().add(chart);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithGeometryWithDrawingTools() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Geometry geo = new Geometry();
		geo.setBinding("geometry");
		geo.setType(GeometryInputType.point);
		item.setWidget(geo);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("drawingTools:'point'"), "Geometry with drawing tools should contain drawingTools:'point': " + code);
	}

	@Test
	void renderViewWithGeometryMapWithDrawingTools() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		GeometryMap gm = new GeometryMap();
		gm.setBinding("geometry");
		gm.setType(GeometryInputType.polygon);
		item.setWidget(gm);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("drawingTools:'polygon'"), "GeometryMap with drawing tools should contain drawingTools:'polygon': " + code);
	}

	@Test
	void renderViewWithSpacerInContainer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");
		view.getContained().add(createSimpleForm("text"));

		HBox hbox = new HBox();
		Spacer spacer = new Spacer();
		spacer.setPixelWidth(Integer.valueOf(20));
		hbox.getContained().add(spacer);
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("isc.LayoutSpacer.create("), "Spacer in container should render as LayoutSpacer: " + code);
	}

	@Test
	void renderViewWithStaticImageInFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		StaticImage image = new StaticImage();
		image.setRelativeFile("images/logo.png");
		item.setWidget(image);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("isc.BizImage.create("), "StaticImage in form item should render as BizImage: " + code);
	}

	@Test
	void renderViewWithStaticImageInContainer() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");
		view.getContained().add(createSimpleForm("text"));

		HBox hbox = new HBox();
		StaticImage image = new StaticImage();
		image.setRelativeFile("images/logo.png");
		hbox.getContained().add(image);
		view.getContained().add(hbox);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("isc.BizImage.create("), "StaticImage in container should render as BizImage: " + code);
	}

	@Test
	void renderViewWithLinkInFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Link link = new Link();
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://www.example.com");
		link.setReference(ref);
		item.setWidget(link);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("type:'blurb'"), "Link in form item should render as blurb type: " + code);
	}

	@Test
	void renderViewWithBlurbInFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Blurb blurb = new Blurb();
		blurb.setMarkup("<b>Test blurb in form</b>");
		item.setWidget(blurb);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("Test blurb in form") || code.contains("defaultValue"),
				"Blurb in form item should render with content: " + code);
	}

	@Test
	void renderViewWithLabelWithValueInFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		Label label = new Label();
		label.setValue("Static label text");
		item.setWidget(label);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("defaultValue") || code.contains("Static label text"),
				"Label with value in form item should render with defaultValue: " + code);
	}

	@Test
	void renderViewWithLabelWithBindingInFormItem() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

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

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("name:'text'") || code.contains("type:'blurb'"),
				"Label with binding in form item should render as blurb with name binding: " + code);
	}

	@Test
	void renderViewWithListGridShowFlagsOff() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListGrid listGrid = new ListGrid();
		listGrid.setQueryName("qExpressionQuery");
		listGrid.setTitle("Grid Title");
		listGrid.setShowAdd(Boolean.FALSE);
		listGrid.setShowZoom(Boolean.FALSE);
		listGrid.setShowEdit(Boolean.FALSE);
		listGrid.setShowRemove(Boolean.FALSE);
		listGrid.setShowDeselect(Boolean.FALSE);
		listGrid.setShowFilter(Boolean.FALSE);
		listGrid.setShowSummary(Boolean.FALSE);
		listGrid.setShowExport(Boolean.FALSE);
		listGrid.setShowChart(Boolean.FALSE);
		listGrid.setShowSnap(Boolean.FALSE);
		listGrid.setShowTag(Boolean.FALSE);
		listGrid.setShowFlag(Boolean.FALSE);
		listGrid.setAutoPopulate(Boolean.FALSE);
		view.getContained().add(listGrid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("showAdd:false"), "ListGrid with showAdd:false: " + code);
		assertTrue(code.contains("showZoom:false"), "ListGrid with showZoom:false: " + code);
		assertTrue(code.contains("showEdit:false"), "ListGrid with showEdit:false: " + code);
		assertTrue(code.contains("showRemove:false"), "ListGrid with showRemove:false: " + code);
		assertTrue(code.contains("showDeselect:false"), "ListGrid with showDeselect:false: " + code);
		assertTrue(code.contains("showFilter:false"), "ListGrid with showFilter:false: " + code);
		assertTrue(code.contains("showSummary:false"), "ListGrid with showSummary:false: " + code);
		assertTrue(code.contains("showExport:false"), "ListGrid with showExport:false: " + code);
		assertTrue(code.contains("showChart:false"), "ListGrid with showChart:false: " + code);
		assertTrue(code.contains("showSnap:false"), "ListGrid with showSnap:false: " + code);
		assertTrue(code.contains("showTag:false"), "ListGrid with showTag:false: " + code);
		assertTrue(code.contains("showFlag:false"), "ListGrid with showFlag:false: " + code);
		assertTrue(code.contains("autoPopulate:false"), "ListGrid with autoPopulate:false: " + code);
		assertTrue(code.contains("Grid Title"), "ListGrid with title: " + code);
	}

	@Test
	void renderViewWithDataGridShowFlagsOffAndInlineWordWrap() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		grid.setShowAdd(Boolean.FALSE);
		grid.setShowZoom(Boolean.FALSE);
		grid.setShowEdit(Boolean.FALSE);
		grid.setShowRemove(Boolean.FALSE);
		grid.setShowDeselect(Boolean.FALSE);
		grid.setInline(Boolean.TRUE);
		grid.setWordWrap(Boolean.TRUE);
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		grid.getColumns().add(col);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("showAdd:false"), "DataGrid showAdd:false: " + code);
		assertTrue(code.contains("showZoom:false"), "DataGrid showZoom:false: " + code);
		assertTrue(code.contains("showEdit:false"), "DataGrid showEdit:false: " + code);
		assertTrue(code.contains("showRemove:false"), "DataGrid showRemove:false: " + code);
		assertTrue(code.contains("showDeselect:false"), "DataGrid showDeselect:false: " + code);
		assertTrue(code.contains("inline:true"), "DataGrid inline:true: " + code);
		assertTrue(code.contains("wordWrap:true"), "DataGrid wordWrap:true: " + code);
	}

	@Test
	void renderViewWithDataRepeaterShowColumnHeadersAndGrid() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater repeater =
				new org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater();
		repeater.setBinding("aggregatedCollection");
		repeater.setShowColumnHeaders(Boolean.TRUE);
		repeater.setShowGrid(Boolean.TRUE);
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		repeater.getColumns().add(col);
		view.getContained().add(repeater);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("isRepeater:true"), "DataRepeater with isRepeater: " + code);
		assertTrue(code.contains("showColumnHeaders:true"), "DataRepeater showColumnHeaders:true: " + code);
		assertTrue(code.contains("showGrid:true"), "DataRepeater showGrid:true: " + code);
	}

	@Test
	void renderViewWithDataRepeaterWithTitleAndShowFlags() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater repeater =
				new org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater();
		repeater.setBinding("aggregatedCollection");
		repeater.setTitle("Repeater Title");
		repeater.setShowColumnHeaders(Boolean.FALSE);
		repeater.setShowGrid(Boolean.FALSE);
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("bizKey");
		repeater.getColumns().add(col);
		view.getContained().add(repeater);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
		assertTrue(code.contains("showColumnHeaders:false"), "DataRepeater showColumnHeaders:false: " + code);
		assertTrue(code.contains("showGrid:false"), "DataRepeater showGrid:false: " + code);
		assertTrue(code.contains("Repeater Title"), "DataRepeater with title: " + code);
	}

	private static Form createSimpleForm(String binding) {
		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		TextField tf = new TextField();
		tf.setBinding(binding);
		item.setWidget(tf);
		row.getItems().add(item);
		return form;
	}

	@Test
	void renderViewWithListGridEditedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListGrid grid = new ListGrid();
		grid.setQueryName("qExpressionQuery");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		grid.getEditedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithListGridRemovedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListGrid grid = new ListGrid();
		grid.setQueryName("qExpressionQuery");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		grid.getRemovedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithListGridSelectedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		ListGrid grid = new ListGrid();
		grid.setQueryName("qExpressionQuery");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		grid.getSelectedActions().add(rerender);
		view.getContained().add(grid);

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLookupDescriptionAddedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setDescriptionBinding("aggregatedAssociation_bizKey");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		ld.getAddedActions().add(rerender);
		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithLookupDescriptionEditedEventHandler() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		Form form = new Form();
		form.getColumns().add(new FormColumn());
		FormRow row = new FormRow();
		form.getRows().add(row);
		FormItem item = new FormItem();
		LookupDescription ld = new LookupDescription();
		ld.setBinding("aggregatedAssociation");
		ld.setDescriptionBinding("aggregatedAssociation_bizKey");
		RerenderEventAction rerender = new RerenderEventAction();
		rerender.setClientValidation(Boolean.FALSE);
		ld.getEditedActions().add(rerender);
		item.setWidget(ld);
		row.getItems().add(item);
		view.getContained().add(form);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnCheckBox() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("booleanFlag");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		CheckBox cb = new CheckBox();
		wr.setWidget(cb);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnTextArea() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("memo");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		TextArea ta = new TextArea();
		wr.setWidget(ta);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnRichText() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("memo");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		RichText rt = new RichText();
		wr.setWidget(rt);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnHTML() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("markup");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		org.skyve.impl.metadata.view.widget.bound.input.HTML html = new org.skyve.impl.metadata.view.widget.bound.input.HTML();
		wr.setWidget(html);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnPassword() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Password pw = new Password();
		wr.setWidget(pw);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnRadio() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("enum3");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Radio radio = new Radio();
		wr.setWidget(radio);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnSlider() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("normalInteger");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Slider slider = new Slider();
		wr.setWidget(slider);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnSpinner() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("normalInteger");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		Spinner spinner = new Spinner();
		wr.setWidget(spinner);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnColourPicker() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("colour");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		ColourPicker cp = new ColourPicker();
		wr.setWidget(cp);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnTextField() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		TextField tf = new TextField();
		wr.setWidget(tf);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnContentImage() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		ContentImage ci = new ContentImage();
		ci.setBinding("text");
		wr.setWidget(ci);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridBoundColumnContentLink() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridBoundColumn col = new DataGridBoundColumn();
		col.setBinding("text");
		col.setEditable(Boolean.TRUE);
		WidgetReference wr = new WidgetReference();
		ContentLink cl = new ContentLink();
		cl.setBinding("text");
		wr.setWidget(cl);
		col.setInputWidget(wr);
		grid.getColumns().add(col);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnStaticImage() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("Image");
		StaticImage si = new StaticImage();
		si.setRelativeFile("images/logo.png");
		containerCol.getWidgets().add(si);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnDynamicImage() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("DynImage");
		DynamicImage di = new DynamicImage();
		di.setName("thumbnail");
		containerCol.getWidgets().add(di);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnLabel() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("Label");
		org.skyve.impl.metadata.view.widget.bound.Label label =
				new org.skyve.impl.metadata.view.widget.bound.Label();
		label.setValue("Test Label");
		containerCol.getWidgets().add(label);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnLink() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("Link");
		Link link = new Link();
		ExternalReference ref = new ExternalReference();
		ref.setHref("https://skyve.org");
		link.setReference(ref);
		containerCol.getWidgets().add(link);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnBlurb() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("Blurb");
		org.skyve.impl.metadata.view.widget.Blurb blurb = new org.skyve.impl.metadata.view.widget.Blurb();
		blurb.setMarkup("Some blurb text");
		containerCol.getWidgets().add(blurb);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnContentImage() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("ContentImage");
		ContentImage ci = new ContentImage();
		ci.setBinding("text");
		containerCol.getWidgets().add(ci);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnContentLink() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("ContentLink");
		ContentLink cl = new ContentLink();
		cl.setBinding("text");
		containerCol.getWidgets().add(cl);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithDataGridContainerColumnContentSignature() {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		DataGrid grid = new DataGrid();
		grid.setBinding("aggregatedCollection");
		DataGridContainerColumn containerCol = new DataGridContainerColumn();
		containerCol.setTitle("ContentSignature");
		org.skyve.impl.metadata.view.widget.bound.input.ContentSignature cs =
				new org.skyve.impl.metadata.view.widget.bound.input.ContentSignature();
		cs.setBinding("text");
		containerCol.getWidgets().add(cs);
		grid.getColumns().add(containerCol);
		view.getContained().add(grid);
		view.getContained().add(createSimpleForm("text"));

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithPrintAction() {
		ViewImpl view = createFormView();
		PrintAction print = new PrintAction();
		print.setName("MyPrint");
		view.putAction(print.toMetaDataAction());

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

	@Test
	void renderViewWithAddAction() {
		ViewImpl view = createFormView();
		AddAction add = new AddAction();
		add.setName("MyAdd");
		view.putAction(add.toMetaDataAction());

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertFalse(code.isEmpty());
	}

}



