package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.metadata.view.View.ViewType;

import modules.test.AbstractSkyveTest;

class SmartClientViewRendererTest extends AbstractSkyveTest {

	private static final String UXUI = "external";

	@Test
	@SuppressWarnings("static-method")
	void renderEmptyEditView() throws Exception {
		ViewImpl view = new ViewImpl();
		view.setName(ViewType.edit.toString());
		view.setTitle("Test");

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
	}

	@Test
	@SuppressWarnings("static-method")
	void renderViewWithForm() throws Exception {
		ViewImpl view = createFormView();
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, false);
		renderer.visit();
		String code = renderer.getCode().toString();
		assertNotNull(code);
		assertFalse(code.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void renderViewWithTextFieldGeneratesFormItem() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderViewWithCheckBox() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderViewWithCombo() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderViewWithTextArea() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderViewWithBlurb() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderViewWithTabPane() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderViewWithDataGrid() throws Exception {
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
	@SuppressWarnings("static-method")
	void renderCreateView() throws Exception {
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
	@SuppressWarnings("static-method")
	void getCodeReturnsStringBuilder() throws Exception {
		ViewImpl view = createFormView();
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(u, m, aapd, view, UXUI, true);
		renderer.visit();
		assertNotNull(renderer.getCode());
		assertFalse(renderer.getCode().length() == 0);
	}

	private static ViewImpl createFormView() {
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

		return view;
	}
}
