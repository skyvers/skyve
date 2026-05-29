package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.client.RenderedComponent;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;

class FlutterRenderersTest {
	@Test
	@SuppressWarnings("static-method")
	void componentRendererCoversPrimaryOutputsAndImports() {
		Set<String> imports = new HashSet<>();
		FlutterComponentRenderer renderer = new FlutterComponentRenderer(imports, "");

		Action action = mock(Action.class);
		when(action.getDisplayName()).thenReturn("Save");
		when(action.getName()).thenReturn("save");
		when(action.getImplicitName()).thenReturn(ImplicitActionName.Save);

		TextField text = new TextField();
		text.setBinding("line.item");
		TextArea textArea = new TextArea();
		textArea.setBinding("line.notes");
		DataGrid dataGrid = new DataGrid();
		dataGrid.setBinding("rows");
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("rows2");

		assertTrue(renderer.view(null, null).toString().contains("Column(children"));
		assertEquals(1, renderer.toolbars(null, null).size());
		renderer.tabPane(null, new TabPane());
		renderer.tab(null, "Overview", new Tab());
		renderer.border(null, "Card", null, null);
		renderer.label(null, "Name");
		renderer.spacer(null, new Spacer());
		renderer.actionButton(null, null, null, new Button(), action);
		renderer.reportButton(null, new Button(), action);
		renderer.downloadButton(null, new Button(), action, "admin", "Doc");
		renderer.staticImage(null, null, null);
		renderer.dynamicImage(null, null, null, null);
		renderer.blurb(null, null, null, null, new Blurb());
		assertTrue(renderer.label(null, null, "Literal", "binding", null).toString().contains("Literal"));
		assertTrue(renderer.label(null, null, null, "line.item", null).toString().contains("line_item"));
		renderer.dataGrid(null, null, false, null, dataGrid);
		renderer.dataRepeater(null, null, null, repeater);

		RenderedComponent current = new RenderedComponent();
		assertSame(current, renderer.addDataGridBoundColumn(null, current, dataGrid, new DataGridBoundColumn(), null, "Title", "line.item", new StringBuilder()));
		assertSame(current, renderer.addedDataGridBoundColumn(null, current));
		assertSame(current, renderer.addDataGridContainerColumn(null, current, dataGrid, "Title", new DataGridContainerColumn()));
		assertSame(current, renderer.addedDataGridContainerColumn(null, current));
		assertSame(current, renderer.addDataGridActionColumn(null, current, dataGrid, null, null, null, false));

		renderer.listGrid(null, null, null, null, null, null, false);
		renderer.listRepeater(null, null, null, null, null, null, null, false, false);
		renderer.listMembership(null, null);
		renderer.checkBox(null, null, null, "Check", null);
		renderer.colourPicker(null, null, null, "Colour", null);
		renderer.combo(null, null, null, "Combo", null);
		renderer.contentImage(null, null, null, "Image", null);
		renderer.contentLink(null, null, null, "Link", null);
		renderer.contentSignature(null, null, null, "Sign", null);
		renderer.html(null, null, null, "Html", null);
		renderer.lookupDescription(null, null, null, "Lookup", null, null, null);
		renderer.password(null, null, null, "Password", null);
		renderer.radio(null, null, null, "Radio", null);
		renderer.richText(null, null, null, "Rich", null);
		renderer.spinner(null, null, null, "Spinner", null);
		assertTrue(renderer.text(null, null, text, "Text", null, null, null, null).toString().contains("line_item"));
		renderer.textArea(null, null, textArea, "Text Area", null, null);
		renderer.actionLink(null, null, null, null, action);
		renderer.report(null, action);
		renderer.download(null, action, "admin", "Doc");
		renderer.upload(null, action);
		assertTrue(renderer.action(null, null, null, action, ImplicitActionName.Cancel, "Cancel").toString().contains("SkyveButton"));
		assertTrue(renderer.action(null, null, null, action, ImplicitActionName.Save, "Save").toString().contains("SkyveButton"));

		assertTrue(imports.contains(FlutterLayoutRenderer.VBOX_IMPORT));
		assertTrue(imports.contains(FlutterComponentRenderer.TAB_IMPORT));
		assertTrue(imports.contains(FlutterComponentRenderer.TEXTFIELD_IMPORT));
	}

	@Test
	@SuppressWarnings("static-method")
	void layoutRendererCoversHierarchyAndFormHelpers() {
		Set<String> imports = new HashSet<>();
		FlutterLayoutRenderer renderer = new FlutterLayoutRenderer(imports);

		assertNull(renderer.viewLayout(null));
		List<RenderedComponent> toolbarLayouts = renderer.toolbarLayouts(null);
		assertEquals(1, toolbarLayouts.size());

		RenderedComponent view = new RenderedComponent();
		RenderedComponent toolbar = new RenderedComponent();
		renderer.addToolbarLayouts(List.of(toolbar), toolbarLayouts);
		renderer.addToolbarsOrLayouts(view, List.of(toolbar));
		assertNotNull(view.getChild(0));

		RenderedComponent root = new RenderedComponent();
		RenderedComponent tabPane = new RenderedComponent();
		root.addChild(tabPane);
		RenderedComponent tab = new RenderedComponent();
		renderer.addTab(tabPane, tab);
		assertSame(root, renderer.addedTab(null, tab));
		assertTrue(renderer.tabLayout(null).toString().contains("SkyveVBox"));
		assertTrue(renderer.vboxLayout(null, new VBox()).toString().contains("SkyveVBox"));
		assertTrue(renderer.hboxLayout(null, new HBox()).toString().contains("SkyveHBox"));
		assertTrue(renderer.formLayout(null, new Form()).toString().contains("SkyveForm"));

		RenderedComponent rowLayout = renderer.formRowLayout(null, new FormRow());
		RenderedComponent formLayout = new RenderedComponent();
		assertSame(rowLayout, renderer.addFormRowLayout(null, formLayout, rowLayout));
		assertSame(formLayout, renderer.addedFormRowLayout(null, rowLayout));

		RenderedComponent row = new RenderedComponent();
		renderer.layoutFormItemLabel(row, new RenderedComponent(), new Form(), new FormItem(), new FormColumn(), "Label", null, null, null);
		renderer.layoutFormItemWidget(row, new RenderedComponent(), new Form(), new FormItem(), new FormColumn(), "Label", 1, null, null, null);
		assertEquals(2, row.toString().split("SkyveFormItem", -1).length - 1);

		RenderedComponent parent = new RenderedComponent();
		RenderedComponent container = new RenderedComponent();
		parent.addChild(container);
		RenderedComponent child = new RenderedComponent();
		assertSame(child, renderer.addToContainer(null, null, container, child, null, null, null, null));
		assertSame(container, child.getParent());
		assertSame(parent, renderer.addedToContainer(null, null, container));
	}
}
