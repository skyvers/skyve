package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

class PrimeReactRendererTest {
	@Test
	@SuppressWarnings("static-method")
	void componentRendererRendersExpectedFragments() {
		Map<String, String> imports = new HashMap<>();
		PrimeReactComponentRenderer renderer = new PrimeReactComponentRenderer(imports, "\t\t");

		Action action = mock(Action.class);
		when(action.getDisplayName()).thenReturn("Save");
		when(action.getName()).thenReturn("save");

		TextField text = new TextField();
		text.setBinding("line.item");
		TextArea textArea = new TextArea();
		textArea.setBinding("line.notes");
		DataGrid grid = new DataGrid();
		grid.setBinding("items");
		DataRepeater repeater = new DataRepeater();
		repeater.setBinding("rows");

		RenderedComponent root = renderer.view(null, null);
		assertTrue(root.toString().contains("<VBox>"));
		assertTrue(imports.containsKey("{VBox}"));

		List<RenderedComponent> toolbars = renderer.toolbars(null, "actions");
		assertEquals(1, toolbars.size());
		assertTrue(toolbars.get(0).toString().contains("<Toolbar>"));

		assertTrue(renderer.tabPane(null, new TabPane()).toString().contains("<TabView>"));
		assertTrue(renderer.tab(null, "Overview", new Tab()).toString().contains("Overview"));
		renderer.border(null, "Card", null, Integer.valueOf(100));
		renderer.spacer(null, new Spacer());
		renderer.actionButton(null, null, null, new Button(), action);
		renderer.reportButton(null, new Button(), action);
		renderer.downloadButton(null, new Button(), action, "admin", "Doc");
		renderer.blurb(null, null, "x", "binding", new Blurb());

		RenderedComponent dataTable = renderer.dataGrid(null, null, false, "Grid", grid);
		assertTrue(dataTable.toString().contains("line.item") || dataTable.toString().contains("items"));
		renderer.dataRepeater(null, null, "Rows", repeater);

		RenderedComponent parent = new RenderedComponent();
		RenderedComponent bound = renderer.addDataGridBoundColumn(null,
				parent,
				grid,
				new DataGridBoundColumn(),
				null,
				"Title",
				"line.item",
				new StringBuilder());
		assertSame(parent, bound.getParent());
		assertSame(parent, renderer.addedDataGridBoundColumn(null, bound));

		RenderedComponent container = renderer.addDataGridContainerColumn(null,
				parent,
				grid,
				"Section",
				new DataGridContainerColumn());
		assertSame(parent, container.getParent());
		assertSame(parent, renderer.addedDataGridContainerColumn(null, container));
		assertSame(parent, renderer.addDataGridActionColumn(null, parent, grid, null, "col", "Doc", false));

		renderer.checkBox(null, null, null, "title", null);
		renderer.combo(null, null, null, "title", null);
		renderer.contentImage(null, null, null, "title", null);
		renderer.contentLink(null, null, null, "title", null);
		renderer.contentSignature(null, null, null, "title", null);
		renderer.html(null, null, null, "title", null);
		renderer.lookupDescription(null, null, null, "title", null, "binding", null);
		renderer.password(null, null, null, "title", null);
		renderer.radio(null, null, null, "title", null);
		renderer.richText(null, null, null, "title", null);
		renderer.spinner(null, null, null, "title", null);
		assertTrue(renderer.text(null, null, text, "title", null, Integer.valueOf(20), null, null).toString().contains("line_item"));
		renderer.textArea(null, null, textArea, "title", null, Integer.valueOf(40));
		renderer.actionLink(null, null, null, null, action);
		renderer.report(null, action);
		renderer.download(null, action, "admin", "Doc");
		renderer.upload(null, action);
		assertTrue(renderer.action(null, null, null, action, ImplicitActionName.Cancel, "Cancel").toString().contains("goBack"));
		assertTrue(renderer.action(null, null, null, action, ImplicitActionName.Save, "Save").toString().contains("action Save"));
	}

	@Test
	@SuppressWarnings("static-method")
	void layoutRendererHandlesContainerHierarchy() {
		Map<String, String> imports = new HashMap<>();
		PrimeReactLayoutRenderer renderer = new PrimeReactLayoutRenderer(imports);

		assertNull(renderer.viewLayout(null));
		List<RenderedComponent> toolbarLayouts = renderer.toolbarLayouts(null);
		assertEquals(1, toolbarLayouts.size());

		RenderedComponent view = new RenderedComponent();
		RenderedComponent toolbar = new RenderedComponent();
		renderer.addToolbarLayouts(List.of(toolbar), toolbarLayouts);
		renderer.addToolbarsOrLayouts(view, List.of(toolbar));
		assertSame(toolbar, view.getChild(0));

		RenderedComponent root = new RenderedComponent();
		RenderedComponent tabPane = new RenderedComponent();
		root.addChild(tabPane);
		RenderedComponent tab = new RenderedComponent();
		renderer.addTab(tabPane, tab);
		assertSame(root, renderer.addedTab(null, tab));

		RenderedComponent tabLayout = renderer.tabLayout(null);
		assertTrue(tabLayout.toString().contains("<VBox>"));
		assertTrue(imports.containsKey("{VBox}"));
		assertSame(tabLayout, renderer.addTabLayout(null, tab, tabLayout));

		RenderedComponent border = new RenderedComponent();
		renderer.addBorderLayout(border, tabLayout);
		assertSame(border, renderer.addedBorderLayout(null, tabLayout));

		assertTrue(renderer.vboxLayout(null, new VBox()).toString().contains("<VBox>"));
		assertTrue(renderer.hboxLayout(null, new HBox()).toString().contains("<HBox>"));
		assertTrue(renderer.formLayout(null, new Form()).toString().contains("<Form>"));

		RenderedComponent rowLayout = renderer.formRowLayout(null, new FormRow());
		RenderedComponent formLayout = new RenderedComponent();
		assertSame(rowLayout, renderer.addFormRowLayout(null, formLayout, rowLayout));
		assertSame(formLayout, renderer.addedFormRowLayout(null, rowLayout));

		RenderedComponent row = new RenderedComponent();
		renderer.layoutFormItemLabel(row, new RenderedComponent(), new Form(), new FormItem(), new FormColumn(), "Label", null, null, null);
		renderer.layoutFormItemWidget(row, new RenderedComponent(), new Form(), new FormItem(), new FormColumn(), "Label", 1, null, null, null);
		assertEquals(2, row.toString().split("<Cell>", -1).length - 1);

		RenderedComponent grandParent = new RenderedComponent();
		RenderedComponent parentContainer = new RenderedComponent();
		RenderedComponent container = new RenderedComponent();
		grandParent.addChild(parentContainer);
		parentContainer.addChild(container);
		RenderedComponent added = renderer.addToContainer(null, null, container, new RenderedComponent(), null, null, null, null);
		assertSame(container, added.getParent().getParent());
		assertSame(grandParent, renderer.addedToContainer(null, null, container));
	}
}
