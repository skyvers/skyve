package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;

class ReactNativeRendererTest {
	@Test
	@SuppressWarnings("static-method")
	void componentRendererCoversPrimaryBranches() {
		Map<String, String> imports = new HashMap<>();
		ReactNativeComponentRenderer renderer = new ReactNativeComponentRenderer(imports, "\t\t");

		Action action = mock(Action.class);
		when(action.getDisplayName()).thenReturn("Save");

		TextField text = new TextField();
		text.setBinding("line.item");
		TextArea textArea = new TextArea();
		textArea.setBinding("line.notes");
		DataGrid grid = new DataGrid();

		assertTrue(renderer.view(null, null).toString().contains("<View>"));
		assertTrue(imports.containsKey("{ View }"));
		assertEquals(1, renderer.toolbars(null, null).size());
		renderer.tabPane(null, new TabPane());
		renderer.tab(null, "One", new Tab());
		renderer.border(null, "Card", null, null);
		assertTrue(renderer.label(null, "value").toString().contains("value"));
		renderer.spacer(null, null);
		renderer.actionButton(null, null, null, new Button(), action);
		renderer.reportButton(null, new Button(), action);
		renderer.downloadButton(null, new Button(), action, "admin", "Doc");
		renderer.staticImage(null, null, null);
		renderer.dynamicImage(null, null, null, null);
		renderer.blurb(null, null, null, null, null);
		renderer.label(null, null, null, null, null);

		renderer.dataGrid(null, null, false, null, grid);
		renderer.dataRepeater(null, null, null, null);

		RenderedComponent parent = new RenderedComponent();
		RenderedComponent child = renderer.addDataGridBoundColumn(null,
				parent,
				grid,
				new DataGridBoundColumn(),
				null,
				"Title",
				"line.item",
				new StringBuilder());
		assertSame(parent, child.getParent());
		assertSame(parent, renderer.addedDataGridBoundColumn(null, child));

		RenderedComponent container = renderer.addDataGridContainerColumn(null,
				parent,
				grid,
				"Title",
				new DataGridContainerColumn());
		assertSame(parent, container.getParent());
		assertSame(parent, renderer.addedDataGridContainerColumn(null, container));
		assertSame(parent, renderer.addDataGridActionColumn(null, parent, grid, null, null, null, false));

		renderer.listGrid(null, null, null, null, null, null, false);
		renderer.listRepeater(null, null, null, null, null, null, null, false, false);
		renderer.listMembership(null, null);
		renderer.checkBox(null, null, null, null, null);
		renderer.colourPicker(null, null, null, null, null);
		renderer.combo(null, null, null, null, null);
		renderer.contentImage(null, null, null, null, null);
		renderer.contentLink(null, null, null, null, null);
		renderer.contentSignature(null, null, null, null, null);
		renderer.html(null, null, null, null, null);
		renderer.lookupDescription(null, null, null, null, null, null, null);
		renderer.password(null, null, null, null, null);
		renderer.radio(null, null, null, null, null);
		renderer.richText(null, null, null, null, null);
		renderer.spinner(null, null, null, null, null);
		assertTrue(renderer.text(null, null, text, null, null, null, null, null).toString().contains("text"));
		renderer.textArea(null, null, textArea, null, null, null);
		renderer.actionLink(null, null, null, null, action);
		renderer.report(null, action);
		renderer.download(null, action, "admin", "Doc");
		renderer.upload(null, action);
		assertTrue(renderer.action(null, null, null, action, ImplicitActionName.Cancel, "Cancel").toString().contains("action"));
	}

	@Test
	@SuppressWarnings("static-method")
	void layoutRendererCoversContainerMethods() {
		Map<String, String> imports = new HashMap<>();
		ReactNativeLayoutRenderer renderer = new ReactNativeLayoutRenderer(imports);

		assertNull(renderer.viewLayout(null));
		List<RenderedComponent> toolbars = renderer.toolbarLayouts(null);
		assertEquals(1, toolbars.size());

		RenderedComponent view = new RenderedComponent();
		renderer.addToolbarsOrLayouts(view, toolbars);
		assertNotNull(view.getChild(0));

		RenderedComponent root = new RenderedComponent();
		RenderedComponent pane = new RenderedComponent();
		root.addChild(pane);
		RenderedComponent tab = new RenderedComponent();
		renderer.addTab(pane, tab);
		assertSame(root, renderer.addedTab(null, tab));

		RenderedComponent tabLayout = renderer.tabLayout(null);
		assertTrue(tabLayout.toString().contains("<View>"));
		assertNotNull(imports.get("{ View }"));
		assertSame(tabLayout, renderer.addTabLayout(null, tab, tabLayout));

		RenderedComponent border = new RenderedComponent();
		renderer.addBorderLayout(border, tabLayout);
		assertSame(border, renderer.addedBorderLayout(null, tabLayout));

		assertTrue(renderer.vboxLayout(null, new VBox()).toString().contains("<View>"));
		assertTrue(renderer.hboxLayout(null, new HBox()).toString().contains("<View>"));
		assertTrue(renderer.formLayout(null, new Form()).toString().contains("<View>"));

		RenderedComponent rowLayout = renderer.formRowLayout(null, new FormRow());
		RenderedComponent formLayout = new RenderedComponent();
		assertSame(rowLayout, renderer.addFormRowLayout(null, formLayout, rowLayout));
		assertSame(formLayout, renderer.addedFormRowLayout(null, rowLayout));

		RenderedComponent row = new RenderedComponent();
		renderer.layoutFormItemLabel(row, new RenderedComponent(), new Form(), new FormItem(), new FormColumn(), "Label", null, null, null);
		renderer.layoutFormItemWidget(row, new RenderedComponent(), new Form(), new FormItem(), new FormColumn(), "Label", 1, null, null, null);
		assertNotNull(row.getChild(0));
		assertNotNull(row.getChild(1));

		RenderedComponent grandParent = new RenderedComponent();
		RenderedComponent outer = new RenderedComponent();
		RenderedComponent container = new RenderedComponent();
		grandParent.addChild(outer);
		outer.addChild(container);
		RenderedComponent nested = new RenderedComponent();
		RenderedComponent added = renderer.addToContainer(null, null, container, nested, null, null, null, null);
		assertSame(nested, added);
		assertSame(container, nested.getParent().getParent());
		assertSame(grandParent, renderer.addedToContainer(null, null, container));
	}
}
