package org.skyve.impl.generate.client.react;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.skyve.impl.generate.client.LayoutRenderer;
import org.skyve.impl.generate.client.RenderedComponent;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
 
public class PrimeReactLayoutRenderer extends LayoutRenderer {
	private Map<String, String> imports;

	public PrimeReactLayoutRenderer(Map<String, String> imports) {
		this.imports = imports;
	}
	
	@Override
	public RenderedComponent viewLayout(RenderedComponent component) {
		return null; // no layout required
	}

	@Override
	public List<RenderedComponent> toolbarLayouts(List<RenderedComponent> components) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("toolbarLayout");
		return Collections.singletonList(result);
	}

	@Override
	public void addToolbarLayouts(List<RenderedComponent> toolbars, List<RenderedComponent> toolbarLayouts) {
		for (int i = 0, s = toolbars.size(); i < s; i++) {
			toolbars.get(i).addChild(toolbarLayouts.get(i));
		}
	}

	@Override
	public void addToolbarsOrLayouts(RenderedComponent view, List<RenderedComponent> toolbarsOrLayouts) {
		view.addChild(0, toolbarsOrLayouts.get(0));
	}

	@Override
	public RenderedComponent tabLayout(RenderedComponent component) {
		imports.put("{VBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<VBox>");
		result.setAfter("</VBox>");
		return result;
	}

	@Override
	public RenderedComponent addTabLayout(RenderedComponent component,
											RenderedComponent tab,
											RenderedComponent tabLayout) {
		tab.addChild(tabLayout);
		return tabLayout;
	}

	@Override
	public void addTab(RenderedComponent tabPane, RenderedComponent tab) {
		tabPane.addChild(tab);
	}

	@Override
	public RenderedComponent addedTab(RenderedComponent component, RenderedComponent tab) {
		return tab.getParent().getParent();
	}

	@Override
	public void addBorderLayout(RenderedComponent border, RenderedComponent borderLayout) {
		border.addChild(borderLayout);
	}

	@Override
	public RenderedComponent addedBorderLayout(RenderedComponent component, RenderedComponent borderLayout) {
		return borderLayout.getParent();
	}

	@Override
	public RenderedComponent vboxLayout(RenderedComponent component, VBox vbox) {
		imports.put("{VBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</VBox>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<VBox>");
		return result;
	}

	@Override
	public RenderedComponent hboxLayout(RenderedComponent component, HBox hbox) {
		imports.put("{HBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</HBox>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<HBox>");
		return result;
	}

	@Override
	public RenderedComponent formLayout(RenderedComponent component, Form form) {
		imports.put("{Form}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</Form>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<Form>");
		return result;
	}

	@Override
	public RenderedComponent formRowLayout(RenderedComponent component, FormRow row) {
		imports.put("{HBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</HBox>");
		StringBuilder output = result.getOutput();
		output.append("<HBox>");
		return result;
	}

	@Override
	public RenderedComponent addFormRowLayout(RenderedComponent component,
												RenderedComponent formLayout,
												RenderedComponent rowLayout) {
		formLayout.addChild(rowLayout);
		return rowLayout;
	}

	@Override
	public RenderedComponent addedFormRowLayout(RenderedComponent component, RenderedComponent rowLayout) {
		return rowLayout.getParent();
	}

	@Override
	public void layoutFormItemLabel(RenderedComponent formOrRowLayout,
										RenderedComponent formItemComponent,
										Form currentForm,
										FormItem currentFormItem,
										FormColumn currentFormColumn,
										String widgetLabel,
										boolean widgetRequired,
										String widgetInvisible,
										String widgetHelpText) {
		imports.put("{Cell}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent cell = new RenderedComponent();
		cell.getOutput().append("<Cell>").append(widgetLabel).append("</Cell>");
		formOrRowLayout.addChild(cell);
	}

	@Override
	public void layoutFormItemWidget(RenderedComponent formOrRowLayout,
										RenderedComponent formItemComponent,
										Form currentForm,
										FormItem currentFormItem,
										FormColumn currentFormColumn,
										String widgetLabel,
										boolean widgetRequired,
										String widgetInvisible,
										String widgetHelpText) {
		imports.put("{Cell}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent cell = new RenderedComponent();
		cell.getOutput().append("<Cell>");
		cell.setAfter("</Cell>");
		formOrRowLayout.addChild(cell);
		cell.addChild(formItemComponent);
	}

	@Override
	public RenderedComponent addToContainer(RenderedComponent component,
												Container viewContainer,
												RenderedComponent container,
												RenderedComponent componentToAdd,
												Integer pixelWidth,
												Integer responsiveWidth,
												Integer percentageWidth,
												String invisibleConditionName) {
		if (component != null) {
			return component;
		}

		// add a cell for either HBox or VBox, it doesn't matter as the parent controls horizontal or vertical
		imports.put("{Cell}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent cell = new RenderedComponent();
		cell.getOutput().append("<Cell>");
		cell.setAfter("</Cell>");
		container.addChild(cell);
		cell.addChild(componentToAdd);

		return componentToAdd;
	}

	@Override
	public RenderedComponent addedToContainer(RenderedComponent component,
												Container viewContainer,
												RenderedComponent container) {
		return container.getParent().getParent();
	}
}
