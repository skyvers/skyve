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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
 
/**
 * PrimeReact-specific layout/container renderer.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate fragments of generated PrimeReact layout output.
public class PrimeReactLayoutRenderer extends LayoutRenderer {
	private Map<String, String> imports;

	public PrimeReactLayoutRenderer(@Nonnull Map<String, String> imports) {
		this.imports = imports;
	}
	
	@Override
	public @Nullable RenderedComponent viewLayout(@Nullable RenderedComponent component) {
		return null; // no layout required
	}

	@Override
	public @Nonnull List<RenderedComponent> toolbarLayouts(@Nullable List<RenderedComponent> components) {
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("toolbarLayout");
		return Collections.singletonList(result);
	}

	@Override
	public void addToolbarLayouts(@Nonnull List<RenderedComponent> toolbars, @Nonnull List<RenderedComponent> toolbarLayouts) {
		for (int i = 0, s = toolbars.size(); i < s; i++) {
			toolbars.get(i).addChild(toolbarLayouts.get(i));
		}
	}

	@Override
	public void addToolbarsOrLayouts(@Nonnull RenderedComponent view, @Nonnull List<RenderedComponent> toolbarsOrLayouts) {
		view.addChild(0, toolbarsOrLayouts.get(0));
	}

	@Override
	public @Nonnull RenderedComponent tabLayout(@Nullable RenderedComponent component) {
		imports.put("{VBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent();
		StringBuilder output = result.getOutput();
		output.append("<VBox>");
		result.setAfter("</VBox>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent addTabLayout(@Nullable RenderedComponent component,
													@Nonnull RenderedComponent tab,
													@Nonnull RenderedComponent tabLayout) {
		tab.addChild(tabLayout);
		return tabLayout;
	}

	@Override
	public void addTab(@Nonnull RenderedComponent tabPane, @Nonnull RenderedComponent tab) {
		tabPane.addChild(tab);
	}

	@Override
	public @Nonnull RenderedComponent addedTab(@Nullable RenderedComponent component, @Nonnull RenderedComponent tab) {
		return tab.getNullSafeParent().getNullSafeParent();
	}

	@Override
	public void addBorderLayout(@Nonnull RenderedComponent border, @Nonnull RenderedComponent borderLayout) {
		border.addChild(borderLayout);
	}

	@Override
	public @Nonnull RenderedComponent addedBorderLayout(@Nullable RenderedComponent component, @Nonnull RenderedComponent borderLayout) {
		return borderLayout.getNullSafeParent();
	}

	@Override
	public @Nonnull RenderedComponent vboxLayout(@Nullable RenderedComponent component, @Nonnull VBox vbox) {
		imports.put("{VBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</VBox>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<VBox>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent hboxLayout(@Nullable RenderedComponent component, @Nonnull HBox hbox) {
		imports.put("{HBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</HBox>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<HBox>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent formLayout(@Nullable RenderedComponent component, @Nonnull Form form) {
		imports.put("{Form}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</Form>").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("<Form>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent formRowLayout(@Nullable RenderedComponent component, @Nonnull FormRow row) {
		imports.put("{HBox}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent result = new RenderedComponent().setAfter("</HBox>");
		StringBuilder output = result.getOutput();
		output.append("<HBox>");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent addFormRowLayout(@Nullable RenderedComponent component,
														@Nonnull RenderedComponent formLayout,
														@Nonnull RenderedComponent rowLayout) {
		formLayout.addChild(rowLayout);
		return rowLayout;
	}

	@Override
	public @Nonnull RenderedComponent addedFormRowLayout(@Nullable RenderedComponent component, @Nonnull RenderedComponent rowLayout) {
		return rowLayout.getNullSafeParent();
	}

	@Override
	public void layoutFormItemLabel(@Nonnull RenderedComponent formOrRowLayout,
										@Nonnull RenderedComponent formItemComponent,
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem,
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel,
										@Nullable String widgetRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText) {
		imports.put("{Cell}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent cell = new RenderedComponent();
		cell.getOutput().append("<Cell>").append(widgetLabel).append("</Cell>");
		formOrRowLayout.addChild(cell);
	}

	@Override
	public void layoutFormItemWidget(@Nonnull RenderedComponent formOrRowLayout,
										@Nonnull RenderedComponent formItemComponent,
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem,
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel,
										int formWidgetLabel,
										@Nullable String widgetRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText) {
		imports.put("{Cell}", PrimeReactViewRenderer.PRIME_REACT_VIEW_FILE);
		RenderedComponent cell = new RenderedComponent();
		cell.getOutput().append("<Cell>");
		cell.setAfter("</Cell>");
		formOrRowLayout.addChild(cell);
		cell.addChild(formItemComponent);
	}

	@Override
	public @Nonnull RenderedComponent addToContainer(@Nullable RenderedComponent component,
														@Nonnull Container viewContainer,
														@Nonnull RenderedComponent container,
														@Nonnull RenderedComponent componentToAdd,
														@Nullable Integer pixelWidth,
														@Nullable Integer responsiveWidth,
														@Nullable Integer percentageWidth,
														@Nullable String invisibleCondition) {
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
	public @Nonnull RenderedComponent addedToContainer(@Nullable RenderedComponent component,
														@Nonnull Container viewContainer,
														@Nonnull RenderedComponent container) {
		return container.getNullSafeParent().getNullSafeParent();
	}
}
