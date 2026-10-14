package org.skyve.impl.generate.client.flutter;

import java.util.Collections;
import java.util.List;
import java.util.Set;

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
 * Flutter-specific layout/container renderer.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate fragments of generated Flutter layout output.
public class FlutterLayoutRenderer extends LayoutRenderer {
	public static final String VBOX_IMPORT = "widgets/skyve_vbox";
	public static final String HBOX_IMPORT = "widgets/skyve_hbox";
	public static final String FORM_IMPORT = "widgets/skyve_form";
	public static final String FORMROW_IMPORT = "widgets/skyve_formrow";
	public static final String FORMCOLUMN_IMPORT = "widgets/skyve_formcolumn";
	public static final String FORMITEM_IMPORT = "widgets/skyve_formitem";

	private Set<String> imports;

	public FlutterLayoutRenderer(@Nonnull Set<String> imports) {
		this.imports = imports;
	}
	
	@Override
	public @Nullable RenderedComponent viewLayout(@Nullable RenderedComponent component) {
		return null; // no layout required
	}

	@Override
	public @Nonnull List<RenderedComponent> toolbarLayouts(@Nullable List<RenderedComponent> components) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("Container(padding: const EdgeInsets.symmetric(horizontal: 10.0), child: Wrap(alignment: WrapAlignment.center, spacing: 8.0, children: [");
		result.setAfter("])),");
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
		imports.add(VBOX_IMPORT);
		
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveVBox(children: [");
		result.setAfter("]),");
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
		return tab.getParent().getParent();
	}

	@Override
	public void addBorderLayout(@Nonnull RenderedComponent border, @Nonnull RenderedComponent borderLayout) {
		border.addChild(borderLayout);
	}

	@Override
	public @Nonnull RenderedComponent addedBorderLayout(@Nullable RenderedComponent component, @Nonnull RenderedComponent borderLayout) {
		return borderLayout.getParent();
	}

	@Override
	public @Nonnull RenderedComponent vboxLayout(@Nullable RenderedComponent component, @Nonnull VBox vbox) {
		imports.add(VBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveVBox(children: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent hboxLayout(@Nullable RenderedComponent component, @Nonnull HBox hbox) {
		imports.add(HBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveHBox(children: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent formLayout(@Nullable RenderedComponent component, @Nonnull Form form) {
		imports.add(FORM_IMPORT);
		imports.add(FORMCOLUMN_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append(
				"SkyveForm(formCols: [], formRows: [");
		return result;
	}

	@Override
	public @Nonnull RenderedComponent formRowLayout(@Nullable RenderedComponent component, @Nonnull FormRow row) {
		imports.add(FORMROW_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append(
				"SkyveFormRow(formItems: [");
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
		return rowLayout.getParent();
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
		imports.add(FlutterComponentRenderer.LABEL_IMPORT);
		RenderedComponent cell = new RenderedComponent(FlutterGenerator.INDENT);
		cell.getOutput().append("SkyveFormItem(const SkyveLabel('").append(widgetLabel).append("')),");
		formOrRowLayout.addChild(cell);
	}

	@Override
	public void layoutFormItemWidget(@Nonnull RenderedComponent formOrRowLayout,
										@Nonnull RenderedComponent formItemComponent,
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem,
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel,
										int formWidgetColspan,
										@Nullable String widgetRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText) {
		imports.add(FORMITEM_IMPORT);
RenderedComponent col = new RenderedComponent(FlutterGenerator.INDENT).setAfter("),").setIndent("");
		col.getOutput().append("SkyveFormItem(");
		formOrRowLayout.addChild(col);
		col.addChild(formItemComponent);

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
/*
		// add a cell for either HBox or VBox, it doesn't matter as the parent controls horizontal or vertical
		imports.add("{Cell}");
		RenderedComponent cell = new RenderedComponent(FlutterGenerator.INDENT);
		cell.getOutput().append("<Cell>");
		cell.setAfter("</Cell>");
		container.addChild(cell);
		cell.addChild(componentToAdd);
*/

		container.addChild(componentToAdd);
		
		return componentToAdd;
	}

	@Override
	public @Nonnull RenderedComponent addedToContainer(@Nullable RenderedComponent component,
														@Nonnull Container viewContainer,
														@Nonnull RenderedComponent container) {
// NB add another getParent() if a Cell like intermediate layout object is added in above addToContainer method.
//		return container.getParent().getParent();
		return container.getParent();
	}

}
