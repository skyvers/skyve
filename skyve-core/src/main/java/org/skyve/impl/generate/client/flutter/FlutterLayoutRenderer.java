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
 
public class FlutterLayoutRenderer extends LayoutRenderer {
	public static final String VBOX_IMPORT = "widgets/skyve_vbox";
	public static final String HBOX_IMPORT = "widgets/skyve_hbox";
	public static final String FORM_IMPORT = "widgets/skyve_form";
	public static final String FORMROW_IMPORT = "widgets/skyve_formrow";
	public static final String FORMCOLUMN_IMPORT = "widgets/skyve_formcolumn";
	public static final String FORMITEM_IMPORT = "widgets/skyve_formitem";

	private Set<String> imports;

	public FlutterLayoutRenderer(Set<String> imports) {
		this.imports = imports;
	}
	
	@Override
	public RenderedComponent viewLayout(RenderedComponent component) {
		return null; // no layout required
	}

	@Override
	public List<RenderedComponent> toolbarLayouts(List<RenderedComponent> components) {
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("Container(padding: const EdgeInsets.symmetric(horizontal: 10.0), child: Wrap(alignment: WrapAlignment.center, spacing: 8.0, children: [");
		result.setAfter("])),");
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
		imports.add(VBOX_IMPORT);
		
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT);
		StringBuilder output = result.getOutput();
		output.append("SkyveVBox(children: [");
		result.setAfter("]),");
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
		imports.add(VBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveVBox(children: [");
		return result;
	}

	@Override
	public RenderedComponent hboxLayout(RenderedComponent component, HBox hbox) {
		imports.add(HBOX_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append("SkyveHBox(children: [");
		return result;
	}

	@Override
	public RenderedComponent formLayout(RenderedComponent component, Form form) {
		imports.add(FORM_IMPORT);
		imports.add(FORMCOLUMN_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),").setIndent("");
		StringBuilder output = result.getOutput();
		output.append(
				"SkyveForm(formCols: [], formRows: [");
		return result;
	}

	@Override
	public RenderedComponent formRowLayout(RenderedComponent component, FormRow row) {
		imports.add(FORMROW_IMPORT);
		RenderedComponent result = new RenderedComponent(FlutterGenerator.INDENT).setAfter("]),");
		StringBuilder output = result.getOutput();
		output.append(
				"SkyveFormRow(formItems: [");
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
		imports.add(FlutterComponentRenderer.LABEL_IMPORT);
		RenderedComponent cell = new RenderedComponent(FlutterGenerator.INDENT);
		cell.getOutput().append("SkyveFormItem(const SkyveLabel('").append(widgetLabel).append("')),");
		formOrRowLayout.addChild(cell);
	}

	@Override
	public void layoutFormItemWidget(RenderedComponent formOrRowLayout, RenderedComponent formItemComponent, Form currentForm,
			FormItem currentFormItem, FormColumn currentFormColumn, String widgetLabel, int formWidgetColspan,
			boolean widgetRequired, String widgetInvisible, String widgetHelpText) {
		imports.add(FORMITEM_IMPORT);
RenderedComponent col = new RenderedComponent(FlutterGenerator.INDENT).setAfter("),").setIndent("");
		col.getOutput().append("SkyveFormItem(");
		formOrRowLayout.addChild(col);
		col.addChild(formItemComponent);

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
	public RenderedComponent addedToContainer(RenderedComponent component,
												Container viewContainer,
												RenderedComponent container) {
// NB add another getParent() if a Cell like intermediate layout object is added in above addToContainer method.
//		return container.getParent().getParent();
		return container.getParent();
	}

}
