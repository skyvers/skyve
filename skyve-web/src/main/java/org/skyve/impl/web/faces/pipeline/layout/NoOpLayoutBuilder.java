package org.skyve.impl.web.faces.pipeline.layout;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;

public class NoOpLayoutBuilder extends LayoutBuilder {
	@Override
	public UIComponent viewLayout(UIComponent component) {
		return component;
	}

	@Override
	public List<UIComponent> toolbarLayouts(List<UIComponent> components) {
		return components;
	}

	@Override
	public UIComponent tabLayout(UIComponent component) {
		return component;
	}

	@Override
	public UIComponent vboxLayout(UIComponent component, VBox vbox) {
		return component;
	}

	@Override
	public UIComponent hboxLayout(UIComponent component, HBox hbox) {
		return component;
	}
	
	@Override
	public UIComponent sidebarLayout(UIComponent component, Sidebar sidebar) {
		return component;
	}

	@Override
	public UIComponent formLayout(UIComponent component, Form form) {
		return component;
	}

	@Override
	public UIComponent formRowLayout(UIComponent component, FormRow row) {
		return component;
	}

	@Override
	public void addToolbarLayouts(List<UIComponent> toolbars, List<UIComponent> toolbarLayouts) {
		// do nothing
	}

	@Override
	public void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts) {
		// do nothing
	}

	@Override
	public UIComponent addTabLayout(UIComponent component, UIComponent tab, UIComponent tabLayout) {
		return component;
	}

	@Override
	public void addTab(UIComponent tabPane, UIComponent tab) {
		// do nothing
	}

	@Override
	public UIComponent addedTab(UIComponent component, UIComponent tab) {
		return component;
	}

	@Override
	public void addBorderLayout(UIComponent border, UIComponent borderLayout) {
		// do nothing
	}

	@Override
	public UIComponent addedBorderLayout(UIComponent component, UIComponent borderLayout) {
		return component;
	}

	@Override
	public UIComponent addFormRowLayout(UIComponent component, UIComponent formLayout, UIComponent rowLayout) {
		return component;
	}

	@Override
	public UIComponent addedFormRowLayout(UIComponent component, UIComponent rowLayout) {
		return component;
	}

	@Override
	public void layoutFormItemLabel(UIComponent formOrRowLayout,
										UIComponent formItemComponent,
										Form currentForm,
										FormItem currentFormItem,
										FormColumn currentFormColumn,
										String widgetLabel,
										boolean widgetRequired,
										String widgetInvisible,
										String widgetHelpText) {
		// do nothing
	}

	@Override
	public void layoutFormItemWidget(UIComponent formOrRowLayout,
										UIComponent formItemComponent,
										Form currentForm,
										FormItem currentFormItem,
										FormColumn currentFormColumn,
										String widgetLabel,
										int widgetColspan,
										boolean widgetRequired,
										String widgetInvisible,
										String widgetHelpText) {
		// do nothing
	}

	@Override
	public UIComponent contentSignatureLayout(UIComponent component, ContentSignature signature) {
		return component;
	}
	
	@Override
	public UIComponent addToContainer(UIComponent component, 
										Container viewContainer,
										UIComponent container,
										UIComponent componentToAdd,
										Integer pixelWidth,
										Integer responsiveWidth,
										Integer percentageWidth,
										Integer sm,
										Integer md,
										Integer lg,
										Integer xl,
										String invisibleConditionName) {
		return component;
	}

	@Override
	public UIComponent addedToContainer(UIComponent component, Container viewContainer, UIComponent container) {
		return component;
	}
}
