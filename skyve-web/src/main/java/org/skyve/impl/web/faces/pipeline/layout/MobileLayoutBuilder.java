package org.skyve.impl.web.faces.pipeline.layout;

import javax.faces.component.UIComponent;

import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.mobile.component.field.Field;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;

public class MobileLayoutBuilder extends TabularLayoutBuilder {
	@Override
	public UIComponent viewLayout() {
		return null;
	}

	@Override
	public UIComponent tabLayout() {
		return null;
	}
	
	@Override
	public UIComponent addedTab(UIComponent component) {
		return component.getParent();
	}
	
	@Override
	public UIComponent hboxLayout(HBox hbox) {
		// HBox is a Panel grid with 1 column when mobile
		PanelGrid result = panelGrid(hbox.getPixelWidth(),
										null,
										null, // the parent container sets the percentage width
										hbox.getPixelHeight(),
										hbox.getPercentageHeight(),
										hbox.getInvisibleConditionName(),
										hbox.getWidgetId());
		result.setColumns(1);
		return result;
	}

	@Override
	public UIComponent formLayout(Form form) {
		return panelGroup(false, false, true, form.getInvisibleConditionName(), form.getWidgetId());
	}
	
	@Override
	public UIComponent formRowLayout(FormRow row) {
		return null;
	}
	
	@Override
	public UIComponent addFormRowLayout(UIComponent formLayout, UIComponent rowLayout) {
		formLayout.getChildren().add(rowLayout);
		return rowLayout;
	}

	@Override
	public UIComponent addedFormRowLayout(UIComponent rowLayout) {
		return null;
	}

	@Override
	public void layoutFormItem(UIComponent formOrRowLayout,
								UIComponent formItemComponent,
								Form currentForm,
								FormItem currentFormItem,
								int currentFormColumn,
								String widgetLabel,
								boolean widgetRequired,
								String widgetInvisible,
								boolean widgetShowsLabelByDefault,
								String widgetHelpText) {
		UIComponent field = null;

		// The label
		if (! Boolean.FALSE.equals(currentFormItem.getShowLabel())) {
			String label = currentFormItem.getLabel();
			if (label == null) {
				label = widgetLabel;
			}
			if (label != null) {
				field = field(widgetInvisible);
				OutputLabel l = label(label, null, widgetRequired);
				field.getChildren().add(l);
				formOrRowLayout.getChildren().add(field);
			}
		}
		// The field
		if (field == null) {
			field = field(null);
			formOrRowLayout.getChildren().add(field);
		}
		field.getChildren().add(formItemComponent);
	}
	
	@Override
	public UIComponent addToContainer(Container viewContainer,
										UIComponent container, 
										UIComponent componentToAdd, 
										Integer pixelWidth, 
										Integer responsiveWidth,
										Integer percentageWidth,
										String widgetInvisible) {
		container.getChildren().add(componentToAdd);
		return componentToAdd;
	}
	
	@Override
	public UIComponent addedToContainer(Container viewContainer, UIComponent container) {
		return container.getParent(); // account for the previously pushed component
	}

	private Field field(String invisibleConditionName) {
		Field result = (Field) a.createComponent(Field.COMPONENT_TYPE);
		setId(result, null);
		setInvisible(result, invisibleConditionName, null);
		return result;
	}
}
