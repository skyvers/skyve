package org.skyve.impl.web.faces.pipeline.layout;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.column.Column;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.row.Row;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;

public class TabularLayoutBuilder extends LayoutBuilder {
	@Override
	public UIComponent viewLayout() {
        // Add the panel grid layout for the view container aspect
		PanelGrid result = panelGrid(null, null, ONE_HUNDRED, null, null, null, null);
		result.setColumns(1);
    	return result;
	}

	@Override
	public List<UIComponent> toolbarLayouts() {
		UIComponent layout = panelGroup(false, false, false, null, null);
		List<UIComponent> result = new ArrayList<>(1);
		result.add(layout);
		return result;
	}
	
	@Override
	public void addToolbarLayouts(List<UIComponent> toolbars, List<UIComponent> toolbarLayouts) {
		for (int i = 0, s = toolbars.size(); i < s; i++) {
			toolbars.get(i).getFacets().put("left", toolbarLayouts.get(i));
/*		
			Sticky sticky = (Sticky) a.createComponent(Sticky.COMPONENT_TYPE);
			sticky.setTarget(toolbar.getId());
			sticky.setMargin(45);
			toolbar.getParent().getChildren().add(sticky);
*/
		}
	}
	
	/**
	 * There's only 1 toolbar for this layout and its at the top.
	 */
	@Override
	public void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts) {
		view.getChildren().add(0, toolbarsOrLayouts.get(0));
	}

	@Override
	public UIComponent tabLayout() {
		PanelGrid result = panelGrid(null, null, ONE_HUNDRED, null, null, null, null);
		result.setColumns(1);
		return result;
	}
	
	@Override
	public UIComponent addTabLayout(UIComponent tab, UIComponent tabLayout) {
		tab.getChildren().add(tabLayout);
		return tabLayout;
	}
	
	@Override
	public void addTab(UIComponent tabPane, UIComponent tab) {
		tabPane.getChildren().add(tab);
	}
	
	@Override
	public UIComponent addedTab(UIComponent component) {
		// need to remove the layout grid and the tab
		return component.getParent().getParent();
	}
	
	@Override
	public void addBorderLayout(UIComponent border, UIComponent borderLayout) {
		border.getChildren().add(borderLayout);
	}
	
	@Override
	public UIComponent addedBorderLayout(UIComponent borderLayout) {
		return borderLayout.getParent();
	}
	
	@Override
	public UIComponent vboxLayout(VBox vbox) {
		// VBox is a Panel grid with 1 column
		PanelGrid result = panelGrid(vbox.getPixelWidth(),
										null,
										null, // the parent container sets the percentage width
										vbox.getPixelHeight(),
										vbox.getPercentageHeight(),
										vbox.getInvisibleConditionName(),
										vbox.getWidgetId());
		result.setColumns(1);
		return result;
	}
	
	@Override
	public UIComponent hboxLayout(HBox hbox) {
		// HBox is a Panel grid with n columns
		return panelGrid(hbox.getPixelWidth(),
							null,
							null, // the parent container sets the percentage width
							hbox.getPixelHeight(),
							hbox.getPercentageHeight(),
							hbox.getInvisibleConditionName(),
							hbox.getWidgetId());
	}

	@Override
	public UIComponent formLayout(Form form) {
		return panelGrid(form.getPixelWidth(),
							null,
							null, // the parent container sets the percentage width
							form.getPixelHeight(), 
							form.getPercentageHeight(),
							form.getInvisibleConditionName(),
							form.getWidgetId());
	}
	
	@Override
	public UIComponent formRowLayout(FormRow row) {
		return row();
	}
	
	@Override
	public UIComponent addFormRowLayout(UIComponent formLayout, UIComponent rowLayout) {
		formLayout.getChildren().add(rowLayout);
		return rowLayout;
	}

	@Override
	public UIComponent addedFormRowLayout(UIComponent rowLayout) {
		return rowLayout.getParent();
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
		int mutableCurrentFormColumn = currentFormColumn;
		UIComponent columnOrField = null;

		// The label
		if (! Boolean.FALSE.equals(currentFormItem.getShowLabel())) {
			String label = currentFormItem.getLabel();
			if (label == null) {
				label = widgetLabel;
			}
			if (label != null) {
				List<FormColumn> formColumns = currentForm.getColumns();
				if (currentFormColumn >= formColumns.size()) {
					mutableCurrentFormColumn = 0;
				}
				FormColumn formColumn = formColumns.get(mutableCurrentFormColumn++);
				columnOrField = column(widgetInvisible, 
											true,
											false,
											formColumn.getPixelWidth(), 
											formColumn.getResponsiveWidth(),
											formColumn.getPercentageWidth(),
											null,
											null);
				formOrRowLayout.getChildren().add(columnOrField);
				HtmlPanelGroup pg = panelGroup(true, true, false, widgetInvisible, null);
				columnOrField.getChildren().add(pg);
				OutputLabel l = label(label, formItemComponent.getId(), widgetRequired);
				pg.getChildren().add(l);
				Message m = message(formItemComponent.getId());
				pg.getChildren().add(m);
			}
		}
		// The field
		List<FormColumn> formColumns = currentForm.getColumns();
		if (currentFormColumn >= formColumns.size()) {
			mutableCurrentFormColumn = 0;
		}
		FormColumn formColumn = formColumns.get(mutableCurrentFormColumn++);
		Column col = column(widgetInvisible,
								true,
								false,
								formColumn.getPixelWidth(),
								formColumn.getResponsiveWidth(),
								formColumn.getPercentageWidth(),
								currentFormItem.getColspan(),
								currentFormItem.getRowspan());
		formOrRowLayout.getChildren().add(col);
		col.getChildren().add(formItemComponent);
	}
	
	@Override
	public UIComponent addToContainer(Container viewContainer,
										UIComponent container, 
										UIComponent componentToAdd, 
										Integer pixelWidth, 
										Integer responsiveWidth,
										Integer percentageWidth,
										String widgetInvisible) {
		if (container instanceof PanelGrid) {
			if (viewContainer instanceof HBox) {
				// get the row or add a row if there is none
				Row r = null;
				if (container.getChildCount() == 0) {
					r = row();
					container.getChildren().add(r);
				}
				else {
					r = (Row) container.getChildren().get(0);
				}

				// add a column
				Column col = column(widgetInvisible, false, true, pixelWidth, responsiveWidth, percentageWidth, null, null);
				col.getChildren().add(componentToAdd);
				r.getChildren().add(col);
			}
			else { // every other container is a vertical layout and already has the panel grid columns set to 1
				container.getChildren().add(componentToAdd);
			}
		}
		else {
			throw new IllegalStateException("Trying to add to a container but the current faces component is not a panel grid!!! - " + container);
		}
		return componentToAdd;
	}
	
	@Override
	public UIComponent addedToContainer(Container viewContainer, UIComponent container) {
		UIComponent result = container.getParent(); // account for the previously pushed component

		// strip off the column and the row for HBox containers
		// All other components of containers are direct children
		if (viewContainer instanceof HBox) {
			result = result.getParent().getParent();
		}

		return result;
	}

	protected OutputLabel label(String value, String forId, boolean required) {
		OutputLabel result = (OutputLabel) a.createComponent(OutputLabel.COMPONENT_TYPE);
		result.setValue(required ? value + "&nbsp;*:" : value + ":");
		result.setEscape(false);
		setId(result, null);
		result.setFor(forId);
		return result;
	}

	protected Message message(String forId) {
		Message message = (Message) a.createComponent(Message.COMPONENT_TYPE);
		setId(message, null);
		message.setFor(forId);
		message.setShowDetail(true);
		message.setShowSummary(false);
		message.setDisplay("icon");

		return message;
	}

	protected PanelGrid panelGrid(Integer pixelWidth, 
									Integer responsiveWidth,
									Integer percentageWidth, 
									Integer pixelHeight,
									Integer percentageHeight,
									String invisibleConditionName,
									String widgetId) {
		PanelGrid result = (PanelGrid) a.createComponent(PanelGrid.COMPONENT_TYPE);
		setInvisible(result, invisibleConditionName, null);
		setSize(result, 
					null, 
					pixelWidth, 
					responsiveWidth, 
					percentageWidth, 
					pixelHeight, 
					percentageHeight, 
					NINETY_EIGHT);
		setId(result, widgetId);
		result.setStyleClass("ui-panelgrid-blank");
		return result;
	}

	private Row row() {
		Row result = (Row) a.createComponent(Row.COMPONENT_TYPE);
		setId(result, null);
		return result;
	}
}
