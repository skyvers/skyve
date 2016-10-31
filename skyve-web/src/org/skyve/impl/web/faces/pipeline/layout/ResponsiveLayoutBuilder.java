package org.skyve.impl.web.faces.pipeline.layout;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputLabel;
import javax.faces.component.html.HtmlPanelGrid;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.message.Message;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.metadata.MetaData;

public class ResponsiveLayoutBuilder extends TabularLayoutBuilder {
	@Override
	public UIComponent toolbarLayout() {
		return responsiveColumn(null, null, false);
	}
	
	@Override
	public void addToolbarLayout(UIComponent toolbar, UIComponent toolbarLayout) {
		toolbar.getChildren().add(toolbarLayout);
	}
	
	@Override
	public UIComponent viewLayout() {
		return responsiveContainer();
	}
	
	@Override
	public UIComponent tabLayout() {
		return responsiveContainer();
	}
	
	@Override
	public UIComponent vboxLayout(VBox vbox) {
		return responsiveContainer();
	}
	
	@Override
	public UIComponent hboxLayout(HBox hbox) {
		return responsiveContainer();
	}

	@Override
	public UIComponent addToContainer(Container viewContainer, 
										UIComponent container, 
										UIComponent componentToAdd,
										Integer pixelWidth, 
										Integer percentageWidth) {
System.out.println(viewContainer + " : " + container +  " : " + componentToAdd);
		Integer mutablePercentageWidth = percentageWidth;
		boolean nopad = false;

		// If we have layout within Layout lovin', use nopad to keep the lineup real!
		if ((container instanceof HtmlPanelGroup) && (componentToAdd instanceof HtmlPanelGroup)) {
			nopad = true;
		}
/*
		// NB View is an implicit VBox
		if ((viewContainer instanceof VBox) || (viewContainer instanceof org.skyve.metadata.view.View)) {
			// we are adding more responsive layout, go with nopad
			if (componentToAdd instanceof HtmlPanelGroup) {
				nopad = true;
			}
		}
*/
		if ((pixelWidth == null) && (percentageWidth == null) && (viewContainer instanceof HBox)) {
			int unsizedCols = 0;
			int mediumColsRemaining = 12;
			for (MetaData contained : viewContainer.getContained()) {
				if (contained instanceof AbsoluteWidth) {
					Integer containedPixelWidth = ((AbsoluteWidth) contained).getPixelWidth();
					if (containedPixelWidth != null) {
						int width = containedPixelWidth.intValue();
						mediumColsRemaining -= (int) Math.ceil(width / MAX_MD * MAX_COLS);
					}
					else if (contained instanceof RelativeSize) {
						Integer containedPercentageWidth = ((RelativeSize) contained).getPercentageWidth();
						if (containedPercentageWidth != null) {
							int cols = (int) Math.ceil(containedPercentageWidth.intValue() / 100.0 * MAX_COLS);
							mediumColsRemaining -= cols;
						}
						else {
							unsizedCols++;
						}
					}
					else {
						unsizedCols++;
					}
				}
				else {
					unsizedCols++;
				}
			}
			mutablePercentageWidth = Integer.valueOf((int) ((mediumColsRemaining / unsizedCols) / 12.0 * 100.0));
		}
		HtmlPanelGroup div = responsiveColumn(pixelWidth, mutablePercentageWidth, nopad);
		div.getChildren().add(componentToAdd);
		container.getChildren().add(div);
		return componentToAdd;
	}
	
	@Override
	public UIComponent addedToContainer(Container viewContainer, UIComponent container) {
		return container.getParent().getParent(); // account for the previously pushed component, and the grid css div
	}
	
	private String[] formColumnStyles;
	
	@Override
	public UIComponent formLayout(Form form) {
		formColumnStyles = responsiveFormStyleClasses(form.getColumns());
		
		HtmlPanelGroup result = panelGroup(false, false, true, null);
		result.setStyleClass("ui-g ui-g-nopad ui-fluid");
		return result;
	}
	
	@Override
	public UIComponent formRowLayout(FormRow row) {
		HtmlPanelGroup result = panelGroup(false, false, true, null);
		result.setStyleClass("ui-g-12 ui-g-nopad");
		return result;
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
								String widgetInvisible) {
		int mutableCurrentFormColumn = currentFormColumn;

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
				HtmlPanelGroup div = panelGroup(false, false, false, null);
				setInvisible(div, widgetInvisible, null);
				div.setStyleClass(formColumnStyles[mutableCurrentFormColumn++]);
				formOrRowLayout.getChildren().add(div);
				HtmlPanelGrid pg = (HtmlPanelGrid) a.createComponent(HtmlPanelGrid.COMPONENT_TYPE);
				setId(pg);
				pg.setColumns(2);
				div.getChildren().add(pg);
				HtmlOutputLabel l = label(label, formItemComponent.getId(), widgetRequired);
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
//		FormColumn formColumn = formColumns.get(mutableCurrentFormColumn++);
		// TODO Calculate colspan and row span
		//currentFormItem.getColspan(),
		//currentFormItem.getRowspan());
		HtmlPanelGroup div = panelGroup(false, false, false, null);
		setInvisible(div, widgetInvisible, null);
		div.setStyleClass(formColumnStyles[mutableCurrentFormColumn++]);
		formOrRowLayout.getChildren().add(div);
		div.getChildren().add(formItemComponent);
	}
	
	private HtmlPanelGroup responsiveContainer() {
		HtmlPanelGroup result = panelGroup(false, false, true, null);
		result.setStyleClass("ui-g");
		return result;
	}

	private HtmlPanelGroup responsiveColumn(Integer pixelWidth, Integer percentageWidth, boolean nopad) {
		HtmlPanelGroup result = panelGroup(false, false, true, null);
		
		String responsiveGridStyleClasses = responsiveGridStyleClasses(pixelWidth, percentageWidth);
		if (responsiveGridStyleClasses != null) {
			result.setStyleClass(nopad ? responsiveGridStyleClasses + " ui-g-nopad" : responsiveGridStyleClasses);
		}
		return result;
	}

	private float MAX_MD = 1024;
	private float MAX_LG = 1440;
	private int MAX_COLS = 12;
	private String responsiveGridStyleClasses(Integer pixelWidth, Integer percentageWidth) {
		if (pixelWidth != null) {
			int width = pixelWidth.intValue();
			int medium = (int) Math.ceil(width / MAX_MD * MAX_COLS);
			int large = (int) Math.ceil(width / MAX_LG * MAX_COLS);
			return String.format("ui-g-12 ui-md-%s ui-lg-%s", Integer.toString(medium), Integer.toString(large));
		}
		else if (percentageWidth != null) {
			int width = percentageWidth.intValue();
			Integer result = Integer.valueOf((int) Math.ceil(width / 100.0 * MAX_COLS));
			return String.format("ui-g-12 ui-md-%s ui-lg-%s", result, result);
		}
		
		return "ui-g-12";
	}
	
	// TODO Need to cater for colspan in forms
	private String[] responsiveFormStyleClasses(List<FormColumn> formColumns) {
		String[] result = new String[formColumns.size()];
		
		// max number of columns
		int mediumColsRemaining = 12;
		int largeColsRemaining = 12;
		
		int unsizedCols = 0;
		
		for (int i = 0, l = formColumns.size(); i < l; i++) {
			FormColumn formColumn = formColumns.get(i);
			Integer pixelWidth = formColumn.getPixelWidth();
			Integer percentageWidth = formColumn.getPercentageWidth();
			if (pixelWidth != null) {
				int width = pixelWidth.intValue();
				int medium = (int) Math.ceil(width / MAX_MD * MAX_COLS);
				int large = (int) Math.ceil(width / MAX_LG * MAX_COLS);
				mediumColsRemaining -= medium;
				largeColsRemaining -= large;
				result[i] = String.format("ui-g-12 ui-md-%s ui-lg-%s", Integer.toString(medium), Integer.toString(large));
			}
			else if (percentageWidth != null) {
				int cols = (int) Math.ceil(percentageWidth.intValue() / 100.0 * MAX_COLS);
				mediumColsRemaining -= cols;
				largeColsRemaining -= cols;
				String col = Integer.toString(cols);
				result[i] = String.format("ui-g-12 ui-md-%s ui-lg-%s", col, col);
			}
			else {
				unsizedCols++;
			}
		}

		if (unsizedCols > 0) {
			int medium = mediumColsRemaining / unsizedCols;
			int large = largeColsRemaining / unsizedCols;

			for (int i = 0, l = formColumns.size(); i < l; i++) {
				FormColumn formColumn = formColumns.get(i);
				Integer pixelWidth = formColumn.getPixelWidth();
				Integer percentageWidth = formColumn.getPercentageWidth();
				if ((pixelWidth == null) && (percentageWidth == null)) {
					result[i] = String.format("ui-g-12 ui-md-%s ui-lg-%s", Integer.toString(medium), Integer.toString(large));
				}
			}
		}
		
		return result;
	}
}
