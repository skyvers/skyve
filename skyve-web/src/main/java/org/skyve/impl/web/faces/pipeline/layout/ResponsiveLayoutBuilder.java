package org.skyve.impl.web.faces.pipeline.layout;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputLabel;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.message.Message;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.LayoutUtil;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid.ResponsiveGridStyle;
import org.skyve.metadata.MetaData;

public class ResponsiveLayoutBuilder extends TabularLayoutBuilder {
/*
	@Override
	public UIComponent toolbarLayout() {
		return panelGroup(false, false, true, null);
//		return responsiveColumn(null, null, false);
	}
*/	

	/**
	 * There's only 1 toolbar for this layout and its at the top.
	 */
	@Override
	public void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts) {
		HtmlPanelGroup div = panelGroup(false, false, true, null, null);
		div.setStyleClass(UtilImpl.PRIMEFLEX ? "p-col-12" : "ui-g-12");
		div.getChildren().add(toolbarsOrLayouts.get(0));
		view.getChildren().add(0, div);
	}

	@Override
	public UIComponent viewLayout(UIComponent component) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = responsiveColumn(null, Integer.valueOf(12), null, null, null, null, null, null, true);
		if (UtilImpl.PRIMEFLEX) {
			result.setStyleClass(result.getStyleClass() + " p-col-nogutter");
		}
		return result;
	}
	
	@Override
	public UIComponent tabLayout(UIComponent component) {
		if (component != null) {
			return component;
		}

		return responsiveContainer(null, null, null, null);
	}
	
	@Override
	public UIComponent vboxLayout(UIComponent component, VBox vbox) {
		if (component != null) {
			return component;
		}

		return responsiveContainer(vbox.getVerticalAlignment(),
									vbox.getHorizontalAlignment(),
									vbox.getInvisibleConditionName(),
									vbox.getWidgetId());
	}
	
	@Override
	public UIComponent hboxLayout(UIComponent component, HBox hbox) {
		if (component != null) {
			return component;
		}

		return responsiveContainer(hbox.getVerticalAlignment(),
									hbox.getHorizontalAlignment(),
									hbox.getInvisibleConditionName(),
									hbox.getWidgetId());
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
										String widgetInvisible) {
		if (component != null) {
			return component;
		}

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
		if ((pixelWidth == null) && 
				(responsiveWidth == null) && 
				(percentageWidth == null) && 
				(viewContainer instanceof HBox)) {
			int unsizedCols = 0;
			int mediumColsRemaining = LayoutUtil.MAX_RESPONSIVE_WIDTH_COLUMNS;
			for (MetaData contained : viewContainer.getContained()) {
				if (contained instanceof AbsoluteWidth) {
					Integer containedPixelWidth = ((AbsoluteWidth) contained).getPixelWidth();
					if (containedPixelWidth != null) {
						mediumColsRemaining -= LayoutUtil.pixelWidthToMediumResponsiveWidth(containedPixelWidth.doubleValue());
					}
					else if (contained instanceof RelativeWidth) {
						Integer containedPercentageWidth = ((RelativeWidth) contained).getPercentageWidth();
						if (containedPercentageWidth != null) {
							mediumColsRemaining -= LayoutUtil.percentageWidthToResponsiveWidth(containedPercentageWidth.doubleValue());
						}
						else {
							unsizedCols++;
						}
					}
					else {
						unsizedCols++;
					}
				}
				else if (! (contained instanceof Inject)) { // inject takes up no layout
					unsizedCols++;
				}
			}
			mutablePercentageWidth = Integer.valueOf(LayoutUtil.responsiveWidthToPercentageWidth(mediumColsRemaining / unsizedCols));
		}
		HtmlPanelGroup div = responsiveColumn(pixelWidth, responsiveWidth, mutablePercentageWidth, sm, md, lg, xl, widgetInvisible, nopad);
		div.getChildren().add(componentToAdd);
		container.getChildren().add(div);
		return componentToAdd;
	}
	
	@Override
	public UIComponent addedToContainer(UIComponent component, Container viewContainer, UIComponent container) {
		if (component != null) {
			return component;
		}

		return container.getParent().getParent(); // account for the previously pushed component, and the grid css div
	}
	
	@Override
	public UIComponent formLayout(UIComponent component, Form form) {
		if (component != null) {
			return component;
		}

		// Add the set of form column styles to the Faces ViewRoot.
		ResponsiveGridStyle[] formColumnStyles = responsiveFormStyleClasses(form.getColumns());
		ResponsiveFormGrid grid = new ResponsiveFormGrid(formColumnStyles);
		addResponsiveStyles(grid);
		
		HtmlPanelGroup result = panelGroup(false, false, true, form.getInvisibleConditionName(), form.getWidgetId());
		result.setStyleClass(UtilImpl.PRIMEFLEX ? "p-grid p-nogutter ui-fluid" : "ui-g ui-g-nopad ui-fluid");
		return result;
	}

	// Used to create dynamic style classes for responsive form layouts
	private int formIndex = Integer.MIN_VALUE;
	
	/**
	 * Add the responsive form grid to the faces view root.
	 * Return the index to the responsive form grid added for use
	 * in the EL expressions in the ensuing form markup.
	 * @param grid	The responsive form layout grid definition
	 */
	public void addResponsiveStyles(ResponsiveFormGrid grid) {
		Map<String, Object> attributes = fc.getViewRoot().getAttributes();
		@SuppressWarnings("unchecked")
		List<ResponsiveFormGrid> formStyles = (List<ResponsiveFormGrid>) attributes.get(FacesUtil.FORM_STYLES_KEY);
		if (formStyles == null) {
			formStyles = new ArrayList<>(5);
			attributes.put(FacesUtil.FORM_STYLES_KEY, formStyles);
		}
		formStyles.add(grid);
		formIndex = formStyles.size() - 1;
	}

	@Override
	public UIComponent formRowLayout(UIComponent component, FormRow row) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = panelGroup(false, false, true, null, null);
		// style="<repsonsive column reset method call>"
		String expression = String.format("#{%s.resetResponsiveFormStyle(%s)}", 
											managedBeanName, 
											Integer.toString(formIndex));
		result.setValueExpression("styleClass", 
									ef.createValueExpression(elc, expression, String.class));
		return result;
	}

	/**
	 * Overridden to add an extra <div class="ui-g or p-grid" /> to support responsive nesting.
	 */
	@Override
	public UIComponent addFormRowLayout(UIComponent component, UIComponent formLayout, UIComponent rowLayout) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup grid = panelGroup(false, false, true, null, null);
		grid.setStyleClass(UtilImpl.PRIMEFLEX ? "p-grid" : "ui-g");
		rowLayout.getChildren().add(grid);
		formLayout.getChildren().add(rowLayout);

		return grid;
	}

	// respect responsive width if it is defined in this renderer
	@Override
	protected void setSizeAndTextAlignStyle(UIComponent component, 
												String styleAttributeNameOverride, // if null, "style" is used
												String existingStyle, 
												Integer pixelWidth, 
												Integer responsiveWidth,
												Integer percentageWidth, 
												Integer pixelHeight, 
												Integer percentageHeight, 
												Integer defaultPercentageWidth,
												HorizontalAlignment textAlign) {
		if (responsiveWidth != null) {
			super.setSizeAndTextAlignStyle(component, styleAttributeNameOverride, existingStyle, null, responsiveWidth, null, pixelHeight, percentageHeight, null, textAlign);
		}
		else {
			super.setSizeAndTextAlignStyle(component, styleAttributeNameOverride, existingStyle, pixelWidth, responsiveWidth, percentageWidth, pixelHeight, percentageHeight, null, textAlign);
		}
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
		String label = currentFormItem.getLocalisedLabel();
		if (label == null) {
			label = widgetLabel;
		}
		HtmlPanelGroup div = panelGroup(false, false, true, null, null);
		setInvisible(div, widgetInvisible, null);
		// style="<repsonsive column calc method call>"
        String alignment = alignment(currentFormItem.getLabelHorizontalAlignment(), true);
		String expression = String.format("#{%s.getResponsiveFormStyle(%s, '%s', 1)}", 
											managedBeanName,
											Integer.toString(formIndex),
											alignment);
		div.setValueExpression("styleClass", 
								ef.createValueExpression(elc, expression, String.class));
		formOrRowLayout.getChildren().add(div);
		HtmlOutputLabel l = label(label, formItemComponent.getId(), widgetRequired);
		div.getChildren().add(l);
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
										String widgetHelpText,
										Integer widgetPixelWidth,
										boolean showLabel,
										boolean topLabel) {
		HtmlPanelGroup flex = panelGroup(false, false, true, null, null);
		if (showLabel && topLabel) {
			flex.setStyle("flex-wrap:nowrap;padding-top:16px");
		}
		else {
			flex.setStyle("flex-wrap:nowrap");
		}
		setInvisible(flex, widgetInvisible, null);
		List<UIComponent> flexChildren = flex.getChildren();
		
		// Add message, component and help to flex box as required
		
		Message m = message(formItemComponent.getId());
		m.setStyleClass("formMessageStyle");
		flexChildren.add(m);

		if (showLabel && topLabel) {
			HtmlPanelGroup fieldDiv = panelGroup(false, false, true, null, null);
			fieldDiv.setStyleClass("field");
			if (widgetPixelWidth != null) {
				fieldDiv.setStyle("width:" + widgetPixelWidth + "px");
			}
			else {
				fieldDiv.setStyle("width:100%");
			}
			HtmlPanelGroup floatSpan = panelGroup(false, false, false, null, null);
			floatSpan.setStyleClass("ui-float-label");
			fieldDiv.getChildren().add(floatSpan);
			List<UIComponent> floatSpanChildren = floatSpan.getChildren();
			floatSpanChildren.add(formItemComponent);

			String label = currentFormItem.getLocalisedLabel();
			if (label == null) {
				label = widgetLabel;
			}

			floatSpanChildren.add(label(label, formItemComponent.getId(), widgetRequired));

			flexChildren.add(fieldDiv);
		}
		else {
			flexChildren.add(formItemComponent);
		}

		String helpText = (Boolean.FALSE.equals(currentFormItem.getShowHelp()) ? null : widgetHelpText);
		if (helpText != null) {
			HtmlOutputText output = new HtmlOutputText();
			output.setEscape(false);
			output.setValue(String.format("<i class=\"fa fa-info-circle help\" data-tooltip=\"%s\"></i>",
											helpText));
			flexChildren.add(output);
		}
		
		// Update div's style
		// colspan should be 1.
		if (widgetColspan <= 1) {
			// styleClass="<repsonsive column calc method call>"
			String expression = String.format("#{%s.getResponsiveFormStyle(%s, '%s', 1)}", 
												managedBeanName, 
												Integer.toString(formIndex),
												alignment(currentFormItem.getHorizontalAlignment(), false));
			flex.setValueExpression("styleClass", 
									ef.createValueExpression(elc, expression, String.class));
		}
		else { // colspan > 1
			// styleClass="<repsonsive column calc method call>"
			String expression = String.format("#{%s.getResponsiveFormStyle(%s, '%s', %s)}", 
												managedBeanName, 
												Integer.toString(formIndex),
												alignment(currentFormItem.getHorizontalAlignment(), false),
												Integer.toString(widgetColspan));
			flex.setValueExpression("styleClass", 
									ef.createValueExpression(elc, expression, String.class));
		}
		formOrRowLayout.getChildren().add(flex);
	}
	
	private static String alignment(HorizontalAlignment alignment, boolean forFormLabel) {
		String result = null;
		if (alignment == null) {
			result = forFormLabel ? 
						HorizontalAlignment.right.toAlignmentString() : 
						HorizontalAlignment.left.toAlignmentString();
		}
		else {
			result = alignment.toAlignmentString();
		}

		return result + (UtilImpl.PRIMEFLEX ? "FormFlex" : "Form");
	}
	
	private HtmlPanelGroup responsiveContainer(VerticalAlignment vertical,
												HorizontalAlignment horizontal,
												String invisibleConditionName,
												String widgetId) {
		HtmlPanelGroup result = panelGroup(false, false, true, null, widgetId);
		setInvisible(result, invisibleConditionName, null);
		if (UtilImpl.PRIMEFLEX) {
			String styleClass = "p-grid";
			if (vertical == VerticalAlignment.top) {
				styleClass += " p-align-start";
			}
			else if (vertical == VerticalAlignment.middle) {
				styleClass += " p-align-center";
			}
			else if (vertical == VerticalAlignment.bottom) {
				styleClass += " p-align-end";
			}
			
			if (horizontal == HorizontalAlignment.centre) {
				styleClass += " p-justify-center";
			}
			else if (horizontal == HorizontalAlignment.right) {
				styleClass += " p-justify-end";
			}

			result.setStyleClass(styleClass);
		}
		else {
			result.setStyleClass("ui-g");
		}
		
		return result;
	}

	private HtmlPanelGroup responsiveColumn(Integer pixelWidth, 
												Integer responsiveWidth, 
												Integer percentageWidth,
												Integer sm,
												Integer md,
												Integer lg,
												Integer xl,
												String widgetInvisible,
												boolean nopad) {
		HtmlPanelGroup result = panelGroup(false, false, true, null, null);
		
		String styleClasses = null;
		if (UtilImpl.PRIMEFLEX && (pixelWidth != null)) {
			styleClasses = "p-col-fixed";
			result.setStyle(new StringBuilder(16).append("width:").append(pixelWidth).append("px").toString());
		}
		else {
			styleClasses = responsiveGridStyleClasses(pixelWidth, responsiveWidth, percentageWidth, sm, md, lg, xl);
		}
		if (styleClasses != null) {
			if (nopad && (! UtilImpl.PRIMEFLEX)) {
				result.setStyleClass(styleClasses + " ui-g-nopad");
			}
			else {
				result.setStyleClass(styleClasses);
			}
		}

		setInvisible(result, widgetInvisible, null);
		return result;
	}

	private static String responsiveGridStyleClasses(Integer pixelWidth,
														Integer responsiveWidth,
														Integer percentageWidth,
														Integer sm,
														Integer md,
														Integer lg,
														Integer xl) {
		int small = 12;
		int medium = 12;
		int large = 12;
		int extraLarge = 12;
		
		if (responsiveWidth != null) {
			medium = responsiveWidth.intValue();
			large = medium;
			extraLarge = medium;
		}
		else if (pixelWidth != null) {
			double width = pixelWidth.doubleValue();
			medium = LayoutUtil.pixelWidthToMediumResponsiveWidth(width);
			large = LayoutUtil.pixelWidthToLargeResponsiveWidth(width);
			extraLarge = large;
		}
		else if (percentageWidth != null) {
			medium = LayoutUtil.percentageWidthToResponsiveWidth(percentageWidth.doubleValue());
			large = medium;
			extraLarge = medium;
		}

		if (sm != null) {
			small = sm.intValue();
		}
		if (md != null) {
			medium = md.intValue();
		}
		if (lg != null) {
			large = lg.intValue();
		}
		if (xl != null) {
			extraLarge = xl.intValue();
		}

		return new ResponsiveGridStyle(small, medium, large, extraLarge).toString();
	}
	
	private static ResponsiveGridStyle[] responsiveFormStyleClasses(List<FormColumn> formColumns) {
		ResponsiveGridStyle[] result = new ResponsiveGridStyle[formColumns.size()];
		
		// max number of columns
		int mediumColsRemaining = LayoutUtil.MAX_RESPONSIVE_WIDTH_COLUMNS;
		int largeColsRemaining = LayoutUtil.MAX_RESPONSIVE_WIDTH_COLUMNS;
		
		int unsizedCols = 0;
		
		for (int i = 0, l = formColumns.size(); i < l; i++) {
			FormColumn formColumn = formColumns.get(i);
			Integer pixelWidth = formColumn.getPixelWidth();
			Integer responsiveWidth = formColumn.getResponsiveWidth();
			Integer sm = formColumn.getSm();
			Integer md = formColumn.getMd();
			Integer lg = formColumn.getLg();
			Integer xl = formColumn.getXl();
			Integer percentageWidth = formColumn.getPercentageWidth();

			int small = 12;
			int medium = 0;
			int large = 0;
			int extraLarge = 0;
			boolean sized = true;
			
			if (responsiveWidth != null) {
				medium = responsiveWidth.intValue();
				large = medium;
				extraLarge = medium;
			}
			else if (pixelWidth != null) {
				double width = pixelWidth.doubleValue();
				medium = LayoutUtil.pixelWidthToMediumResponsiveWidth(width);
				large = LayoutUtil.pixelWidthToLargeResponsiveWidth(width);
				extraLarge = large;
			}
			else if (percentageWidth != null) {
				medium = LayoutUtil.percentageWidthToResponsiveWidth(percentageWidth.doubleValue());
				large = medium;
				extraLarge = medium;
			}
			else {
				sized = false;
				unsizedCols++;
			}
			
			if (sized) {
				if (sm != null) {
					small = sm.intValue();
				}
				if (md != null) {
					medium = md.intValue();
				}
				if (lg != null) {
					large = lg.intValue();
				}
				if (xl != null) {
					extraLarge = xl.intValue();
				}

				mediumColsRemaining -= medium;
				largeColsRemaining -= large;
				result[i] = new ResponsiveGridStyle(small, medium, large, extraLarge);
			}
		}

		if (unsizedCols > 0) {
			int medium = mediumColsRemaining / unsizedCols;
			int large = largeColsRemaining / unsizedCols;

			for (int i = 0, l = formColumns.size(); i < l; i++) {
				if (result[i] == null) {
					result[i] = new ResponsiveGridStyle(12, medium, large, large);
				}
			}
		}
		
		return result;
	}
}
