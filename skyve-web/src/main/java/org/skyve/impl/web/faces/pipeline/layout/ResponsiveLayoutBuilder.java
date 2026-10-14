package org.skyve.impl.web.faces.pipeline.layout;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid.ResponsiveGridStyle;
import org.skyve.metadata.MetaData;
import org.skyve.util.Icons;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlOutputLabel;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGroup;

@SuppressWarnings("java:S1192") // Repeated literals are deliberate responsive-layout attribute fragments.
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
	public void addToolbarsOrLayouts(@Nonnull UIComponent view, @Nonnull List<UIComponent> toolbarsOrLayouts) {
		HtmlPanelGroup div = panelGroup(false, false, true, null, null);
		div.setStyleClass((UtilImpl.PRIMEFLEX ? "p-col-12 " : "ui-g-12 ") + ACTION_BAR_WRAPPER_STYLE_CLASS);
		div.getChildren().add(toolbarsOrLayouts.get(0));
		view.getChildren().add(0, div);
	}

	@Override
	public @Nullable UIComponent viewLayout(@Nullable UIComponent component) {
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
	public @Nullable UIComponent tabLayout(@Nullable UIComponent component) {
		if (component != null) {
			return component;
		}

		return responsiveContainer(null, null, null, null);
	}
	
	@Override
	public @Nullable UIComponent vboxLayout(@Nullable UIComponent component, @Nonnull VBox vbox) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = responsiveContainer(vbox.getVerticalAlignment(),
									vbox.getHorizontalAlignment(),
									vbox.getInvisibleCondition(),
									vbox.getWidgetId());
		Integer responsiveWidth = vbox.getResponsiveWidth();
		if ((responsiveWidth == null) || (responsiveWidth.intValue() == LayoutUtil.MAX_RESPONSIVE_WIDTH_COLUMNS)) {
			for (MetaData child : vbox.getContained()) {
				if (child instanceof DataGrid) {
					result.setStyle("flex:1 1 100%;min-width:0;width:100%");
					break;
				}
			}
		}
		return result;
	}
	
	@Override
	public @Nullable UIComponent hboxLayout(@Nullable UIComponent component, @Nonnull HBox hbox) {
		if (component != null) {
			return component;
		}

		return responsiveContainer(hbox.getVerticalAlignment(),
									hbox.getHorizontalAlignment(),
									hbox.getInvisibleCondition(),
									hbox.getWidgetId());
	}

	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
	public @Nullable UIComponent addToContainer(@Nullable UIComponent component,
										@Nonnull Container viewContainer, 
										@Nonnull UIComponent container, 
										@Nonnull UIComponent componentToAdd,
										@Nullable Integer pixelWidth, 
										@Nullable Integer responsiveWidth,
										@Nullable Integer percentageWidth,
										@Nullable Integer sm,
										@Nullable Integer md,
										@Nullable Integer lg,
										@Nullable Integer xl,
										@Nullable String widgetInvisible) {
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
				if (contained instanceof AbsoluteWidth absolute) {
					Integer containedPixelWidth = absolute.getPixelWidth();
					if (containedPixelWidth != null) {
						mediumColsRemaining -= LayoutUtil.pixelWidthToMediumResponsiveWidth(containedPixelWidth.doubleValue());
					}
					else if (contained instanceof RelativeWidth relative) {
						Integer containedPercentageWidth = relative.getPercentageWidth();
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
			if (unsizedCols > 0) {
				mutablePercentageWidth = Integer.valueOf(LayoutUtil.responsiveWidthToPercentageWidth(((double) mediumColsRemaining) / unsizedCols));
			}
		}
		HtmlPanelGroup div = responsiveColumn(pixelWidth, responsiveWidth, mutablePercentageWidth, sm, md, lg, xl, widgetInvisible, nopad);
		div.getChildren().add(componentToAdd);
		container.getChildren().add(div);
		return componentToAdd;
	}
	
	@Override
	public @Nullable UIComponent addedToContainer(@Nullable UIComponent component, @Nonnull Container viewContainer, @Nonnull UIComponent container) {
		if (component != null) {
			return component;
		}

		return container.getParent().getParent(); // account for the previously pushed component, and the grid css div
	}
	
	@Override
	public @Nullable UIComponent formLayout(@Nullable UIComponent component, @Nonnull Form form) {
		if (component != null) {
			return component;
		}

		// Add the set of form column styles to the Faces ViewRoot.
		ResponsiveGridStyle[] formColumnStyles = responsiveFormStyleClasses(form.getColumns());
		ResponsiveFormGrid grid = new ResponsiveFormGrid(formColumnStyles);
		addResponsiveStyles(grid);
		
		HtmlPanelGroup result = panelGroup(false, false, true, form.getInvisibleCondition(), form.getWidgetId());
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
	public void addResponsiveStyles(@Nonnull ResponsiveFormGrid grid) {
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
	public @Nullable UIComponent formRowLayout(@Nullable UIComponent component, @Nonnull FormRow row) {
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
	public @Nullable UIComponent addFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent formLayout, @Nonnull UIComponent rowLayout) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup grid = panelGroup(false, false, true, null, null);
		grid.setStyleClass(UtilImpl.PRIMEFLEX ? "p-grid skyve-form-row" : "ui-g skyve-form-row");
		rowLayout.getChildren().add(grid);
		formLayout.getChildren().add(rowLayout);

		return grid;
	}

	// respect responsive width if it is defined in this renderer
	@Override
	protected void setSizeAndTextAlignStyle(@Nonnull UIComponent component, 
												@Nullable String existingStyle, 
												@Nullable Integer pixelWidth, 
												@Nullable Integer responsiveWidth,
												@Nullable Integer percentageWidth, 
												@Nullable Integer pixelHeight, 
												@Nullable Integer percentageHeight, 
												@Nullable Integer defaultPercentageWidth,
												@Nullable HorizontalAlignment textAlign,
												@Nullable String styleAttributeNameOverride, // if null, "style" is used
												@Nullable String rightPaddingIfNecessary) {
		if (responsiveWidth != null) {
			super.setSizeAndTextAlignStyle(component, existingStyle, null, responsiveWidth, null, pixelHeight, percentageHeight, null, textAlign, styleAttributeNameOverride, rightPaddingIfNecessary);
		}
		else {
			super.setSizeAndTextAlignStyle(component, existingStyle, pixelWidth, responsiveWidth, percentageWidth, pixelHeight, percentageHeight, null, textAlign, styleAttributeNameOverride, rightPaddingIfNecessary);
		}
	}
	
	/**
	 * Lays out the responsive form-item label column.
	 *
	 * <p>Side effects: appends a responsive wrapper to {@code formOrRowLayout}
	 * and adds an output label whose metadata text and required marker are
	 * separate output responsibilities. A {@code null} label or required-message
	 * escape flag has the default escaped behaviour; only {@code Boolean.FALSE}
	 * allows trusted markup for the paired metadata text.
	 *
	 * @param formOrRowLayout target form or row layout
	 * @param formItemComponent form-item component being labelled
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel raw fallback label text
	 * @param widgetEscapeLabel resolved label escape decision
	 * @param widgetRequiredMessage required-message text normalised for unescaped
	 *        PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 * @param widgetInvisible invisible-condition expression
	 * @param widgetHelpText help text
	 */
	@Override
	public void layoutFormItemLabel(@Nonnull UIComponent formOrRowLayout, 
										@Nonnull UIComponent formItemComponent, 
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem, 
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel, 
										boolean widgetEscapeLabel,
										@Nullable String widgetRequiredMessage,
										boolean widgetEscapeRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText) {
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
		HtmlOutputLabel l = label(label, widgetEscapeLabel, formItemComponent.getId(), widgetRequiredMessage);
		div.getChildren().add(l);
	}
	
	/**
	 * Lays out the responsive form-item widget column.
	 *
	 * <p>Side effects: appends a responsive widget wrapper to
	 * {@code formOrRowLayout}, adds an unescaped PF message component, and may add
	 * a top label and help icon.
	 *
	 * @param formOrRowLayout target form or row layout
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel raw fallback label text
	 * @param widgetEscapeLabel resolved label escape decision
	 * @param widgetColspan widget column span
	 * @param widgetRequiredMessage required-message text normalised for unescaped
	 *        PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 * @param widgetInvisible invisible-condition expression
	 * @param widgetHelpText help text
	 * @param widgetEscapeHelp resolved help escape decision
	 * @param widgetPixelWidth optional pixel width
	 * @param showLabel whether labels are shown
	 * @param topLabel whether labels render above widgets
	 */
	@Override
	public void layoutFormItemWidget(@Nonnull UIComponent formOrRowLayout, 
										@Nonnull UIComponent formItemComponent, 
										@Nonnull Form currentForm,
										@Nonnull FormItem currentFormItem, 
										@Nonnull FormColumn currentFormColumn,
										@Nullable String widgetLabel,
										boolean widgetEscapeLabel,
										int widgetColspan,
										@Nullable String widgetRequiredMessage,
										boolean widgetEscapeRequiredMessage,
										@Nullable String widgetInvisible,
										@Nullable String widgetHelpText,
										boolean widgetEscapeHelp,
										@Nullable Integer widgetPixelWidth,
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
		
		Message m = message(formItemComponent.getId(), widgetEscapeRequiredMessage);
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
			floatSpan.setStyleClass("ui-float-label skyve-floating-label");
			fieldDiv.getChildren().add(floatSpan);
			List<UIComponent> floatSpanChildren = floatSpan.getChildren();
			floatSpanChildren.add(formItemComponent);

			String label = currentFormItem.getLocalisedLabel();
			if (label == null) {
				label = widgetLabel;
			}

			floatSpanChildren.add(label(label, widgetEscapeLabel, formItemComponent.getId(), widgetRequiredMessage));

			flexChildren.add(fieldDiv);
		}
		else {
			flexChildren.add(formItemComponent);
		}

		String helpText = (Boolean.FALSE.equals(currentFormItem.getShowHelp()) ? null : widgetHelpText);
		if (helpText != null) {
			HtmlOutputText output = new HtmlOutputText();
			output.setEscape(false);
			output.setValue(String.format("<i class=\"%s help\" data-tooltip=\"%s\"></i>", Icons.FONT_HELP, escapeFacesAttribute(helpText, widgetEscapeHelp)));
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
	
	private static @Nonnull String alignment(@Nullable HorizontalAlignment alignment, boolean forFormLabel) {
		String result = null;
		if (alignment == null) {
			result = forFormLabel ? 
						HorizontalAlignment.right.toTextAlignmentString() : 
						HorizontalAlignment.left.toTextAlignmentString();
		}
		else {
			result = alignment.toTextAlignmentString();
		}

		return result + (UtilImpl.PRIMEFLEX ? "FormFlex" : "Form");
	}
	
	private @Nonnull HtmlPanelGroup responsiveContainer(@Nullable VerticalAlignment vertical,
												@Nullable HorizontalAlignment horizontal,
												@Nullable String invisibleCondition,
												@Nullable String widgetId) {
		HtmlPanelGroup result = panelGroup(false, false, true, null, widgetId);
		setInvisible(result, invisibleCondition, null);
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

	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	private @Nonnull HtmlPanelGroup responsiveColumn(@Nullable Integer pixelWidth, 
												@Nullable Integer responsiveWidth, 
												@Nullable Integer percentageWidth,
												@Nullable Integer sm,
												@Nullable Integer md,
												@Nullable Integer lg,
												@Nullable Integer xl,
												@Nullable String widgetInvisible,
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

	private static @Nonnull String responsiveGridStyleClasses(@Nullable Integer pixelWidth,
														@Nullable Integer responsiveWidth,
														@Nullable Integer percentageWidth,
														@Nullable Integer sm,
														@Nullable Integer md,
														@Nullable Integer lg,
														@Nullable Integer xl) {
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
	
	@SuppressWarnings("java:S3776") // Complexity OK
	private static @Nonnull ResponsiveGridStyle[] responsiveFormStyleClasses(@Nonnull List<FormColumn> formColumns) {
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
