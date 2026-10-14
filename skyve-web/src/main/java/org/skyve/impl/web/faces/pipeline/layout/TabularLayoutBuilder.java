package org.skyve.impl.web.faces.pipeline.layout;

import java.util.ArrayList;
import java.util.List;

import org.primefaces.component.column.Column;
import org.primefaces.component.message.Message;
import org.primefaces.component.outputlabel.OutputLabel;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.panelgrid.PanelGrid;
import org.primefaces.component.row.Row;
import org.primefaces.component.toolbar.ToolbarGroup;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.metadata.view.TextOutput.Sanitisation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGrid;
import jakarta.faces.component.html.HtmlPanelGroup;

public class TabularLayoutBuilder extends LayoutBuilder {
	/**
	 * Builds the root view layout as a single-column panel grid.
	 *
	 * @param component existing component
	 * @return existing component when provided, otherwise a new view layout
	 */
	@Override
	public @Nullable UIComponent viewLayout(@Nullable UIComponent component) {
		if (component != null) {
			return component;
		}

		// Add the panel grid layout for the view container aspect
		PanelGrid result = panelGrid(null, null, ONE_HUNDRED, null, null, null, null);
		result.setColumns(1);
    	return result;
	}

	/**
	 * Builds toolbar layout containers.
	 *
	 * @param components existing toolbar layout components
	 * @return existing components when provided, otherwise a new toolbar layout list
	 */
	@Override
	public @Nullable List<UIComponent> toolbarLayouts(@Nullable List<UIComponent> components) {
		if (components != null) {
			return components;
		}

		ToolbarGroup toolbarGroup = (ToolbarGroup) a.createComponent(ToolbarGroup.COMPONENT_TYPE);
		setId(toolbarGroup, null);

		List<UIComponent> result = new ArrayList<>(1);
		result.add(toolbarGroup);
		return result;
	}
	
	/**
	 * Adds generated toolbar layouts into the supplied toolbar components.
	 *
	 * @param toolbars toolbar components
	 * @param toolbarLayouts toolbar layout components
	 */
	@Override
	public void addToolbarLayouts(@Nonnull List<UIComponent> toolbars, @Nonnull List<UIComponent> toolbarLayouts) {
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
	 *
	 * @param view view component
	 * @param toolbarsOrLayouts toolbar or layout components
	 */
	@Override
	public void addToolbarsOrLayouts(@Nonnull UIComponent view, @Nonnull List<UIComponent> toolbarsOrLayouts) {
		view.getChildren().add(0, toolbarsOrLayouts.get(0));
	}

	/**
	 * Builds the tab layout container as a single-column panel grid.
	 *
	 * @param component existing component
	 * @return existing component when provided, otherwise a new tab layout
	 */
	@Override
	public @Nullable UIComponent tabLayout(@Nullable UIComponent component) {
		if (component != null) {
			return component;
		}

		PanelGrid result = panelGrid(null, null, ONE_HUNDRED, null, null, null, null);
		result.setColumns(1);
		return result;
	}
	
	/**
	 * Adds tab layout content into a tab component.
	 *
	 * @param component existing component
	 * @param tab tab component
	 * @param tabLayout tab layout component
	 * @return existing component when provided, otherwise the tab layout
	 */
	@Override
	public @Nullable UIComponent addTabLayout(@Nullable UIComponent component, @Nonnull UIComponent tab, @Nonnull UIComponent tabLayout) {
		if (component != null) {
			return component;
		}

		tab.getChildren().add(tabLayout);
		return tabLayout;
	}
	
	/**
	 * Adds a tab component into a tab-pane container.
	 *
	 * @param tabPane tab-pane component
	 * @param tab tab component
	 */
	@Override
	public void addTab(@Nonnull UIComponent tabPane, @Nonnull UIComponent tab) {
		tabPane.getChildren().add(tab);
	}
	
	/**
	 * Completes tab processing by returning to the tab-pane parent context.
	 *
	 * @param component existing component
	 * @param tab tab component
	 * @return existing component when provided, otherwise the parent context component
	 */
	@Override
	public @Nullable UIComponent addedTab(@Nullable UIComponent component, @Nonnull UIComponent tab) {
		if (component != null) {
			return component;
		}

		// need to remove the layout grid and the tab
		return tab.getParent().getParent();
	}
	
	/**
	 * Adds border layout content into a border component.
	 *
	 * @param border border component
	 * @param borderLayout border layout component
	 */
	@Override
	public void addBorderLayout(@Nonnull UIComponent border, @Nonnull UIComponent borderLayout) {
		border.getChildren().add(borderLayout);
	}
	
	/**
	 * Completes border-layout processing by returning to the parent context.
	 *
	 * @param component existing component
	 * @param borderLayout border layout component
	 * @return existing component when provided, otherwise the border parent component
	 */
	@Override
	public @Nullable UIComponent addedBorderLayout(@Nullable UIComponent component, @Nonnull UIComponent borderLayout) {
		if (component != null) {
			return component;
		}

		return borderLayout.getParent();
	}
	
	/**
	 * Builds vertical-box layout as a single-column panel grid.
	 *
	 * @param component existing component
	 * @param vbox vbox metadata
	 * @return existing component when provided, otherwise a new vbox layout
	 */
	@Override
	public @Nullable UIComponent vboxLayout(@Nullable UIComponent component, @Nonnull VBox vbox) {
		if (component != null) {
			return component;
		}

		// VBox is a Panel grid with 1 column
		PanelGrid result = panelGrid(vbox.getPixelWidth(),
										null,
										null, // the parent container sets the percentage width
										vbox.getPixelHeight(),
										vbox.getPercentageHeight(),
										vbox.getInvisibleCondition(),
										vbox.getWidgetId());
		result.setColumns(1);
		return result;
	}
	
	/**
	 * Builds horizontal-box layout as a multi-column panel grid.
	 *
	 * @param component existing component
	 * @param hbox hbox metadata
	 * @return existing component when provided, otherwise a new hbox layout
	 */
	@Override
	public @Nullable UIComponent hboxLayout(@Nullable UIComponent component, @Nonnull HBox hbox) {
		if (component != null) {
			return component;
		}

		// HBox is a Panel grid with n columns
		return panelGrid(hbox.getPixelWidth(),
							null,
							null, // the parent container sets the percentage width
							hbox.getPixelHeight(),
							hbox.getPercentageHeight(),
							hbox.getInvisibleCondition(),
							hbox.getWidgetId());
	}

	/**
	 * Builds sidebar layout markup for tabular rendering.
	 *
	 * @param component existing component
	 * @param sidebar sidebar metadata
	 * @param createView whether create-view mode is active
	 * @return existing component when provided, otherwise a new sidebar layout
	 */
	@Override
	public @Nullable UIComponent sidebarLayout(@Nullable UIComponent component, @Nonnull Sidebar sidebar, boolean createView) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = panelGroup(false, false, true, sidebar.getInvisibleCondition(), sidebar.getWidgetId());
		result.setStyleClass("sidebar" + (createView ? "Create" : "Edit"));
		result.setStyle("display:none"); // shown by JS in SKYVE.PF.sidebar()
		HtmlPanelGroup inner = panelGroup(false, false, true, null, null);
		inner.setStyleClass("inner");
		result.getChildren().add(inner);

		// Don't render the sidebar if there is no bean selected as 
		// it'll cause a cascade of stack traces as the EL is evaluated
		StringBuilder rendered = new StringBuilder(64);
		rendered.append('(').append(managedBeanName).append(".currentBean ne null) and (");
		rendered.append(managedBeanName).append(".currentBean.getBean() ne null) and (not ");
		rendered.append(managedBeanName).append(".currentBean['").append(createView  ? "created" : "notCreated").append("'])");
		String invisibleCondition = sidebar.getInvisibleCondition();
		if (invisibleCondition != null) {
			rendered.append(" and (not ").append(createConditionFragment(managedBeanName + ".currentBean", invisibleCondition)).append(')');
		}
		result.setValueExpression("rendered",
									createValueExpressionFromFragment(null,
																		false,
																		rendered.toString(),
																		false,
																		null,
																		Boolean.class,
																		false,
																		Sanitisation.none));

		return result;
	}

	
	/**
	 * Builds form layout as a panel grid.
	 *
	 * @param component existing component
	 * @param form form metadata
	 * @return existing component when provided, otherwise a new form layout
	 */
	@Override
	public @Nullable UIComponent formLayout(@Nullable UIComponent component, @Nonnull Form form) {
		if (component != null) {
			return component;
		}

		return panelGrid(form.getPixelWidth(),
							null,
							null, // the parent container sets the percentage width
							form.getPixelHeight(), 
							form.getPercentageHeight(),
							form.getInvisibleCondition(),
							form.getWidgetId());
	}
	
	/**
	 * Builds a form-row layout wrapper.
	 *
	 * @param component existing component
	 * @param row form-row metadata
	 * @return existing component when provided, otherwise a new row component
	 */
	@Override
	public @Nullable UIComponent formRowLayout(@Nullable UIComponent component, @Nonnull FormRow row) {
		if (component != null) {
			return component;
		}

		return row();
	}
	
	/**
	 * Adds a row layout into the form layout.
	 *
	 * @param component existing component
	 * @param formLayout form layout component
	 * @param rowLayout row layout component
	 * @return existing component when provided, otherwise the row layout
	 */
	@Override
	public @Nullable UIComponent addFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent formLayout, @Nonnull UIComponent rowLayout) {
		if (component != null) {
			return component;
		}

		formLayout.getChildren().add(rowLayout);
		return rowLayout;
	}

	/**
	 * Completes form-row layout processing by returning to the parent context.
	 *
	 * @param component existing component
	 * @param rowLayout row layout component
	 * @return existing component when provided, otherwise the row-layout parent
	 */
	@Override
	public @Nullable UIComponent addedFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent rowLayout) {
		if (component != null) {
			return component;
		}

		return rowLayout.getParent();
	}

	/**
	 * Lays out form-item label cell content.
	 *
	 * @param formOrRowLayout form or row layout component
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel widget label fallback
	 * @param widgetEscapeLabel resolved escape decision for widget label
	 * @param widgetRequiredMessage optional required-message text normalised for
	 *        unescaped PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 *        API compatibility
	 * @param widgetInvisible widget invisible condition
	 * @param widgetHelpText widget help text
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
		// The label
		String label = currentFormItem.getLocalisedLabel();
		if (label == null) {
			label = widgetLabel;
		}
		Column column = column(widgetInvisible, 
								true,
								false,
								currentFormColumn.getPixelWidth(), 
								currentFormColumn.getResponsiveWidth(),
								currentFormColumn.getPercentageWidth(),
								1,
								1);
		formOrRowLayout.getChildren().add(column);
		HtmlPanelGroup pg = panelGroup(true, true, false, widgetInvisible, null);
		column.getChildren().add(pg);
		OutputLabel l = label(label, widgetEscapeLabel, formItemComponent.getId(), widgetRequiredMessage);
		pg.getChildren().add(l);
		Message m = message(formItemComponent.getId(), widgetEscapeRequiredMessage);
		pg.getChildren().add(m);
	}

	/**
	 * Lays out form-item widget cell content.
	 *
	 * @param formOrRowLayout form or row layout component
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel widget label
	 * @param widgetEscapeLabel resolved escape decision for widget label
	 * @param widgetColspan widget column span
	 * @param widgetRequiredMessage optional required-message text normalised for
	 *        unescaped PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 *        API compatibility
	 * @param widgetInvisible widget invisible condition
	 * @param widgetHelpText widget help text
	 * @param widgetEscapeHelp resolved escape decision for widget help text
	 * @param widgetPixelWidth widget pixel width
	 * @param showLabel whether label is shown
	 * @param topLabel whether top-label mode is active
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
		Integer rowspan = currentFormItem.getRowspan();
		Column col = column(widgetInvisible,
								true,
								false,
								currentFormColumn.getPixelWidth(),
								currentFormColumn.getResponsiveWidth(),
								currentFormColumn.getPercentageWidth(),
								widgetColspan,
								(rowspan == null) ? 0 : rowspan.intValue());
		formOrRowLayout.getChildren().add(col);
		col.getChildren().add(formItemComponent);
	}
	
	/**
	 * Builds layout wrapper for content-signature widgets.
	 *
	 * @param component existing component
	 * @param signature content-signature metadata
	 * @return existing component when provided, otherwise a new signature layout
	 */
	@Override
	public @Nullable UIComponent contentSignatureLayout(@Nullable UIComponent component, @Nonnull ContentSignature signature) {
		if (component != null) {
			return component;
		}

		// Signature Grid
		HtmlPanelGrid result = (HtmlPanelGrid) a.createComponent(HtmlPanelGrid.COMPONENT_TYPE);
		setId(result, null);
		result.setColumns(1);
		setInvisible(result, signature.getInvisibleCondition(), null);
		return result;
	}

	/**
	 * Adds a component into the current container layout.
	 *
	 * @param component existing component
	 * @param viewContainer current view-container metadata
	 * @param container current container component
	 * @param componentToAdd component to add
	 * @param pixelWidth optional fixed width
	 * @param responsiveWidth optional responsive width
	 * @param percentageWidth optional percentage width
	 * @param sm optional small breakpoint width
	 * @param md optional medium breakpoint width
	 * @param lg optional large breakpoint width
	 * @param xl optional extra-large breakpoint width
	 * @param widgetInvisible widget invisible condition
	 * @return existing component when provided, otherwise the added component
	 */
	@Override
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
				Column col = column(widgetInvisible, false, true, pixelWidth, responsiveWidth, percentageWidth, 0, 0);
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
	
	/**
	 * Completes container-add processing by returning to the parent context.
	 *
	 * @param component existing component
	 * @param viewContainer current view-container metadata
	 * @param container current container component
	 * @return existing component when provided, otherwise the parent context component
	 */
	@Override
	public @Nullable UIComponent addedToContainer(@Nullable UIComponent component, @Nonnull Container viewContainer, @Nonnull UIComponent container) {
		if (component != null) {
			return component;
		}

		UIComponent result = container.getParent(); // account for the previously pushed component

		// strip off the column and the row for HBox containers
		// All other components of containers are direct children
		if (viewContainer instanceof HBox) {
			result = result.getParent().getParent();
		}

		return result;
	}

	/**
	 * Creates a standard output label for a form item.
	 *
	 * <p>Side effects: asks the JSF application to create an output label and child
	 * output text components. The metadata label remains raw on its own child and
	 * uses the paired escape decision; the required marker and trailing colon are
	 * renderer-owned text on separate unescaped children.
	 *
	 * @param value label text
	 * @param escapeLabel resolved escape decision for {@code value}
	 * @param forId target component id
	 * @param requiredMessage optional required-message text
	 * @return configured output label
	 */
	protected @Nonnull OutputLabel label(@Nullable String value, boolean escapeLabel, @Nonnull String forId, @Nullable String requiredMessage) {
		OutputLabel result = (OutputLabel) a.createComponent(OutputLabel.COMPONENT_TYPE);
		result.setEscape(false);
		setId(result, null);
		result.setFor(forId);

		HtmlOutputText labelText = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		labelText.setValue(value);
		labelText.setEscape(escapeLabel);
		result.getChildren().add(labelText);

		HtmlOutputText suffix = (HtmlOutputText) a.createComponent(HtmlOutputText.COMPONENT_TYPE);
		suffix.setValue((requiredMessage != null) ? "&nbsp;*:" : ":");
		suffix.setEscape(false);
		result.getChildren().add(suffix);
		return result;
	}

	/**
	 * Creates a panel-grid container configured with invisibility, sizing, and widget identity.
	 *
	 * @param pixelWidth optional pixel width
	 * @param responsiveWidth optional responsive width
	 * @param percentageWidth optional percentage width
	 * @param pixelHeight optional pixel height
	 * @param percentageHeight optional percentage height
	 * @param invisibleCondition invisible condition name
	 * @param widgetId widget id
	 * @return configured panel grid
	 */
	protected @Nonnull PanelGrid panelGrid(@Nullable Integer pixelWidth, 
									@Nullable Integer responsiveWidth,
									@Nullable Integer percentageWidth, 
									@Nullable Integer pixelHeight,
									@Nullable Integer percentageHeight,
									@Nullable String invisibleCondition,
									@Nullable String widgetId) {
		PanelGrid result = (PanelGrid) a.createComponent(PanelGrid.COMPONENT_TYPE);
		setInvisible(result, invisibleCondition, null);
		setSizeAndTextAlignStyle(result, 
									null, 
									pixelWidth, 
									responsiveWidth, 
									percentageWidth, 
									pixelHeight, 
									percentageHeight, 
									null);
		setId(result, widgetId);
		result.setStyleClass("ui-panelgrid-blank");
		return result;
	}

	/**
	 * Creates a panel container configured with invisibility, sizing, and widget identity.
	 *
	 * @param pixelWidth optional pixel width
	 * @param responsiveWidth optional responsive width
	 * @param percentageWidth optional percentage width
	 * @param pixelHeight optional pixel height
	 * @param percentageHeight optional percentage height
	 * @param invisibleCondition invisible condition name
	 * @param widgetId widget id
	 * @return configured panel
	 */
	protected @Nonnull Panel panel(@Nullable Integer pixelWidth, 
							@Nullable Integer responsiveWidth,
							@Nullable Integer percentageWidth, 
							@Nullable Integer pixelHeight,
							@Nullable Integer percentageHeight,
							@Nullable String invisibleCondition,
							@Nullable String widgetId) {
		Panel result = (Panel) a.createComponent(Panel.COMPONENT_TYPE);
		setInvisible(result, invisibleCondition, null);
		setSizeAndTextAlignStyle(result, 
									null, 
									pixelWidth, 
									responsiveWidth, 
									percentageWidth, 
									pixelHeight, 
									percentageHeight, 
									null);
		setId(result, widgetId);			
		return result;
	}

	/**
	 * Creates a row component with generated identifier.
	 *
	 * @return configured row component
	 */
	private @Nonnull Row row() {
		Row result = (Row) a.createComponent(Row.COMPONENT_TYPE);
		setId(result, null);
		return result;
	}
}
