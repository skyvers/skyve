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

import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;
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
	public UIComponent viewLayout(UIComponent component) {
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
	public List<UIComponent> toolbarLayouts(List<UIComponent> components) {
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
	 *
	 * @param view view component
	 * @param toolbarsOrLayouts toolbar or layout components
	 */
	@Override
	public void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts) {
		view.getChildren().add(0, toolbarsOrLayouts.get(0));
	}

	/**
	 * Builds the tab layout container as a single-column panel grid.
	 *
	 * @param component existing component
	 * @return existing component when provided, otherwise a new tab layout
	 */
	@Override
	public UIComponent tabLayout(UIComponent component) {
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
	public UIComponent addTabLayout(UIComponent component, UIComponent tab, UIComponent tabLayout) {
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
	public void addTab(UIComponent tabPane, UIComponent tab) {
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
	public UIComponent addedTab(UIComponent component, UIComponent tab) {
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
	public void addBorderLayout(UIComponent border, UIComponent borderLayout) {
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
	public UIComponent addedBorderLayout(UIComponent component, UIComponent borderLayout) {
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
	public UIComponent vboxLayout(UIComponent component, VBox vbox) {
		if (component != null) {
			return component;
		}

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
	
	/**
	 * Builds horizontal-box layout as a multi-column panel grid.
	 *
	 * @param component existing component
	 * @param hbox hbox metadata
	 * @return existing component when provided, otherwise a new hbox layout
	 */
	@Override
	public UIComponent hboxLayout(UIComponent component, HBox hbox) {
		if (component != null) {
			return component;
		}

		// HBox is a Panel grid with n columns
		return panelGrid(hbox.getPixelWidth(),
							null,
							null, // the parent container sets the percentage width
							hbox.getPixelHeight(),
							hbox.getPercentageHeight(),
							hbox.getInvisibleConditionName(),
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
	public UIComponent sidebarLayout(UIComponent component, Sidebar sidebar, boolean createView) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = panelGroup(false, false, true, sidebar.getInvisibleConditionName(), sidebar.getWidgetId());
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
		String invisibleConditionName = sidebar.getInvisibleConditionName();
		if (invisibleConditionName != null) {
			rendered.append(" and (not ").append(managedBeanName).append(".currentBean['");
			rendered.append(invisibleConditionName).append("'])");
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
	public UIComponent formLayout(UIComponent component, Form form) {
		if (component != null) {
			return component;
		}

		return panelGrid(form.getPixelWidth(),
							null,
							null, // the parent container sets the percentage width
							form.getPixelHeight(), 
							form.getPercentageHeight(),
							form.getInvisibleConditionName(),
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
	public UIComponent formRowLayout(UIComponent component, FormRow row) {
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
	public UIComponent addFormRowLayout(UIComponent component, UIComponent formLayout, UIComponent rowLayout) {
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
	public UIComponent addedFormRowLayout(UIComponent component, UIComponent rowLayout) {
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
	 * @param widgetRequiredMessage optional required-message text
	 * @param widgetInvisible widget invisible condition
	 * @param widgetHelpText widget help text
	 */
	@Override
	public void layoutFormItemLabel(UIComponent formOrRowLayout,
										UIComponent formItemComponent,
										Form currentForm,
										FormItem currentFormItem,
										FormColumn currentFormColumn,
										String widgetLabel,
										@Nullable String widgetRequiredMessage,
										String widgetInvisible,
										String widgetHelpText) {
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
		OutputLabel l = label(label, formItemComponent.getId(), widgetRequiredMessage);
		pg.getChildren().add(l);
		Message m = message(formItemComponent.getId());
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
	 * @param widgetColspan widget column span
	 * @param widgetRequiredMessage optional required-message text
	 * @param widgetInvisible widget invisible condition
	 * @param widgetHelpText widget help text
	 * @param widgetPixelWidth widget pixel width
	 * @param showLabel whether label is shown
	 * @param topLabel whether top-label mode is active
	 */
	@Override
	public void layoutFormItemWidget(UIComponent formOrRowLayout,
										UIComponent formItemComponent,
										Form currentForm,
										FormItem currentFormItem,
										FormColumn currentFormColumn,
										String widgetLabel,
										int widgetColspan,
										@Nullable String widgetRequiredMessage,
										String widgetInvisible,
										String widgetHelpText,
										Integer widgetPixelWidth,
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
	public UIComponent contentSignatureLayout(UIComponent component, ContentSignature signature) {
		if (component != null) {
			return component;
		}

		// Signature Grid
		HtmlPanelGrid result = (HtmlPanelGrid) a.createComponent(HtmlPanelGrid.COMPONENT_TYPE);
		setId(result, null);
		result.setColumns(2);
		setInvisible(result, signature.getInvisibleConditionName(), null);
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
	public UIComponent addedToContainer(UIComponent component, Container viewContainer, UIComponent container) {
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
	 * Creates a standard output label for a form item, appending required markup when needed.
	 *
	 * @param value label text
	 * @param forId target component id
	 * @param requiredMessage optional required-message text
	 * @return configured output label
	 */
	protected OutputLabel label(String value, String forId, @Nullable String requiredMessage) {
		OutputLabel result = (OutputLabel) a.createComponent(OutputLabel.COMPONENT_TYPE);
		result.setValue((requiredMessage != null) ? value + "&nbsp;*:" : value + ":");
		result.setEscape(false);
		setId(result, null);
		result.setFor(forId);
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
	 * @param invisibleConditionName invisible condition name
	 * @param widgetId widget id
	 * @return configured panel grid
	 */
	protected PanelGrid panelGrid(Integer pixelWidth, 
									Integer responsiveWidth,
									Integer percentageWidth, 
									Integer pixelHeight,
									Integer percentageHeight,
									String invisibleConditionName,
									String widgetId) {
		PanelGrid result = (PanelGrid) a.createComponent(PanelGrid.COMPONENT_TYPE);
		setInvisible(result, invisibleConditionName, null);
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
	 * @param invisibleConditionName invisible condition name
	 * @param widgetId widget id
	 * @return configured panel
	 */
	protected Panel panel(Integer pixelWidth, 
							Integer responsiveWidth,
							Integer percentageWidth, 
							Integer pixelHeight,
							Integer percentageHeight,
							String invisibleConditionName,
							String widgetId) {
		Panel result = (Panel) a.createComponent(Panel.COMPONENT_TYPE);
		setInvisible(result, invisibleConditionName, null);
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
	private Row row() {
		Row result = (Row) a.createComponent(Row.COMPONENT_TYPE);
		setId(result, null);
		return result;
	}
}
