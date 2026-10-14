package org.skyve.impl.web.faces.pipeline.layout;

import java.util.List;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

/**
 * Builds Faces components for a specific rendering concern in the pipeline.
 */
public class LayoutBuilderChain extends LayoutBuilder {
	private LayoutBuilder[] builders;
	
	/**
	 * Creates a layout builder chain that delegates to the supplied builders in order.
	 */
	public LayoutBuilderChain(@Nonnull LayoutBuilder... builders) {
		this.builders = builders;
	}
	
	/**
	 * Sets managed bean name on this chain and all delegated builders.
	 *
	 * @param managedBeanName the managed bean name
	 */
	@Override
	public void setManagedBeanName(@Nullable String managedBeanName) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setManagedBeanName(managedBeanName);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setManagedBeanName(managedBeanName);
		}
	}
	
	/**
	 * Sets the SAIL managed bean on this chain and all delegated builders.
	 *
	 * @param managedBean the managed bean instance
	 */
	@Override
	public void setSAILManagedBean(@Nonnull FacesView managedBean) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setSAILManagedBean(managedBean);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setSAILManagedBean(managedBean);
		}
	}

	/**
	 * Sets JSF process expression on this chain and all delegated builders.
	 *
	 * @param process process expression
	 */
	@Override
	public void setProcess(@Nullable String process) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setProcess(process);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setProcess(process);
		}
	}

	/**
	 * Sets JSF update expression on this chain and all delegated builders.
	 *
	 * @param update update expression
	 */
	@Override
	public void setUpdate(@Nullable String update) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUpdate(update);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setUpdate(update);
		}
	}

	/**
	 * Sets user-agent type on this chain and all delegated builders.
	 *
	 * @param userAgentType user-agent type
	 */
	@Override
	public void setUserAgentType(@Nonnull UserAgentType userAgentType) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUserAgentType(userAgentType);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setUserAgentType(userAgentType);
		}
	}

	/**
	 * Builds the view layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent viewLayout(@Nullable UIComponent component) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.viewLayout(result);
		}
		return result;
	}

	/**
	 * Builds toolbar layouts by delegating through the chain in order.
	 *
	 * @param components input toolbar components
	 * @return resulting toolbar components
	 */
	@Override
	public @Nullable List<UIComponent> toolbarLayouts(@Nullable List<UIComponent> components) {
		List<UIComponent> result = components;
		for (LayoutBuilder builder : builders) {
			result = builder.toolbarLayouts(result);
		}
		return result;
	}

	/**
	 * Builds tab layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent tabLayout(@Nullable UIComponent component) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.tabLayout(result);
		}
		return result;
	}

	/**
	 * Builds vertical-box layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param vbox vbox metadata
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent vboxLayout(@Nullable UIComponent component, @Nonnull VBox vbox) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.vboxLayout(result, vbox);
		}
		return result;
	}

	/**
	 * Builds horizontal-box layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param hbox hbox metadata
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent hboxLayout(@Nullable UIComponent component, @Nonnull HBox hbox) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.hboxLayout(result, hbox);
		}
		return result;
	}
	
	/**
	 * Builds sidebar layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param sidebar sidebar metadata
	 * @param createView whether create-view mode is active
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent sidebarLayout(@Nullable UIComponent component, @Nonnull Sidebar sidebar, boolean createView) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.sidebarLayout(result, sidebar, createView);
		}
		return result;
	}

	/**
	 * Builds form layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param form form metadata
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent formLayout(@Nullable UIComponent component, @Nonnull Form form) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.formLayout(result, form);
		}
		return result;
	}

	/**
	 * Builds form-row layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param row form-row metadata
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent formRowLayout(@Nullable UIComponent component, @Nonnull FormRow row) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.formRowLayout(result, row);
		}
		return result;
	}

	/**
	 * Adds toolbar layouts by delegating to each builder.
	 *
	 * @param toolbars toolbar components
	 * @param toolbarLayouts toolbar layout components
	 */
	@Override
	public void addToolbarLayouts(@Nonnull List<UIComponent> toolbars, @Nonnull List<UIComponent> toolbarLayouts) {
		for (LayoutBuilder builder : builders) {
			builder.addToolbarLayouts(toolbars, toolbarLayouts);
		}
	}

	/**
	 * Adds toolbars or layouts to the view by delegating to each builder.
	 *
	 * @param view view component
	 * @param toolbarsOrLayouts toolbar or layout components
	 */
	@Override
	public void addToolbarsOrLayouts(@Nonnull UIComponent view, @Nonnull List<UIComponent> toolbarsOrLayouts) {
		for (LayoutBuilder builder : builders) {
			builder.addToolbarsOrLayouts(view, toolbarsOrLayouts);
		}
	}

	/**
	 * Adds tab layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param tab tab component
	 * @param tabLayout tab layout component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent addTabLayout(@Nullable UIComponent component, @Nonnull UIComponent tab, @Nonnull UIComponent tabLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addTabLayout(result, tab, tabLayout);
		}
		return result;
	}

	/**
	 * Adds a tab by delegating to each builder.
	 *
	 * @param tabPane tab-pane component
	 * @param tab tab component
	 */
	@Override
	public void addTab(@Nonnull UIComponent tabPane, @Nonnull UIComponent tab) {
		for (LayoutBuilder builder : builders) {
			builder.addTab(tabPane, tab);
		}
	}

	/**
	 * Finalizes tab addition by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param tab tab component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent addedTab(@Nullable UIComponent component, @Nonnull UIComponent tab) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedTab(result, tab);
		}
		return result;
	}

	/**
	 * Adds a border layout by delegating to each builder.
	 *
	 * @param border border component
	 * @param borderLayout border layout component
	 */
	@Override
	public void addBorderLayout(@Nonnull UIComponent border, @Nonnull UIComponent borderLayout) {
		for (LayoutBuilder builder : builders) {
			builder.addBorderLayout(border, borderLayout);
		}
	}

	/**
	 * Finalizes border layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param borderLayout border layout component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent addedBorderLayout(@Nullable UIComponent component, @Nonnull UIComponent borderLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedBorderLayout(result, borderLayout);
		}
		return result;
	}

	/**
	 * Adds form-row layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param formLayout form layout component
	 * @param rowLayout row layout component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent addFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent formLayout, @Nonnull UIComponent rowLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addFormRowLayout(result, formLayout, rowLayout);
		}
		return result;
	}

	/**
	 * Finalizes form-row layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param rowLayout row layout component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent addedFormRowLayout(@Nullable UIComponent component, @Nonnull UIComponent rowLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedFormRowLayout(result, rowLayout);
		}
		return result;
	}

	/**
	 * Lays out a form-item label by delegating to each builder.
	 *
	 * @param formOrRowLayout form or row layout component
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel widget label
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
		for (LayoutBuilder builder : builders) {
			builder.layoutFormItemLabel(formOrRowLayout,
											formItemComponent,
											currentForm,
											currentFormItem,
											currentFormColumn,
											widgetLabel,
											widgetEscapeLabel,
											widgetRequiredMessage,
											widgetEscapeRequiredMessage,
											widgetInvisible,
											widgetHelpText);
		}
	}

	/**
	 * Lays out a form-item widget by delegating to each builder.
	 *
	 * @param formOrRowLayout form or row layout component
	 * @param formItemComponent form-item component
	 * @param currentForm current form metadata
	 * @param currentFormItem current form-item metadata
	 * @param currentFormColumn current form-column metadata
	 * @param widgetLabel widget label
	 * @param widgetEscapeLabel resolved escape decision for widget label
	 * @param widgetColspan widget colspan
	 * @param widgetRequiredMessage optional required-message text normalised for
	 *        unescaped PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 *        API compatibility
	 * @param widgetInvisible widget invisible condition
	 * @param widgetHelpText widget help text
	 * @param widgetEscapeHelp resolved escape decision for widget help text
	 * @param widgetPixelWidth widget pixel width
	 * @param showLabel whether label is shown
	 * @param topLabel whether top-label mode is enabled
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
		for (LayoutBuilder builder : builders) {
			builder.layoutFormItemWidget(formOrRowLayout,
											formItemComponent,
											currentForm,
											currentFormItem,
											currentFormColumn,
											widgetLabel,
											widgetEscapeLabel,
											widgetColspan,
											widgetRequiredMessage,
											widgetEscapeRequiredMessage,
											widgetInvisible,
											widgetHelpText,
											widgetEscapeHelp,
											widgetPixelWidth,
											showLabel,
											topLabel);
		}
	}

	/**
	 * Builds content-signature layout by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param signature content-signature metadata
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent contentSignatureLayout(@Nullable UIComponent component, @Nonnull ContentSignature signature) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.contentSignatureLayout(result, signature);
		}
		return result;
	}
	
	/**
	 * Adds a component to a container by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param viewContainer current view container metadata
	 * @param container target container component
	 * @param componentToAdd component to add
	 * @param pixelWidth optional pixel width
	 * @param responsiveWidth optional responsive width
	 * @param percentageWidth optional percentage width
	 * @param sm optional small breakpoint width
	 * @param md optional medium breakpoint width
	 * @param lg optional large breakpoint width
	 * @param xl optional extra-large breakpoint width
	 * @param invisibleCondition invisible condition name
	 * @return resulting component
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
										@Nullable String invisibleCondition) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addToContainer(result,
												viewContainer,
												container,
												componentToAdd,
												pixelWidth,
												responsiveWidth,
												percentageWidth,
												sm,
												md,
												lg,
												xl,
												invisibleCondition);
		}
		return result;
	}

	/**
	 * Finalizes container addition by delegating through the chain in order.
	 *
	 * @param component input component
	 * @param viewContainer current view container metadata
	 * @param container container component
	 * @return resulting component
	 */
	@Override
	public @Nullable UIComponent addedToContainer(@Nullable UIComponent component, @Nonnull Container viewContainer, @Nonnull UIComponent container) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedToContainer(result, viewContainer, container);
		}
		return result;
	}
}
