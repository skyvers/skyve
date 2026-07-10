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
import org.skyve.impl.web.faces.pipeline.AbstractFacesBuilder;

import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

/**
 * Defines the layout-stage contract for arranging rendered JSF components into view containers.
 */
public abstract class LayoutBuilder extends AbstractFacesBuilder {
	public static final String ACTION_BAR_WRAPPER_STYLE_CLASS = "skyve-action-bar-wrapper action-bar-wrapper";
	public static final String LAYOUT_BUILDER_CLASS_KEY = "layoutBuilderClass";

	/**
	 * Creates the outer layout container for a generated view.
	 *
	 * @param component the current layout component
	 * @return the resulting view layout component
	 */
	public abstract UIComponent viewLayout(UIComponent component);
	
	/**
	 * Creates per-toolbar layout containers.
	 *
	 * @param components the toolbar components
	 * @return the generated toolbar layout components
	 */
	public abstract List<UIComponent> toolbarLayouts(List<UIComponent> components);

	/**
	 * Associates generated toolbar layouts with their corresponding toolbars.
	 *
	 * @param toolbars the toolbar components
	 * @param toolbarLayouts the matching toolbar layouts
	 */
	public abstract void addToolbarLayouts(List<UIComponent> toolbars, List<UIComponent> toolbarLayouts);

	/**
	 * Add the toolbars/toolbar layouts to the view.
	 *
	 * @param view the generated view component (children are appended here)
	 * @param toolbarsOrLayouts either toolbars or toolbar layouts depending on builder output
	 */
	public abstract void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts);
	
	/**
	 * Creates a tab content layout container.
	 *
	 * @param component the current layout component
	 * @return the tab layout component
	 */
	public abstract UIComponent tabLayout(UIComponent component);
	
	/**
	 * Adds a tab layout into the tab component container.
	 *
	 * @param component the current layout component
	 * @param tab the tab component
	 * @param tabLayout the tab layout component
	 * @return the resulting component
	 */
	public abstract UIComponent addTabLayout(UIComponent component, UIComponent tab, UIComponent tabLayout);
	
	/**
	 * Adds a tab component to a tab pane.
	 *
	 * @param tabPane the tab-pane component
	 * @param tab the tab component to add
	 */
	public abstract void addTab(UIComponent tabPane, UIComponent tab);
	
	/**
	 * Finalises tab processing after it has been added.
	 *
	 * @param component the current layout component
	 * @param tab the tab component
	 * @return the resulting component
	 */
	public abstract UIComponent addedTab(UIComponent component, UIComponent tab);

	/**
	 * Adds a border layout component into a border container.
	 *
	 * @param border the border component
	 * @param borderLayout the border layout component
	 */
	public abstract void addBorderLayout(UIComponent border, UIComponent borderLayout);
	
	/**
	 * Finalises border layout processing after insertion.
	 *
	 * @param component the current layout component
	 * @param borderLayout the border layout component
	 * @return the resulting component
	 */
	public abstract UIComponent addedBorderLayout(UIComponent component, UIComponent borderLayout);

	/**
	 * Creates a vertical box layout container.
	 *
	 * @param component the current layout component
	 * @param vbox the VBox metadata
	 * @return the resulting VBox component
	 */
	public abstract UIComponent vboxLayout(UIComponent component, VBox vbox);
	
	/**
	 * Creates a horizontal box layout container.
	 *
	 * @param component the current layout component
	 * @param hbox the HBox metadata
	 * @return the resulting HBox component
	 */
	public abstract UIComponent hboxLayout(UIComponent component, HBox hbox);

	/**
	 * Creates a sidebar layout container.
	 *
	 * @param component the current layout component
	 * @param sidebar the sidebar metadata
	 * @param createView whether the create view is being rendered
	 * @return the resulting sidebar component
	 */
	public abstract UIComponent sidebarLayout(UIComponent component, Sidebar sidebar, boolean createView);	

	/**
	 * Creates a form layout container.
	 *
	 * @param component the current layout component
	 * @param form the form metadata
	 * @return the resulting form component
	 */
	public abstract UIComponent formLayout(UIComponent component, Form form);
	
	/**
	 * Creates a form-row layout container.
	 *
	 * @param component the current layout component
	 * @param row the form-row metadata
	 * @return the resulting row component
	 */
	public abstract UIComponent formRowLayout(UIComponent component, FormRow row);
	
	/**
	 * Adds a form-row layout to a form layout container.
	 *
	 * @param component the current layout component
	 * @param formLayout the form layout component
	 * @param rowLayout the row layout component
	 * @return the resulting component
	 */
	public abstract UIComponent addFormRowLayout(UIComponent component,
													UIComponent formLayout,
													UIComponent rowLayout);
	
													/**
	 * Finalises form-row layout processing after insertion.
	 *
	 * @param component the current layout component
	 * @param rowLayout the row layout component
	 * @return the resulting component
	 */
	public abstract UIComponent addedFormRowLayout(UIComponent component, UIComponent rowLayout);
	
	/**
	 * Lays out the label portion of a form item.
	 *
	 * @param formOrRowLayout the target form or row layout
	 * @param formItemComponent the form item component wrapper
	 * @param currentForm the current form metadata
	 * @param currentFormItem the current form item metadata
	 * @param currentFormColumn the current form column metadata
	 * @param widgetLabel the widget label text
	 * @param widgetEscapeLabel resolved escape decision for {@code widgetLabel}
	 * @param widgetRequiredMessage the optional required message normalised for
	 *        unescaped PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 *        API compatibility
	 * @param widgetInvisible the invisible condition expression
	 * @param widgetHelpText the help text
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract void layoutFormItemLabel(UIComponent formOrRowLayout,
												UIComponent formItemComponent,
												Form currentForm,
												FormItem currentFormItem,
												FormColumn currentFormColumn,
												String widgetLabel,
												boolean widgetEscapeLabel,
												@Nullable String widgetRequiredMessage,
												boolean widgetEscapeRequiredMessage,
												String widgetInvisible,
												String widgetHelpText);

	/**
	 * Lays out the widget portion of a form item.
	 *
	 * @param formOrRowLayout the target form or row layout
	 * @param formItemComponent the form item component wrapper
	 * @param currentForm the current form metadata
	 * @param currentFormItem the current form item metadata
	 * @param currentFormColumn the current form column metadata
	 * @param widgetLabel the widget label text
	 * @param widgetEscapeLabel resolved escape decision for {@code widgetLabel}
	 * @param widgetColspan the widget column span
	 * @param widgetRequiredMessage the optional required message normalised for
	 *        unescaped PrimeFaces message rendering
	 * @param widgetEscapeRequiredMessage resolved escape decision retained for layout
	 *        API compatibility
	 * @param widgetInvisible the invisible condition expression
	 * @param widgetHelpText the help text
	 * @param widgetEscapeHelp resolved escape decision for {@code widgetHelpText}
	 * @param widgetPixelWidth the optional pixel width
	 * @param showLabel whether the label is shown
	 * @param topLabel whether the label is rendered above the widget
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract void layoutFormItemWidget(UIComponent formOrRowLayout,
												UIComponent formItemComponent,
												Form currentForm,
												FormItem currentFormItem,
												FormColumn currentFormColumn,
												String widgetLabel,
												boolean widgetEscapeLabel,
												int widgetColspan,
												@Nullable String widgetRequiredMessage,
												boolean widgetEscapeRequiredMessage,
												String widgetInvisible,
												String widgetHelpText,
												boolean widgetEscapeHelp,
												Integer widgetPixelWidth,
												boolean showLabel,
												boolean topLabel);
	
	/**
	 * Creates a layout wrapper for a content-signature component.
	 *
	 * @param component the current layout component
	 * @param signature the signature metadata
	 * @return the resulting layout component
	 */
	public abstract UIComponent contentSignatureLayout(UIComponent component, ContentSignature signature);
	
	/**
	 * Adds a component to a layout container using width and visibility metadata.
	 *
	 * @param component the current layout component
	 * @param viewContainer the source view container metadata
	 * @param container the target JSF container component
	 * @param componentToAdd the component to add
	 * @param pixelWidth optional fixed pixel width
	 * @param responsiveWidth optional responsive width setting
	 * @param percentageWidth optional percentage width
	 * @param sm optional small breakpoint width
	 * @param md optional medium breakpoint width
	 * @param lg optional large breakpoint width
	 * @param xl optional extra-large breakpoint width
	 * @param invisibleConditionName optional invisible condition expression
	 * @return the resulting current component for subsequent layout operations
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract UIComponent addToContainer(UIComponent component,
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
												String invisibleConditionName);
	
	/**
	 * Finalises processing after a component has been added to a container.
	 *
	 * @param component the current layout component
	 * @param viewContainer the source view container metadata
	 * @param container the target JSF container component
	 * @return the resulting current component for subsequent layout operations
	 */
	public abstract UIComponent addedToContainer(UIComponent component, Container viewContainer, UIComponent container);
}
