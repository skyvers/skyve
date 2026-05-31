package org.skyve.impl.generate.client;

import java.util.List;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;

import jakarta.annotation.Nullable;

/**
 * Abstract renderer for container and layout structures in generated clients.
 */
public abstract class LayoutRenderer extends AbstractRenderer {
	/**
	 * Creates the root layout container for a rendered view.
	 *
	 * @param component the parent render node when the target renderer requires one
	 * @return the view layout container, or {@code null} when the renderer does not need one
	 */
	public abstract RenderedComponent viewLayout(RenderedComponent component);
	
	/**
	 * Creates per-toolbar layout shells for the current view.
	 *
	 * @param components the current render context nodes
	 * @return ordered toolbar layout nodes; may be empty when no toolbar layout is required
	 */
	public abstract List<RenderedComponent> toolbarLayouts(List<RenderedComponent> components);

	/**
	 * Binds generated toolbar layouts to their corresponding toolbar components.
	 *
	 * <p>Precondition: {@code toolbars.size() == toolbarLayouts.size()}.
	 *
	 * @param toolbars the generated toolbar components
	 * @param toolbarLayouts the generated layout wrappers for each toolbar
	 */
	public abstract void addToolbarLayouts(List<RenderedComponent> toolbars, List<RenderedComponent> toolbarLayouts);

	/**
	 * Attaches the final toolbar nodes to the view root.
	 *
	 * @param view the generated view root component
	 * @param toolbarsOrLayouts either toolbar components or bare toolbar layouts, depending on renderer strategy
	 */
	public abstract void addToolbarsOrLayouts(RenderedComponent view, List<RenderedComponent> toolbarsOrLayouts);
	
	/**
	 * Creates the container used to hold tab content for a tab pane.
	 *
	 * @param component the parent render node
	 * @return the generated tab-content layout
	 */
	public abstract RenderedComponent tabLayout(RenderedComponent component);

	/**
	 * Adds a tab-content layout beneath the provided tab component.
	 *
	 * @param component the parent render context
	 * @param tab the generated tab component
	 * @param tabLayout the generated tab-content layout
	 * @return the active render node to continue traversal with
	 */
	public abstract RenderedComponent addTabLayout(RenderedComponent component, RenderedComponent tab, RenderedComponent tabLayout);

	/**
	 * Adds a generated tab to a tab-pane container.
	 *
	 * @param tabPane the generated tab-pane component
	 * @param tab the generated tab component
	 */
	public abstract void addTab(RenderedComponent tabPane, RenderedComponent tab);

	/**
	 * Finalises tab insertion and returns the next active render node.
	 *
	 * @param component the parent render context
	 * @param tab the tab that has just been populated
	 * @return the next active render node
	 */
	public abstract RenderedComponent addedTab(RenderedComponent component, RenderedComponent tab);

	/**
	 * Inserts a layout component inside a generated border wrapper.
	 *
	 * @param border the generated border component
	 * @param borderLayout the generated layout to place inside the border
	 */
	public abstract void addBorderLayout(RenderedComponent border, RenderedComponent borderLayout);

	/**
	 * Finalises bordered layout insertion and returns the next active render node.
	 *
	 * @param component the parent render context
	 * @param borderLayout the inner bordered layout
	 * @return the next active render node
	 */
	public abstract RenderedComponent addedBorderLayout(RenderedComponent component, RenderedComponent borderLayout);

	/**
	 * Creates a vertical layout container for the supplied Skyve VBox definition.
	 */
	public abstract RenderedComponent vboxLayout(RenderedComponent component, VBox vbox);

	/**
	 * Creates a horizontal layout container for the supplied Skyve HBox definition.
	 */
	public abstract RenderedComponent hboxLayout(RenderedComponent component, HBox hbox);

	/**
	 * Creates a layout container for the supplied form.
	 */
	public abstract RenderedComponent formLayout(RenderedComponent component, Form form);

	/**
	 * Creates a layout container for a form row.
	 */
	public abstract RenderedComponent formRowLayout(RenderedComponent component, FormRow row);

	/**
	 * Adds a form-row layout into an existing form layout.
	 */
	public abstract RenderedComponent addFormRowLayout(RenderedComponent component,
														RenderedComponent formLayout,
														RenderedComponent rowLayout);

	/**
	 * Finalises form-row insertion and returns the active render node.
	 */
	public abstract RenderedComponent addedFormRowLayout(RenderedComponent component, RenderedComponent rowLayout);

	/**
	 * Places a form-item label within the current form or row layout.
	 *
	 * <p>Side effects: mutates {@code formOrRowLayout} by appending label-rendering content.
	 */
	public abstract void layoutFormItemLabel(RenderedComponent formOrRowLayout,
												RenderedComponent formItemComponent,
												Form currentForm,
												FormItem currentFormItem,
												FormColumn currentFormColumn,
												String widgetLabel,
												@Nullable String widgetRequiredMessage,
												String widgetInvisible,
												String widgetHelpText);

	/**
	 * Places the widget component for a form item within the current layout.
	 *
	 * <p>Side effects: mutates {@code formOrRowLayout} by appending widget-rendering content.
	 */
	public abstract void layoutFormItemWidget(RenderedComponent formOrRowLayout,
												RenderedComponent formItemComponent,
												Form currentForm,
												FormItem currentFormItem,
												FormColumn currentFormColumn,
												String widgetLabel,
												int formWidgetColspan,
												@Nullable String widgetRequiredMessage,
												String widgetInvisible,
												String widgetHelpText);
	
	/**
	 * Adds a rendered child component to a generated container.
	 *
	 * <p>Side effects: mutates {@code container} and may return a new active component for nested rendering.
	 *
	 * @param component the parent render context
	 * @param viewContainer the source Skyve container definition being rendered
	 * @param container the generated container component receiving the child
	 * @param componentToAdd the generated child component to insert
	 * @param pixelWidth fixed width constraint, if present
	 * @param responsiveWidth responsive width constraint, if present
	 * @param percentageWidth percentage width constraint, if present
	 * @param invisibleConditionName visibility condition expression for the child
	 * @return the active render node to continue processing with
	 */
	public abstract RenderedComponent addToContainer(RenderedComponent component,
														Container viewContainer,
														RenderedComponent container, 
														RenderedComponent componentToAdd, 
														Integer pixelWidth, 
														Integer responsiveWidth,
														Integer percentageWidth,
														String invisibleConditionName);
	
	/**
	 * Finalises insertion into a generated container.
	 *
	 * @param component the parent render context
	 * @param viewContainer the source Skyve container definition
	 * @param container the generated container that just received a child
	 * @return the active render node for subsequent rendering steps
	 */
	public abstract RenderedComponent addedToContainer(RenderedComponent component, Container viewContainer, RenderedComponent container);
}