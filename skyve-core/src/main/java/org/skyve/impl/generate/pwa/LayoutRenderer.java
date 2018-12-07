package org.skyve.impl.generate.pwa;

import java.util.List;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;

public abstract class LayoutRenderer extends AbstractRenderer {
	/**
	 * The view Layout for the view internally
	 * @return	The layout component added
	 */
	public abstract RenderedComponent viewLayout(RenderedComponent component);
	
	/**
	 * The layout within each toolbar for the view.
	 * @return	The toolbar layout(s)
	 */
	public abstract List<RenderedComponent> toolbarLayouts(List<RenderedComponent> components);

	/**
	 * The number of toolbars and the number of layouts will always be equal.
	 * @param toolbars
	 * @param toolbarLayouts
	 */
	public abstract void addToolbarLayouts(List<RenderedComponent> toolbars, List<RenderedComponent> toolbarLayouts);

	/**
	 * Add the toolbars/toolbar layouts to the view.
	 * @param view	UIComponent generated for the view (use getChildren() to add to).
	 * @param toolbarsOrLayouts	Either a list of toolbars if ComponentBuilder.toolbars() returned some toolbars or toolbar layouts if not.
	 */
	public abstract void addToolbarsOrLayouts(RenderedComponent view, List<RenderedComponent> toolbarsOrLayouts);
	
	public abstract RenderedComponent tabLayout(RenderedComponent component);
	public abstract RenderedComponent addTabLayout(RenderedComponent component, RenderedComponent tab, RenderedComponent tabLayout);
	public abstract void addTab(RenderedComponent tabPane, RenderedComponent tab);
	public abstract RenderedComponent addedTab(RenderedComponent component, RenderedComponent tab);

	public abstract void addBorderLayout(RenderedComponent border, RenderedComponent borderLayout);
	public abstract RenderedComponent addedBorderLayout(RenderedComponent component, RenderedComponent borderLayout);

	public abstract RenderedComponent vboxLayout(RenderedComponent component, VBox vbox);
	public abstract RenderedComponent hboxLayout(RenderedComponent component, HBox hbox);

	public abstract RenderedComponent formLayout(RenderedComponent component, Form form);
	public abstract RenderedComponent formRowLayout(RenderedComponent component, FormRow row);
	public abstract RenderedComponent addFormRowLayout(RenderedComponent component,
														RenderedComponent formLayout,
														RenderedComponent rowLayout);
	public abstract RenderedComponent addedFormRowLayout(RenderedComponent component, RenderedComponent rowLayout);
	public abstract void layoutFormItem(RenderedComponent formOrRowLayout,
											RenderedComponent formItemComponent,
											Form currentForm,
											FormItem currentFormItem,
											int currentFormColumn,
											String widgetLabel,
											boolean widgetRequired,
											String widgetInvisible,
											boolean widgetShowsLabelByDefault,
											String widgetHelpText);
	
	/**
	 * 
	 * @param viewContainer	Container from the skyve view that the component is being added to.
	 * @param container
	 * @param componentToAdd
	 * @param pixelWidth
	 * @param responsiveWidth
	 * @param percentageWidth
	 * @param invisibleConditionName
	 * @return	The new current component to continue processing with.
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
	 * 
	 * @param viewContainer	Container from the skyve view that the component is being added to.
	 * @param container
	 * @return	The new current component to continue processing with.
	 */
	public abstract RenderedComponent addedToContainer(RenderedComponent component, Container viewContainer, RenderedComponent container);
}