package org.skyve.impl.web.faces.pipeline.layout;

import javax.faces.component.UIComponent;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.web.faces.pipeline.AbstractFacesBuilder;

public abstract class LayoutBuilder extends AbstractFacesBuilder {
	/**
	 * The view Layout for the view internally
	 * @return	The layout component added
	 */
	public abstract UIComponent viewLayout();
	
	/**
	 * The toolbar layout for the view.
	 * @return	The toolbar layout
	 */
	public abstract UIComponent toolbarLayout();
	public abstract void addToolbarLayout(UIComponent toolbar, UIComponent toolbarLayout);
	
	public abstract UIComponent tabLayout();
	public abstract UIComponent addTabLayout(UIComponent tab, UIComponent tabLayout);
	public abstract void addTab(UIComponent tabPane, UIComponent tab);
	public abstract UIComponent addedTab(UIComponent tab);

	public abstract void addBorderLayout(UIComponent border, UIComponent borderLayout);
	public abstract UIComponent addedBorderLayout(UIComponent borderLayout);

	public abstract UIComponent vboxLayout(VBox vbox);
	public abstract UIComponent hboxLayout(HBox vbox);

	public abstract UIComponent formLayout(Form form);
	public abstract UIComponent formRowLayout(FormRow row);
	public abstract UIComponent addFormRowLayout(UIComponent formLayout, UIComponent rowLayout);
	public abstract UIComponent addedFormRowLayout(UIComponent rowLayout);
	public abstract void layoutFormItem(UIComponent formOrRowLayout,
											UIComponent formItemComponent,
											Form currentForm,
											FormItem currentFormItem,
											int currentFormColumn,
											String widgetLabel,
											boolean widgetRequired,
											String widgetInvisible);
	
	/**
	 * 
	 * @param viewContainer	Container from the skyve view that the component is being added to.
	 * @param container
	 * @param componentToAdd
	 * @param pixelWidth
	 * @param responsiveWidth
	 * @param percentageWidth
	 * @return	The new current component to continue processing with.
	 */
	public abstract UIComponent addToContainer(Container viewContainer,
												UIComponent container, 
												UIComponent componentToAdd, 
												Integer pixelWidth, 
												Integer responsiveWidth,
												Integer percentageWidth);
	
	/**
	 * 
	 * @param viewContainer	Container from the skyve view that the component is being added to.
	 * @param container
	 * @return	The new current component to continue processing with.
	 */
	public abstract UIComponent addedToContainer(Container viewContainer, UIComponent container);
}