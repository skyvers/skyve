package org.skyve.impl.web.faces.pipeline.layout;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.web.faces.pipeline.AbstractFacesBuilder;

public abstract class LayoutBuilder extends AbstractFacesBuilder {
	/**
	 * The view Layout for the view internally
	 * @return	The layout component added
	 */
	public abstract UIComponent viewLayout(UIComponent component);
	
	/**
	 * The layout within each toolbar for the view.
	 * @return	The toolbar layout(s)
	 */
	public abstract List<UIComponent> toolbarLayouts(List<UIComponent> components);

	/**
	 * The number of toolbars and the number of layouts will always be equal.
	 * @param toolbars
	 * @param toolbarLayouts
	 */
	public abstract void addToolbarLayouts(List<UIComponent> toolbars, List<UIComponent> toolbarLayouts);

	/**
	 * Add the toolbars/toolbar layouts to the view.
	 * @param view	UIComponent generated for the view (use getChildren() to add to).
	 * @param toolbarsOrLayouts	Either a list of toolbars if ComponentBuilder.toolbars() returned some toolbars or toolbar layouts if not.
	 */
	public abstract void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts);
	
	public abstract UIComponent tabLayout(UIComponent component);
	public abstract UIComponent addTabLayout(UIComponent component, UIComponent tab, UIComponent tabLayout);
	public abstract void addTab(UIComponent tabPane, UIComponent tab);
	public abstract UIComponent addedTab(UIComponent component, UIComponent tab);

	public abstract void addBorderLayout(UIComponent border, UIComponent borderLayout);
	public abstract UIComponent addedBorderLayout(UIComponent component, UIComponent borderLayout);

	public abstract UIComponent vboxLayout(UIComponent component, VBox vbox);
	public abstract UIComponent hboxLayout(UIComponent component, HBox hbox);

	public abstract UIComponent formLayout(UIComponent component, Form form);
	public abstract UIComponent formRowLayout(UIComponent component, FormRow row);
	public abstract UIComponent addFormRowLayout(UIComponent component,
													UIComponent formLayout,
													UIComponent rowLayout);
	public abstract UIComponent addedFormRowLayout(UIComponent component, UIComponent rowLayout);
	public abstract void layoutFormItemLabel(UIComponent formOrRowLayout,
												UIComponent formItemComponent,
												Form currentForm,
												FormItem currentFormItem,
												FormColumn currentFormColumn,
												String widgetLabel,
												boolean widgetRequired,
												String widgetInvisible,
												String widgetHelpText);
	public abstract void layoutFormItemWidget(UIComponent formOrRowLayout,
												UIComponent formItemComponent,
												Form currentForm,
												FormItem currentFormItem,
												FormColumn currentFormColumn,
												String widgetLabel,
												boolean widgetRequired,
												String widgetInvisible,
												String widgetHelpText);
	
	public abstract UIComponent contentSignatureLayout(UIComponent component, ContentSignature signature);
	
	/**
	 * 
	 * @param viewContainer	Container from the skyve view that the component is being added to.
	 * @param container
	 * @param componentToAdd
	 * @param pixelWidth
	 * @param responsiveWidth
	 * @param percentageWidth
	 * @param sm
	 * @param md
	 * @param lg
	 * @param xl
	 * @param invisibleConditionName
	 * @return	The new current component to continue processing with.
	 */
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
	 * 
	 * @param viewContainer	Container from the skyve view that the component is being added to.
	 * @param container
	 * @return	The new current component to continue processing with.
	 */
	public abstract UIComponent addedToContainer(UIComponent component, Container viewContainer, UIComponent container);
}