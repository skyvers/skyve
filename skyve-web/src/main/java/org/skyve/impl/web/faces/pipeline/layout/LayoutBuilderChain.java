package org.skyve.impl.web.faces.pipeline.layout;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.web.UserAgentType;

public class LayoutBuilderChain extends LayoutBuilder {
	private LayoutBuilder[] builders;
	
	public LayoutBuilderChain(LayoutBuilder... builders) {
		this.builders = builders;
	}
	
	@Override
	public void setManagedBeanName(String managedBeanName) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setManagedBeanName(managedBeanName);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setManagedBeanName(managedBeanName);
		}
	}
	
	@Override
	public void setSAILManagedBean(FacesView<?> managedBean) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setSAILManagedBean(managedBean);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setSAILManagedBean(managedBean);
		}
	}

	@Override
	public void setProcess(String process) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setProcess(process);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setProcess(process);
		}
	}

	@Override
	public void setUpdate(String update) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUpdate(update);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setUpdate(update);
		}
	}

	@Override
	public void setUserAgentType(UserAgentType userAgentType) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUserAgentType(userAgentType);
		// Now set the state on all builders in the chain
		for (LayoutBuilder builder : builders) {
			builder.setUserAgentType(userAgentType);
		}
	}

	@Override
	public UIComponent viewLayout(UIComponent component) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.viewLayout(result);
		}
		return result;
	}

	@Override
	public List<UIComponent> toolbarLayouts(List<UIComponent> components) {
		List<UIComponent> result = components;
		for (LayoutBuilder builder : builders) {
			result = builder.toolbarLayouts(result);
		}
		return result;
	}

	@Override
	public UIComponent tabLayout(UIComponent component) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.tabLayout(result);
		}
		return result;
	}

	@Override
	public UIComponent vboxLayout(UIComponent component, VBox vbox) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.vboxLayout(result, vbox);
		}
		return result;
	}

	@Override
	public UIComponent hboxLayout(UIComponent component, HBox hbox) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.hboxLayout(result, hbox);
		}
		return result;
	}
	
	@Override
	public UIComponent sidebarLayout(UIComponent component, Sidebar sidebar, boolean createView) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.sidebarLayout(result, sidebar, createView);
		}
		return result;
	}

	@Override
	public UIComponent formLayout(UIComponent component, Form form) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.formLayout(result, form);
		}
		return result;
	}

	@Override
	public UIComponent formRowLayout(UIComponent component, FormRow row) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.formRowLayout(result, row);
		}
		return result;
	}

	@Override
	public void addToolbarLayouts(List<UIComponent> toolbars, List<UIComponent> toolbarLayouts) {
		for (LayoutBuilder builder : builders) {
			builder.addToolbarLayouts(toolbars, toolbarLayouts);
		}
	}

	@Override
	public void addToolbarsOrLayouts(UIComponent view, List<UIComponent> toolbarsOrLayouts) {
		for (LayoutBuilder builder : builders) {
			builder.addToolbarsOrLayouts(view, toolbarsOrLayouts);
		}
	}

	@Override
	public UIComponent addTabLayout(UIComponent component, UIComponent tab, UIComponent tabLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addTabLayout(result, tab, tabLayout);
		}
		return result;
	}

	@Override
	public void addTab(UIComponent tabPane, UIComponent tab) {
		for (LayoutBuilder builder : builders) {
			builder.addTab(tabPane, tab);
		}
	}

	@Override
	public UIComponent addedTab(UIComponent component, UIComponent tab) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedTab(result, tab);
		}
		return result;
	}

	@Override
	public void addBorderLayout(UIComponent border, UIComponent borderLayout) {
		for (LayoutBuilder builder : builders) {
			builder.addBorderLayout(border, borderLayout);
		}
	}

	@Override
	public UIComponent addedBorderLayout(UIComponent component, UIComponent borderLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedBorderLayout(result, borderLayout);
		}
		return result;
	}

	@Override
	public UIComponent addFormRowLayout(UIComponent component, UIComponent formLayout, UIComponent rowLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addFormRowLayout(result, formLayout, rowLayout);
		}
		return result;
	}

	@Override
	public UIComponent addedFormRowLayout(UIComponent component, UIComponent rowLayout) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedFormRowLayout(result, rowLayout);
		}
		return result;
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
		for (LayoutBuilder builder : builders) {
			builder.layoutFormItemLabel(formOrRowLayout,
											formItemComponent,
											currentForm,
											currentFormItem,
											currentFormColumn,
											widgetLabel,
											widgetRequired,
											widgetInvisible,
											widgetHelpText);
		}
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
										boolean showLabel,
										boolean topLabel) {
		for (LayoutBuilder builder : builders) {
			builder.layoutFormItemWidget(formOrRowLayout,
											formItemComponent,
											currentForm,
											currentFormItem,
											currentFormColumn,
											widgetLabel,
											widgetColspan,
											widgetRequired,
											widgetInvisible,
											widgetHelpText,
											showLabel,
											topLabel);
		}
	}

	@Override
	public UIComponent contentSignatureLayout(UIComponent component, ContentSignature signature) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.contentSignatureLayout(result, signature);
		}
		return result;
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
										String invisibleConditionName) {
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
												invisibleConditionName);
		}
		return result;
	}

	@Override
	public UIComponent addedToContainer(UIComponent component, Container viewContainer, UIComponent container) {
		UIComponent result = component;
		for (LayoutBuilder builder : builders) {
			result = builder.addedToContainer(result, viewContainer, container);
		}
		return result;
	}
}
