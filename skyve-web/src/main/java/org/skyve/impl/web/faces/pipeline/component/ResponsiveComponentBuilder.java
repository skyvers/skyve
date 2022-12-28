package org.skyve.impl.web.faces.pipeline.component;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.panel.Panel;
import org.primefaces.component.toolbar.Toolbar;
import org.skyve.impl.util.UtilImpl;

public class ResponsiveComponentBuilder extends TabularComponentBuilder {
	// Overridden to set the grid styling
	@Override
	public UIComponent view(UIComponent component, String invisibleConditionName) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = (HtmlPanelGroup) super.view(component, invisibleConditionName);
		result.setStyleClass(UtilImpl.PRIMEFLEX ? "p-grid" : "ui-g");
		return result;
	}

	// Overridden to not set the toolbar style width to 100% since it's wrapped in a ui-g-12 or p-col-12
	// for the responsive renderer.
	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		if (components != null) {
			return components;
		}

		Toolbar toolbar = (Toolbar) a.createComponent(Toolbar.COMPONENT_TYPE);
		setId(toolbar, widgetId);
		
		List<UIComponent> result = new ArrayList<>(1);
		result.add(toolbar);
		return result;
	}

	@Override
	public UIComponent border(UIComponent component, 
								String borderTitle,
								String invisibleConditionName,
								Integer pixelWidth) {
		if (component != null) {
			return component;
		}
		
		Panel result = (Panel) a.createComponent(Panel.COMPONENT_TYPE);
		setValueOrValueExpression(borderTitle, result::setHeader, "header", result);
		setInvisible(result, invisibleConditionName, null);
		setId(result, null);
		result.setStyleClass(UtilImpl.PRIMEFLEX ? "p-col-12" : "ui-g-12");
		return result;
	}

	@Override
	protected void setSize(UIComponent component, 
							String existingStyle, 
							Integer pixelWidth, 
							Integer responsiveWidth,
							Integer percentageWidth,
							Integer pixelHeight, 
							Integer percentageHeight, 
							Integer defaultPercentageWidth) {
		// Ensure no default percentage width is ever set for this renderer.
		// Leave this to the responsive shit.
		super.setSize(component, 
						existingStyle, 
						pixelWidth, 
						responsiveWidth, 
						percentageWidth, 
						pixelHeight, 
						percentageHeight, 
						null);
	}
}
