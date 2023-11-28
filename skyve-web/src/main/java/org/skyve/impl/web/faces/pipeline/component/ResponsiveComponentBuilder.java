package org.skyve.impl.web.faces.pipeline.component;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.panel.Panel;
import org.primefaces.component.toolbar.Toolbar;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.util.UtilImpl;

public class ResponsiveComponentBuilder extends TabularComponentBuilder {
	// Overridden to set the grid styling
	@Override
	public UIComponent view(UIComponent component, boolean createView) {
		if (component != null) {
			return component;
		}

		// A tabular span with nowrap is the result of the super call
		HtmlPanelGroup result = (HtmlPanelGroup) super.view(component, createView);
		// Change from a span with nowrap to a responsive div
		result.setLayout("block");
		result.setStyle(null);
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
								Integer pixelWidth,
								Collapsible collapsible) {
		Panel result = (Panel) super.border(component, borderTitle, invisibleConditionName, pixelWidth, collapsible);
		// reset percentage width styles
		result.setStyle(null);
		result.setStyleClass(UtilImpl.PRIMEFLEX ? "p-col-12" : "ui-g-12");
		return result;
	}

	@Override
	protected void setSizeAndTextAlignStyle(UIComponent component, 
												String styleAttributeNameOverride,
												String existingStyle, 
												Integer pixelWidth, 
												Integer responsiveWidth,
												Integer percentageWidth,
												Integer pixelHeight, 
												Integer percentageHeight, 
												Integer defaultPercentageWidth,
												HorizontalAlignment textAlign) {
		// Ensure no default percentage width is ever set for this renderer.
		// Leave this to the responsive shit.
		super.setSizeAndTextAlignStyle(component,
										styleAttributeNameOverride,
										existingStyle, 
										pixelWidth, 
										responsiveWidth, 
										percentageWidth, 
										pixelHeight, 
										percentageHeight, 
										null,
										textAlign);
	}
}
