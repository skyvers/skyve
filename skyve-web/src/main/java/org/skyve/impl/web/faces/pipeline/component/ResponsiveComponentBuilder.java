package org.skyve.impl.web.faces.pipeline.component;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.panel.Panel;
import org.primefaces.component.toolbar.Toolbar;

public class ResponsiveComponentBuilder extends TabularComponentBuilder {
	@Override
	public UIComponent view(UIComponent component, String invisibleConditionName) {
		if (component != null) {
			return component;
		}

		HtmlPanelGroup result = panelGroup(false, false, true, invisibleConditionName, null);
		result.setStyleClass("ui-g");
		return result;
	}

	// Overridden to not set the toolbar style width to 100% since it's wrapped in a ui-g-12
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
		
/*
		HtmlPanelGroup card = panelGroup(false, false, true, invisibleConditionName);
		card.setStyleClass("card");
		if (borderTitle != null) {
			HtmlPanelGroup title = panelGroup(false, false, false, null);
			UIOutput output = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			setId(output);
			output.setValue(borderTitle);
			title.getChildren().add(output);
			card.getChildren().add(title);
		}
		return card;
*/
		Panel result = (Panel) a.createComponent(Panel.COMPONENT_TYPE);
		if (borderTitle != null) {
			result.setHeader(borderTitle);
		}

		setInvisible(result, invisibleConditionName, null);
		setId(result, null);
		result.setStyleClass("ui-g-12");
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
