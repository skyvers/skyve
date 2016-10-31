package org.skyve.impl.web.faces.pipeline.component;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.panel.Panel;

public class ResponsiveComponentBuilder extends TabularComponentBuilder {
	@Override
	public HtmlPanelGroup view(String invisibleConditionName) {
		HtmlPanelGroup result = panelGroup(false, false, true, invisibleConditionName);
		result.setStyleClass("ui-g");
		return result;
	}
	
	@Override
	public UIComponent toolbar() {
		HtmlPanelGroup result = panelGroup(false, false, true, null);
		result.setStyleClass("ui-g");
		return result;
	}
	
	@Override
	public UIComponent border(String borderTitle, String invisibleConditionName, Integer pixelWidth) {
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
		setId(result);
		return result;
	}

	@Override
	protected void setSize(UIComponent component, 
							String existingStyle, 
							Integer pixelWidth, 
							Integer percentageWidth,
							Integer pixelHeight, 
							Integer percentageHeight, 
							Integer defaultPercentageWidth) {
		// ensure no default percentage width is ever set for this renderer - leave this to the responsive shit.
		super.setSize(component, existingStyle, pixelWidth, percentageWidth, pixelHeight, percentageHeight, null);
	}
}
