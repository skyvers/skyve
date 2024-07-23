package org.skyve.impl.web.faces.pipeline.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.primefaces.component.panel.Panel;
import org.primefaces.component.toolbar.Toolbar;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.util.UtilImpl;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGroup;

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
		result.setStyle("flex-grow:1; max-width:100%"); // to ensure it takes up all the space it can in the sidebarWrapper div
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
	
	// Add floating label support to other input widgets
	
	@Override
	public UIComponent blurb(UIComponent component, String dataWidgetVar, String value, String binding, Blurb blurb) {
		UIComponent result = super.blurb(component, dataWidgetVar, value, binding, blurb);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											boolean required) {
		EventSourceComponent result = super.checkBox(component, dataWidgetVar, checkBox, formDisabledConditionName, title, required);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												boolean required,
												HorizontalAlignment textAlignment) {
		EventSourceComponent result = super.colourPicker(component, dataWidgetVar, colour, formDisabledConditionName, title, required, textAlignment);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public UIComponent contentLink(UIComponent component,
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									boolean required,
									HorizontalAlignment textAlignment) {
		UIComponent result = super.contentLink(component, dataWidgetVar, link, formDisabledConditionName, title, required, textAlignment);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public UIComponent contentImage(UIComponent component,
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										boolean required) {
		UIComponent result = super.contentImage(component, dataWidgetVar, image, formDisabledConditionName, title, required);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public UIComponent addContentSignature(UIComponent component,
											UIComponent layout,
											ContentSignature signature,
											String formDisabledConditionName,
											String title,
											boolean required) {
		addFloatLabelClass(layout);
		return super.addContentSignature(component, layout, signature, formDisabledConditionName, title, required);
	}

	@Override
	public EventSourceComponent geometry(EventSourceComponent component,
											String dataWidgetVar,
											Geometry geometry,
											String formDisabledConditionName,
											String title,
											boolean required,
											HorizontalAlignment textAlignment) {
		EventSourceComponent result = super.geometry(component, dataWidgetVar, geometry, formDisabledConditionName, title, required, textAlignment);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public EventSourceComponent geometryMap(EventSourceComponent component,
												GeometryMap geometry,
												String formDisabledConditionName,
												String title,
												boolean required) {
		EventSourceComponent result = super.geometryMap(component, geometry, formDisabledConditionName, title, required);
		addFloatLabelClass(result.getComponent());
		
		// Grow the map form item in its flex grid, if it has no width defined
		if ((geometry.getPixelWidth() == null) && (geometry.getPercentageWidth() == null) && (geometry.getResponsiveWidth() == null)) {
			Map<String, Object> resultAttributes = result.getComponent().getAttributes();
			String style = (String) resultAttributes.get("style");
			resultAttributes.put("style", ((style == null) || style.isBlank()) ? "flex-grow:1" : style + ";flex-grow:1");
		}
		return result;
	}
	
	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								boolean required) {
		UIComponent result = super.html(component, dataWidgetVar, html, formDisabledConditionName, title, required);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public UIComponent label(UIComponent component, String dataWidgetVar, String value, String binding, Label label) {
		UIComponent result = super.label(component, dataWidgetVar, value, binding, label);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public UIComponent label(UIComponent component, String value) {
		UIComponent result =  super.label(component, value);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title,
										boolean required) {
		EventSourceComponent result = super.radio(component, dataWidgetVar, radio, formDisabledConditionName, title, required);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public EventSourceComponent richText(EventSourceComponent component,
											String dataWidgetVar,
											RichText text,
											String formDisabledConditionName,
											String title,
											boolean required) {
		EventSourceComponent result = super.richText(component, dataWidgetVar, text, formDisabledConditionName, title, required);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	private static void addFloatLabelClass(UIComponent component) {
		Map<String, Object> attributes = component.getAttributes();
		String styleClass = (String) attributes.get("styleClass");
		attributes.put("styleClass", ((styleClass == null) || styleClass.isBlank()) ? "ui-inputwrapper-focus" : styleClass + " ui-inputwrapper-focus");
	}
}
