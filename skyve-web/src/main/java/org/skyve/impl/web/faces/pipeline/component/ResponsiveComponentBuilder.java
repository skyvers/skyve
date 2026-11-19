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
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGroup;

public class ResponsiveComponentBuilder extends TabularComponentBuilder {
	// Overridden to set the grid styling
	@Override
	public @Nonnull UIComponent view(@Nullable UIComponent component, boolean createView) {
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
	public @Nonnull List<UIComponent> toolbars(@Nullable List<UIComponent> components, @Nullable String widgetId) {
		if (components != null) {
			return components;
		}

		Toolbar toolbar = (Toolbar) a.createComponent(Toolbar.COMPONENT_TYPE);
		setId(toolbar, widgetId);
		toolbar.setStyleClass(ACTION_BAR_STYLE_CLASS);
		
		List<UIComponent> result = new ArrayList<>(1);
		result.add(toolbar);
		return result;
	}

	@Override
	public @Nonnull UIComponent border(@Nullable UIComponent component, 
										@Nullable String borderTitle,
										@Nullable String invisibleCondition,
										@Nullable Integer pixelWidth,
										@Nullable Collapsible collapsible) {
		Panel result = (Panel) super.border(component, borderTitle, invisibleCondition, pixelWidth, collapsible);
		// reset percentage width styles
		result.setStyle(null);
		result.setStyleClass(UtilImpl.PRIMEFLEX ? "p-col-12" : "ui-g-12");
		return result;
	}
	
	// Add floating label support to other input widgets
	
	@Override
	public @Nonnull UIComponent blurb(@Nullable UIComponent component, @Nullable String dataWidgetVar, @Nullable String value, @Nullable String binding, @Nonnull Blurb blurb) {
		UIComponent result = super.blurb(component, dataWidgetVar, value, binding, blurb);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public @Nonnull EventSourceComponent checkBox(@Nullable EventSourceComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull CheckBox checkBox,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage) {
		EventSourceComponent result = super.checkBox(component, dataWidgetVar, checkBox, formDisabledCondition, title, requiredMessage);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public @Nonnull EventSourceComponent colourPicker(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull ColourPicker colour,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable HorizontalAlignment textAlignment) {
		EventSourceComponent result = super.colourPicker(component, dataWidgetVar, colour, formDisabledCondition, title, requiredMessage, textAlignment);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public @Nonnull UIComponent content(@Nullable UIComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull ContentUpload content,
											@Nullable String formDisabledCondition,
											@Nullable String title,
											@Nullable String requiredMessage,
											@Nullable HorizontalAlignment textAlignment,
											boolean formContext,
											boolean imageUpload) {
		UIComponent result = super.content(component, dataWidgetVar, content, formDisabledCondition, title, requiredMessage, textAlignment, formContext, imageUpload);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public @Nonnull UIComponent addContentSignature(@Nullable UIComponent component,
													@Nonnull UIComponent layout,
													@Nonnull ContentSignature signature,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage) {
		addFloatLabelClass(layout);
		return super.addContentSignature(component, layout, signature, formDisabledCondition, title, requiredMessage);
	}

	@Override
	public @Nonnull EventSourceComponent geometry(@Nullable EventSourceComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull Geometry geometry,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage,
													@Nullable HorizontalAlignment textAlignment) {
		EventSourceComponent result = super.geometry(component, dataWidgetVar, geometry, formDisabledCondition, title, requiredMessage, textAlignment);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public @Nonnull EventSourceComponent geometryMap(@Nullable EventSourceComponent component,
														@Nonnull GeometryMap geometry,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage) {
		EventSourceComponent result = super.geometryMap(component, geometry, formDisabledCondition, title, requiredMessage);
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
	public @Nonnull UIComponent html(@Nullable UIComponent component,
										@Nullable String dataWidgetVar,
										@Nonnull HTML html,
										@Nullable String formDisabledCondition,
										@Nullable String title,
										@Nullable String requiredMessage) {
		UIComponent result = super.html(component, dataWidgetVar, html, formDisabledCondition, title, requiredMessage);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public @Nonnull UIComponent label(@Nullable UIComponent component, @Nullable String dataWidgetVar, @Nullable String value, @Nullable String binding, @Nonnull Label label) {
		UIComponent result = super.label(component, dataWidgetVar, value, binding, label);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public @Nonnull UIComponent label(@Nullable UIComponent component, @Nonnull String value) {
		UIComponent result =  super.label(component, value);
		addFloatLabelClass(result);
		return result;
	}
	
	@Override
	public @Nonnull EventSourceComponent radio(@Nullable EventSourceComponent component,
												@Nullable String dataWidgetVar,
												@Nonnull Radio radio,
												@Nullable String formDisabledCondition,
												@Nullable String title,
												@Nullable String requiredMessage) {
		EventSourceComponent result = super.radio(component, dataWidgetVar, radio, formDisabledCondition, title, requiredMessage);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	@Override
	public @Nonnull EventSourceComponent richText(@Nullable EventSourceComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull RichText text,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage) {
		EventSourceComponent result = super.richText(component, dataWidgetVar, text, formDisabledCondition, title, requiredMessage);
		addFloatLabelClass(result.getComponent());
		return result;
	}
	
	private static void addFloatLabelClass(@Nonnull UIComponent component) {
		Map<String, Object> attributes = component.getAttributes();
		String styleClass = (String) attributes.get("styleClass");
		attributes.put("styleClass", ((styleClass == null) || styleClass.isBlank()) ? "ui-inputwrapper-focus" : styleClass + " ui-inputwrapper-focus");
	}
}
