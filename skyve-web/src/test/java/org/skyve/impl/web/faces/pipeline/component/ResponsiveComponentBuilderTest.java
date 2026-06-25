package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.toolbar.Toolbar;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder.EventSourceComponent;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlOutputText;
import jakarta.faces.component.html.HtmlPanelGroup;
import jakarta.faces.context.FacesContext;

class ResponsiveComponentBuilderTest {

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	private static Application mockApplication;
	private static FacesView mockManagedBean;

	@BeforeAll
	static void setUpFacesContext() {
		FacesContext facesContext = mock(FacesContext.class);
		mockApplication = mock(Application.class);
		ExpressionFactory mockExpressionFactory = mock(ExpressionFactory.class);
		ELContext elContext = mock(ELContext.class);
		when(facesContext.getApplication()).thenReturn(mockApplication);
		when(facesContext.getELContext()).thenReturn(elContext);
		when(mockApplication.getExpressionFactory()).thenReturn(mockExpressionFactory);
		mockManagedBean = mock(FacesView.class);
		when(mockManagedBean.nextId()).thenReturn("id1");
		FacesContextBridge.setCurrent(facesContext);
	}

	@AfterAll
	static void tearDownFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	@SuppressWarnings("static-method")
	void viewReturnsExistingComponentWhenNotNull() {
		ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		UIComponent existing = mock(UIComponent.class);
		assertSame(existing, builder.view(existing, false));
	}

	@Test
	@SuppressWarnings("static-method")
	void viewCreatesResponsiveDiv() {
		ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		builder.setSAILManagedBean(mockManagedBean);
		HtmlPanelGroup panel = mock(HtmlPanelGroup.class);
		when(mockApplication.createComponent(HtmlPanelGroup.COMPONENT_TYPE)).thenReturn(panel);
		when(panel.getId()).thenReturn("view1");
		UIComponent result = builder.view(null, false);
		assertSame(panel, result);
		verify(panel).setLayout("block");
	}

	@Test
	@SuppressWarnings("static-method")
	void toolbarsReturnsExistingWhenNotNull() {
		ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		List<UIComponent> existing = List.of(mock(UIComponent.class));
		assertSame(existing, builder.toolbars(existing, "widgetId"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toolbarsCreatesToolbarWhenNull() {
		ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		Toolbar toolbar = mock(Toolbar.class);
		when(mockApplication.createComponent(Toolbar.COMPONENT_TYPE)).thenReturn(toolbar);
		List<UIComponent> result = builder.toolbars(null, "toolbar1");
		assertNotNull(result);
		assertSame(toolbar, result.get(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void borderReturnsExistingWhenNotNull() {
		ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		Panel existing = mock(Panel.class);
		assertSame(existing, builder.border(existing, "title", null, null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void borderCreatesAndResetsStyle() {
		ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
		builder.setUserAgentType(UserAgentType.desktop);
		builder.setSAILManagedBean(mockManagedBean);
		Panel panel = mock(Panel.class);
		HtmlOutputText header = mock(HtmlOutputText.class);
		Map<String, UIComponent> facets = new HashMap<>();
		when(mockApplication.createComponent(Panel.COMPONENT_TYPE)).thenReturn(panel);
		when(mockApplication.createComponent(HtmlOutputText.COMPONENT_TYPE)).thenReturn(header);
		when(panel.getFacets()).thenReturn(facets);
		when(panel.getId()).thenReturn("border1");
		Panel result = (Panel) builder.border(null, "Title", null, null, null);
		assertSame(panel, result);
		verify(panel).setStyle(null);
	}

        @Test
        @SuppressWarnings("static-method")
        void blurbShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                UIComponent existing = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.Blurb blurb = new org.skyve.impl.metadata.view.widget.Blurb();
                UIComponent result = builder.blurb(existing, "row", null, null, blurb);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void checkBoxShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                ComponentBuilder.EventSourceComponent existing = mock(ComponentBuilder.EventSourceComponent.class);
                UIComponent panel = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getComponent()).thenReturn(panel);
                when(panel.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.CheckBox checkBox =
                        new org.skyve.impl.metadata.view.widget.bound.input.CheckBox();
                ComponentBuilder.EventSourceComponent result =
                        builder.checkBox(existing, "row", checkBox, null, "Check", null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void colourPickerShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                ComponentBuilder.EventSourceComponent existing = mock(ComponentBuilder.EventSourceComponent.class);
                UIComponent panel = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getComponent()).thenReturn(panel);
                when(panel.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.ColourPicker colour =
                        new org.skyve.impl.metadata.view.widget.bound.input.ColourPicker();
                ComponentBuilder.EventSourceComponent result =
                        builder.colourPicker(existing, "row", colour, null, "Color", null, null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void radioShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                ComponentBuilder.EventSourceComponent existing = mock(ComponentBuilder.EventSourceComponent.class);
                UIComponent panel = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getComponent()).thenReturn(panel);
                when(panel.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.Radio radio =
                        new org.skyve.impl.metadata.view.widget.bound.input.Radio();
                ComponentBuilder.EventSourceComponent result =
                        builder.radio(existing, "row", radio, null, "Radio", null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void htmlShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                UIComponent existing = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.HTML html =
                        new org.skyve.impl.metadata.view.widget.bound.input.HTML();
                UIComponent result = builder.html(existing, "row", html, null, "HTML", null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void labelValueShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                UIComponent existing = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getAttributes()).thenReturn(attrs);
                UIComponent result = builder.label(existing, "Test Label");
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void labelBindingShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                UIComponent existing = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.Label label =
                        new org.skyve.impl.metadata.view.widget.bound.Label();
                UIComponent result = builder.label(existing, "row", "Value", "binding", label);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void richTextShortcutReturnsExistingAndAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                ComponentBuilder.EventSourceComponent existing = mock(ComponentBuilder.EventSourceComponent.class);
                UIComponent panel = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getComponent()).thenReturn(panel);
                when(panel.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.RichText richText =
                        new org.skyve.impl.metadata.view.widget.bound.input.RichText();
                ComponentBuilder.EventSourceComponent result =
                        builder.richText(existing, "row", richText, null, "RichText", null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void addContentSignatureShortcutAddsFloatLabelOnLayout() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                builder.setUserAgentType(UserAgentType.desktop);
                UIComponent layout = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(layout.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.ContentSignature sig =
                        new org.skyve.impl.metadata.view.widget.bound.input.ContentSignature();
                UIComponent existingComp = mock(UIComponent.class);
                builder.addContentSignature(existingComp, layout, sig, null, "Sig", null);
                assertNotNull(attrs.get("styleClass"));
        }

		@Test
		@SuppressWarnings("static-method")
		void geometryShortcutAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                EventSourceComponent existing = mock(EventSourceComponent.class);
                UIComponent existingComp = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getComponent()).thenReturn(existingComp);
                when(existingComp.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.Geometry geometry =
                        new org.skyve.impl.metadata.view.widget.bound.input.Geometry();
                EventSourceComponent result = builder.geometry(existing, "row", geometry, null, "Geo", null, null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }

        @Test
        @SuppressWarnings("static-method")
        void geometryMapShortcutAddsFloatLabel() {
                ResponsiveComponentBuilder builder = new ResponsiveComponentBuilder();
                EventSourceComponent existing = mock(EventSourceComponent.class);
                UIComponent existingComp = mock(UIComponent.class);
                java.util.Map<String, Object> attrs = new java.util.HashMap<>();
                when(existing.getComponent()).thenReturn(existingComp);
                when(existingComp.getAttributes()).thenReturn(attrs);
                org.skyve.impl.metadata.view.widget.bound.input.GeometryMap geomMap =
                        new org.skyve.impl.metadata.view.widget.bound.input.GeometryMap();
                EventSourceComponent result = builder.geometryMap(existing, geomMap, null, "GeomMap", null);
                assertSame(existing, result);
                assertNotNull(attrs.get("styleClass"));
        }
}
