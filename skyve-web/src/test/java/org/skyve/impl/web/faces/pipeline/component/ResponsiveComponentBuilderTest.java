package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.primefaces.component.panel.Panel;
import org.primefaces.component.toolbar.Toolbar;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.web.UserAgentType;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
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
		when(mockApplication.createComponent(anyString())).thenReturn(panel);
		when(panel.getId()).thenReturn("border1");
		Panel result = (Panel) builder.border(null, "Title", null, null, null);
		assertSame(panel, result);
		verify(panel).setStyle(null);
	}
}
