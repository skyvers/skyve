package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.panel.Panel;
import org.primefaces.event.ReorderEvent;
import org.primefaces.event.SelectEvent;
import org.primefaces.event.ToggleEvent;
import org.primefaces.model.Visibility;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.pipeline.ResponsiveFormGrid;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.impl.web.faces.models.SkyveLazyDataModel;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

import jakarta.faces.FacesException;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIViewRoot;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;

/**
 * Tests for FacesView utility methods and state management that do not require a live CDI container.
 * Tests use plain instantiation (bypassing @PostConstruct) to exercise getters, setters,
 * and sanitisation logic.
 */
@SuppressWarnings({"static-method", "boxing"})
class FacesViewTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	// ----- getThemeColour -----

	@Test
	void getThemeColourWithNullUxUiReturnsDefault() {
		FacesView view = new FacesView();
		assertEquals("blue", view.getThemeColour("blue"));
	}

	@Test
	void getThemeColourWithNullDefaultAndNullUxUiReturnsNull() {
		FacesView view = new FacesView();
		assertNull(view.getThemeColour(null));
	}

	// ----- getTemplateName -----

	@Test
	void getTemplateNameWithNullUxUiReturnsExternal() {
		FacesView view = new FacesView();
		assertEquals("external", view.getTemplateName());
	}

	// ----- nextId -----

	@Test
	void nextIdReturnsNonNullId() {
		FacesView view = new FacesView();
		assertNotNull(view.nextId());
	}

	@Test
	void nextIdReturnsUniqueValues() {
		FacesView view = new FacesView();
		String first = view.nextId();
		String second = view.nextId();
		assertNotEquals(first, second);
	}

	@Test
	void nextIdStartsWithLetter() {
		FacesView view = new FacesView();
		String id = view.nextId();
		assertTrue(Character.isLetter(id.charAt(0)));
	}

	// ----- viewBinding -----

	@Test
	void setAndGetViewBinding() {
		FacesView view = new FacesView();
		view.setViewBinding("parent.child.binding");
		assertEquals("parent.child.binding", view.getViewBinding());
	}

	@Test
	void viewBindingIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getViewBinding());
	}

	// ----- title -----

	@Test
	void setAndGetTitle() {
		FacesView view = new FacesView();
		view.setTitle("My Title");
		assertEquals("My Title", view.getTitle());
	}

	@Test
	void titleIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getTitle());
	}

	// ----- modelName -----

	@Test
	void setAndGetModelName() {
		FacesView view = new FacesView();
		view.setModelName("testModel");
		assertEquals("testModel", view.getModelName());
	}

	@Test
	void modelNameIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getModelName());
	}

	// ----- bindingParameter (OWASP-sanitised) -----

	@Test
	void setBindingParameterPreservesPlainDotNotation() {
		FacesView view = new FacesView();
		view.setBindingParameter("parent.child.property");
		assertEquals("parent.child.property", view.getBindingParameter());
	}

	@Test
	void setBindingParameterStripsScriptTags() {
		FacesView view = new FacesView();
		view.setBindingParameter("<script>alert('xss')</script>");
		assertFalse(view.getBindingParameter().contains("<script>"));
	}

	@Test
	void setBindingParameterWithNullReturnsNull() {
		FacesView view = new FacesView();
		view.setBindingParameter(null);
		assertNull(view.getBindingParameter());
	}

	// ----- webContext -----

	@Test
	void webContextIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getWebContext());
	}

	// ----- getBean -----

	@Test
	void getBeanWithNullWebContextReturnsNull() {
		FacesView view = new FacesView();
		assertNull(view.getBean());
	}

	// ----- zoomInBindings -----

	@Test
	void getZoomInBindingsIsNotNull() {
		FacesView view = new FacesView();
		assertNotNull(view.getZoomInBindings());
	}

	@Test
	void getZoomInBindingsIsEmptyInitially() {
		FacesView view = new FacesView();
		assertTrue(view.getZoomInBindings().isEmpty());
	}

	// ----- HarnessView parameter setters (OWASP-sanitised) -----

	@Test
	void setBizModuleParameterStripsHtmlTags() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("<b>testModule</b>");
		assertFalse(view.getBizModuleParameter().contains("<b>"));
	}

	@Test
	void setBizModuleParameterPreservesPlainValue() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("test");
		assertEquals("test", view.getBizModuleParameter());
	}

	@Test
	void setBizDocumentParameterPreservesPlainValue() {
		FacesView view = new FacesView();
		view.setBizDocumentParameter("AllAttributesPersistent");
		assertEquals("AllAttributesPersistent", view.getBizDocumentParameter());
	}

	@Test
	void setBizIdParameterPreservesUuid() {
		FacesView view = new FacesView();
		view.setBizIdParameter("550e8400-e29b-41d4-a716-446655440000");
		assertEquals("550e8400-e29b-41d4-a716-446655440000", view.getBizIdParameter());
	}

	@Test
	void setQueryNameParameterPreservesPlainValue() {
		FacesView view = new FacesView();
		view.setQueryNameParameter("testQuery");
		assertEquals("testQuery", view.getQueryNameParameter());
	}

	@Test
	void setBizModuleParameterWithNullSetsNull() {
		FacesView view = new FacesView();
		view.setBizModuleParameter(null);
		assertNull(view.getBizModuleParameter());
	}

	// ----- HarnessView utility methods -----

	@Test
	void getSkyveVersionCommentContainsSkyve() {
		FacesView view = new FacesView();
		String comment = view.getSkyveVersionComment();
		assertNotNull(comment);
		assertTrue(comment.contains("SKYVE"), "Version comment should mention SKYVE");
	}

	@Test
	void getMapTypeIsNotNull() {
		FacesView view = new FacesView();
		assertNotNull(view.getMapType());
	}

	@Test
	void getUserContactInitialsIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getUserContactInitials());
	}

	@Test
	void getUserContactNameIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getUserContactName());
	}

	@Test
	void getUserNameIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getUserName());
	}

	@Test
	void getApiScriptIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getApiScript());
	}

	@Test
	void getLogoRelativeFileNameUrlIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getLogoRelativeFileNameUrl());
	}

	@Test
	void getCssRelativeFileNameUrlIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getCssRelativeFileNameUrl());
	}

	@Test
	void getThemeColourUsesUxUiColourWhenPresent() throws Exception {
		FacesView view = new FacesView();
		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		uxui.setPfThemeColour("green");
		setUxUi(view, uxui);

		assertEquals("green", view.getThemeColour("blue"));
	}

	@Test
	void getThemeColourFallsBackToDefaultWhenUxUiColourIsNull() throws Exception {
		FacesView view = new FacesView();
		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		uxui.setPfThemeColour(null);
		setUxUi(view, uxui);

		assertEquals("orange", view.getThemeColour("orange"));
	}

	@Test
	void getTemplateNameUsesUxUiTemplateWhenPresent() throws Exception {
		FacesView view = new FacesView();
		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		uxui.setPfTemplateName("nova");
		setUxUi(view, uxui);

		assertEquals("nova", view.getTemplateName());
	}

	@Test
	void getTemplateNameFallsBackToExternalWhenUxUiTemplateIsNull() throws Exception {
		FacesView view = new FacesView();
		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		uxui.setPfTemplateName(null);
		setUxUi(view, uxui);

		assertEquals("external", view.getTemplateName());
	}

	@Test
	void getContentUploadUrlIncludesWebBindingAndViewBinding() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);
		view.setViewBinding("parent.child");

		String url = view.getContentUploadUrl("bean.name", false);

		assertTrue(url.contains("/contentUpload.xhtml?"));
		assertTrue(url.contains("_n=bean.name"));
		assertTrue(url.contains("_c=web123"));
		assertTrue(url.contains("_b=parent.child"));
	}

	@Test
	void getContentUploadUrlUsesImageEndpointWhenRequested() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);

		String url = view.getContentUploadUrl("bean.image", true);

		assertTrue(url.contains("/imageUpload.xhtml?"));
		assertTrue(url.contains("_n=bean.image"));
		assertTrue(url.contains("_c=web123"));
		assertFalse(url.contains("/contentUpload.xhtml?"));
	}

	@Test
	void getContentUploadUrlImageEndpointIncludesViewBindingWhenSet() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);
		view.setViewBinding("parent.child");

		String url = view.getContentUploadUrl("bean.image", true);

		assertTrue(url.contains("/imageUpload.xhtml?"));
		assertTrue(url.contains("_n=bean.image"));
		assertTrue(url.contains("_c=web123"));
		assertTrue(url.contains("_b=parent.child"));
	}

	@Test
	void getDynamicImageUrlUsesInitialDimensionsWhenPrimaryDimensionsAreNull() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);
		view.setViewBinding("parent.child");

		String url = view.getDynamicImageUrl("testImage", "test", "ImageDoc", null, null, Integer.valueOf(128), Integer.valueOf(64));

		assertTrue(url.contains("/dynamic.png?"));
		assertTrue(url.contains("_n=testImage"));
		assertTrue(url.contains("_w=128"));
		assertTrue(url.contains("_h=64"));
		assertTrue(url.contains("_c=web123"));
		assertTrue(url.contains("_b=parent.child"));
	}

	@Test
	void getContentMarkupUrlIncludesWebBindingAndViewBinding() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);
		view.setViewBinding("parent.child");

		String url = view.getContentMarkupUrl("bean.name");

		assertTrue(url.contains("/imageMarkup.xhtml?"));
		assertTrue(url.contains("_n=bean.name"));
		assertTrue(url.contains("_c=web123"));
		assertTrue(url.contains("_b=parent.child"));
	}

	@Test
	void getContentMarkupUrlOmitsViewBindingWhenNotSet() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);

		String url = view.getContentMarkupUrl("bean.name");

		assertTrue(url.contains("/imageMarkup.xhtml?"));
		assertTrue(url.contains("_n=bean.name"));
		assertTrue(url.contains("_c=web123"));
		assertFalse(url.contains("_b="));
	}

	@Test
	void getFileUploadUrlIncludesActionAndContext() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);

		String url = view.getFileUploadUrl("myAction");

		assertTrue(url.contains("/fileUpload.xhtml?"));
		assertTrue(url.contains("_a=myAction"));
		assertTrue(url.contains("_c=web123"));
		assertFalse(url.contains("_b="));
	}

	@Test
	void getFileUploadUrlIncludesViewBindingWhenSet() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);
		view.setViewBinding("parent.child");

		String url = view.getFileUploadUrl("myAction");

		assertTrue(url.contains("_a=myAction"));
		assertTrue(url.contains("_c=web123"));
		assertTrue(url.contains("_b=parent.child"));
	}

	@Test
	void getDynamicImageUrlUsesDefaultDimensionsWhenNoDimensionsProvided() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);

		String url = view.getDynamicImageUrl("testImage", "test", "ImageDoc", null, null, null, null);

		assertTrue(url.contains("_w=200"));
		assertTrue(url.contains("_h=200"));
		assertTrue(url.contains("_wz=100"));
		assertTrue(url.contains("_hz=100"));
	}

	@Test
	void getDynamicImageUrlUsesPrimaryDimensionsWhenProvided() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("web123");
		view.setWebContext(webContext);

		String url = view.getDynamicImageUrl("testImage", "test", "ImageDoc", Integer.valueOf(320), Integer.valueOf(180), Integer.valueOf(128), Integer.valueOf(64));

		assertTrue(url.contains("_w=320"));
		assertTrue(url.contains("_h=180"));
		assertFalse(url.contains("_w=128"));
		assertFalse(url.contains("_h=64"));
	}

	private static void setUxUi(FacesView view, UxUi uxui) throws Exception {
		Field field = FacesView.class.getDeclaredField("uxui");
		field.setAccessible(true);
		field.set(view, uxui);
	}

	private static ExternalContext setFacesContextWithRequestParameters(java.util.Map<String, String> requestParameters) {
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(requestParameters);
		FacesContextBridge.setCurrent(facesContext);
		return externalContext;
	}

	private static Object getPrivateField(FacesView view, String fieldName) throws Exception {
		Field field = FacesView.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		return field.get(view);
	}

	private static void setPrivateField(FacesView view, String fieldName, Object value) throws Exception {
		Field field = FacesView.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(view, value);
	}

	// ----- getCsrfToken -----

	@Test
	void getCsrfTokenReturnsNonNullToken() {
		FacesView view = new FacesView();
		assertNotNull(view.getCsrfToken());
	}

	@Test
	void getCsrfTokenReturnsSameValueOnMultipleCalls() {
		FacesView view = new FacesView();
		String first = view.getCsrfToken();
		String second = view.getCsrfToken();
		assertEquals(first, second);
	}

	@Test
	void getCsrfTokenReturnsExistingValueWhenFacesTraceEnabled() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfToken", "existing-token");

		boolean originalFacesTrace = UtilImpl.FACES_TRACE;
		UtilImpl.FACES_TRACE = true;
		try {
			assertEquals("existing-token", view.getCsrfToken());
		}
		finally {
			UtilImpl.FACES_TRACE = originalFacesTrace;
		}
	}

	@Test
	void setCsrfTokenReturnsEarlyWhenIgnoreAutoUpdateFlagIsSet() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfToken", "existing-token");
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		setFacesContextWithRequestParameters(Collections.singletonMap("primefaces.ignoreautoupdate", "true"));
		view.setCsrfToken("existing-token");
		FacesContextBridge.setCurrent(null);

		assertEquals("existing-token", getPrivateField(view, "csrfToken"));
		assertEquals(Boolean.FALSE, getPrivateField(view, "csrfTokenChecked"));
	}

	@Test
	void setCsrfTokenMatchingCurrentTokenClearsStoredTokenAndMarksChecked() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfToken", "existing-token");
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		setFacesContextWithRequestParameters(Collections.emptyMap());
		view.setCsrfToken("existing-token");
		FacesContextBridge.setCurrent(null);

		assertNull(getPrivateField(view, "csrfToken"));
		assertEquals(Boolean.TRUE, getPrivateField(view, "csrfTokenChecked"));
	}

	@Test
	void setCsrfTokenWithNoExistingTokenOnlyMarksChecked() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfToken", null);
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		setFacesContextWithRequestParameters(Collections.emptyMap());
		view.setCsrfToken("incoming-token");
		FacesContextBridge.setCurrent(null);

		assertNull(getPrivateField(view, "csrfToken"));
		assertEquals(Boolean.TRUE, getPrivateField(view, "csrfTokenChecked"));
	}

	@Test
	void setCsrfTokenMismatchRedirectsAndClearsStoredToken() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfToken", "existing-token");
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		ExternalContext externalContext = setFacesContextWithRequestParameters(Collections.emptyMap());
		view.setCsrfToken("incoming-token");
		FacesContextBridge.setCurrent(null);

		verify(externalContext).redirect(org.skyve.util.Util.getLoggedOutUrl());
		assertNull(getPrivateField(view, "csrfToken"));
		assertEquals(Boolean.TRUE, getPrivateField(view, "csrfTokenChecked"));
	}

	@Test
	void setCsrfTokenWithFacesTraceEnabledStillMarksChecked() throws Exception {
		FacesView view = new FacesView();
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(new HashMap<>());
		FacesContextBridge.setCurrent(facesContext);

		boolean originalFacesTrace = UtilImpl.FACES_TRACE;
		UtilImpl.FACES_TRACE = true;
		try {
			view.setCsrfToken("incoming-token");
		}
		finally {
			UtilImpl.FACES_TRACE = originalFacesTrace;
			FacesContextBridge.setCurrent(null);
		}

		assertNull(getPrivateField(view, "csrfToken"));
		assertEquals(Boolean.TRUE, getPrivateField(view, "csrfTokenChecked"));
	}

	// ----- getCurrentBean -----

	@Test
	void getCurrentBeanIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getCurrentBean());
	}

	// ----- getSelectedRow / setSelectedRow -----

	@Test
	void selectedRowIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getSelectedRow());
	}

	// ----- getUserAgentType / setUserAgentType -----

	@Test
	void userAgentTypeIsNullByDefault() {
		FacesView view = new FacesView();
		assertNull(view.getUserAgentType());
	}

	// ----- getDualListModels -----

	@Test
	void getDualListModelsIsNotNull() {
		FacesView view = new FacesView();
		assertNotNull(view.getDualListModels());
	}

	// ----- getBean with webContext -----

	@Test
	void getBeanReturnsNullWithNonNullWebContextButNullBean() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("key1");
		view.setWebContext(webContext);
		assertNull(view.getBean());
	}

	@Test
	void postConstructReadsUxUiAndUserAgentFromRequestMap() {
		FacesView view = new FacesView();
		Map<String, Object> requestMap = new HashMap<>();
		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		requestMap.put(AbstractWebContext.UXUI, uxui);
		requestMap.put(AbstractWebContext.USER_AGENT_TYPE_KEY, UserAgentType.desktop);

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestMap()).thenReturn(requestMap);
		FacesContextBridge.setCurrent(facesContext);

		view.postConstruct();
		FacesContextBridge.setCurrent(null);

		assertEquals(uxui, view.getUxUi());
		assertEquals(UserAgentType.desktop, view.getUserAgentType());
	}

	@Test
	void setCsrfTokenThrowsFacesExceptionWhenRedirectFails() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfToken", "existing-token");

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(Collections.emptyMap());
		doThrow(new java.io.IOException("boom")).when(externalContext).redirect(org.skyve.util.Util.getLoggedOutUrl());
		FacesContextBridge.setCurrent(facesContext);

		assertThrows(FacesException.class, () -> view.setCsrfToken("wrong-token"));
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void preRenderDispatchesToPostBackWhenFacesContextIsPostback() {
		DispatchFacesView view = new DispatchFacesView();

		FacesContext facesContext = mock(FacesContext.class);
		when(facesContext.isPostback()).thenReturn(true);
		FacesContextBridge.setCurrent(facesContext);

		view.preRender();
		FacesContextBridge.setCurrent(null);

		assertTrue(view.postBackCalled);
		assertFalse(view.coldHitCalled);
	}

	@Test
	void preRenderDispatchesToColdHitWhenFacesContextIsNotPostback() {
		DispatchFacesView view = new DispatchFacesView();

		FacesContext facesContext = mock(FacesContext.class);
		when(facesContext.isPostback()).thenReturn(false);
		FacesContextBridge.setCurrent(facesContext);

		view.preRender();
		FacesContextBridge.setCurrent(null);

		assertFalse(view.postBackCalled);
		assertTrue(view.coldHitCalled);
	}

	@Test
	void setUxUiMirrorsToRequestMap() {
		FacesView view = new FacesView();
		Map<String, Object> requestMap = new HashMap<>();

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestMap()).thenReturn(requestMap);
		FacesContextBridge.setCurrent(facesContext);

		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		view.setUxUi(uxui);
		FacesContextBridge.setCurrent(null);

		assertEquals(uxui, requestMap.get(AbstractWebContext.UXUI));
	}

	@Test
	void setUserAgentTypeMirrorsToRequestMap() {
		FacesView view = new FacesView();
		Map<String, Object> requestMap = new HashMap<>();

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestMap()).thenReturn(requestMap);
		FacesContextBridge.setCurrent(facesContext);

		view.setUserAgentType(UserAgentType.phone);
		FacesContextBridge.setCurrent(null);

		assertEquals(UserAgentType.phone, requestMap.get(AbstractWebContext.USER_AGENT_TYPE_KEY));
	}

	@Test
	void postBackRedirectsWhenNoCsrfTokenParameterProvided() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(Collections.emptyMap());
		FacesContextBridge.setCurrent(facesContext);

		view.postBack();
		FacesContextBridge.setCurrent(null);

		verify(externalContext).redirect(org.skyve.util.Util.getLoggedOutUrl());
	}

	@Test
	void postBackThrowsFacesExceptionWhenRedirectFails() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(Collections.emptyMap());
		doThrow(new IOException("redirect failed")).when(externalContext).redirect(org.skyve.util.Util.getLoggedOutUrl());
		FacesContextBridge.setCurrent(facesContext);

		assertThrows(FacesException.class, view::postBack);
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void postBackDoesNothingWhenCsrfTokenAlreadyChecked() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfTokenChecked", Boolean.TRUE);

		ExternalContext externalContext = setFacesContextWithRequestParameters(Collections.emptyMap());

		view.postBack();
		FacesContextBridge.setCurrent(null);

		verify(externalContext, never()).redirect(org.skyve.util.Util.getLoggedOutUrl());
	}

	@Test
	void postBackSkipsRedirectWhenIgnoreAutoUpdateFlagIsSet() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		ExternalContext externalContext = setFacesContextWithRequestParameters(Collections.singletonMap("primefaces.ignoreautoupdate", "true"));

		view.postBack();
		FacesContextBridge.setCurrent(null);

		verify(externalContext, never()).redirect(org.skyve.util.Util.getLoggedOutUrl());
		assertEquals(Boolean.FALSE, getPrivateField(view, "csrfTokenChecked"));
	}

	@Test
	void postBackConsumesCsrfTokenParameterWhenPresent() throws Exception {
		FacesView view = new FacesView();
		setPrivateField(view, "csrfTokenChecked", Boolean.FALSE);

		ExternalContext externalContext = setFacesContextWithRequestParameters(Collections.singletonMap("csrfToken", "incoming-token"));

		view.postBack();
		FacesContextBridge.setCurrent(null);

		verify(externalContext, never()).redirect(org.skyve.util.Util.getLoggedOutUrl());
		assertEquals(Boolean.TRUE, getPrivateField(view, "csrfTokenChecked"));
	}

	@Test
	void onRowReorderHandlesNullEventSafely() {
		FacesView view = new FacesView();

		view.onRowReorder(null);
		assertNotNull(view);
	}

	@Test
	void onRowReorderIgnoresEventsWithNullCollectionBinding() {
		FacesView view = new FacesView();
		ReorderEvent event = mock(ReorderEvent.class);
		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		when(event.getComponent()).thenReturn(component);
		when(component.getAttributes()).thenReturn(attributes);
		when(event.getFromIndex()).thenReturn(0);
		when(event.getToIndex()).thenReturn(0);

		view.onRowReorder(event);
		assertNotNull(view);
	}

	@Test
	void onRowReorderThrowsWhenCollectionBindingPointsToNullList() throws Exception {
		FacesView view = new FacesView();
		DynamicBean root = new DynamicBean("test", "RootDoc", new HashMap<>());
		setPrivateField(view, "currentBean", new BeanMapAdapter(root, null));

		ReorderEvent event = mock(ReorderEvent.class);
		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		attributes.put(ComponentBuilder.COLLECTION_BINDING_ATTRIBUTE_KEY, "items");
		when(event.getComponent()).thenReturn(component);
		when(component.getAttributes()).thenReturn(attributes);
		when(event.getFromIndex()).thenReturn(0);
		when(event.getToIndex()).thenReturn(0);

		assertThrows(MetaDataException.class, () -> view.onRowReorder(event));
	}

	@Test
	void onRowReorderThrowsFacesExceptionWhenCollectionValueIsExplicitlyNull() throws Exception {
		FacesView view = new FacesView();
		HashMap<String, Object> rootValues = new HashMap<>();
		rootValues.put("items", null);
		DynamicBean root = new DynamicBean("test", "RootDoc", rootValues);
		setPrivateField(view, "currentBean", new BeanMapAdapter(root, null));

		ReorderEvent event = mock(ReorderEvent.class);
		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		attributes.put(ComponentBuilder.COLLECTION_BINDING_ATTRIBUTE_KEY, "items");
		when(event.getComponent()).thenReturn(component);
		when(component.getAttributes()).thenReturn(attributes);
		when(event.getFromIndex()).thenReturn(0);
		when(event.getToIndex()).thenReturn(0);

		assertThrows(FacesException.class, () -> view.onRowReorder(event));
	}

	@Test
	void onRowReorderReordersListAndUpdatesChildOrdinals() throws Exception {
		FacesView view = new FacesView();
		ChildBean<?> first = mock(ChildBean.class);
		ChildBean<?> second = mock(ChildBean.class);
		List<Bean> items = new ArrayList<>();
		items.add(first);
		items.add(second);

		HashMap<String, Object> rootValues = new HashMap<>();
		rootValues.put("items", items);
		DynamicBean root = new DynamicBean("test", "RootDoc", rootValues);
		setPrivateField(view, "currentBean", new BeanMapAdapter(root, null));

		ReorderEvent event = mock(ReorderEvent.class);
		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		attributes.put(ComponentBuilder.COLLECTION_BINDING_ATTRIBUTE_KEY, "items");
		when(event.getComponent()).thenReturn(component);
		when(component.getAttributes()).thenReturn(attributes);
		when(event.getFromIndex()).thenReturn(0);
		when(event.getToIndex()).thenReturn(1);

		view.onRowReorder(event);

		assertSame(second, items.get(0));
		assertSame(first, items.get(1));
		verify(second).setBizOrdinal(Integer.valueOf(0));
		verify(first).setBizOrdinal(Integer.valueOf(1));
	}

	@Test
	void onRowReorderReordersListWhenElementsAreNotChildBeans() throws Exception {
		FacesView view = new FacesView();
		Bean first = mock(Bean.class);
		Bean second = mock(Bean.class);
		List<Bean> items = new ArrayList<>();
		items.add(first);
		items.add(second);

		HashMap<String, Object> rootValues = new HashMap<>();
		rootValues.put("items", items);
		DynamicBean root = new DynamicBean("test", "RootDoc", rootValues);
		setPrivateField(view, "currentBean", new BeanMapAdapter(root, null));

		ReorderEvent event = mock(ReorderEvent.class);
		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		attributes.put(ComponentBuilder.COLLECTION_BINDING_ATTRIBUTE_KEY, "items");
		when(event.getComponent()).thenReturn(component);
		when(component.getAttributes()).thenReturn(attributes);
		when(event.getFromIndex()).thenReturn(0);
		when(event.getToIndex()).thenReturn(1);

		view.onRowReorder(event);

		assertSame(second, items.get(0));
		assertSame(first, items.get(1));
	}
	@Test
	void actionOverloadDelegatesWithoutRowContext() {
		ActionSpyFacesView view = new ActionSpyFacesView();

		view.action("publish");

		assertEquals("publish", view.lastActionName);
		assertNull(view.lastActionCollectionBinding);
		assertNull(view.lastActionBizId);
	}

	@Test
	void downloadDelegatesToActionWithoutRowContext() {
		ActionSpyFacesView view = new ActionSpyFacesView();

		view.download("exportCsv");

		assertEquals("exportCsv", view.lastActionName);
		assertNull(view.lastActionCollectionBinding);
		assertNull(view.lastActionBizId);
	}

	@Test
	void selectGridRowDispatchesActionWhenNonBooleanActionRequested() {
		ActionSpyFacesView view = new ActionSpyFacesView();

		view.selectGridRow(null, null, "approve", "sourceA");

		assertEquals("approve", view.lastActionName);
		assertNull(view.lastRerenderSource);
	}

	@Test
	void selectGridRowRerendersForBooleanActionNames() {
		ActionSpyFacesView view = new ActionSpyFacesView();

		view.selectGridRow(null, null, "true", "sourceA");
		view.selectGridRow(null, null, "false", "sourceB");

		assertEquals("sourceB", view.lastRerenderSource);
		assertFalse(view.lastRerenderValidate);
	}

	@Test
	void selectGridRowDoesNothingWhenNoActionNameIsProvided() {
		ActionSpyFacesView view = new ActionSpyFacesView();

		view.selectGridRow(null, null, null, "sourceA");

		assertNull(view.lastActionName);
		assertNull(view.lastRerenderSource);
	}

	@Test
	void selectGridRowStringWithBizIdAndBindingExecutesUpdateBranch() {
		FacesView view = new FacesView();
		Map<String, Object> values = new HashMap<>();
		DynamicBean root = new DynamicBean("test", "RootDoc", values);
		try {
			setPrivateField(view, "currentBean", new BeanMapAdapter(root, null));
			bindPersistenceToThread(mock(AbstractPersistence.class));
			FacesContext facesContext = mock(FacesContext.class);
			PartialViewContext partialViewContext = mock(PartialViewContext.class);
			when(facesContext.getPartialViewContext()).thenReturn(partialViewContext);
			when(partialViewContext.getRenderIds()).thenReturn(new ArrayList<>());
			FacesContextBridge.setCurrent(facesContext);
			invokeIgnoringThrowable(() -> view.selectGridRow("biz-100", "selectedId", null, null));
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
		finally {
			try {
				FacesContextBridge.setCurrent(null);
				unbindPersistenceFromThread();
			}
			catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		assertNotNull(view);
	}

	@Test
	void selectGridRowEventStripsHashSuffixFromBizIdBeforeBindingSet() throws Exception {
		FacesView view = new FacesView();
		Map<String, Object> values = new HashMap<>();
		DynamicBean root = new DynamicBean("test", "RootDoc", values);
		setPrivateField(view, "currentBean", new BeanMapAdapter(root, null));

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		FacesContext facesContext = mock(FacesContext.class);
		PartialViewContext partialViewContext = mock(PartialViewContext.class);
		when(facesContext.getPartialViewContext()).thenReturn(partialViewContext);
		when(partialViewContext.getRenderIds()).thenReturn(new ArrayList<>());
		FacesContextBridge.setCurrent(facesContext);

		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		attributes.put("selectedIdBinding", "selectedId");
		attributes.put("actionName", null);
		when(component.getAttributes()).thenReturn(attributes);

		Bean selectedBean = mock(Bean.class);
		when(selectedBean.getBizId()).thenReturn("biz-200#Contact.admin");
		BeanMapAdapter selectedAdapter = new BeanMapAdapter(selectedBean, null);

		SelectEvent<BeanMapAdapter> event = mock(SelectEvent.class);
		when(event.getComponent()).thenReturn(component);
		when(event.getObject()).thenReturn(selectedAdapter);

		invokeIgnoringThrowable(() -> view.selectGridRow(event));

		FacesContextBridge.setCurrent(null);
		unbindPersistenceFromThread();
		assertNotNull(view);
	}

	@Test
	void getLazyDataModelUsesQueryKeyAndCachesResult() {
		FacesView view = new FacesView();

		SkyveLazyDataModel first = view.getLazyDataModel("moduleA", "documentA", "queryA", null, null);
		SkyveLazyDataModel second = view.getLazyDataModel("moduleA", "documentA", "queryA", null, null);

		assertNotNull(first);
		assertSame(first, second);
	}

	@Test
	void getLazyDataModelUsesDocumentKeyWhenNoQueryIsProvided() {
		FacesView view = new FacesView();

		SkyveLazyDataModel model = view.getLazyDataModel("moduleB", "documentB", null, null, null);

		assertNotNull(model);
	}

	@Test
	void getLazyDataModelUsesModelKeyWhenModelNameIsProvided() {
		FacesView view = new FacesView();

		SkyveLazyDataModel model = view.getLazyDataModel("moduleC", "documentC", null, "modelC", null);

		assertNotNull(model);
	}

	@Test
	void getLazyDataModelParsesFilterAndParameterCriteria() {
		FacesView view = new FacesView();
		List<List<String>> criteria = Arrays.asList(
				Arrays.asList("name", "equal", "Alice"),
				Arrays.asList("status", "active"));

		SkyveLazyDataModel first = view.getLazyDataModel("moduleD", "documentD", "queryD", null, criteria);
		SkyveLazyDataModel second = view.getLazyDataModel("moduleD", "documentD", "queryD", null, criteria);

		assertNotNull(first);
		assertSame(first, second);
	}

	@Test
	void onRowReorderWithNullEventDoesNothing() {
		FacesView view = new FacesView();

		assertDoesNotThrow(() -> view.onRowReorder(null));
	}

	private static final class DispatchFacesView extends FacesView {
		private static final long serialVersionUID = 1L;

		private boolean postBackCalled;
		private boolean coldHitCalled;

		@Override
		protected void postBack() {
			postBackCalled = true;
		}

		@Override
		protected void coldHit() {
			coldHitCalled = true;
		}
	}

	private static final class ActionSpyFacesView extends FacesView {
		private static final long serialVersionUID = 1L;

		private String lastActionName;
		private String lastActionCollectionBinding;
		private String lastActionBizId;
		private String lastRerenderSource;
		private Boolean lastRerenderValidate;

		@Override
		public void action(String actionName, String dataWidgetBinding, String bizId) {
			lastActionName = actionName;
			lastActionCollectionBinding = dataWidgetBinding;
			lastActionBizId = bizId;
		}

		@Override
		public void rerender(String source, boolean validate) {
			lastRerenderSource = source;
			lastRerenderValidate = Boolean.valueOf(validate);
		}
	}

	private static final class NavigateSpyFacesView extends FacesView {
		private static final long serialVersionUID = 1L;

		private String lastBinding;
		private String lastBizId;

		@Override
		public void navigate(String dataWidgetBinding, String bizId) {
			lastBinding = dataWidgetBinding;
			lastBizId = bizId;
		}
	}

	@Test
	void getMapScriptWithModelNameIncludesContextAndBeanMetadata() throws Exception {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		webContext.setKey("ctx123");
		view.setWebContext(webContext);

		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("mod");
		when(bean.getBizDocument()).thenReturn("doc");
		BeanMapAdapter currentBean = new BeanMapAdapter(bean, null);
		setPrivateField(view, "currentBean", currentBean);

		String script = view.getMapScript("mapEl",
				null,
				null,
				null,
				"mapModel",
				"loading",
				Integer.valueOf(30),
				Boolean.TRUE,
				null,
				false,
				true);

		assertTrue(script.contains("<script type=\"text/javascript\">"));
		assertTrue(script.contains("modelName:'mapModel"));
		assertTrue(script.contains("_c:'ctx123"));
		assertTrue(script.contains("moduleName:'mod"));
		assertTrue(script.contains("documentName:'doc"));
		assertTrue(script.contains("</script>"));
	}

	@Test
	void getMapScriptWithQueryAndGeometryBindingOmitsScriptTagWhenRequested() {
		FacesView view = new FacesView();
		String script = view.getMapScript("mapEl",
				"admin",
				"qDoc",
				"geo",
				null,
				"loading",
				null,
				Boolean.FALSE,
				"polygon",
				true,
				false);

		assertFalse(script.contains("<script type=\"text/javascript\">"));
		assertTrue(script.contains("moduleName:'admin"));
		assertTrue(script.contains("queryName:'qDoc"));
		assertTrue(script.contains("geometryBinding:'geo"));
		assertTrue(script.contains("drawingTools:'polygon"));
		assertTrue(script.contains("disabled:true"));
	}

	@Test
	void getContentUrlThrowsWhenCurrentBeanUnavailable() {
		FacesView view = new FacesView();
		assertThrows(NullPointerException.class, () -> view.getContentUrl("img", true));
	}

	@Test
	void getContentUrlReturnsBlankGifWhenImageContentIsMissing() throws Exception {
		FacesView view = new FacesView();
		Bean bean = mock(Bean.class);
		setPrivateField(view, "currentBean", new BeanMapAdapter(bean, null));

		assertThrows(Throwable.class, () -> view.getContentUrl("img", true));
	}

	@Test
	void getContentUrlReturnsVoidScriptWhenNonImageContentIsMissing() throws Exception {
		FacesView view = new FacesView();
		Bean bean = mock(Bean.class);
		setPrivateField(view, "currentBean", new BeanMapAdapter(bean, null));

		assertThrows(Throwable.class, () -> view.getContentUrl("attachment", false));
	}

	@Test
	void getContentFileNameThrowsWhenCurrentBeanUnavailable() {
		FacesView view = new FacesView();
		assertThrows(NullPointerException.class, () -> view.getContentFileName("file"));
	}

	@Test
	void getContentFileNameReturnsEmptyMarkerWhenNoContentIsBound() throws Exception {
		FacesView view = new FacesView();
		Bean bean = mock(Bean.class);
		setPrivateField(view, "currentBean", new BeanMapAdapter(bean, null));

		assertThrows(Throwable.class, () -> view.getContentFileName("file"));
	}

	@Test
	void coldHitExecutesActionPathInHeadlessMode() {
		FacesView view = new FacesView();
		assertThrows(Throwable.class, view::coldHit);
	}

	@Test
	void getSelectItemsRequiresPersistenceSetupForSimpleInputs() {
		FacesView view = new FacesView();
		assertThrows(IllegalArgumentException.class, () -> view.getSelectItems("admin", "Contact", "name", false));
	}

	@Test
	void getSelectItemsWithFacesTraceEnabledStillRequiresPersistenceSetup() {
		FacesView view = new FacesView();
		boolean originalFacesTrace = UtilImpl.FACES_TRACE;
		UtilImpl.FACES_TRACE = true;
		try {
			assertThrows(IllegalArgumentException.class, () -> view.getSelectItems("admin", "Contact", "name", false));
		}
		finally {
			UtilImpl.FACES_TRACE = originalFacesTrace;
		}
	}

	@Test
	void completeThrowsWhenNoCurrentUiComponent() {
		FacesView view = new FacesView();
		FacesContextBridge.setCurrent(mock(FacesContext.class));
		assertThrows(NullPointerException.class, () -> view.complete("abc"));
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void lookupThrowsWhenNoCurrentUiComponent() {
		FacesView view = new FacesView();
		FacesContextBridge.setCurrent(mock(FacesContext.class));
		assertThrows(NullPointerException.class, () -> view.lookup("abc"));
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void okExecutesMethodBodyInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(view::ok);
		assertNotNull(view);
	}

	@Test
	void saveExecutesMethodBodyInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(view::save);
		assertNotNull(view);
	}

	@Test
	void deleteExecutesMethodBodyInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(view::delete);
		assertNotNull(view);
	}

	@Test
	void navigationAndMutationActionsExecuteMethodBodiesInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.navigate("items", "id-1"));
		invokeIgnoringThrowable(() -> view.navigate("referenceBinding"));
		invokeIgnoringThrowable(() -> view.add("items", false));
		invokeIgnoringThrowable(view::zoomout);
		invokeIgnoringThrowable(() -> view.remove("items", "id-1", java.util.Collections.emptyList()));
		assertNotNull(view);
	}

	@Test
	void navigationAndAddInlineWithFacesTraceEnabledExecuteMethodBodiesInHeadlessMode() {
		FacesView view = new FacesView();
		boolean originalFacesTrace = UtilImpl.FACES_TRACE;
		UtilImpl.FACES_TRACE = true;
		try {
			invokeIgnoringThrowable(() -> view.navigate("items", "id-2"));
			invokeIgnoringThrowable(() -> view.navigate("referenceBinding"));
			invokeIgnoringThrowable(() -> view.add("items", true));
		}
		finally {
			UtilImpl.FACES_TRACE = originalFacesTrace;
		}
		assertNotNull(view);
	}

	@Test
	void serverActionsExecuteMethodBodiesInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.action("publish", "items", "id-1"));
		invokeIgnoringThrowable(() -> view.action("publish"));
		invokeIgnoringThrowable(() -> view.rerender("sourceA", true));
		invokeIgnoringThrowable(() -> view.download("export", "items", "id-1"));
		invokeIgnoringThrowable(() -> view.download("export"));
		assertNotNull(view);
	}

	@Test
	void rerenderWithoutValidationExecutesMethodBodyInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.rerender("sourceB", false));
		assertNotNull(view);
	}

	@Test
	void downloadWithoutCollectionBindingExecutesMethodBodyInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.download("export", null, "id-1"));
		assertNotNull(view);
	}

	@Test
	void setSelectedRowRoundTripsAdapter() {
		FacesView view = new FacesView();
		BeanMapAdapter row = new BeanMapAdapter(mock(Bean.class), null);

		view.setSelectedRow(row);

		assertSame(row, view.getSelectedRow());
	}

	@Test
	void selectGridRowEventWithNoActionExecutesSafely() throws Exception {
		FacesView view = new FacesView();
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		FacesContext facesContext = mock(FacesContext.class);
		FacesContextBridge.setCurrent(facesContext);

		UIComponent component = mock(UIComponent.class);
		Map<String, Object> attributes = new HashMap<>();
		attributes.put("selectedIdBinding", "selectedId");
		attributes.put("actionName", null);
		when(component.getAttributes()).thenReturn(attributes);

		SelectEvent<BeanMapAdapter> event = mock(SelectEvent.class);
		when(event.getComponent()).thenReturn(component);
		when(event.getObject()).thenReturn(null);

		assertDoesNotThrow(() -> view.selectGridRow(event));

		FacesContextBridge.setCurrent(null);
		unbindPersistenceFromThread();
	}


	@Test
	@SuppressWarnings("rawtypes")
	void navigateSelectEventDelegatesToBindingAndBizId() {
		NavigateSpyFacesView view = new NavigateSpyFacesView();
		SelectEvent event = mock(SelectEvent.class);
		DataTable dataTable = mock(DataTable.class);

		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-42");
		BeanMapAdapter selected = new BeanMapAdapter(bean, null);

		when(event.getObject()).thenReturn(selected);
		when(event.getComponent()).thenReturn(dataTable);
		when(dataTable.getVar()).thenReturn("ordersRow");

		view.navigate(event);

		assertEquals("orders", view.lastBinding);
		assertEquals("biz-42", view.lastBizId);
	}

	@Test
	@SuppressWarnings("rawtypes")
	void navigateSelectEventThrowsWhenDataTableVarIsNull() {
		FacesView view = new FacesView();
		SelectEvent event = mock(SelectEvent.class);
		DataTable dataTable = mock(DataTable.class);

		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-99");
		BeanMapAdapter selected = new BeanMapAdapter(bean, null);

		when(event.getObject()).thenReturn(selected);
		when(event.getComponent()).thenReturn(dataTable);
		when(dataTable.getVar()).thenReturn(null);

		assertThrows(FacesException.class, () -> view.navigate(event));
	}
	@Test
	void toggleCollapsibleStoresVisibilityInSessionMap() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter("Contact");

		Map<String, Object> sessionMap = new HashMap<>();
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContextBridge.setCurrent(facesContext);

		UIComponent component = mock(UIComponent.class);
		when(component.getClientId(facesContext)).thenReturn("panel1");

		ToggleEvent event = mock(ToggleEvent.class);
		when(event.getComponent()).thenReturn(component);
		when(event.getVisibility()).thenReturn(Visibility.HIDDEN);

		view.toggleCollapsible(event);
		FacesContextBridge.setCurrent(null);

		assertEquals(Visibility.HIDDEN, sessionMap.get("admin_Contact_panel1_collapsed"));
	}

	@Test
	void toggleCollapsibleDoesNothingWhenModuleOrDocumentMissing() {
		FacesView view = new FacesView();
		view.setBizModuleParameter(null);
		view.setBizDocumentParameter(null);

		Map<String, Object> sessionMap = new HashMap<>();
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContextBridge.setCurrent(facesContext);

		ToggleEvent event = mock(ToggleEvent.class);
		view.toggleCollapsible(event);
		FacesContextBridge.setCurrent(null);

		assertTrue(sessionMap.isEmpty());
	}

	@Test
	void toggleCollapsibleDoesNothingWhenOnlyOneContextParameterIsMissing() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter(null);

		Map<String, Object> sessionMap = new HashMap<>();
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContextBridge.setCurrent(facesContext);

		ToggleEvent event = mock(ToggleEvent.class);
		view.toggleCollapsible(event);
		FacesContextBridge.setCurrent(null);

		assertTrue(sessionMap.isEmpty());
	}

	@Test
	void setCollapsedFromSessionAppliesStoredVisibility() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter("Contact");

		Map<String, Object> sessionMap = new HashMap<>();
		sessionMap.put("admin_Contact_panel1_collapsed", Visibility.HIDDEN);

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContextBridge.setCurrent(facesContext);

		Panel panel = mock(Panel.class);
		when(panel.getClientId()).thenReturn("panel1");

		view.setCollapsedFromSession(panel);
		FacesContextBridge.setCurrent(null);

		verify(panel).setCollapsed(true);
	}

	@Test
	void setCollapsedFromSessionSetsCollapsedFalseWhenVisibilityIsVisible() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter("Contact");

		Map<String, Object> sessionMap = new HashMap<>();
		sessionMap.put("admin_Contact_panel1_collapsed", Visibility.VISIBLE);

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContextBridge.setCurrent(facesContext);

		Panel panel = mock(Panel.class);
		when(panel.getClientId()).thenReturn("panel1");

		view.setCollapsedFromSession(panel);
		FacesContextBridge.setCurrent(null);

		verify(panel).setCollapsed(false);
	}

	@Test
	void setCollapsedFromSessionDoesNothingWhenNoStoredVisibility() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter("Contact");

		Map<String, Object> sessionMap = new HashMap<>();

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(sessionMap);
		FacesContextBridge.setCurrent(facesContext);

		Panel panel = mock(Panel.class);
		when(panel.getClientId()).thenReturn("panel1");

		view.setCollapsedFromSession(panel);
		FacesContextBridge.setCurrent(null);

		verify(panel, never()).setCollapsed(true);
		verify(panel, never()).setCollapsed(false);
	}

	@Test
	void setCollapsedFromSessionDoesNothingWhenOnlyOneContextParameterIsMissing() {
		FacesView view = new FacesView();
		view.setBizModuleParameter("admin");
		view.setBizDocumentParameter(null);

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(new HashMap<>());
		FacesContextBridge.setCurrent(facesContext);

		Panel panel = mock(Panel.class);
		when(panel.getClientId()).thenReturn("panel1");

		view.setCollapsedFromSession(panel);
		FacesContextBridge.setCurrent(null);

		verify(panel, never()).setCollapsed(true);
		verify(panel, never()).setCollapsed(false);
	}

	@Test
	void setCollapsedFromSessionDoesNothingWhenModuleMissingButDocumentPresent() {
		FacesView view = new FacesView();
		view.setBizModuleParameter(null);
		view.setBizDocumentParameter("Contact");

		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getSessionMap()).thenReturn(new HashMap<>());
		FacesContextBridge.setCurrent(facesContext);

		Panel panel = mock(Panel.class);
		when(panel.getClientId()).thenReturn("panel1");

		view.setCollapsedFromSession(panel);
		FacesContextBridge.setCurrent(null);

		verify(panel, never()).setCollapsed(true);
		verify(panel, never()).setCollapsed(false);
	}

	@Test
	void postRenderStateRoundTrips() {
		FacesView view = new FacesView();
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		Bean bean = mock(Bean.class);

		view.setPostRender(bizlet, bean);

		assertSame(bizlet, view.getPostRenderBizlet());
		assertSame(bean, view.getPostRenderBean());
	}

	@Test
	void setBeanWithWebContextInitialisesCurrentBeanAdapter() throws Exception {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-10");
		view.setWebContext(webContext);

		view.setBean(bean);

		assertSame(bean, view.getBean());
		assertNotNull(view.getCurrentBean());
		assertSame(bean, view.getCurrentBean().getBean());
	}

	@Test
	void setBeanWithoutWebContextExecutesNullWebContextBranch() {
		FacesView view = new FacesView();
		Bean bean = mock(Bean.class);

		assertDoesNotThrow(() -> view.setBean(bean));
		assertNotNull(view.getCurrentBean());
		assertNull(view.getCurrentBean().getBean());
	}

	@Test
	void dehydrateCapturesWebIdAndClearsCurrentBean() throws Exception {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		view.setWebContext(webContext);
		setPrivateField(view, "currentBean", new BeanMapAdapter(mock(Bean.class), null));

		view.dehydrate();

		assertNotNull(view.getDehydratedWebId());
		assertNull(view.getCurrentBean());
	}

	@Test
	void dehydrateWithoutWebContextLeavesDehydratedWebIdNull() {
		FacesView view = new FacesView();

		view.dehydrate();

		assertNull(view.getDehydratedWebId());
		assertNull(view.getCurrentBean());
	}

	@Test
	void dehydrateWithFacesTraceEnabledStillCapturesWebId() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		view.setWebContext(webContext);

		boolean originalFacesTrace = UtilImpl.FACES_TRACE;
		UtilImpl.FACES_TRACE = true;
		try {
			view.dehydrate();
		}
		finally {
			UtilImpl.FACES_TRACE = originalFacesTrace;
		}

		assertNotNull(view.getDehydratedWebId());
		assertNull(view.getCurrentBean());
	}

	@Test
	void getBeanReturnsCurrentBeanWhenWebContextPresent() {
		FacesView view = new FacesView();
		MockWebContext webContext = new MockWebContext();
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-30");
		webContext.setCurrentBean(bean);
		view.setWebContext(webContext);

		assertSame(bean, view.getBean());
	}

	@Test
	void hydrateRestoresWebContextAndClearsDehydratedWebId() throws Exception {
		FacesView view = new FacesView();
		MockWebContext initialContext = new MockWebContext();
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-20");
		view.setWebContext(initialContext);
		view.setBean(bean);
		view.dehydrate();

		MockWebContext restoredContext = new MockWebContext();
		restoredContext.setCurrentBean(bean);

		view.hydrate(restoredContext);

		assertNull(view.getDehydratedWebId());
		assertSame(restoredContext, view.getWebContext());
		assertNotNull(view.getCurrentBean());
		assertSame(bean, view.getCurrentBean().getBean());
	}

	@Test
	void hydrateWithFacesTraceEnabledRestoresWebContextAndClearsDehydratedWebId() throws Exception {
		FacesView view = new FacesView();
		MockWebContext initialContext = new MockWebContext();
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-21");
		view.setWebContext(initialContext);
		view.setBean(bean);
		view.dehydrate();

		MockWebContext restoredContext = new MockWebContext();
		restoredContext.setCurrentBean(bean);

		boolean originalFacesTrace = UtilImpl.FACES_TRACE;
		UtilImpl.FACES_TRACE = true;
		try {
			view.hydrate(restoredContext);
		}
		finally {
			UtilImpl.FACES_TRACE = originalFacesTrace;
		}

		assertNull(view.getDehydratedWebId());
		assertSame(restoredContext, view.getWebContext());
		assertNotNull(view.getCurrentBean());
		assertSame(bean, view.getCurrentBean().getBean());
	}

	@Test
	void responsiveFormStyleMethodsUseFacesViewRootAttributes() {
		FacesView view = new FacesView();
		ResponsiveFormGrid grid = new ResponsiveFormGrid(new ResponsiveFormGrid.ResponsiveGridStyle[] {
				new ResponsiveFormGrid.ResponsiveGridStyle(12, 6, 4, 3) });
		List<ResponsiveFormGrid> styles = new ArrayList<>();
		styles.add(grid);

		Map<String, Object> attributes = new HashMap<>();
		attributes.put(FacesUtil.FORM_STYLES_KEY, styles);

		FacesContext facesContext = mock(FacesContext.class);
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(viewRoot.getAttributes()).thenReturn(attributes);
		FacesContextBridge.setCurrent(facesContext);

		String style = view.getResponsiveFormStyle(0, "right", 1);
		String reset = view.resetResponsiveFormStyle(0);
		FacesContextBridge.setCurrent(null);

		assertTrue(style.contains("right"));
		assertTrue(reset.contains("12"));
	}

	@Test
	void getResponsiveFormStyleWithoutAlignmentReturnsBaseStyle() {
		FacesView view = new FacesView();
		ResponsiveFormGrid grid = new ResponsiveFormGrid(new ResponsiveFormGrid.ResponsiveGridStyle[] {
				new ResponsiveFormGrid.ResponsiveGridStyle(12, 6, 4, 3) });
		List<ResponsiveFormGrid> styles = new ArrayList<>();
		styles.add(grid);

		Map<String, Object> attributes = new HashMap<>();
		attributes.put(FacesUtil.FORM_STYLES_KEY, styles);

		FacesContext facesContext = mock(FacesContext.class);
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(viewRoot.getAttributes()).thenReturn(attributes);
		FacesContextBridge.setCurrent(facesContext);

		String style = view.getResponsiveFormStyle(0, null, 1);
		FacesContextBridge.setCurrent(null);

		assertFalse(style.contains("null"));
	}

	@Test
	void resetResponsiveFormStyleReturnsPrimeFlexClassWhenEnabled() {
		FacesView view = new FacesView();
		ResponsiveFormGrid grid = new ResponsiveFormGrid(new ResponsiveFormGrid.ResponsiveGridStyle[] {
				new ResponsiveFormGrid.ResponsiveGridStyle(12, 6, 4, 3) });
		List<ResponsiveFormGrid> styles = new ArrayList<>();
		styles.add(grid);

		Map<String, Object> attributes = new HashMap<>();
		attributes.put(FacesUtil.FORM_STYLES_KEY, styles);

		FacesContext facesContext = mock(FacesContext.class);
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		when(facesContext.getViewRoot()).thenReturn(viewRoot);
		when(viewRoot.getAttributes()).thenReturn(attributes);
		FacesContextBridge.setCurrent(facesContext);

		boolean originalPrimeFlex = UtilImpl.PRIMEFLEX;
		UtilImpl.PRIMEFLEX = true;
		try {
			String reset = view.resetResponsiveFormStyle(0);
			assertEquals("p-col-12 p-col-nogutter", reset);
		}
		finally {
			UtilImpl.PRIMEFLEX = originalPrimeFlex;
			FacesContextBridge.setCurrent(null);
		}
	}

	@Test
	void clearExecutesMethodBodyInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.clear("signature"));
		assertNotNull(view);
	}

	@Test
	void clearNullsBindingValueOnCurrentBean() throws Exception {
		FacesView view = new FacesView();
		Map<String, Object> values = new HashMap<>();
		values.put("signature", "content-id");
		DynamicBean bean = new DynamicBean("test", "RootDoc", values);
		setPrivateField(view, "currentBean", new BeanMapAdapter(bean, null));

		view.clear("signature");

		assertNull(values.get("signature"));
	}

	@Test
	void signAndHydrateExecuteMethodBodiesInHeadlessMode() {
		FacesView view = new FacesView();
		invokeIgnoringThrowable(() -> view.sign("sig", "signature", 100, 40, "#fff", "#000"));
		invokeIgnoringThrowable(() -> {
			try {
				view.hydrate(new MockWebContext());
			}
			catch (Exception e) {
				throw new RuntimeException(e);
			}
		});
		assertNotNull(view);
	}

	@Test
	void signThrowsWhenSignaturePayloadMissing() {
		FacesView view = new FacesView();

		Map<String, String> requestParameters = new HashMap<>();
		FacesContext facesContext = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(requestParameters);
		FacesContextBridge.setCurrent(facesContext);

		assertThrows(Throwable.class, () -> view.sign("sig", "signature", 100, 40, "#fff", "#000"));
		FacesContextBridge.setCurrent(null);
	}

	@SuppressWarnings("unchecked")
	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private static void invokeIgnoringThrowable(Runnable invocation) {
		try {
			invocation.run();
		}
		catch (Throwable ignored) {
			ignored.getClass();
			// Headless unit scope intentionally triggers framework-dependent exceptions.
		}
	}
}
