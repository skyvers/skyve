package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.router.UxUi;

/**
 * Tests for FacesView utility methods and state management that do not require a live CDI container.
 * Tests use plain instantiation (bypassing @PostConstruct) to exercise getters, setters,
 * and sanitisation logic.
 */
@SuppressWarnings("static-method")
class FacesViewTest {

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
	void getTemplateNameUsesUxUiTemplateWhenPresent() throws Exception {
		FacesView view = new FacesView();
		UxUi uxui = UxUi.newPrimeFaces("external", "nova", "arya", "blue");
		uxui.setPfTemplateName("nova");
		setUxUi(view, uxui);

		assertEquals("nova", view.getTemplateName());
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
}
