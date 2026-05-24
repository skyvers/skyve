package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;

import jakarta.faces.model.SelectItem;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

/**
 * Tests for the FacesAction callback methods.
 * Tests exercise the main code paths using AllAttributesPersistent + AbstractSkyveTest.
 */
class FacesActionsTest extends AbstractSkyveTest {

	// ----- GetContentURLAction -----

	@Test
	void getContentURLActionWithNullContentReturnsJavaScriptVoid() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		// markup has no default value, so getMarkup() returns null
		String result = new GetContentURLAction(bean, AllAttributesPersistent.markupPropertyName, false).callback();
		assertNotNull(result);
		assertTrue(result.contains("void"), "Should return javascript void for null content");
	}

	@Test
	void getContentURLActionWithNullContentAndImageReturnsBlankGif() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		String result = new GetContentURLAction(bean, AllAttributesPersistent.markupPropertyName, true).callback();
		assertNotNull(result);
		assertTrue(result.contains("blank.gif"), "Should return blank.gif for null image content");
	}

	@Test
	void getContentURLActionWithContentIdBuildsURL() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setMarkup("content-id-123");
		String result = new GetContentURLAction(bean, AllAttributesPersistent.markupPropertyName, false).callback();
		assertNotNull(result);
		assertTrue(result.contains("content"), "Should build content URL");
	}

	// ----- GetContentFileNameAction -----

	@Test
	void getContentFileNameActionWithNullContentReturnsEmptyPlaceholder() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		// markup has no default value, so getMarkup() returns null
		String result = new GetContentFileNameAction(bean, AllAttributesPersistent.markupPropertyName).callback();
		assertNotNull(result);
		// When no contentId, returns "&lt;Empty&gt;"
		assertTrue(result.contains("Empty"), "Should return Empty placeholder for null content");
	}

	// ----- SetTitleAction -----

	@Test
	void setTitleActionWithNullBeanJustLogs() throws Exception {
		FacesView facesView = new FacesView();
		// webContext is null => getBean() returns null => action just logs warning
		new SetTitleAction(facesView).callback();
		// no exception expected
	}

	// ----- RerenderAction -----

	@Test
	void rerenderActionWithNullTargetBeanJustLogs() throws Exception {
		FacesView facesView = new FacesView();
		// webContext is null => getBean() returns null => target bean null => just logs
		new RerenderAction(facesView, "source", false).callback();
		// no exception expected
	}

	@Test
	void rerenderActionWithBeanAndValidateFalseSucceeds() throws Exception {
		FacesView facesView = new FacesView();
		AbstractWebContext ctx = mockWebContext();
		facesView.setWebContext(ctx);
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		ctx.setCurrentBean(bean);

		// validate=false avoids FacesContext dependency
		new RerenderAction(facesView, "testSource", false).callback();
		// no exception expected
	}

	// ----- ActionUtil.getMetaDataQuery -----

	@Test
	void actionUtilGetMetaDataQueryReturnsDefaultQueryForDocument() {
		MetaDataQueryDefinition query = ActionUtil.getMetaDataQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(query);
	}

	// ----- ActionUtil.getTargetBeanForView -----

	@Test
	void actionUtilGetTargetBeanForViewReturnsNullWhenFacesViewHasNoBean() throws Exception {
		FacesView facesView = new FacesView();
		Bean result = ActionUtil.getTargetBeanForView(facesView);
		assertNull(result, "Should return null when FacesView has no bean");
	}

	@Test
	void actionUtilGetTargetBeanForViewReturnsBeanWhenNoViewBinding() throws Exception {
		FacesView facesView = new FacesView();
		AbstractWebContext ctx = mockWebContext();
		facesView.setWebContext(ctx);
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		ctx.setCurrentBean(bean);

		Bean result = ActionUtil.getTargetBeanForView(facesView);
		assertNotNull(result);
		assertEquals(bean.getBizId(), result.getBizId());
	}

	@Test
	void actionUtilGetTargetBeanForViewAndReferenceBindingReturnsNullWhenNoBean() throws Exception {
		FacesView facesView = new FacesView();
		Bean result = ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, null);
		assertNull(result, "Should return null when FacesView has no bean");
	}

	@Test
	void actionUtilGetTargetBeanForViewAndReferenceBindingReturnsBeanWhenSet() throws Exception {
		FacesView facesView = new FacesView();
		AbstractWebContext ctx = mockWebContext();
		facesView.setWebContext(ctx);
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		ctx.setCurrentBean(bean);

		Bean result = ActionUtil.getTargetBeanForViewAndReferenceBinding(facesView, null);
		assertNotNull(result);
	}

	// ----- GetSelectItemsAction (bean constructor) -----

	@Test
	void getSelectItemsActionWithBeanConstructorAndEnumReturnsItems() throws Exception {
		AbstractWebContext ctx = mockWebContext();
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		ctx.setCurrentBean(bean);

		List<SelectItem> items = new GetSelectItemsAction(
				bean, ctx, AllAttributesPersistent.enum3PropertyName, false).callback();

		assertNotNull(items);
		assertFalse(items.isEmpty());
	}

	@Test
	void getSelectItemsActionWithBeanConstructorAndEnumIncludesEmptyItem() throws Exception {
		AbstractWebContext ctx = mockWebContext();
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		ctx.setCurrentBean(bean);

		List<SelectItem> items = new GetSelectItemsAction(
				bean, ctx, AllAttributesPersistent.enum3PropertyName, true).callback();

		assertNotNull(items);
		assertTrue(items.size() >= 4, "Should include at least 4 items (3 enum values + empty)");
		assertTrue(items.get(0).getLabel().isEmpty(), "First item should be empty");
	}

	// ----- PopulateAction -----

	@Test
	void populateActionWithQueryNameSetsDocumentAndTitle() throws Exception {
		FacesView facesView = new FacesView();
		facesView.setBizModuleParameter(AllAttributesPersistent.MODULE_NAME);
		facesView.setQueryNameParameter(AllAttributesPersistent.DOCUMENT_NAME);
		// bizDocument is null => goes to else branch => ActionUtil.getMetaDataQuery
		new PopulateAction(facesView).callback();
		// no exception expected; query set on view
		assertNotNull(facesView.getBizDocumentParameter());
	}
}
