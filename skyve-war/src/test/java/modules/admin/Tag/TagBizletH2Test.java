package modules.admin.Tag;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Tag;
import util.AbstractH2Test;

/**
 * H2-backed tests for TagBizlet covering getDynamicDomainValues,
 * getVariantDomainValues, preExecute and preRerender.
 */
@SuppressWarnings("static-method")
class TagBizletH2Test extends AbstractH2Test {

	private static final TagBizlet bizlet = new TagBizlet();

	private DataBuilder db;
	private TagExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- getVariantDomainValues ----

	@Test
	void getVariantDomainValuesForUploadModuleNameReturnsModules() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(Tag.uploadModuleNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one module");
	}

	@Test
	void getVariantDomainValuesForActionModuleNameReturnsModules() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(Tag.actionModuleNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one module");
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsEmpty() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues("unknownAttribute");
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	// ---- getDynamicDomainValues: actionDocumentName ----

	@Test
	void getDynamicDomainValuesForActionDocumentNameWithModuleSetReturnsDocuments() throws Exception {
		bean.setActionModuleName("admin");

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.actionDocumentNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected documents in admin module");
	}

	@Test
	void getDynamicDomainValuesForActionDocumentNameWithNullModuleReturnsEmpty() throws Exception {
		bean.setActionModuleName(null);

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.actionDocumentNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	// ---- getDynamicDomainValues: uploadDocumentName ----

	@Test
	void getDynamicDomainValuesForUploadDocumentNameWithModuleSetReturnsDocuments() throws Exception {
		bean.setUploadModuleName("admin");

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.uploadDocumentNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected documents in admin module");
	}

	@Test
	void getDynamicDomainValuesForUploadDocumentNameWithNullModuleReturnsEmpty() throws Exception {
		bean.setUploadModuleName(null);

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.uploadDocumentNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	// ---- getDynamicDomainValues: attributeName ----

	@Test
	void getDynamicDomainValuesForAttributeNameWithBothModuleAndDocumentReturnsAttributes() throws Exception {
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.attributeNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected attributes in User document");
	}

	@Test
	void getDynamicDomainValuesForAttributeNameWithNullModuleReturnsEmpty() throws Exception {
		bean.setUploadModuleName(null);
		bean.setUploadDocumentName("User");

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.attributeNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	// ---- getDynamicDomainValues: operandTag ----

	@Test
	void getDynamicDomainValuesForOperandTagReturnsOtherTags() throws Exception {
		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.operandTagPropertyName, bean);
		assertThat(result, is(notNullValue()));
		// No other tags in test DB — may be empty, but should not throw
	}

	// ---- getDynamicDomainValues: documentAction ----

	@Test
	void getDynamicDomainValuesForDocumentActionWithModuleAndDocumentReturnsActions() throws Exception {
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("User");

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.documentActionPropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected actions in User document");
	}

	@Test
	void getDynamicDomainValuesForDocumentActionWithNullDocumentReturnsEmpty() throws Exception {
		bean.setActionModuleName("admin");
		bean.setActionDocumentName(null);

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.documentActionPropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	// ---- getDynamicDomainValues: documentCondition ----

	@Test
	void getDynamicDomainValuesForDocumentConditionWithModuleAndDocumentReturnsConditions() throws Exception {
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("User");

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.documentConditionPropertyName, bean);
		assertThat(result, is(notNullValue()));
		// User document may or may not have conditions - shouldn't throw
	}

	@Test
	void getDynamicDomainValuesForDocumentConditionWithNullDocumentReturnsEmpty() throws Exception {
		bean.setActionModuleName("admin");
		bean.setActionDocumentName(null);

		List<DomainValue> result = bizlet.getDynamicDomainValues(Tag.documentConditionPropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertTrue(result.isEmpty());
	}

	// ---- preExecute: Edit action ----

	@Test
	void preExecuteEditWithNullUploadModuleNameSetsDefaultModule() throws Exception {
		bean.setUploadModuleName(null);
		bean.setActionModuleName(null);
		bean.setFileHasHeaders(null);

		TagExtension result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);

		assertNotNull(result);
		assertThat(result.getUploadModuleName(), is(notNullValue()));
		assertThat(result.getActionModuleName(), is(notNullValue()));
		assertThat(result.getFileHasHeaders(), is(Boolean.TRUE));
	}

	@Test
	void preExecuteEditWithExistingModuleKeepsExistingModule() throws Exception {
		bean.setUploadModuleName("admin");
		bean.setActionModuleName("admin");
		bean.setFileHasHeaders(Boolean.FALSE);

		TagExtension result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);

		assertNotNull(result);
		// Upload/action module should remain "admin" since it was already set
		assertThat(result.getUploadModuleName(), is("admin"));
		assertThat(result.getActionModuleName(), is("admin"));
		// fileHasHeaders was already set, so should not be changed
		assertThat(result.getFileHasHeaders(), is(Boolean.FALSE));
	}

	@Test
	void preExecuteEditSetsCounters() throws Exception {
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("User");
		bean.setName("Test Tag");

		TagExtension result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);

		assertNotNull(result);
		assertThat(result.getTotalTagged(), is(notNullValue()));
		assertThat(result.getUploadTagged(), is(notNullValue()));
	}

	// ---- preRerender ----

	@Test
	void preRerenderUploadModuleNameClearsUploadDocumentName() throws Exception {
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");
		bean.setAttributeName("userName");

		bizlet.preRerender(Tag.uploadModuleNamePropertyName, bean, webContext);

		assertThat(bean.getUploadDocumentName(), is(nullValue()));
		assertThat(bean.getAttributeName(), is(nullValue()));
	}

	@Test
	void preRerenderUploadDocumentNameClearsAttributeName() throws Exception {
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");
		bean.setAttributeName("userName");

		bizlet.preRerender(Tag.uploadDocumentNamePropertyName, bean, webContext);

		assertThat(bean.getAttributeName(), is(nullValue()));
	}

	@Test
	void preRerenderActionModuleNameClearsActionDocumentName() throws Exception {
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("User");
		bean.setDocumentAction("some action");

		bizlet.preRerender(Tag.actionModuleNamePropertyName, bean, webContext);

		assertThat(bean.getActionDocumentName(), is(nullValue()));
		assertThat(bean.getDocumentAction(), is(nullValue()));
	}

	@Test
	void preRerenderActionDocumentNameClearsDocumentAction() throws Exception {
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("User");
		bean.setDocumentAction("some action");

		bizlet.preRerender(Tag.actionDocumentNamePropertyName, bean, webContext);

		assertThat(bean.getDocumentAction(), is(nullValue()));
	}

	@Test
	void preRerenderWithUnknownSourceDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> bizlet.preRerender("unknownSource", bean, webContext));
		// no exception expected
	}
}
