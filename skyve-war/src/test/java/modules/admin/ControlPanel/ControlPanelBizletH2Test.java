package modules.admin.ControlPanel;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.domain.ControlPanel;
import util.AbstractH2Test;

/**
 * H2-backed tests for ControlPanelBizlet covering getConstantDomainValues,
 * getDynamicDomainValues, getVariantDomainValues, complete, preRerender and newInstance.
 */
public class ControlPanelBizletH2Test extends AbstractH2Test {

	@Inject
	private ControlPanelBizlet bizlet;

	private DataBuilder db;
	private ControlPanelExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- newInstance ----

	@Test
	void newInstanceSetsSailUser() throws Exception {
		ControlPanelExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertNotNull(result.getSailUser());
	}

	@Test
	void newInstanceSetsSailBaseUrl() throws Exception {
		ControlPanelExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertNotNull(result.getSailBaseUrl());
	}

	@Test
	void newInstanceSetsSessionCount() throws Exception {
		ControlPanelExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertNotNull(result.getSessionCount());
	}

	// ---- getConstantDomainValues: selectedCache ----

	@Test
	void getConstantDomainValuesForSelectedCacheReturnsNonEmptyList() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ControlPanel.selectedCachePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least conversation cache");
	}

	// ---- getConstantDomainValues: testModuleName ----

	@Test
	void getConstantDomainValuesForTestModuleNameReturnsModules() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ControlPanel.testModuleNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one module");
	}

	// ---- getConstantDomainValues: unknown attribute ----

	@Test
	void getConstantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues("unknownAttribute");
		assertNull(result);
	}

	// ---- getDynamicDomainValues: testDocumentNames ----

	@Test
	void getDynamicDomainValuesForTestDocumentNamesWithModuleSetReturnsDocuments() throws Exception {
		bean.setTestModuleName("admin");

		List<DomainValue> result = bizlet.getDynamicDomainValues(ControlPanel.testDocumentNamesPropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected persistable documents in admin module");
	}

	@Test
	void getDynamicDomainValuesForTestDocumentNamesWithNullModuleReturnsEmpty() throws Exception {
		bean.setTestModuleName(null);

		List<DomainValue> result = bizlet.getDynamicDomainValues(ControlPanel.testDocumentNamesPropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertThat(result.isEmpty(), is(true));
	}

	// ---- getVariantDomainValues: customerNameToSwapTo ----

	@Test
	void getVariantDomainValuesForCustomerNameToSwapToReturnsCustomers() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(ControlPanel.customerNameToSwapToPropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one customer");
	}

	// ---- getVariantDomainValues: sailModuleName ----

	@Test
	void getVariantDomainValuesForSailModuleNameReturnsModules() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(ControlPanel.sailModuleNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one module");
	}

	// ---- getVariantDomainValues: sailUxUi ----

	@Test
	void getVariantDomainValuesForSailUxUiReturnsUxUis() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(ControlPanel.sailUxUiPropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one UX/UI");
	}

	// ---- complete: testTagName ----

	@Test
	void completeForTestTagNameReturnsListWithoutException() throws Exception {
		List<String> result = bizlet.complete(ControlPanel.testTagNamePropertyName, "test", bean);
		assertThat(result, is(notNullValue()));
	}

	// ---- preRerender: push ----

	@Test
	void preRerenderPushClearsTestDocumentNames() throws Exception {
		bean.getTestDocumentNames().add(modules.admin.domain.ModuleDocument.newInstance());

		bizlet.preRerender("push", bean, webContext);

		assertThat(bean.getTestDocumentNames().isEmpty(), is(true));
	}

	@Test
	void preRerenderUnknownSourceDoesNotThrow() throws Exception {
		bizlet.preRerender("unknown", bean, webContext);
		// no exception expected
	}
}
