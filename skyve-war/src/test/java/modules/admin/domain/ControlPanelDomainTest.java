package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.AbstractSkyveTest;

public class ControlPanelDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesControlPanel() throws Exception {
		ControlPanel bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesControlPanel() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		assertNotNull(bean);
		assertEquals("admin", bean.getBizModule());
		assertEquals("ControlPanel", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypeDesktopSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUserAgentType(ControlPanel.SailUserAgentType.desktop);
		assertEquals(ControlPanel.SailUserAgentType.desktop, bean.getSailUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypeTabletSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUserAgentType(ControlPanel.SailUserAgentType.tablet);
		assertEquals(ControlPanel.SailUserAgentType.tablet, bean.getSailUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypePhoneSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUserAgentType(ControlPanel.SailUserAgentType.phone);
		assertEquals(ControlPanel.SailUserAgentType.phone, bean.getSailUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailTestStrategyAssertSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailTestStrategy(ControlPanel.SailTestStrategy.Assert);
		assertEquals(ControlPanel.SailTestStrategy.Assert, bean.getSailTestStrategy());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailTestStrategyVerifySetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailTestStrategy(ControlPanel.SailTestStrategy.Verify);
		assertEquals(ControlPanel.SailTestStrategy.Verify, bean.getSailTestStrategy());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailExecutorPrimeFacesSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailExecutor(ControlPanel.SailExecutor.primeFacesInlineSelenese);
		assertEquals(ControlPanel.SailExecutor.primeFacesInlineSelenese, bean.getSailExecutor());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUxUiSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUxUi("external");
		assertEquals("external", bean.getSailUxUi());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailBaseUrlSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailBaseUrl("http://localhost:8080/test");
		assertEquals("http://localhost:8080/test", bean.getSailBaseUrl());
	}

	@Test
	@SuppressWarnings("static-method")
	void xmlTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setXmlTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getXmlTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void httpTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setHttpTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getHttpTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setQueryTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getQueryTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void commandTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setCommandTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getCommandTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void facesTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setFacesTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getFacesTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setContentTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getContentTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void securityTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSecurityTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getSecurityTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setBizletTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getBizletTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void dirtyTraceSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setDirtyTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getDirtyTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void querySetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setQuery("SELECT b FROM admin$User b");
		assertEquals("SELECT b FROM admin$User b", bean.getQuery());
	}

	@Test
	@SuppressWarnings("static-method")
	void customerNameToSwapToSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setCustomerNameToSwapTo("acme");
		assertEquals("acme", bean.getCustomerNameToSwapTo());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailModuleNameSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailModuleName("admin");
		assertEquals("admin", bean.getSailModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultsSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setResults("some results");
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains("some results"), "Results should contain the set value");
	}

	@Test
	@SuppressWarnings("static-method")
	void tabIndexSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTabIndex(Integer.valueOf(2));
		assertEquals(Integer.valueOf(2), bean.getTabIndex());
	}

	@Test
	@SuppressWarnings("static-method")
	void selectedCacheSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSelectedCache("repositoryCache");
		assertEquals("repositoryCache", bean.getSelectedCache());
	}

	@Test
	@SuppressWarnings("static-method")
	void sessionCountSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSessionCount(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), bean.getSessionCount());
	}

	@Test
	@SuppressWarnings("static-method")
	void testNumberToGenerateSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestNumberToGenerate(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), bean.getTestNumberToGenerate());
	}

	@Test
	@SuppressWarnings("static-method")
	void testModuleNameSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestModuleName("kitchensink");
		assertEquals("kitchensink", bean.getTestModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testTagNameSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestTagName("myTag");
		assertEquals("myTag", bean.getTestTagName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testTagGeneratedDataSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestTagGeneratedData(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getTestTagGeneratedData());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSail("sail_content");
		assertEquals("sail_content", bean.getSail());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailLoginCustomerSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailLoginCustomer("demo");
		assertEquals("demo", bean.getSailLoginCustomer());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailLoginPasswordSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailLoginPassword("secret");
		assertEquals("secret", bean.getSailLoginPassword());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailComponentBuilderSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailComponentBuilder("com.example.MyBuilder");
		assertEquals("com.example.MyBuilder", bean.getSailComponentBuilder());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailLayoutBuilderSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailLayoutBuilder("com.example.MyLayoutBuilder");
		assertEquals("com.example.MyLayoutBuilder", bean.getSailLayoutBuilder());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailExecutorWebDriverSetAndGet() throws Exception {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailExecutor(ControlPanel.SailExecutor.primeFacesInlineWebDriver);
		assertEquals(ControlPanel.SailExecutor.primeFacesInlineWebDriver, bean.getSailExecutor());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypePhoneEnumValues() {
		assertEquals("phone", ControlPanel.SailUserAgentType.phone.toCode());
		assertNotNull(ControlPanel.SailUserAgentType.phone.toDomainValue());
		assertNotNull(ControlPanel.SailUserAgentType.phone.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypeFromCode() {
		assertEquals(ControlPanel.SailUserAgentType.desktop, ControlPanel.SailUserAgentType.fromCode("desktop"));
		assertEquals(ControlPanel.SailUserAgentType.tablet, ControlPanel.SailUserAgentType.fromCode("tablet"));
		assertEquals(ControlPanel.SailUserAgentType.phone, ControlPanel.SailUserAgentType.fromCode("phone"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sailTestStrategyEnumValues() {
		assertEquals("Assert", ControlPanel.SailTestStrategy.Assert.toCode());
		assertNotNull(ControlPanel.SailTestStrategy.toDomainValues());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailExecutorEnumFromCode() {
		assertEquals(ControlPanel.SailExecutor.primeFacesInlineSelenese,
				ControlPanel.SailExecutor.fromCode("org.skyve.impl.sail.execution.PrimeFacesInlineSeleneseExecutor"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypeToDomainValues() {
		assertNotNull(ControlPanel.SailUserAgentType.toDomainValues());
		assertEquals(4, ControlPanel.SailUserAgentType.toDomainValues().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailExecutorToDomainValues() {
		assertNotNull(ControlPanel.SailExecutor.toDomainValues());
	}
}
