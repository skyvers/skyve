package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.AbstractSkyveTest;

class ControlPanelDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesControlPanel() {
		ControlPanel bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesControlPanel() {
		ControlPanel bean = ControlPanel.newInstance();
		assertNotNull(bean);
		assertEquals("admin", bean.getBizModule());
		assertEquals("ControlPanel", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypeDesktopSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUserAgentType(ControlPanel.SailUserAgentType.desktop);
		assertEquals(ControlPanel.SailUserAgentType.desktop, bean.getSailUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypeTabletSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUserAgentType(ControlPanel.SailUserAgentType.tablet);
		assertEquals(ControlPanel.SailUserAgentType.tablet, bean.getSailUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUserAgentTypePhoneSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUserAgentType(ControlPanel.SailUserAgentType.phone);
		assertEquals(ControlPanel.SailUserAgentType.phone, bean.getSailUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailTestStrategyAssertSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailTestStrategy(ControlPanel.SailTestStrategy.Assert);
		assertEquals(ControlPanel.SailTestStrategy.Assert, bean.getSailTestStrategy());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailTestStrategyVerifySetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailTestStrategy(ControlPanel.SailTestStrategy.Verify);
		assertEquals(ControlPanel.SailTestStrategy.Verify, bean.getSailTestStrategy());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailExecutorPrimeFacesSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailExecutor(ControlPanel.SailExecutor.primeFacesInlineSelenese);
		assertEquals(ControlPanel.SailExecutor.primeFacesInlineSelenese, bean.getSailExecutor());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailUxUiSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailUxUi("external");
		assertEquals("external", bean.getSailUxUi());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailBaseUrlSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailBaseUrl("http://localhost:8080/test");
		assertEquals("http://localhost:8080/test", bean.getSailBaseUrl());
	}

	@Test
	@SuppressWarnings("static-method")
	void xmlTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setXmlTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getXmlTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void httpTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setHttpTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getHttpTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void queryTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setQueryTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getQueryTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void commandTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setCommandTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getCommandTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void facesTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setFacesTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getFacesTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setContentTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getContentTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void securityTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSecurityTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getSecurityTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setBizletTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getBizletTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void dirtyTraceSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setDirtyTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getDirtyTrace());
	}

	@Test
	@SuppressWarnings("static-method")
	void querySetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setQuery("SELECT b FROM admin$User b");
		assertEquals("SELECT b FROM admin$User b", bean.getQuery());
	}

	@Test
	@SuppressWarnings("static-method")
	void customerNameToSwapToSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setCustomerNameToSwapTo("acme");
		assertEquals("acme", bean.getCustomerNameToSwapTo());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailModuleNameSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailModuleName("admin");
		assertEquals("admin", bean.getSailModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultsSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setResults("some results");
		assertNotNull(bean.getResults());
		assertTrue(bean.getResults().contains("some results"), "Results should contain the set value");
	}

	@Test
	@SuppressWarnings("static-method")
	void tabIndexSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTabIndex(Integer.valueOf(2));
		assertEquals(Integer.valueOf(2), bean.getTabIndex());
	}

	@Test
	@SuppressWarnings("static-method")
	void selectedCacheSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSelectedCache("repositoryCache");
		assertEquals("repositoryCache", bean.getSelectedCache());
	}

	@Test
	@SuppressWarnings("static-method")
	void sessionCountSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSessionCount(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), bean.getSessionCount());
	}

	@Test
	@SuppressWarnings("static-method")
	void testNumberToGenerateSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestNumberToGenerate(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), bean.getTestNumberToGenerate());
	}

	@Test
	@SuppressWarnings("static-method")
	void testModuleNameSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestModuleName("kitchensink");
		assertEquals("kitchensink", bean.getTestModuleName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testTagNameSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestTagName("myTag");
		assertEquals("myTag", bean.getTestTagName());
	}

	@Test
	@SuppressWarnings("static-method")
	void testTagGeneratedDataSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setTestTagGeneratedData(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getTestTagGeneratedData());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSail("sail_content");
		assertEquals("sail_content", bean.getSail());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailLoginCustomerSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailLoginCustomer("demo");
		assertEquals("demo", bean.getSailLoginCustomer());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailLoginPasswordSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailLoginPassword("secret");
		assertEquals("secret", bean.getSailLoginPassword());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailComponentBuilderSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailComponentBuilder("com.example.MyBuilder");
		assertEquals("com.example.MyBuilder", bean.getSailComponentBuilder());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailLayoutBuilderSetAndGet() {
		ControlPanel bean = ControlPanel.newInstance();
		bean.setSailLayoutBuilder("com.example.MyLayoutBuilder");
		assertEquals("com.example.MyLayoutBuilder", bean.getSailLayoutBuilder());
	}

	@Test
	@SuppressWarnings("static-method")
	void sailExecutorWebDriverSetAndGet() {
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

        @Test
        @SuppressWarnings("static-method")
        void sailUserAgentTypeToLocalisedDescription() {
                assertNotNull(ControlPanel.SailUserAgentType.desktop.toLocalisedDescription());
        }

        @Test
        @SuppressWarnings("static-method")
        void sailUserAgentTypeToDomainValue() {
                assertNotNull(ControlPanel.SailUserAgentType.desktop.toDomainValue());
        }

        @Test
        @SuppressWarnings("static-method")
        void sailUserAgentTypeFromLocalisedDescription() {
                String desc = ControlPanel.SailUserAgentType.desktop.toLocalisedDescription();
                assertEquals(ControlPanel.SailUserAgentType.desktop,
                                ControlPanel.SailUserAgentType.fromLocalisedDescription(desc));
        }

        @Test
        @SuppressWarnings("static-method")
        void sailUserAgentTypeFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(ControlPanel.SailUserAgentType.fromLocalisedDescription("nonexistent_xyz"));
        }

        @Test
        @SuppressWarnings("static-method")
        void sailTestStrategyToLocalisedDescription() {
                assertNotNull(ControlPanel.SailTestStrategy.Assert.toLocalisedDescription());
        }

        @Test
        @SuppressWarnings("static-method")
        void sailTestStrategyFromCode() {
                assertEquals(ControlPanel.SailTestStrategy.Assert,
                                ControlPanel.SailTestStrategy.fromCode("Assert"));
        }

        @Test
        @SuppressWarnings("static-method")
        void sailTestStrategyFromLocalisedDescription() {
                String desc = ControlPanel.SailTestStrategy.Assert.toLocalisedDescription();
                assertEquals(ControlPanel.SailTestStrategy.Assert,
                                ControlPanel.SailTestStrategy.fromLocalisedDescription(desc));
        }

        @Test
        @SuppressWarnings("static-method")
        void sailTestStrategyFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(ControlPanel.SailTestStrategy.fromLocalisedDescription("nonexistent_xyz"));
        }

        @Test
        @SuppressWarnings("static-method")
        void sailExecutorToLocalisedDescription() {
                assertNotNull(ControlPanel.SailExecutor.primeFacesInlineSelenese.toLocalisedDescription());
        }

        @Test
        @SuppressWarnings("static-method")
        void sailExecutorFromLocalisedDescription() {
                String desc = ControlPanel.SailExecutor.primeFacesInlineSelenese.toLocalisedDescription();
                assertEquals(ControlPanel.SailExecutor.primeFacesInlineSelenese,
                                ControlPanel.SailExecutor.fromLocalisedDescription(desc));
        }

        @Test
        @SuppressWarnings("static-method")
        void sailExecutorFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(ControlPanel.SailExecutor.fromLocalisedDescription("nonexistent_xyz"));
        }

	@Test
	@SuppressWarnings("static-method")
	void conditionsIsResultsNotNull() {
		ControlPanel bean = ControlPanel.newInstance();
		assertFalse(bean.isResultsNotNull());
		assertTrue(bean.isNotResultsNotNull());
		bean.setResults("some result");
		assertTrue(bean.isResultsNotNull());
		assertFalse(bean.isNotResultsNotNull());
	}

	@Test
	@SuppressWarnings("static-method")
	void conditionsIsTaggingGeneratedDataSelected() {
		ControlPanel bean = ControlPanel.newInstance();
		assertFalse(bean.isTaggingGeneratedDataSelected());
		assertTrue(bean.isNotTaggingGeneratedDataSelected());
		bean.setTestTagGeneratedData(Boolean.TRUE);
		assertTrue(bean.isTaggingGeneratedDataSelected());
		assertFalse(bean.isNotTaggingGeneratedDataSelected());
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void conditionsIsFixedCustomer() {
		ControlPanel bean = ControlPanel.newInstance();
		// Just calling the methods ensures coverage regardless of the result
		boolean fixed = bean.isFixedCustomer();
		assertEquals(! fixed, bean.isNotFixedCustomer());
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void conditionsIsProductionInstance() {
		ControlPanel bean = ControlPanel.newInstance();
		// Just calling the methods ensures coverage regardless of the result
		boolean prod = bean.isProductionInstance();
		assertEquals(! prod, bean.isNotProductionInstance());
	}

	@Test
	@SuppressWarnings("static-method")
	void testDocumentNamesListManipulation() {
		ControlPanel bean = ControlPanel.newInstance();
		assertNotNull(bean.getTestDocumentNames());
		// Null-safe fromCode paths
		assertNull(ControlPanel.SailUserAgentType.fromCode("nonexistent"));
		assertNull(ControlPanel.SailExecutor.fromCode("nonexistent"));
		assertNull(ControlPanel.SailTestStrategy.fromCode("nonexistent"));
	}
}
