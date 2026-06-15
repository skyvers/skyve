package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ModuleDocumentDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		ModuleDocument bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ModuleDocument.MODULE_NAME, ModuleDocument.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		ModuleDocument bean = ModuleDocument.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("ModuleDocument", bean.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() {
		ModuleDocument bean = ModuleDocument.newInstance();
		bean.setModuleName("admin");
		assertEquals("admin", bean.getModuleName());
	}

	@Test
	void documentNameSetAndGet() {
		ModuleDocument bean = ModuleDocument.newInstance();
		bean.setDocumentName("User");
		assertEquals("User", bean.getDocumentName());
	}

	@Test
	void includeSetAndGet() {
		ModuleDocument bean = ModuleDocument.newInstance();
		bean.setInclude(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getInclude());
	}
}
