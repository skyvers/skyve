package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class ModuleDocumentDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		ModuleDocument bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ModuleDocument.MODULE_NAME, ModuleDocument.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		ModuleDocument bean = ModuleDocument.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("ModuleDocument", bean.getBizDocument());
	}

	@Test
	void moduleNameSetAndGet() throws Exception {
		ModuleDocument bean = ModuleDocument.newInstance();
		bean.setModuleName("admin");
		assertEquals("admin", bean.getModuleName());
	}

	@Test
	void documentNameSetAndGet() throws Exception {
		ModuleDocument bean = ModuleDocument.newInstance();
		bean.setDocumentName("User");
		assertEquals("User", bean.getDocumentName());
	}

	@Test
	void includeSetAndGet() throws Exception {
		ModuleDocument bean = ModuleDocument.newInstance();
		bean.setInclude(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getInclude());
	}
}
