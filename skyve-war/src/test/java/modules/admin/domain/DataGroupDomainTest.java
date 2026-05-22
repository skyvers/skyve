package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class DataGroupDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		DataGroup bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		DataGroup bean = DataGroup.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("DataGroup", bean.getBizDocument());
	}

	@Test
	void nameSetAndGet() throws Exception {
		DataGroup bean = DataGroup.newInstance();
		bean.setName("TestGroup");
		assertEquals("TestGroup", bean.getName());
	}

	@Test
	void descriptionSetAndGet() throws Exception {
		DataGroup bean = DataGroup.newInstance();
		bean.setDescription("A test group description");
		assertEquals("A test group description", bean.getDescription());
	}
}
