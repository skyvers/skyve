package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class CountryDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		Country bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Country.MODULE_NAME, Country.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		Country bean = Country.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Country", bean.getBizDocument());
	}

	@Test
	void nameSetAndGet() {
		Country bean = Country.newInstance();
		bean.setName("Australia");
		assertEquals("Australia", bean.getName());
	}
}
