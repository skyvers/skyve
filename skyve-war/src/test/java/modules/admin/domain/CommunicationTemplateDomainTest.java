package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.AbstractSkyveTest;

public class CommunicationTemplateDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesCommunicationTemplate() throws Exception {
		CommunicationTemplate bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(CommunicationTemplate.MODULE_NAME, CommunicationTemplate.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() throws Exception {
		CommunicationTemplate bean = new CommunicationTemplate();
		assertEquals("admin", bean.getBizModule());
		assertEquals("CommunicationTemplate", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void nameSetAndGet() throws Exception {
		CommunicationTemplate bean = new CommunicationTemplate();
		bean.setName("Welcome Email Template");
		assertEquals("Welcome Email Template", bean.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void descriptionSetAndGet() throws Exception {
		CommunicationTemplate bean = new CommunicationTemplate();
		bean.setDescription("Template for welcome emails");
		assertEquals("Template for welcome emails", bean.getDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void templateSetAndGet() throws Exception {
		CommunicationTemplate bean = new CommunicationTemplate();
		bean.setTemplate("<html>Hello {{name}}</html>");
		assertEquals("<html>Hello {{name}}</html>", bean.getTemplate());
	}
}
