package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.AbstractSkyveTest;

public class ReportParameterDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesReportParameter() throws Exception {
		ReportParameter bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ReportParameter.MODULE_NAME, ReportParameter.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesReportParameter() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		assertNotNull(bean);
		assertEquals("admin", bean.getBizModule());
		assertEquals("ReportParameter", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void nameSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setName("myParam");
		assertEquals("myParam", bean.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void descriptionSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setDescription("A test parameter");
		assertEquals("A test parameter", bean.getDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeTextSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(ReportParameter.Type.text);
		assertEquals(ReportParameter.Type.text, bean.getType());
		assertTrue(bean.isTextValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeIntegerSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(ReportParameter.Type.integer);
		assertEquals(ReportParameter.Type.integer, bean.getType());
		assertTrue(bean.isNumericalValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeLongIntegerSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(ReportParameter.Type.longInteger);
		assertEquals(ReportParameter.Type.longInteger, bean.getType());
		assertTrue(bean.isNumericalValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeDateSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(ReportParameter.Type.date);
		assertEquals(ReportParameter.Type.date, bean.getType());
		assertTrue(bean.isDateValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void requiredSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setRequired(Boolean.TRUE);
		assertTrue(bean.getRequired());
	}

	@Test
	@SuppressWarnings("static-method")
	void dateDefaultValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		DateOnly d = new DateOnly();
		bean.setDateDefaultValue(d);
		assertEquals(d, bean.getDateDefaultValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void numericalDefaultValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setNumericalDefaultValue(Long.valueOf(42L));
		assertEquals(Long.valueOf(42L), bean.getNumericalDefaultValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void textDefaultValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setTextDefaultValue("default text");
		assertEquals("default text", bean.getTextDefaultValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void reportInputValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setReportInputValue("input value");
		assertEquals("input value", bean.getReportInputValue());
	}
}
