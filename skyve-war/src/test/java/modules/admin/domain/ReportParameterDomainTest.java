package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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

	@Test
	@SuppressWarnings("static-method")
	void dateTestValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		DateOnly d = new DateOnly();
		bean.setDateTestValue(d);
		assertEquals(d, bean.getDateTestValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void numericalTestValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setNumericalTestValue(Long.valueOf(99L));
		assertEquals(Long.valueOf(99L), bean.getNumericalTestValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void textTestValueSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setTextTestValue("test text");
		assertEquals("test text", bean.getTextTestValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultValueStringSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setDefaultValueString("default");
		assertEquals("default", bean.getDefaultValueString());
	}

	@Test
	@SuppressWarnings("static-method")
	void testValueStringFromTextTestValue() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setTextTestValue("myTestVal");
		assertEquals("myTestVal", bean.getTestValueString());
	}

	@Test
	@SuppressWarnings("static-method")
	void formattedInputValueFromDefaultValueString() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setDefaultValueString("myDefault");
		assertEquals("myDefault", bean.getFormattedInputValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeTextIsNotNumericalOrDate() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(ReportParameter.Type.text);
		assertTrue(bean.isNotNumericalValue());
		assertTrue(bean.isNotDateValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeDateIsNotTextOrNumerical() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(ReportParameter.Type.date);
		assertTrue(bean.isNotTextValue());
		assertTrue(bean.isNotNumericalValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeNumericalIsNotTextOrDate() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(modules.admin.domain.ReportParameter.Type.integer);
		assertTrue(bean.isNumericalValue());
		assertTrue(bean.isNotTextValue());
		assertTrue(bean.isNotDateValue());
		assertFalse(bean.isNotNumericalValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void isTextValueWhenTypeText() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(modules.admin.domain.ReportParameter.Type.text);
		assertTrue(bean.isTextValue());
		assertFalse(bean.isNotTextValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void isDateValueWhenTypeDate() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setType(modules.admin.domain.ReportParameter.Type.date);
		assertTrue(bean.isDateValue());
		assertFalse(bean.isNotDateValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeFromCodeText() {
		assertEquals(modules.admin.domain.ReportParameter.Type.text,
				modules.admin.domain.ReportParameter.Type.fromCode("text"));
	}

	@Test
	@SuppressWarnings("static-method")
	void typeFromCodeUnknownReturnsNull() {
		assertNull(modules.admin.domain.ReportParameter.Type.fromCode("unknown_xyz"));
	}

	@Test
	@SuppressWarnings("static-method")
	void typeToDomainValues() {
		assertNotNull(modules.admin.domain.ReportParameter.Type.toDomainValues());
		assertFalse(modules.admin.domain.ReportParameter.Type.toDomainValues().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeToDomainValue() {
		assertNotNull(modules.admin.domain.ReportParameter.Type.text.toDomainValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeToLocalisedDescription() {
		assertNotNull(modules.admin.domain.ReportParameter.Type.text.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	void typeFromLocalisedDescription() {
		String desc = modules.admin.domain.ReportParameter.Type.text.toLocalisedDescription();
		assertEquals(modules.admin.domain.ReportParameter.Type.text,
				modules.admin.domain.ReportParameter.Type.fromLocalisedDescription(desc));
	}

	@Test
	@SuppressWarnings("static-method")
	void typeFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(modules.admin.domain.ReportParameter.Type.fromLocalisedDescription("nonexistent xyz"));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizOrdinalSetAndGet() throws Exception {
		ReportParameter bean = ReportParameter.newInstance();
		bean.setBizOrdinal(42);
		assertEquals(42, bean.getBizOrdinal());
	}
}
