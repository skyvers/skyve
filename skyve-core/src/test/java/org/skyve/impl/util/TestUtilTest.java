package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal;
import org.junit.jupiter.api.Assertions;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.metadata.model.Attribute;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.persistence.AbstractDocumentQuery;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.test.TestUtil;

@SuppressWarnings({"static-method", "boxing"})
class TestUtilTest {

	@Test
void testRandomRegexAllowedDigitCount() {
		// setup the test data
		final String expression = "(P|AH){1}\\d{5}";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 10);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result, anyOf(startsWith("P"), startsWith("AH")));
		assertTrue(result.matches("(P|AH){1}\\d{5}"));
	}

	@Test
void testRandomRegexListOfAllowedValues() {
		// setup the test data
		final String expression = "(ACT|NSW|NT|QLD|SA|TAS|VIC|WA)";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 3);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat("Should generate a value that is in the list",
				Arrays.asList("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"), hasItem(result));
	}

	@Test
void testRandomRegexListOfAllowedValuesWithAnchors() {
		// setup the test data
		final String expression = "^(ACT|NSW|NT|QLD|SA|TAS|VIC|WA)$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 3);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat("Should generate a value that is in the list",
				Arrays.asList("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"), hasItem(result));
	}

	@Test
void testRandomRegexLowercaseDashSeparatedString() {
		// setup the test data
		final String expression = "[a-z0-9]+(-[a-z0-9]+)*";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 20);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertTrue(result.matches("[a-z0-9]+(-[a-z0-9]+)*"));
	}

	@Test
void testRandomRegexLowercaseDashSeparatedStringWithAnchors() {
		// setup the test data
		final String expression = "^[a-z0-9]+(-[a-z0-9]+)*$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 20);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertTrue(result.matches("[a-z0-9]+(-[a-z0-9]+)*"));
	}

	@Test
void testRandomRegexStripsAnchors() {
		// setup the test data
		final String expression = "^^\\$$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 10);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result, is("^$"));
	}

	@Test
void testRandomEmail() {
		// setup the test data
		final int length = 20;

		// call the method under test
		String result = TestUtil.randomEmail(length);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.length(), is(length));
		assertTrue(result.matches("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"));
	}

	@Test
void testRandomDecimal() {
		// setup the test data
		Decimal10 decimal10 = new Decimal10();
		Decimal5 decimal5 = new Decimal5();
		Decimal2 decimal2 = new Decimal2();

		// call the method under test
		Decimal result10 = TestUtil.randomDecimal(decimal10);
		Decimal result5 = TestUtil.randomDecimal(decimal5);
		Decimal result2 = TestUtil.randomDecimal(decimal2);

		// verify the results
		assertThat(result10, is(notNullValue()));
		assertThat(result5, is(notNullValue()));
		assertThat(result2, is(notNullValue()));
		
		// Check decimal places
		assertThat(result10.toString().split("\\.")[1].length(), is(lessThanOrEqualTo(10)));
		assertThat(result5.toString().split("\\.")[1].length(), is(lessThanOrEqualTo(5)));
		assertThat(result2.toString().split("\\.")[1].length(), is(lessThanOrEqualTo(2)));
	}

	@Test
void testRandomInteger() {
		// setup the test data
		Integer integer = new Integer();
		LongInteger longInteger = new LongInteger();
		
		// Set up validators
		IntegerValidator intValidator = new IntegerValidator();
		intValidator.setMin(0);
		intValidator.setMax(100);
		integer.setValidator(intValidator);
		
		LongValidator longValidator = new LongValidator();
		longValidator.setMin(0L);
		longValidator.setMax(1000L);
		longInteger.setValidator(longValidator);

		// call the method under test
		java.lang.Integer resultInt = TestUtil.randomInteger(integer);
		java.lang.Integer resultLong = TestUtil.randomInteger(longInteger);

		// verify the results
		assertThat(resultInt, is(notNullValue()));
		assertThat(resultLong, is(notNullValue()));
		assertThat(resultInt, is(greaterThanOrEqualTo(0)));
		assertThat(resultInt, is(lessThanOrEqualTo(100)));
		assertThat(resultLong, is(greaterThanOrEqualTo(0)));
		assertThat(resultLong, is(lessThanOrEqualTo(1000)));
	}

	@Test
void testRandomEnum() {
		// setup the test data
		enum TestEnum { A, B, C, D }
		Class<TestEnum> enumClass = TestEnum.class;

		// call the method under test
		TestEnum result1 = TestUtil.randomEnum(enumClass, null);
		TestEnum result2 = TestUtil.randomEnum(enumClass, 1); // B

		// verify the results
		assertThat(result1, is(notNullValue()));
		assertThat(result2, is(notNullValue()));
		assertThat(Arrays.asList(TestEnum.values()), hasItem(result1));
		assertThat(Arrays.asList(TestEnum.values()), hasItem(result2));
		assertThat(result2, is(not(TestEnum.B))); // Should not return the current value
	}

	@Test
	@SuppressWarnings({ "rawtypes", "unchecked" })
	void testRandomEnumReturnsNullForNonEnumClass() {
		Class rawNonEnum = String.class;
		Object result = TestUtil.randomEnum(rawNonEnum, null);
		assertThat(result, is((Object) null));
	}

	@Test
void testRandomRegexReturnsNullForInvalidExpression() {
		Assertions.assertThrows(IllegalArgumentException.class, () -> TestUtil.randomRegex("(", 10));
	}

	@Test
void testRandomIntegerDefaultRangeForGenericAttribute() {
		Attribute attribute = org.mockito.Mockito.mock(Attribute.class);
		java.lang.Integer result = TestUtil.randomInteger(attribute);
		assertThat(result, is(greaterThanOrEqualTo(0)));
		assertThat(result, is(lessThanOrEqualTo(10000)));
	}

	@Test
void testRandomDecimalHonoursValidatorRangeWhenConfigured() {
		Decimal2 decimal2 = new Decimal2();
		org.skyve.impl.metadata.model.document.field.validator.DecimalValidator validator =
				new org.skyve.impl.metadata.model.document.field.validator.DecimalValidator();
		validator.setMin(new org.skyve.domain.types.Decimal2(5d));
		validator.setMax(new org.skyve.domain.types.Decimal2(5d));
		decimal2.setValidator(validator);

		Decimal result = TestUtil.randomDecimal(decimal2);
		assertThat(result, is(notNullValue()));
		assertEquals(5, result.intValue());
	}

	@Test
void testRetrieveExcludedUpdateAttributesFromFactoryAnnotation() {
		Module module = org.mockito.Mockito.mock(Module.class);
		Document document = org.mockito.Mockito.mock(Document.class);
		org.mockito.Mockito.when(module.getName()).thenReturn("utmod");
		org.mockito.Mockito.when(document.getName()).thenReturn("UtDoc");

		List<String> excluded = TestUtil.retrieveExcludedUpdateAttributes(module, document);
		assertThat(excluded, is(Arrays.asList("auditStamp", "lockVersion")));
	}

	@Test
void testRetrieveExcludedUpdateAttributesReturnsEmptyWhenFactoryMissing() {
		Module module = org.mockito.Mockito.mock(Module.class);
		Document document = org.mockito.Mockito.mock(Document.class);
		org.mockito.Mockito.when(module.getName()).thenReturn("missing");
		org.mockito.Mockito.when(document.getName()).thenReturn("NoFactory");

		List<String> excluded = TestUtil.retrieveExcludedUpdateAttributes(module, document);
		Assertions.assertTrue(excluded.isEmpty());
	}

	@Test
	@SuppressWarnings("unchecked")
	void testReadFromInputStreamHandlesNullAndReadsLines() throws Exception {
		Method method = TestUtil.class.getDeclaredMethod("readFromInputStream", java.io.InputStream.class);
		method.setAccessible(true);

		Object nullResult = method.invoke(null, new Object[] { null });
		assertThat(nullResult, is((Object) null));

		ByteArrayInputStream input = new ByteArrayInputStream("one\ntwo\nthree\n".getBytes());
		List<String> result = (List<String>) method.invoke(null, input);
		assertThat(result, is(Arrays.asList("one", "two", "three")));
	}

	@Test
void testHasExtensionAndAttributeKeyPrivateHelpers() throws Exception {
		Method hasExtension = TestUtil.class.getDeclaredMethod("hasExtension", String.class);
		hasExtension.setAccessible(true);

		Assertions.assertTrue(((Boolean) hasExtension.invoke(null, "file.txt")).booleanValue());
		Assertions.assertFalse(((Boolean) hasExtension.invoke(null, "file")).booleanValue());
		Assertions.assertFalse(((Boolean) hasExtension.invoke(null, ".env")).booleanValue());

		Method attributeKey = TestUtil.class.getDeclaredMethod("attributeKey", Module.class, Document.class, String.class);
		attributeKey.setAccessible(true);

		Module module = org.mockito.Mockito.mock(Module.class);
		Document document = org.mockito.Mockito.mock(Document.class);
		org.mockito.Mockito.when(module.getName()).thenReturn("utmod");
		org.mockito.Mockito.when(document.getName()).thenReturn("UtDoc");

		assertThat((String) attributeKey.invoke(null, module, document, "name"), is("utmod.UtDoc.name"));
		assertThat((String) attributeKey.invoke(null, null, null, "name"), is("name"));
		assertThat(attributeKey.invoke(null, module, document, null), is((Object) null));
	}

	@Test
void testRandomTextWithFormatMask() throws Exception {
		Text text = new Text();
		text.setName("code");
		text.setLength(8);
		TextFormat format = new TextFormat();
		format.setMask("LL-###");
		text.setFormat(format);

		String result = TestUtil.randomText("customer", null, null, text);
		assertThat(result, is(notNullValue()));
		Assertions.assertEquals(6, result.length());
		Assertions.assertTrue(result.matches("[a-z]{2}-\\d{3}"));
	}

	@Test
void testRandomTextWithValidatorTypeEmail() throws Exception {
		Text text = new Text();
		text.setName("email");
		text.setLength(18);
		TextValidator validator = new TextValidator();
		validator.setType(TextValidator.ValidatorType.email);
		text.setValidator(validator);

		String result = TestUtil.randomText("customer", null, null, text);
		assertThat(result, is(notNullValue()));
		Assertions.assertEquals(18, result.length());
		Assertions.assertTrue(result.contains("@"));
	}

	@Test
void testRandomTextPrefersRegexOverFormatWhenValidatorTypeIsNull() throws Exception {
		Text text = new Text();
		text.setName("code");
		text.setLength(10);
		TextFormat format = new TextFormat();
		format.setMask("LL-###");
		text.setFormat(format);
		TextValidator validator = new TextValidator();
		validator.setRegularExpression("^[0-9]{4}$");
		text.setValidator(validator);

		String result = TestUtil.randomText("customer", null, null, text);
		assertThat(result, is(notNullValue()));
		Assertions.assertTrue(result.matches("\\d{4}"));
	}

	@Test
void testRandomTextWithRegexAndNoFormatWhenValidatorTypeIsNull() throws Exception {
		Text text = new Text();
		text.setName("token");
		text.setLength(6);
		TextValidator validator = new TextValidator();
		validator.setRegularExpression("^[A-Z]{3}$");
		text.setValidator(validator);

		String result = TestUtil.randomText("customer", null, null, text);
		assertThat(result, is(notNullValue()));
		Assertions.assertTrue(result.matches("[A-Z]{3}"));
	}

	@Test
void testRandomTextWithValidatorTypeAndRegex() throws Exception {
		Text text = new Text();
		text.setName("digits");
		text.setLength(5);
		TextValidator validator = new TextValidator();
		validator.setType(TextValidator.ValidatorType.creditCard);
		validator.setRegularExpression("^[0-9]{5}$");
		text.setValidator(validator);

		String result = TestUtil.randomText("customer", null, null, text);
		assertThat(result, is(notNullValue()));
		Assertions.assertTrue(result.matches("\\d{5}"));
	}

	@Test
void testRandomFormatPrivateHelperWithCaseAndTruncate() throws Exception {
		Method randomFormat = TestUtil.class.getDeclaredMethod("randomFormat", TextFormat.class, int.class);
		randomFormat.setAccessible(true);

		TextFormat textFormat = new TextFormat();
		textFormat.setCase(org.skyve.domain.types.converters.Format.TextCase.upper);
		String result = (String) randomFormat.invoke(null, new Object[] { textFormat, java.lang.Integer.valueOf(5) });

		assertThat(result, is(notNullValue()));
		Assertions.assertTrue(result.length() <= 5);
		assertThat(result, is(result.toUpperCase()));
	}

	@Test
void testRandomValueFromFileReturnsNullForNullAttributeName() throws Exception {
		Method randomValueFromFile = TestUtil.class.getDeclaredMethod("randomValueFromFile",
				String.class,
				Module.class,
				Document.class,
				String.class,
				String[].class);
		randomValueFromFile.setAccessible(true);

		Object result = randomValueFromFile.invoke(null, "customer", null, null, null, new String[] {});
		assertThat(result, is((Object) null));
	}

	@Test
void testFindRandomDocumentQueryResultReturnsNullWhenNoRows() {
		AbstractDocumentQuery query = org.mockito.Mockito.mock(AbstractDocumentQuery.class);
		org.mockito.Mockito.when(query.scalarResults(Number.class)).thenReturn(Arrays.asList(Long.valueOf(0)));

		Bean result = TestUtil.findRandomDocumentQueryResult(query);
		assertThat(result, is((Bean) null));
		org.mockito.Mockito.verify(query).clearProjections();
		org.mockito.Mockito.verify(query).addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
	}

	@Test
void testFindRandomDocumentQueryResultReturnsBeanWhenRowsExist() {
		AbstractDocumentQuery query = org.mockito.Mockito.mock(AbstractDocumentQuery.class);
		Bean bean = org.mockito.Mockito.mock(Bean.class);
		org.mockito.Mockito.when(query.scalarResults(Number.class)).thenReturn(Arrays.asList(Long.valueOf(2)));
		org.mockito.Mockito.when(query.beanResults()).thenReturn(Arrays.asList(bean));

		Bean result = TestUtil.findRandomDocumentQueryResult(query);
		assertThat(result, is(bean));
		org.mockito.Mockito.verify(query).setFirstResult(0);
		org.mockito.Mockito.verify(query).setMaxResults(1);
	}

	@Test
void testShuffleArrayAndRandomStringPrivateHelpers() throws Exception {
		Method shuffleArray = TestUtil.class.getDeclaredMethod("shuffleArray", String[].class);
		shuffleArray.setAccessible(true);
		String[] input = { "alpha", "beta", "gamma" };
		shuffleArray.invoke(null, new Object[] { input });

		List<String> shuffled = Arrays.asList(input);
		Assertions.assertEquals(3, shuffled.size());
		Assertions.assertTrue(shuffled.contains("alpha"));
		Assertions.assertTrue(shuffled.contains("beta"));
		Assertions.assertTrue(shuffled.contains("gamma"));

		Method randomString = TestUtil.class.getDeclaredMethod("randomString", int.class);
		randomString.setAccessible(true);
		String value = (String) randomString.invoke(null, new Object[] { java.lang.Integer.valueOf(7) });
		Assertions.assertEquals(7, value.length());
	}

	// --- randomFormat coverage for uncovered branches ---

	@Test
void testRandomFormatWithAlphanumericMask() throws Exception {
		Method randomFormat = TestUtil.class.getDeclaredMethod("randomFormat", TextFormat.class, int.class);
		randomFormat.setAccessible(true);

		TextFormat textFormat = new TextFormat();
		textFormat.setMask("AA-##"); // 'A' = alphanumeric
		String result = (String) randomFormat.invoke(null, new Object[] { textFormat, java.lang.Integer.valueOf(10) });

		Assertions.assertNotNull(result);
		// result has 5 chars: 2 alphanumeric + '-' + 2 digits
		Assertions.assertEquals(5, result.length());
	}

	@Test
void testRandomFormatWithCapitalCase() throws Exception {
		Method randomFormat = TestUtil.class.getDeclaredMethod("randomFormat", TextFormat.class, int.class);
		randomFormat.setAccessible(true);

		TextFormat textFormat = new TextFormat();
		textFormat.setCase(org.skyve.domain.types.converters.Format.TextCase.capital);
		String result = (String) randomFormat.invoke(null, new Object[] { textFormat, java.lang.Integer.valueOf(10) });

		Assertions.assertNotNull(result);
	}

	@Test
void testRandomFormatWithLowerCase() throws Exception {
		Method randomFormat = TestUtil.class.getDeclaredMethod("randomFormat", TextFormat.class, int.class);
		randomFormat.setAccessible(true);

		TextFormat textFormat = new TextFormat();
		textFormat.setCase(org.skyve.domain.types.converters.Format.TextCase.lower);
		String result = (String) randomFormat.invoke(null, new Object[] { textFormat, java.lang.Integer.valueOf(10) });

		Assertions.assertNotNull(result);
		Assertions.assertEquals(result, result.toLowerCase());
	}

	// --- updateAttribute coverage ---

	@Test
void testUpdateAttributeBool() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();
		bean.setFlagProp(Boolean.TRUE);

		org.skyve.impl.metadata.model.document.field.Boolean boolField =
			new org.skyve.impl.metadata.model.document.field.Boolean();
		boolField.setName("flagProp");

		TestUtil.updateAttribute(bean, boolField);
		// Value should have been toggled to false
		Assertions.assertEquals(Boolean.FALSE, bean.getFlagProp());
	}

	@Test
void testUpdateAttributeBoolNullStartValue() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();
		bean.setFlagProp(null);

		org.skyve.impl.metadata.model.document.field.Boolean boolField =
			new org.skyve.impl.metadata.model.document.field.Boolean();
		boolField.setName("flagProp");

		TestUtil.updateAttribute(bean, boolField);
		Assertions.assertEquals(Boolean.FALSE, bean.getFlagProp());
	}

	@Test
void testUpdateAttributeColour() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.Colour colourField =
			new org.skyve.impl.metadata.model.document.field.Colour();
		colourField.setName("colourProp");

		TestUtil.updateAttribute(bean, colourField);
		Assertions.assertEquals("#FFFFFF", bean.getColourProp());
	}

	@Test
void testUpdateAttributeDate() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.Date dateField =
			new org.skyve.impl.metadata.model.document.field.Date();
		dateField.setName("dateProp");

		TestUtil.updateAttribute(bean, dateField);
		Assertions.assertNotNull(bean.getDateProp());
	}

	@Test
void testUpdateAttributeDateTime() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.DateTime dtField =
			new org.skyve.impl.metadata.model.document.field.DateTime();
		dtField.setName("dateTimeProp");

		TestUtil.updateAttribute(bean, dtField);
		Assertions.assertNotNull(bean.getDateTimeProp());
	}

	@Test
void testUpdateAttributeDecimal2() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.Decimal2 dec2Field =
			new org.skyve.impl.metadata.model.document.field.Decimal2();
		dec2Field.setName("decimal2Prop");

		TestUtil.updateAttribute(bean, dec2Field);
		Assertions.assertNotNull(bean.getDecimal2Prop());
	}

	@Test
void testUpdateAttributeInteger() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.Integer intField =
			new org.skyve.impl.metadata.model.document.field.Integer();
		intField.setName("intProp");

		TestUtil.updateAttribute(bean, intField);
		Assertions.assertNotNull(bean.getIntProp());
	}

	@Test
void testUpdateAttributeLongInteger() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.LongInteger longField =
			new org.skyve.impl.metadata.model.document.field.LongInteger();
		longField.setName("longProp");

		TestUtil.updateAttribute(bean, longField);
		Assertions.assertNotNull(bean.getLongProp());
	}

	@Test
void testUpdateAttributeGeometry() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.Geometry geoField =
			new org.skyve.impl.metadata.model.document.field.Geometry();
		geoField.setName("geoProp");

		TestUtil.updateAttribute(bean, geoField);
		Assertions.assertNotNull(bean.getGeoProp());
	}

	@Test
void testUpdateAttributeId() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		org.skyve.impl.metadata.model.document.field.Id idField =
			new org.skyve.impl.metadata.model.document.field.Id();
		idField.setName("idProp");

		TestUtil.updateAttribute(bean, idField);
		Assertions.assertNotNull(bean.getIdProp());
	}

	@Test
void testUpdateAttributeText() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		Text textField = new Text();
		textField.setName("textProp");
		textField.setLength(10);
		// Use a regex validator so randomText doesn't need to load lorem ipsum file
		org.skyve.impl.metadata.model.document.field.validator.TextValidator validator =
			new org.skyve.impl.metadata.model.document.field.validator.TextValidator();
		validator.setRegularExpression("^[A-Z]{5}$");
		textField.setValidator(validator);

		// randomText with regex but no module/document uses the private randomText(Module, Document, Attribute)
		// which calls CORE.getUser() - set up thread-local persistence
		org.skyve.metadata.user.User user = org.mockito.Mockito.mock(org.skyve.metadata.user.User.class);
		org.skyve.metadata.customer.Customer customer = org.mockito.Mockito.mock(org.skyve.metadata.customer.Customer.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.when(user.getCustomerName()).thenReturn("test");
		org.skyve.impl.persistence.AbstractPersistence persistence =
			org.mockito.Mockito.mock(org.skyve.impl.persistence.AbstractPersistence.class,
				org.mockito.Mockito.withSettings().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			TestUtil.updateAttribute(bean, textField);
			Assertions.assertNotNull(bean.getTextProp());
		} finally {
			java.lang.reflect.Field f = org.skyve.impl.persistence.AbstractPersistence.class
				.getDeclaredField("threadLocalPersistence");
			f.setAccessible(true);
			((ThreadLocal<?>) f.get(null)).remove();
		}
	}

	@Test
void testUpdateAttributeNullAttributeReturnsSameBean() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();

		UpdateAttributeTestBean result = TestUtil.updateAttribute(bean, null);
		Assertions.assertSame(bean, result);
	}

	@Test
void testUpdateAttributeEnumeration() throws Exception {
		UpdateAttributeTestBean bean = new UpdateAttributeTestBean();
		bean.setStatusProp(UpdateAttributeTestBean.Status.ACTIVE);

		org.skyve.impl.metadata.model.document.field.Enumeration enumField =
			new org.skyve.impl.metadata.model.document.field.Enumeration();
		enumField.setName("statusProp");

		TestUtil.updateAttribute(bean, enumField);
		// the enum value should have been changed or stayed same (only 2 values)
		Assertions.assertNotNull(bean.getStatusProp());
	}

	/** Inner bean with typed properties for updateAttribute testing */
	public static class UpdateAttributeTestBean extends org.skyve.impl.domain.AbstractPersistentBean {
		private static final long serialVersionUID = 1L;

		@Override public String getBizModule() { return "test"; }
		@Override public String getBizDocument() { return "UpdateAttributeTestBean"; }
		@Override public String getBizKey() { return "testKey"; }

		public enum Status { ACTIVE, INACTIVE }

		private Boolean flagProp;
		public Boolean getFlagProp() { return flagProp; }
		public void setFlagProp(Boolean v) { flagProp = v; }

		private String colourProp;
		public String getColourProp() { return colourProp; }
		public void setColourProp(String v) { colourProp = v; }

		private org.skyve.domain.types.DateOnly dateProp;
		public org.skyve.domain.types.DateOnly getDateProp() { return dateProp; }
		public void setDateProp(org.skyve.domain.types.DateOnly v) { dateProp = v; }

		private org.skyve.domain.types.DateTime dateTimeProp;
		public org.skyve.domain.types.DateTime getDateTimeProp() { return dateTimeProp; }
		public void setDateTimeProp(org.skyve.domain.types.DateTime v) { dateTimeProp = v; }

		private org.skyve.domain.types.Decimal2 decimal2Prop;
		public org.skyve.domain.types.Decimal2 getDecimal2Prop() { return decimal2Prop; }
		public void setDecimal2Prop(org.skyve.domain.types.Decimal2 v) { decimal2Prop = v; }

		private java.lang.Integer intProp;
		public java.lang.Integer getIntProp() { return intProp; }
		public void setIntProp(java.lang.Integer v) { intProp = v; }

		private Long longProp;
		public Long getLongProp() { return longProp; }
		public void setLongProp(Long v) { longProp = v; }

		private Status statusProp;
		public Status getStatusProp() { return statusProp; }
		public void setStatusProp(Status v) { statusProp = v; }

		private org.locationtech.jts.geom.Geometry geoProp;
		public org.locationtech.jts.geom.Geometry getGeoProp() { return geoProp; }
		public void setGeoProp(org.locationtech.jts.geom.Geometry v) { geoProp = v; }

		private String idProp;
		public String getIdProp() { return idProp; }
		public void setIdProp(String v) { idProp = v; }

		private String textProp;
		public String getTextProp() { return textProp; }
		public void setTextProp(String v) { textProp = v; }
	}
}
