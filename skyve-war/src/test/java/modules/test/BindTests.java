package modules.test;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;
import org.locationtech.jts.io.WKTReader;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.Binder;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.OWASP;
import org.skyve.util.Time;
import org.skyve.util.Util;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Snapshot;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesPersistent.Enum3;

public class BindTests extends AbstractSkyveTest {
	@Test
	@SuppressWarnings("static-method")
	public void testSanitizeBinding() throws Exception {
		Assert.assertNull(BindUtil.sanitiseBinding(null));
		Assert.assertEquals("test", BindUtil.sanitiseBinding("test"));
		Assert.assertEquals("test_test", BindUtil.sanitiseBinding("test.test"));
		Assert.assertEquals("test_test_test", BindUtil.sanitiseBinding("test.test.test"));
		Assert.assertEquals("test1_test2_test3", BindUtil.sanitiseBinding("test1.test2.test3"));
		Assert.assertEquals("test_100__test_test", BindUtil.sanitiseBinding("test[100].test.test"));
		Assert.assertEquals("test_test_0__test", BindUtil.sanitiseBinding("test.test[0].test"));
		Assert.assertEquals("test_100__test_0__test", BindUtil.sanitiseBinding("test[100].test[0].test"));
		Assert.assertEquals("test_100__test_0__test_1_", BindUtil.sanitiseBinding("test[100].test[0].test[1]"));
		Assert.assertEquals("test1_100__test2_0__test3_1_", BindUtil.sanitiseBinding("test1[100].test2[0].test3[1]"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testUnsanitizeBinding() throws Exception {
		Assert.assertNull(BindUtil.unsanitiseBinding(null));
		Assert.assertEquals("test", BindUtil.unsanitiseBinding("test"));
		Assert.assertEquals("test.test", BindUtil.unsanitiseBinding("test_test"));
		Assert.assertEquals("test.test.test", BindUtil.unsanitiseBinding("test_test_test"));
		Assert.assertEquals("test1.test2.test3", BindUtil.unsanitiseBinding("test1_test2_test3"));
		Assert.assertEquals("test[100].test.test", BindUtil.unsanitiseBinding("test_100__test_test"));
		Assert.assertEquals("test.test[0].test", BindUtil.unsanitiseBinding("test_test_0__test"));
		Assert.assertEquals("test[100].test[0].test", BindUtil.unsanitiseBinding("test_100__test_0__test"));
		Assert.assertEquals("test[100].test[0].test[1]", BindUtil.unsanitiseBinding("test_100__test_0__test_1_"));
		Assert.assertEquals("test1[100].test2[0].test3[1]", BindUtil.unsanitiseBinding("test1_100__test2_0__test3_1_"));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testGeneratedJavaIdentifier() throws Exception {
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("1"), "one");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("2"), "two");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("3"), "three");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("4"), "four");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("5"), "five");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("6"), "six");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("7"), "seven");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("8"), "eight");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("9"), "nine");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_1"), "one");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_2"), "two");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_3"), "three");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_4"), "four");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_5"), "five");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_6"), "six");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_7"), "seven");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_8"), "eight");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("_9"), "nine");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("11"), "one1");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("22"), "two2");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("33"), "three3");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("44"), "four4");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("55"), "five5");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("66"), "six6");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("77"), "seven7");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("88"), "eight8");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("99"), "nine9");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v1"), "v1");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v2"), "v2");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v3"), "v3");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v4"), "v4");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v5"), "v5");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v6"), "v6");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v7"), "v7");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v8"), "v8");
		Assert.assertEquals(BindUtil.toJavaInstanceIdentifier("v9"), "v9");
	}
	
	@Test
	public void testCopyProperties() throws Exception {
		AllAttributesPersistent from = Util.constructRandomInstance(u, m, aapd, 2);
		AllAttributesPersistent to = AllAttributesPersistent.newInstance();
		Binder.copy(from, to);
		for (Attribute a: aapd.getAttributes()) {
			String name = a.getName();
				Assert.assertEquals("Property Name " + name, Binder.get(from, name), Binder.get(to, name));
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testCopyPropertiesWithChildCollection() throws Exception {
		UserExtension from = User.newInstance();
		UserRole role = UserRole.newInstance();
		from.getRoles().add(role);
		role.setParent(from);
		User to = User.newInstance();
		Binder.copy(from, to);
		Assert.assertEquals(role.getParent(), to);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testSimpleDynamicBeanProperty() {
		Map<String, Object> map = new TreeMap<>();
		map.put(User.userNamePropertyName, "mike");
		DynamicBean bean = new DynamicBean(User.MODULE_NAME, User.DOCUMENT_NAME, map);
		Assert.assertEquals("mike", Binder.get(bean, User.userNamePropertyName)); 
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCompoundDynamicBeanProperty() {
		String binding = Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName);
		Map<String, Object> map = new TreeMap<>();
		map.put(binding, "mike");
		DynamicBean bean = new DynamicBean(User.MODULE_NAME, User.DOCUMENT_NAME, map);
		Assert.assertEquals("mike", Binder.get(bean, binding)); 
	}
	
	@Test
	public void testSimpleThisProperty() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
	}
	
	@Test
	public void testCompoundThisProperty() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	public void testSimpleMapPropertyOverThisProperty() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		map.put(AllAttributesPersistent.booleanFlagPropertyName, null);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		Assert.assertFalse(Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
	}

	@Test
	public void testCompoundMapPropertyOverThisProperty() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		map.put(binding, null);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		Assert.assertFalse(Binder.get(bean, binding) instanceof Boolean);
	}
	
	@Test
	public void testCompoundPropertyWithoutMappedProperty2Deep() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(AllAttributesPersistent.aggregatedAssociationPropertyName, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	public void testCompoundPropertyWithoutMappedProperty3Deep() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(AllAttributesPersistent.aggregatedAssociationPropertyName, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	public void testFormatMessage() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		aap.setText("Test");
		Assert.assertEquals("Test", Binder.formatMessage("Test", aap));
		Assert.assertEquals("Test", Binder.formatMessage("{text}", aap));
		Assert.assertEquals("TestTest", Binder.formatMessage("{text}{text}", aap));
		// Test escapes
		Assert.assertEquals("{text}Test", Binder.formatMessage("\\{text\\}{text}", aap));
		Assert.assertEquals("{textTest", Binder.formatMessage("\\{text{text}", aap));
		Assert.assertEquals("text}Test", Binder.formatMessage("text\\}{text}", aap));
		Assert.assertEquals("{text", Binder.formatMessage("\\{text", aap));
		aap.setText("{text}");
		// Test '{' and '}' are left in tact when they are part of the value substituted
		Assert.assertEquals("{text}{text}", Binder.formatMessage("{text}{text}", aap));
	}
	
	@Test(expected = MetaDataException.class)
	public void testDanglingMessageFormat() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Assert.assertEquals("{text", Binder.formatMessage("{text", aap));
	}
	
	@Test
	public void testExpressions() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 2);
		bean.setText("Test");
		CORE.getStash().put("text", "Stash");
		CORE.getUser().getAttributes().put("text", "Attribute");
		
		Assert.assertEquals("TestUser", Binder.formatMessage("{USER}", bean));
		Assert.assertEquals("TestUser", Binder.formatMessage("{USERID}", bean));
		Assert.assertEquals("", Binder.formatMessage("{USERNAME}", bean));
		Assert.assertEquals("", Binder.formatMessage("{DATAGROUPID}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{CONTACTID}", bean));
		Assert.assertEquals("bizhub", Binder.formatMessage("{CUSTOMER}", bean));

		String currentDate = CORE.getCustomer().getDefaultDateConverter().toDisplayValue(new DateOnly());
		String currentTime = CORE.getCustomer().getDefaultTimeConverter().toDisplayValue(new TimeOnly());
		String currentDateTime = CORE.getCustomer().getDefaultDateTimeConverter().toDisplayValue(new DateTime());
		String currentTimestamp = CORE.getCustomer().getDefaultTimestampConverter().toDisplayValue(new Timestamp());
		
		Assert.assertEquals(currentDate, Binder.formatMessage("{DATE}", bean));
		Assert.assertEquals(currentTime, Binder.formatMessage("{TIME}", bean));
		Assert.assertEquals(currentDateTime, Binder.formatMessage("{DATETIME}", bean));
		Assert.assertEquals(currentTimestamp, Binder.formatMessage("{TIMESTAMP}", bean));
		Assert.assertEquals(Util.getDocumentUrl(bean), Binder.formatMessage("{URL}", bean));
		
		Assert.assertEquals("Test", Binder.formatMessage("{text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{ text }", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean:text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean: text }", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean : text }", bean));
		Assert.assertEquals("Text", Binder.formatMessage("{disp:text}", bean));
		Assert.assertEquals("", Binder.formatMessage("{desc:text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		Assert.assertEquals("Stash", Binder.formatMessage("{el:stash['text']}", bean));
		Assert.assertEquals("Attribute", Binder.formatMessage("{el:user.attributes['text']}", bean));
		Assert.assertEquals("", Binder.formatMessage("{el:stash['nothing']}", bean));
		Assert.assertEquals("", Binder.formatMessage("{el:user.attributes['nothing']}", bean));

		Assert.assertEquals("some.non-existent.key", Binder.formatMessage("{i18n:some.non-existent.key}", bean));
		Assert.assertEquals("Yes", Binder.formatMessage("{role:admin.BasicUser}", bean));
		Assert.assertEquals("Stash", Binder.formatMessage("{stash:text}", bean));
		Assert.assertEquals("", Binder.formatMessage("{stash:nothing}", bean));
		Assert.assertEquals("Attribute", Binder.formatMessage("{user:text}", bean));
		Assert.assertEquals("", Binder.formatMessage("{user:nothing}", bean));
		
		// Test functions and imports
		// NB re-evaluate these in case we've ticked over a boundary in the mean time. 
		currentDate = CORE.getCustomer().getDefaultDateConverter().toDisplayValue(new DateOnly());
		String tomorrowsDate = CORE.getCustomer().getDefaultDateConverter().toDisplayValue(Time.addDaysToNew(new DateOnly(), 1));
		currentTime = CORE.getCustomer().getDefaultTimeConverter().toDisplayValue(new TimeOnly());
		currentDateTime = CORE.getCustomer().getDefaultDateTimeConverter().toDisplayValue(new DateTime());
		currentTimestamp = CORE.getCustomer().getDefaultTimestampConverter().toDisplayValue(new Timestamp());

		Assert.assertEquals(currentDate, Binder.formatMessage("{el:newDateOnly()}", bean));
		Assert.assertEquals(tomorrowsDate, Binder.formatMessage("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", bean));
		Assert.assertEquals(String.valueOf(new DateOnly().toLocalDate().getYear()),
				Binder.formatMessage("{el:newDateOnly().toLocalDate().getYear()}", bean));
		Assert.assertEquals(currentTime, Binder.formatMessage("{el:newTimeOnly()}", bean));
		Assert.assertEquals(currentDateTime, Binder.formatMessage("{el:newDateTime()}", bean));
		Assert.assertEquals(currentTimestamp, Binder.formatMessage("{el:newTimestamp()}", bean));
		Assert.assertEquals("0.00", Binder.formatMessage("{el:Decimal2.ZERO}", bean));
		Assert.assertEquals("100.00", Binder.formatMessage("{el:newDecimal2(100)}", bean));
	}
	
	@Test
	public void testExpressionFormatting() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 2);
		bean.setDecimal2(Decimal2.ONE_THOUSAND);
		bean.setDecimal5(Decimal5.ONE_HUNDRED);
		bean.setDecimal10(Decimal10.ONE_HUNDRED);
		
		final String currentDateString = FormatterName.DD_MMM_YYYY.getFormatter().toDisplayValue(new DateOnly());
		final String currentTimeString = FormatterName.HH24_MI.getFormatter().toDisplayValue(new TimeOnly());
		final String currentDateTimeString = FormatterName.YYYY_MM_DD_HH24_MI.getFormatter().toDisplayValue(new DateTime());
		final String currentTimestampString = FormatterName.MMM_DD_YYYY_HH24_MI_SS.getFormatter().toDisplayValue(new Timestamp());
		
		Assert.assertEquals(currentDateString, Binder.formatMessage("{DATE|DD_MMM_YYYY}", bean));
		Assert.assertEquals(currentTimeString, Binder.formatMessage("{TIME|HH24_MI}", bean));
		Assert.assertEquals(currentDateTimeString, Binder.formatMessage("{DATETIME|YYYY_MM_DD_HH24_MI}", bean));
		Assert.assertEquals(currentTimestampString, Binder.formatMessage("{TIMESTAMP|MMM_DD_YYYY_HH24_MI_SS}", bean));
		Assert.assertEquals("01-Jan-1970", Binder.formatMessage("{TIME|DD_MMM_YYYY}", bean));
		Assert.assertEquals(currentDateString, Binder.formatMessage("{DATETIME|DD_MMM_YYYY}", bean));
		Assert.assertEquals(currentDateString, Binder.formatMessage("{TIMESTAMP|DD_MMM_YYYY}", bean));
		
		Assert.assertEquals("1,000.0", Binder.formatMessage("{decimal2|OneDecimalPlace}", bean));
		Assert.assertEquals("1,000.00", Binder.formatMessage("{ decimal2 | TwoDecimalPlaces }", bean));
		Assert.assertEquals("1,000.000", Binder.formatMessage("{bean:decimal2|ThreeDecimalPlaces}", bean));
		Assert.assertEquals("1,000.0000", Binder.formatMessage("{bean: decimal2 | FourDecimalPlaces }", bean));
		Assert.assertEquals("1,000.00000", Binder.formatMessage("{bean : decimal2 | FiveDecimalPlaces }", bean));

		Assert.assertEquals("100.0", Binder.formatMessage("{decimal5|OneDecimalPlace}", bean));
		Assert.assertEquals("100.00", Binder.formatMessage("{ decimal5 | TwoDecimalPlaces }", bean));
		Assert.assertEquals("100.000", Binder.formatMessage("{bean:decimal5|ThreeDecimalPlaces}", bean));
		Assert.assertEquals("100.0000", Binder.formatMessage("{bean: decimal5 | FourDecimalPlaces }", bean));
		Assert.assertEquals("100.00000", Binder.formatMessage("{bean : decimal5 | FiveDecimalPlaces }", bean));

		Assert.assertEquals("100", Binder.formatMessage("{decimal10|OneOptionalDecimalPlace}", bean));
		Assert.assertEquals("100", Binder.formatMessage("{ decimal10 | TwoOptionalDecimalPlaces }", bean));
		Assert.assertEquals("100", Binder.formatMessage("{bean:decimal10|ThreeOptionalDecimalPlaces}", bean));
		Assert.assertEquals("100", Binder.formatMessage("{bean: decimal10 | FourOptionalDecimalPlaces }", bean));
		Assert.assertEquals("100", Binder.formatMessage("{bean : decimal10 | FiveOptionalDecimalPlaces }", bean));

		bean.setDecimal10(new Decimal10("1.0123456789"));
		Assert.assertEquals("1", Binder.formatMessage("{decimal10|OneOptionalDecimalPlace}", bean));
		Assert.assertEquals("1.01", Binder.formatMessage("{ decimal10 | TwoOptionalDecimalPlaces }", bean));
		Assert.assertEquals("1.012", Binder.formatMessage("{bean:decimal10|ThreeOptionalDecimalPlaces}", bean));
		Assert.assertEquals("1.0123", Binder.formatMessage("{bean: decimal10 | FourOptionalDecimalPlaces }", bean));
		Assert.assertEquals("1.01235", Binder.formatMessage("{bean : decimal10 | FiveOptionalDecimalPlaces }", bean));
		Assert.assertEquals("1.012346", Binder.formatMessage("{bean : decimal10 | SixOptionalDecimalPlaces }", bean));

		Assert.assertEquals("1.01", Binder.formatMessage("{el:bean.decimal10|TwoOptionalDecimalPlaces}", bean));
		Assert.assertEquals("0", Binder.formatMessage("{el:Decimal2.ZERO|TwoOptionalDecimalPlaces}", bean));
		Assert.assertEquals("100", Binder.formatMessage("{el:newDecimal2(100)|TwoOptionalDecimalPlaces}", bean));
	}

	@Test
	public void testDynamicExpressions() throws Exception {
		DynamicPersistentBean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		Binder.set(bean, AllAttributesPersistent.textPropertyName, "Test");
		
		Assert.assertEquals("Test", Binder.formatMessage("{text}", bean));
		Assert.assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{condition}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean:text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		Assert.assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{el:bean.condition}", bean));

		// Check falsey boolean evaluation
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, Boolean.TRUE);
		Assert.assertEquals(Boolean.TRUE, ExpressionEvaluator.evaluate("{el:bean.falseyBooleanEvaluation}", bean));
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, Boolean.FALSE);
		Assert.assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{el:bean.falseyBooleanEvaluation}", bean));
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, null);
		Assert.assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{el:bean.falseyBooleanEvaluation}", bean));
		
		bean = aadpd.newInstance(u);
		System.out.println(bean);
		System.out.println();
	}
	
	@Test
	public void testDynamicDefaults() throws Exception {
		Bean bean = aadpd.newInstance(u);
		Assert.assertEquals(Boolean.TRUE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		Assert.assertEquals("#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName));
		Assert.assertEquals(new DateOnly("2021-10-21"), Binder.get(bean, AllAttributesPersistent.datePropertyName));
		Assert.assertEquals(new DateTime("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.dateTimePropertyName));
		Assert.assertEquals(new Decimal10(100.1234567899), Binder.get(bean, AllAttributesPersistent.decimal10PropertyName));
		Assert.assertEquals(new Decimal2(100.12), Binder.get(bean, AllAttributesPersistent.decimal2PropertyName));
		Assert.assertEquals(new Decimal5(100.12345), Binder.get(bean, AllAttributesPersistent.decimal5PropertyName));
		// NB This can't be a real enum value coz there is no generated class
		Assert.assertEquals("one", Binder.get(bean, AllAttributesPersistent.enum3PropertyName));
		// NB This is a real enum value coz its a dynamic reference to a generated class
		Assert.assertEquals(Enum3.one, Binder.get(bean, "enum3Reference"));
		Assert.assertEquals(new WKTReader().read("POINT(0 0)"), Binder.get(bean, AllAttributesPersistent.geometryPropertyName));
		Assert.assertEquals("1234567890", Binder.get(bean, AllAttributesPersistent.idPropertyName));
		Assert.assertEquals(Integer.valueOf(123), Binder.get(bean,  AllAttributesPersistent.normalIntegerPropertyName));
		Assert.assertEquals(Long.valueOf(123), Binder.get(bean,  AllAttributesPersistent.longIntegerPropertyName));
		Assert.assertEquals("<h1>Markup</h1>", Binder.get(bean, AllAttributesPersistent.markupPropertyName));
		Assert.assertEquals("Memo", Binder.get(bean, AllAttributesPersistent.memoPropertyName));
		Assert.assertEquals("Text", Binder.get(bean, AllAttributesPersistent.textPropertyName));
		Assert.assertEquals(new TimeOnly("07:51:26"), Binder.get(bean, AllAttributesPersistent.timePropertyName));
		Assert.assertEquals(new Timestamp("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.timestampPropertyName));
		
		bean = aapd.newInstance(u);
		Assert.assertEquals(Boolean.FALSE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		Assert.assertEquals("#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName));
		Assert.assertEquals(new DateOnly(), Binder.get(bean, AllAttributesPersistent.datePropertyName));
		Assert.assertEquals(new DateTime(), Binder.get(bean, AllAttributesPersistent.dateTimePropertyName));
		Assert.assertEquals(Decimal10.ZERO, Binder.get(bean, AllAttributesPersistent.decimal10PropertyName));
		Assert.assertEquals(Decimal2.ZERO, Binder.get(bean, AllAttributesPersistent.decimal2PropertyName));
		Assert.assertEquals(Decimal5.ZERO, Binder.get(bean, AllAttributesPersistent.decimal5PropertyName));
		Assert.assertEquals(Enum3.one, Binder.get(bean, AllAttributesPersistent.enum3PropertyName));
		Assert.assertEquals(new WKTReader().read("POINT(0 0)"), Binder.get(bean, AllAttributesPersistent.geometryPropertyName));
		Assert.assertEquals("Test POINT (0 0)", Binder.get(bean, AllAttributesPersistent.memoPropertyName));
	}

	@Test
	public void testExpressionValidation() throws Exception {
		// Test implicit expressions
		Assert.assertNull(BindUtil.validateMessageExpressions("{USER}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{USERID}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{USERNAME}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{DATAGROUPID}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{CONTACTID}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{CUSTOMER}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{DATE}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{TIME}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{DATETIME}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{TIMESTAMP}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{URL}", c, aapd));
		
		// Test basic expressions and trimming
		Assert.assertNull(BindUtil.validateMessageExpressions("{text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{ text }", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{bean:text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{bean: text }", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{bean : text }", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{disp:text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{desc:text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:bean.doesNotExist}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{el:bean.doesNotExist}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:bean..doesNotExist}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{el:bean..doesNotExist}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:stash['text']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:stash['text']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:user.attributes['text']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:user.attributes['text']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:stash['nothing']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:stash['nothing']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:user.attributes['nothing']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:user.attributes['nothing']}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{i18n:some.non-existent.key}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{role:admin.BasicUser}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{stash:text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{stash:nothing}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{user:text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{user:nothing}", c, aapd));
		
		// Test malformed and escaped expressions
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{|}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{ |}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{| }", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{|", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{ |", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{bean:}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{bean:|}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{bean: |}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{bean:| }", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{bean:|", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{bean: |", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("|}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("|", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{text\\}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("\\{text}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("\\{{text}", c, aapd));
		
		// Test EL operators
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:bean.normalInteger + bean.normalInteger}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:bean.longInteger + bean.longInteger}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:bean.decimal2.bigDecimalValue() + bean.decimal2.bigDecimalValue()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:bean.decimal5.add(bean.decimal5)}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:bean.date == bean.date}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:empty bean.date}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:not empty bean.date and bean.booleanFlag}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:empty bean.date ? DATE : bean.date}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:bean.text.concat(bean.text)}", c, aapd));

		// Stash operation puts that expression evaluation into typeless mode
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:stash['key'].concat(bean.text)}", c, aapd));
		// Stashy is still a problem though
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{el:stashy['key'].concat(bean.text)}", c, aapd));
		// And the other expression for the concat parameter is still validated
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{el:stash['key'].concat(bean.texty)}", c, aapd));
		// But concaty method name is not chacked coz we lost the type information retrieving from the stash
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:stash['key'].concaty(bean.text)}", c, aapd));

		// Test type validation
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedAssociation}", Bean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:empty bean.aggregatedAssociation}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedCollection}", List.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.date.after(newDateOnly())}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.date.after(newDateOnly())}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.getDate().after(bean.getDate())}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:empty bean.date or bean.getDate().after(bean.getDate())}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.date}", DateOnly.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:empty bean.date or not empty bean.date}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.normalInteger + bean.normalInteger}", Number.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean}", AllAttributesPersistent.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedAssociation}", AllAttributesPersistent.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{aggregatedAssociation}", AllAttributesPersistent.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{aggregatedAssociation}", Bean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{bizId}", String.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{aggregatedAssociation.bizId}", String.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{aggregatedCollection}", List.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{date}", DateOnly.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{condition}", Boolean.class, c, m, aapd));
		Assert.assertNull(ExpressionEvaluator.validate("{aggregatedAssociation.condition}", Boolean.class, c, m, aapd));

		// Test formatter validation
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{USER|Bogus}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{USER|DD_MMM_YYYY}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{TIMESTAMP|DD_MMM_YYYY}", c, aapd));
		Assert.assertNotNull(BindUtil.validateMessageExpressions("{TIMESTAMP|OneDecimalPlace}", c, aapd));
		// Any formatter should return a String
		Assert.assertNotNull(ExpressionEvaluator.validate("{date|MM_DD_YYYY}", DateOnly.class, c, m, aapd));
		Assert.assertNotNull(ExpressionEvaluator.validate("{el:bean.date|DD_MMM_YYYY}", String.class, c, m, aapd));
		
		// Test functions
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:newDateOnly()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:newDateOnly()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:newTimeOnly()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:newTimeOnly()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:newDateTime()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:newDateTime()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{rtel:newTimestamp()}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:newTimestamp()}", c, aapd));
		
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:Decimal2.ZERO}", c, aapd));
		Assert.assertNull(BindUtil.validateMessageExpressions("{el:newDecimal2(100)}", c, aapd));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testExpressionPrefixing() throws Exception {
		Assert.assertEquals("{USER}", BindUtil.prefixMessageExpressions("{USER}", "binding"));
		Assert.assertEquals("{USERID}", BindUtil.prefixMessageExpressions("{USERID}", "binding"));
		Assert.assertEquals("{USERNAME}", BindUtil.prefixMessageExpressions("{USERNAME}", "binding"));
		Assert.assertEquals("{DATAGROUPID}", BindUtil.prefixMessageExpressions("{DATAGROUPID}", "binding"));
		Assert.assertEquals("{CONTACTID}", BindUtil.prefixMessageExpressions("{CONTACTID}", "binding"));
		Assert.assertEquals("{CUSTOMER}", BindUtil.prefixMessageExpressions("{CUSTOMER}", "binding"));
		Assert.assertEquals("{DATE}", BindUtil.prefixMessageExpressions("{DATE}", "binding"));
		Assert.assertEquals("{TIME}", BindUtil.prefixMessageExpressions("{TIME}", "binding"));
		Assert.assertEquals("{DATETIME}", BindUtil.prefixMessageExpressions("{DATETIME}", "binding"));
		Assert.assertEquals("{TIMESTAMP}", BindUtil.prefixMessageExpressions("{TIMESTAMP}", "binding"));
		Assert.assertEquals("{URL}", BindUtil.prefixMessageExpressions("{URL}", "binding"));

		Assert.assertEquals("{binding.text}", BindUtil.prefixMessageExpressions("{text}", "binding"));
		Assert.assertEquals("{binding.text}", BindUtil.prefixMessageExpressions("{ text }", "binding"));
		Assert.assertEquals("Once {binding.text} {binding.text} Twice", BindUtil.prefixMessageExpressions("Once {text} {text} Twice", "binding"));
		Assert.assertEquals("{bean:binding.text}", BindUtil.prefixMessageExpressions("{bean:text}", "binding"));
		Assert.assertEquals("{bean:binding.text}", BindUtil.prefixMessageExpressions("{bean: text }", "binding"));
		Assert.assertEquals("Once {bean:binding.text} {bean:binding.text} Twice", BindUtil.prefixMessageExpressions("Once {bean:text} {bean:text} Twice", "binding"));
		Assert.assertEquals("Once {bean:binding.text} {binding.text} Twice", BindUtil.prefixMessageExpressions("Once {bean:text} {text} Twice", "binding"));
		Assert.assertEquals("{bean:binding.text}", BindUtil.prefixMessageExpressions("{bean : text }", "binding"));
		Assert.assertEquals("{disp:binding.text}", BindUtil.prefixMessageExpressions("{disp:text}", "binding"));
		Assert.assertEquals("{desc:binding.text}", BindUtil.prefixMessageExpressions("{desc:text}", "binding"));
		Assert.assertEquals("{rtel:bean.binding.text}", BindUtil.prefixMessageExpressions("{rtel:bean.text}", "binding"));
		Assert.assertEquals("{el:bean.binding.text}", BindUtil.prefixMessageExpressions("{el:bean.text}", "binding"));
		Assert.assertEquals("{rtel:stash['text']}", BindUtil.prefixMessageExpressions("{rtel:stash['text']}", "binding"));
		Assert.assertEquals("{el:stash['text']}", BindUtil.prefixMessageExpressions("{el:stash['text']}", "binding"));
		Assert.assertEquals("{rtel:user.attributes['text']}", BindUtil.prefixMessageExpressions("{rtel:user.attributes['text']}", "binding"));
		Assert.assertEquals("{el:user.attributes['text']}", BindUtil.prefixMessageExpressions("{el:user.attributes['text']}", "binding"));
		Assert.assertEquals("{rtel:stash['nothing']}", BindUtil.prefixMessageExpressions("{rtel:stash['nothing']}", "binding"));
		Assert.assertEquals("{el:stash['nothing']}", BindUtil.prefixMessageExpressions("{el:stash['nothing']}", "binding"));
		Assert.assertEquals("{rtel:user.attributes['nothing']}", BindUtil.prefixMessageExpressions("{rtel:user.attributes['nothing']}", "binding"));
		Assert.assertEquals("{el:user.attributes['nothing']}", BindUtil.prefixMessageExpressions("{el:user.attributes['nothing']}", "binding"));
		Assert.assertEquals("{i18n:some.non-existent.key}", BindUtil.prefixMessageExpressions("{i18n:some.non-existent.key}", "binding"));
		Assert.assertEquals("{role:admin.BasicUser}", BindUtil.prefixMessageExpressions("{role:admin.BasicUser}", "binding"));
		Assert.assertEquals("{stash:text}", BindUtil.prefixMessageExpressions("{stash:text}", "binding"));
		Assert.assertEquals("{stash:nothing}", BindUtil.prefixMessageExpressions("{stash:nothing}", "binding"));
		Assert.assertEquals("{user:text}", BindUtil.prefixMessageExpressions("{user:text}", "binding"));
		Assert.assertEquals("{user:nothing}", BindUtil.prefixMessageExpressions("{user:nothing}", "binding"));
		
		Assert.assertEquals("}", BindUtil.prefixMessageExpressions("}", "binding"));
		Assert.assertEquals("\\{text}", BindUtil.prefixMessageExpressions("\\{text}", "binding"));
		Assert.assertEquals("\\{{binding.text}", BindUtil.prefixMessageExpressions("\\{{text}", "binding"));

		Assert.assertEquals("{el:bean.binding.normalInteger + bean.binding.normalInteger}", BindUtil.prefixMessageExpressions("{el:bean.normalInteger + bean.normalInteger}", "binding"));
		Assert.assertEquals("{el:bean.binding.longInteger + bean.binding.longInteger}", BindUtil.prefixMessageExpressions("{el:bean.longInteger + bean.longInteger}", "binding"));
		Assert.assertEquals("{el:bean.binding.decimal2.bigDecimalValue() + bean.binding.decimal2.bigDecimalValue()}", BindUtil.prefixMessageExpressions("{el:bean.decimal2.bigDecimalValue() + bean.decimal2.bigDecimalValue()}", "binding"));
		Assert.assertEquals("{el:bean.binding.decimal5.add(bean.binding.decimal5)}", BindUtil.prefixMessageExpressions("{el:bean.decimal5.add(bean.decimal5)}", "binding"));
		Assert.assertEquals("{el:bean.binding.date == bean.binding.date}", BindUtil.prefixMessageExpressions("{el:bean.date == bean.date}", "binding"));
		Assert.assertEquals("{el:empty bean.binding.date}", BindUtil.prefixMessageExpressions("{el:empty bean.date}", "binding"));
		Assert.assertEquals("{el:not empty bean.binding.date and bean.binding.booleanFlag}", BindUtil.prefixMessageExpressions("{el:not empty bean.date and bean.booleanFlag}", "binding"));
		Assert.assertEquals("{el:empty bean.binding.date ? DATE : bean.binding.date}", BindUtil.prefixMessageExpressions("{el:empty bean.date ? DATE : bean.date}", "binding"));
		Assert.assertEquals("{el:bean.binding.text.concat(bean.binding.text)}", BindUtil.prefixMessageExpressions("{el:bean.text.concat(bean.text)}", "binding"));

		Assert.assertEquals("{el:stash['key'].concat(bean.binding.text)}", BindUtil.prefixMessageExpressions("{el:stash['key'].concat(bean.text)}", "binding"));

		Assert.assertEquals("{el:bean.binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedAssociation}", "binding"));
		Assert.assertEquals("{el:empty bean.binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{el:empty bean.aggregatedAssociation}", "binding"));
		Assert.assertEquals("{el:bean.binding.aggregatedCollection}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedCollection}", "binding"));
		Assert.assertEquals("{el:bean.binding.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", "binding"));
		Assert.assertEquals("{el:bean.binding.date.after(newDateOnly())}", ExpressionEvaluator.prefixBinding("{el:bean.date.after(newDateOnly())}", "binding"));
		Assert.assertEquals("{el:bean.binding.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", "binding"));
		Assert.assertEquals("{el:bean.binding.date.after(newDateOnly())}", ExpressionEvaluator.prefixBinding("{el:bean.date.after(newDateOnly())}", "binding"));
		Assert.assertEquals("{el:bean.binding.getDate().after(bean.binding.getDate())}", ExpressionEvaluator.prefixBinding("{el:bean.getDate().after(bean.getDate())}", "binding"));
		Assert.assertEquals("{el:empty bean.binding.date or bean.binding.getDate().after(bean.binding.getDate())}", ExpressionEvaluator.prefixBinding("{el:empty bean.date or bean.getDate().after(bean.getDate())}", "binding"));
		Assert.assertEquals("{el:bean.binding.date}", ExpressionEvaluator.prefixBinding("{el:bean.date}", "binding"));
		Assert.assertEquals("{el:empty bean.binding.date or not empty bean.binding.date}", ExpressionEvaluator.prefixBinding("{el:empty bean.date or not empty bean.date}", "binding"));
		Assert.assertEquals("{el:bean.binding.normalInteger + bean.binding.normalInteger}", ExpressionEvaluator.prefixBinding("{el:bean.normalInteger + bean.normalInteger}", "binding"));
		Assert.assertEquals("{el:bean.binding}", ExpressionEvaluator.prefixBinding("{el:bean}", "binding"));
		Assert.assertEquals("{el:bean.binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedAssociation}", "binding"));
		Assert.assertEquals("{binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{aggregatedAssociation}", "binding"));
		Assert.assertEquals("{binding.bizId}", ExpressionEvaluator.prefixBinding("{bizId}", "binding"));
		Assert.assertEquals("{binding.aggregatedAssociation.bizId}", ExpressionEvaluator.prefixBinding("{aggregatedAssociation.bizId}", "binding"));
		Assert.assertEquals("{binding.aggregatedCollection}", ExpressionEvaluator.prefixBinding("{aggregatedCollection}", "binding"));
		Assert.assertEquals("{binding.date}", ExpressionEvaluator.prefixBinding("{date}", "binding"));
		Assert.assertEquals("{binding.condition}", ExpressionEvaluator.prefixBinding("{condition}", "binding"));
		Assert.assertEquals("{binding.aggregatedAssociation.condition}", ExpressionEvaluator.prefixBinding("{aggregatedAssociation.condition}", "binding"));

		Assert.assertEquals("{rtel:newDateOnly()}", BindUtil.prefixMessageExpressions("{rtel:newDateOnly()}", "binding"));
		Assert.assertEquals("{el:newDateOnly()}", BindUtil.prefixMessageExpressions("{el:newDateOnly()}", "binding"));
		Assert.assertEquals("{rtel:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", BindUtil.prefixMessageExpressions("{rtel:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", "binding"));
		Assert.assertEquals("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", BindUtil.prefixMessageExpressions("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", "binding"));
		Assert.assertEquals("{rtel:newTimeOnly()}", BindUtil.prefixMessageExpressions("{rtel:newTimeOnly()}", "binding"));
		Assert.assertEquals("{el:newTimeOnly()}", BindUtil.prefixMessageExpressions("{el:newTimeOnly()}", "binding"));
		Assert.assertEquals("{rtel:newDateTime()}", BindUtil.prefixMessageExpressions("{rtel:newDateTime()}", "binding"));
		Assert.assertEquals("{el:newDateTime()}", BindUtil.prefixMessageExpressions("{el:newDateTime()}", "binding"));
		Assert.assertEquals("{rtel:newTimestamp()}", BindUtil.prefixMessageExpressions("{rtel:newTimestamp()}", "binding"));
		Assert.assertEquals("{el:newTimestamp()}", BindUtil.prefixMessageExpressions("{el:newTimestamp()}", "binding"));
		
		Assert.assertEquals("{el:Decimal2.ZERO}", BindUtil.prefixMessageExpressions("{el:Decimal2.ZERO}", "binding"));
		Assert.assertEquals("{el:newDecimal2(100)}", BindUtil.prefixMessageExpressions("{el:newDecimal2(100)}", "binding"));
	}

	/**
	 * Test that a DynamicBean with a display binding with a dynamic domain defined by no THIS_ALIAS returns code.
	 */
	@Test
	public void testDynamicBeanWithNoThisReturnsDynamicDomainCode() {
		Map<String, Object> properties = new TreeMap<>();
		properties.put(Snapshot.queryNamePropertyName, "dynamicDomainValue");
		DynamicBean bean = new DynamicBean(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME, properties);
		Assert.assertEquals("Dynamic domain code value with no THIS_ALIAS should return the code value for a DynamicBean",
								"dynamicDomainValue",
								BindUtil.getDisplay(c, bean, Snapshot.queryNamePropertyName));
	}
	
	@Test
	public void testSanitiseAsFunction() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		aap.setText("Test<script>alert(1)</script>Me");

		Assert.assertEquals("Format Message with sanitise function should remove script tag", 
								"<h1>TestMe</h1>",
								BindUtil.formatMessage("<h1>{text}</h1>", displayName -> OWASP.sanitise(Sanitisation.relaxed, displayName), aap));
	}

	@Test(expected = MetaDataException.class)
	public void testGetMetaDataForBindingThrowsOnParentBindingOfNonChildDocument() {
		BindUtil.getMetaDataForBinding(c, m, aapd, ChildBean.PARENT_NAME);
	}

	@Test(expected = MetaDataException.class)
	public void testGetMetaDataForBindingThrowsOnCompoundParentBindingOfNonChildDocument() {
		BindUtil.getMetaDataForBinding(c, m, aapd, AllAttributesPersistent.aggregatedAssociationPropertyName + ChildBean.CHILD_PARENT_NAME_SUFFIX);
	}
	
	@Test(expected = MetaDataException.class)
	public void testGetMetaDataForBindingThrowsOnCompoundBinding() {
		BindUtil.getMetaDataForBinding(c, m, aapd, "bogusPropertyName" + ChildBean.CHILD_PARENT_NAME_SUFFIX);
	}

	@Test
	public void testGetMetaDataForBinding() throws Exception {
		org.skyve.metadata.module.Module admin = c.getModule(Contact.MODULE_NAME);
		Document user = admin.getDocument(c, User.DOCUMENT_NAME);
		Document userRole = admin.getDocument(c, UserRole.DOCUMENT_NAME);
		
		BindUtil.getMetaDataForBinding(c, admin, userRole, UserRole.roleNamePropertyName);
		BindUtil.getMetaDataForBinding(c, admin, userRole, ChildBean.PARENT_NAME);
		BindUtil.getMetaDataForBinding(c, admin, userRole, BindUtil.createCompoundBinding(ChildBean.PARENT_NAME, Bean.DOCUMENT_ID));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIndexedBinding(User.rolesPropertyName, 0), ChildBean.PARENT_NAME));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIdBinding(User.rolesPropertyName, "ID"), ChildBean.PARENT_NAME));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIndexedBinding(User.rolesPropertyName, 0), ChildBean.PARENT_NAME, Bean.DOCUMENT_ID));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIdBinding(User.rolesPropertyName, "ID"), ChildBean.PARENT_NAME, Bean.DOCUMENT_ID));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIndexedBinding(User.rolesPropertyName, 0), ChildBean.PARENT_NAME, User.userNamePropertyName));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIdBinding(User.rolesPropertyName, "ID"), ChildBean.PARENT_NAME, User.userNamePropertyName));
	}
}
