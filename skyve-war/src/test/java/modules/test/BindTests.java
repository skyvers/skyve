package modules.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
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
import modules.admin.domain.UserProxy;
import modules.admin.domain.UserRole;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesPersistent.Enum3;

class BindTests extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void testSanitizeBinding() {
		assertNull(BindUtil.sanitiseBinding(null));
		assertEquals("test", BindUtil.sanitiseBinding("test"));
		assertEquals("test_test", BindUtil.sanitiseBinding("test.test"));
		assertEquals("test_test_test", BindUtil.sanitiseBinding("test.test.test"));
		assertEquals("test1_test2_test3", BindUtil.sanitiseBinding("test1.test2.test3"));
		assertEquals("test_100__test_test", BindUtil.sanitiseBinding("test[100].test.test"));
		assertEquals("test_test_0__test", BindUtil.sanitiseBinding("test.test[0].test"));
		assertEquals("test_100__test_0__test", BindUtil.sanitiseBinding("test[100].test[0].test"));
		assertEquals("test_100__test_0__test_1_", BindUtil.sanitiseBinding("test[100].test[0].test[1]"));
		assertEquals("test1_100__test2_0__test3_1_", BindUtil.sanitiseBinding("test1[100].test2[0].test3[1]"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testUnsanitizeBinding() {
		assertNull(BindUtil.unsanitiseBinding(null));
		assertEquals("test", BindUtil.unsanitiseBinding("test"));
		assertEquals("test.test", BindUtil.unsanitiseBinding("test_test"));
		assertEquals("test.test.test", BindUtil.unsanitiseBinding("test_test_test"));
		assertEquals("test1.test2.test3", BindUtil.unsanitiseBinding("test1_test2_test3"));
		assertEquals("test[100].test.test", BindUtil.unsanitiseBinding("test_100__test_test"));
		assertEquals("test.test[0].test", BindUtil.unsanitiseBinding("test_test_0__test"));
		assertEquals("test[100].test[0].test", BindUtil.unsanitiseBinding("test_100__test_0__test"));
		assertEquals("test[100].test[0].test[1]", BindUtil.unsanitiseBinding("test_100__test_0__test_1_"));
		assertEquals("test1[100].test2[0].test3[1]", BindUtil.unsanitiseBinding("test1_100__test2_0__test3_1_"));
	}
	
	@Test
	@SuppressWarnings({"static-method", "java:S5961"})
	void testGeneratedJavaIdentifier() {
		assertEquals("one", BindUtil.toJavaInstanceIdentifier("1"));
		assertEquals("two", BindUtil.toJavaInstanceIdentifier("2"));
		assertEquals("three", BindUtil.toJavaInstanceIdentifier("3"));
		assertEquals("four", BindUtil.toJavaInstanceIdentifier("4"));
		assertEquals("five", BindUtil.toJavaInstanceIdentifier("5"));
		assertEquals("six", BindUtil.toJavaInstanceIdentifier("6"));
		assertEquals("seven", BindUtil.toJavaInstanceIdentifier("7"));
		assertEquals("eight", BindUtil.toJavaInstanceIdentifier("8"));
		assertEquals("nine", BindUtil.toJavaInstanceIdentifier("9"));
		assertEquals("one", BindUtil.toJavaInstanceIdentifier("_1"));
		assertEquals("two", BindUtil.toJavaInstanceIdentifier("_2"));
		assertEquals("three", BindUtil.toJavaInstanceIdentifier("_3"));
		assertEquals("four", BindUtil.toJavaInstanceIdentifier("_4"));
		assertEquals("five", BindUtil.toJavaInstanceIdentifier("_5"));
		assertEquals("six", BindUtil.toJavaInstanceIdentifier("_6"));
		assertEquals("seven", BindUtil.toJavaInstanceIdentifier("_7"));
		assertEquals("eight", BindUtil.toJavaInstanceIdentifier("_8"));
		assertEquals("nine", BindUtil.toJavaInstanceIdentifier("_9"));
		assertEquals("one1", BindUtil.toJavaInstanceIdentifier("11"));
		assertEquals("two2", BindUtil.toJavaInstanceIdentifier("22"));
		assertEquals("three3", BindUtil.toJavaInstanceIdentifier("33"));
		assertEquals("four4", BindUtil.toJavaInstanceIdentifier("44"));
		assertEquals("five5", BindUtil.toJavaInstanceIdentifier("55"));
		assertEquals("six6", BindUtil.toJavaInstanceIdentifier("66"));
		assertEquals("seven7", BindUtil.toJavaInstanceIdentifier("77"));
		assertEquals("eight8", BindUtil.toJavaInstanceIdentifier("88"));
		assertEquals("nine9", BindUtil.toJavaInstanceIdentifier("99"));
		assertEquals("v1", BindUtil.toJavaInstanceIdentifier("v1"));
		assertEquals("v2", BindUtil.toJavaInstanceIdentifier("v2"));
		assertEquals("v3", BindUtil.toJavaInstanceIdentifier("v3"));
		assertEquals("v4", BindUtil.toJavaInstanceIdentifier("v4"));
		assertEquals("v5", BindUtil.toJavaInstanceIdentifier("v5"));
		assertEquals("v6", BindUtil.toJavaInstanceIdentifier("v6"));
		assertEquals("v7", BindUtil.toJavaInstanceIdentifier("v7"));
		assertEquals("v8", BindUtil.toJavaInstanceIdentifier("v8"));
		assertEquals("v9", BindUtil.toJavaInstanceIdentifier("v9"));
	}
	
	@Test
	void testCopyProperties() throws Exception {
		AllAttributesPersistent from = Util.constructRandomInstance(u, m, aapd, 2);
		AllAttributesPersistent to = AllAttributesPersistent.newInstance();
		Binder.copy(from, to);
		for (Attribute a: aapd.getAttributes()) {
			String name = a.getName();
					assertEquals(Binder.get(from, name), Binder.get(to, name), "Property Name " + name);
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testCopyPropertiesWithChildCollection() {
		UserExtension from = User.newInstance();
		UserRole role = UserRole.newInstance();
		from.getRoles().add(role);
		role.setParent(from);
		User to = User.newInstance();
		Binder.copy(from, to);
		assertEquals(role.getParent(), to);
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSimpleDynamicBeanProperty() {
		Map<String, Object> map = new TreeMap<>();
		map.put(UserProxy.userNamePropertyName, "mike");
		DynamicBean bean = new DynamicBean(User.MODULE_NAME, User.DOCUMENT_NAME, map);
		assertEquals("mike", Binder.get(bean, UserProxy.userNamePropertyName));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCompoundDynamicBeanProperty() {
		String binding = Binder.createCompoundBinding(UserProxy.contactPropertyName, Contact.namePropertyName);
		Map<String, Object> map = new TreeMap<>();
		map.put(binding, "mike");
		DynamicBean bean = new DynamicBean(User.MODULE_NAME, User.DOCUMENT_NAME, map);
		assertEquals("mike", Binder.get(bean, binding));
	}
	
	@Test
	void testSimpleThisProperty() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		assertTrue(Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
	}
	
	@Test
	void testCompoundThisProperty() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	void testSimpleMapPropertyOverThisProperty() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		map.put(AllAttributesPersistent.booleanFlagPropertyName, null);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		assertFalse(Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
	}

	@Test
	void testCompoundMapPropertyOverThisProperty() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DynamicBean.BEAN_PROPERTY_KEY, aap);
		map.put(binding, null);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		assertFalse(Binder.get(bean, binding) instanceof Boolean);
	}
	
	@Test
	void testCompoundPropertyWithoutMappedProperty2Deep() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(AllAttributesPersistent.aggregatedAssociationPropertyName, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	void testCompoundPropertyWithoutMappedProperty3Deep() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(AllAttributesPersistent.aggregatedAssociationPropertyName, aap);
		DynamicBean bean = new DynamicBean(m.getName(), aapd.getName(), map);
		assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	void testFormatMessage() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		aap.setText("Test");
		assertEquals("Test", Binder.formatMessage("Test", aap));
		assertEquals("Test", Binder.formatMessage("{text}", aap));
		assertEquals("TestTest", Binder.formatMessage("{text}{text}", aap));
		// Test escapes
		assertEquals("{text}Test", Binder.formatMessage("\\{text\\}{text}", aap));
		assertEquals("{textTest", Binder.formatMessage("\\{text{text}", aap));
		assertEquals("text}Test", Binder.formatMessage("text\\}{text}", aap));
		assertEquals("{text", Binder.formatMessage("\\{text", aap));
		aap.setText("{text}");
		// Test '{' and '}' are left in tact when they are part of the value substituted
		assertEquals("{text}{text}", Binder.formatMessage("{text}{text}", aap));
	}
	
	@Test
	void testDanglingMessageFormat() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		MetaDataException mde = assertThrows(MetaDataException.class, () -> Binder.formatMessage("{text", aap));

		assertThat(mde.getMessage(), is(notNullValue()));
	}
	
	@Test
	@SuppressWarnings("java:S5961")
	void testExpressions() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 2);
		bean.setText("Test");
		CORE.getStash().put("text", "Stash");
		CORE.getUser().getAttributes().put("text", "Attribute");
		
		assertEquals("TestUser", Binder.formatMessage("{USER}", bean));
		assertEquals("TestUser", Binder.formatMessage("{USERID}", bean));
		assertEquals("", Binder.formatMessage("{USERNAME}", bean));
		assertEquals("", Binder.formatMessage("{DATAGROUPID}", bean));
		assertNotEquals("", Binder.formatMessage("{CONTACTID}", bean));
		assertEquals("bizhub", Binder.formatMessage("{CUSTOMER}", bean));

		String currentDate = CORE.getCustomer().getDefaultDateConverter().toDisplayValue(new DateOnly());
		String currentTime = CORE.getCustomer().getDefaultTimeConverter().toDisplayValue(new TimeOnly());
		String currentDateTime = CORE.getCustomer().getDefaultDateTimeConverter().toDisplayValue(new DateTime());
		String currentTimestamp = CORE.getCustomer().getDefaultTimestampConverter().toDisplayValue(new Timestamp());
		
		assertEquals(currentDate, Binder.formatMessage("{DATE}", bean));
		assertEquals(currentTime, Binder.formatMessage("{TIME}", bean));
		assertEquals(currentDateTime, Binder.formatMessage("{DATETIME}", bean));
		assertEquals(currentTimestamp, Binder.formatMessage("{TIMESTAMP}", bean));
		assertEquals(Util.getDocumentUrl(bean), Binder.formatMessage("{URL}", bean));
		
		assertEquals("Test", Binder.formatMessage("{text}", bean));
		assertEquals("Test", Binder.formatMessage("{ text }", bean));
		assertEquals("Test", Binder.formatMessage("{bean:text}", bean));
		assertEquals("Test", Binder.formatMessage("{bean: text }", bean));
		assertEquals("Test", Binder.formatMessage("{bean : text }", bean));
		assertEquals("Text", Binder.formatMessage("{disp:text}", bean));
		assertEquals("", Binder.formatMessage("{desc:text}", bean));
		assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		assertEquals("Stash", Binder.formatMessage("{el:stash['text']}", bean));
		assertEquals("Attribute", Binder.formatMessage("{el:user.attributes['text']}", bean));
		assertEquals("", Binder.formatMessage("{el:stash['nothing']}", bean));
		assertEquals("", Binder.formatMessage("{el:user.attributes['nothing']}", bean));

		assertEquals("some.non-existent.key", Binder.formatMessage("{i18n:some.non-existent.key}", bean));
		assertEquals("Yes", Binder.formatMessage("{role:admin.BasicUser}", bean));
		assertEquals("Stash", Binder.formatMessage("{stash:text}", bean));
		assertEquals("", Binder.formatMessage("{stash:nothing}", bean));
		assertEquals("Attribute", Binder.formatMessage("{user:text}", bean));
		assertEquals("", Binder.formatMessage("{user:nothing}", bean));
		
		// Test functions and imports
		// NB re-evaluate these in case we've ticked over a boundary in the mean time. 
		currentDate = CORE.getCustomer().getDefaultDateConverter().toDisplayValue(new DateOnly());
		String tomorrowsDate = CORE.getCustomer().getDefaultDateConverter().toDisplayValue(Time.addDaysToNew(new DateOnly(), 1));
		currentTime = CORE.getCustomer().getDefaultTimeConverter().toDisplayValue(new TimeOnly());
		currentDateTime = CORE.getCustomer().getDefaultDateTimeConverter().toDisplayValue(new DateTime());
		currentTimestamp = CORE.getCustomer().getDefaultTimestampConverter().toDisplayValue(new Timestamp());

		assertEquals(currentDate, Binder.formatMessage("{el:newDateOnly()}", bean));
		assertEquals(tomorrowsDate, Binder.formatMessage("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", bean));
		assertEquals(String.valueOf(new DateOnly().toLocalDate().getYear()),
				Binder.formatMessage("{el:newDateOnly().toLocalDate().getYear()}", bean));
		assertEquals(currentTime, Binder.formatMessage("{el:newTimeOnly()}", bean));
		assertEquals(currentDateTime, Binder.formatMessage("{el:newDateTime()}", bean));
		assertEquals(currentTimestamp, Binder.formatMessage("{el:newTimestamp()}", bean));
		assertEquals("0.00", Binder.formatMessage("{el:Decimal2.ZERO}", bean));
		assertEquals("100.00", Binder.formatMessage("{el:newDecimal2(100)}", bean));
	}
	
	@Test
	@SuppressWarnings("java:S5961")
	void testExpressionFormatting() throws Exception {
		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 2);
		bean.setDecimal2(Decimal2.ONE_THOUSAND);
		bean.setDecimal5(Decimal5.ONE_HUNDRED);
		bean.setDecimal10(Decimal10.ONE_HUNDRED);
		
		final String currentDateString = FormatterName.DD_MMM_YYYY.getFormatter().toDisplayValue(new DateOnly());
		final String currentTimeString = FormatterName.HH24_MI.getFormatter().toDisplayValue(new TimeOnly());
		final String currentDateTimeString = FormatterName.YYYY_MM_DD_HH24_MI.getFormatter().toDisplayValue(new DateTime());
		final String currentTimestampString = FormatterName.MMM_DD_YYYY_HH24_MI_SS.getFormatter().toDisplayValue(new Timestamp());
		
		assertEquals(currentDateString, Binder.formatMessage("{DATE|DD_MMM_YYYY}", bean));
		assertEquals(currentTimeString, Binder.formatMessage("{TIME|HH24_MI}", bean));
		assertEquals(currentDateTimeString, Binder.formatMessage("{DATETIME|YYYY_MM_DD_HH24_MI}", bean));
		assertEquals(currentTimestampString, Binder.formatMessage("{TIMESTAMP|MMM_DD_YYYY_HH24_MI_SS}", bean));
		assertEquals("01-Jan-1970", Binder.formatMessage("{TIME|DD_MMM_YYYY}", bean));
		assertEquals(currentDateString, Binder.formatMessage("{DATETIME|DD_MMM_YYYY}", bean));
		assertEquals(currentDateString, Binder.formatMessage("{TIMESTAMP|DD_MMM_YYYY}", bean));
		
		assertEquals("1,000.0", Binder.formatMessage("{decimal2|OneDecimalPlace}", bean));
		assertEquals("1,000.00", Binder.formatMessage("{ decimal2 | TwoDecimalPlaces }", bean));
		assertEquals("1,000.000", Binder.formatMessage("{bean:decimal2|ThreeDecimalPlaces}", bean));
		assertEquals("1,000.0000", Binder.formatMessage("{bean: decimal2 | FourDecimalPlaces }", bean));
		assertEquals("1,000.00000", Binder.formatMessage("{bean : decimal2 | FiveDecimalPlaces }", bean));

		assertEquals("100.0", Binder.formatMessage("{decimal5|OneDecimalPlace}", bean));
		assertEquals("100.00", Binder.formatMessage("{ decimal5 | TwoDecimalPlaces }", bean));
		assertEquals("100.000", Binder.formatMessage("{bean:decimal5|ThreeDecimalPlaces}", bean));
		assertEquals("100.0000", Binder.formatMessage("{bean: decimal5 | FourDecimalPlaces }", bean));
		assertEquals("100.00000", Binder.formatMessage("{bean : decimal5 | FiveDecimalPlaces }", bean));

		assertEquals("100", Binder.formatMessage("{decimal10|OneOptionalDecimalPlace}", bean));
		assertEquals("100", Binder.formatMessage("{ decimal10 | TwoOptionalDecimalPlaces }", bean));
		assertEquals("100", Binder.formatMessage("{bean:decimal10|ThreeOptionalDecimalPlaces}", bean));
		assertEquals("100", Binder.formatMessage("{bean: decimal10 | FourOptionalDecimalPlaces }", bean));
		assertEquals("100", Binder.formatMessage("{bean : decimal10 | FiveOptionalDecimalPlaces }", bean));

		bean.setDecimal10(new Decimal10("1.0123456789"));
		assertEquals("1", Binder.formatMessage("{decimal10|OneOptionalDecimalPlace}", bean));
		assertEquals("1.01", Binder.formatMessage("{ decimal10 | TwoOptionalDecimalPlaces }", bean));
		assertEquals("1.012", Binder.formatMessage("{bean:decimal10|ThreeOptionalDecimalPlaces}", bean));
		assertEquals("1.0123", Binder.formatMessage("{bean: decimal10 | FourOptionalDecimalPlaces }", bean));
		assertEquals("1.01235", Binder.formatMessage("{bean : decimal10 | FiveOptionalDecimalPlaces }", bean));
		assertEquals("1.012346", Binder.formatMessage("{bean : decimal10 | SixOptionalDecimalPlaces }", bean));

		assertEquals("1.01", Binder.formatMessage("{el:bean.decimal10|TwoOptionalDecimalPlaces}", bean));
		assertEquals("0", Binder.formatMessage("{el:Decimal2.ZERO|TwoOptionalDecimalPlaces}", bean));
		assertEquals("100", Binder.formatMessage("{el:newDecimal2(100)|TwoOptionalDecimalPlaces}", bean));

		bean.setText("<script>alert('yikes')</script><span/><div><p/><table style=\"\"><tr/><b>Bold</b></tr></table></div><style>.shite {}</style>");
		assertEquals("<div><p></p><table><tbody><tr></tr></tbody></table><b>Bold</b></div>", Binder.formatMessage("{text|RelaxedHTML}", bean));
		assertEquals("&lt;div&gt;&lt;p&gt;&lt;/p&gt;&lt;table&gt;&lt;tbody&gt;&lt;tr&gt;&lt;/tr&gt;&lt;/tbody&gt;&lt;/table&gt;&lt;b&gt;Bold&lt;/b&gt;&lt;/div&gt;",
								Binder.formatMessage("{text|RelaxedEscapedHTML}", bean));
		assertEquals("<div><p></p><b>Bold</b></div>", Binder.formatMessage("{text|SimpleHTML}", bean));
		assertEquals("&lt;div&gt;&lt;p&gt;&lt;/p&gt;&lt;b&gt;Bold&lt;/b&gt;&lt;/div&gt;", Binder.formatMessage("{text|SimpleEscapedHTML}", bean));
		assertEquals("<b>Bold</b>", Binder.formatMessage("{text|BasicHTML}", bean));
		assertEquals("&lt;b&gt;Bold&lt;/b&gt;", Binder.formatMessage("{text|BasicEscapedHTML}", bean));
		assertEquals("Bold", Binder.formatMessage("{text|TextHTML}", bean));
		assertEquals("Bold", Binder.formatMessage("{text|TextEscapedHTML}", bean));
		assertEquals("&lt;script&gt;alert(&#39;yikes&#39;)&lt;/script&gt;&lt;span/&gt;&lt;div&gt;&lt;p/&gt;&lt;table style=&#34;&#34;&gt;&lt;tr/&gt;&lt;b&gt;Bold&lt;/b&gt;&lt;/tr&gt;&lt;/table&gt;&lt;/div&gt;&lt;style&gt;.shite {}&lt;/style&gt;",
								Binder.formatMessage("{text|EscapedHTML}", bean));
		bean.setText("{\"poo\": \"wee\"}");
		assertEquals("{\\\"poo\\\": \\\"wee\\\"}", Binder.formatMessage("{text|EscapedJSONString}", bean));
		assertEquals("{&quot;poo&quot;: &quot;wee&quot;}", Binder.formatMessage("{text|EscapedJSString}", bean));
	}

	@Test
	@SuppressWarnings("static-method")
	void testExtraDynamicPropertyWithMethodCall() {
		TreeMap<String, Object> map = new TreeMap<>();
		DynamicBean bean = new DynamicBean("admin", "Contact", map);
		bean.putDynamic("test", new StringBuilder());
		Binder.formatMessage("{el:bean.test.append('test')}", bean);
		assertEquals("test", bean.get("test").toString());
	}
	
	@Test
	void testDynamicExpressions() throws Exception {
		DynamicPersistentBean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		Binder.set(bean, AllAttributesPersistent.textPropertyName, "Test");
		
		assertEquals("Test", Binder.formatMessage("{text}", bean));
		assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{condition}", bean));
		assertEquals("Test", Binder.formatMessage("{bean:text}", bean));
		assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{el:bean.condition}", bean));

		// Check falsey boolean evaluation
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, Boolean.TRUE);
		assertEquals(Boolean.TRUE, ExpressionEvaluator.evaluate("{el:bean.falseyBooleanEvaluation}", bean));
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, Boolean.FALSE);
		assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{el:bean.falseyBooleanEvaluation}", bean));
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, null);
		assertEquals(Boolean.FALSE, ExpressionEvaluator.evaluate("{el:bean.falseyBooleanEvaluation}", bean));
		
		bean = aadpd.newInstance(u);
		System.out.println(bean);
		System.out.println();
	}
	
	@Test
	@SuppressWarnings("java:S5961")
	void testDynamicDefaults() throws Exception {
		Bean bean = aadpd.newInstance(u);
		assertEquals(Boolean.TRUE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		assertEquals("#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName));
		assertEquals(new DateOnly("2021-10-21"), Binder.get(bean, AllAttributesPersistent.datePropertyName));
		assertEquals(new DateTime("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.dateTimePropertyName));
		assertEquals(new Decimal10(100.1234567899), Binder.get(bean, AllAttributesPersistent.decimal10PropertyName));
		assertEquals(new Decimal2(100.12), Binder.get(bean, AllAttributesPersistent.decimal2PropertyName));
		assertEquals(new Decimal5(100.12345), Binder.get(bean, AllAttributesPersistent.decimal5PropertyName));
		// NB This can't be a real enum value coz there is no generated class
		assertEquals("one", Binder.get(bean, AllAttributesPersistent.enum3PropertyName));
		// NB This is a real enum value coz its a dynamic reference to a generated class
		assertEquals(Enum3.one, Binder.get(bean, "enum3Reference"));
		assertEquals(new WKTReader().read("POINT(0 0)"), Binder.get(bean, AllAttributesPersistent.geometryPropertyName));
		assertEquals("1234567890", Binder.get(bean, AllAttributesPersistent.idPropertyName));
		assertEquals(Integer.valueOf(123), Binder.get(bean,  AllAttributesPersistent.normalIntegerPropertyName));
		assertEquals(Long.valueOf(123), Binder.get(bean,  AllAttributesPersistent.longIntegerPropertyName));
		assertEquals("<h1>Markup</h1>", Binder.get(bean, AllAttributesPersistent.markupPropertyName));
		assertEquals("Memo", Binder.get(bean, AllAttributesPersistent.memoPropertyName));
		assertEquals("Text", Binder.get(bean, AllAttributesPersistent.textPropertyName));
		assertEquals(new TimeOnly("07:51:26"), Binder.get(bean, AllAttributesPersistent.timePropertyName));
		assertEquals(new Timestamp("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.timestampPropertyName));
		
		bean = aapd.newInstance(u);
		assertEquals(Boolean.FALSE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		assertEquals("#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName));
		assertEquals(new DateOnly(), Binder.get(bean, AllAttributesPersistent.datePropertyName));
		assertEquals(new DateTime(), Binder.get(bean, AllAttributesPersistent.dateTimePropertyName));
		assertEquals(Decimal10.ZERO, Binder.get(bean, AllAttributesPersistent.decimal10PropertyName));
		assertEquals(Decimal2.ZERO, Binder.get(bean, AllAttributesPersistent.decimal2PropertyName));
		assertEquals(Decimal5.ZERO, Binder.get(bean, AllAttributesPersistent.decimal5PropertyName));
		assertEquals(Enum3.one, Binder.get(bean, AllAttributesPersistent.enum3PropertyName));
		assertEquals(new WKTReader().read("POINT(0 0)"), Binder.get(bean, AllAttributesPersistent.geometryPropertyName));
		assertEquals("Test POINT (0 0)", Binder.get(bean, AllAttributesPersistent.memoPropertyName));
	}

	@Test
	@SuppressWarnings("java:S5961")
	void testExpressionValidation() {
		// Test implicit expressions
		assertNull(BindUtil.validateMessageExpressions("{USER}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{USERID}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{USERNAME}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{DATAGROUPID}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{CONTACTID}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{CUSTOMER}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{DATE}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{TIME}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{DATETIME}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{TIMESTAMP}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{URL}", c, aapd));
		
		// Test basic expressions and trimming
		assertNull(BindUtil.validateMessageExpressions("{text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{ text }", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{bean:text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{bean: text }", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{bean : text }", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{disp:text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{desc:text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:bean.doesNotExist}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{el:bean.doesNotExist}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:bean..doesNotExist}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{el:bean..doesNotExist}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:stash['text']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:stash['text']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:user.attributes['text']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:user.attributes['text']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:stash['nothing']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:stash['nothing']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:user.attributes['nothing']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:user.attributes['nothing']}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{i18n:some.non-existent.key}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{role:admin.BasicUser}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{stash:text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{stash:nothing}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{user:text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{user:nothing}", c, aapd));
		
		// Test malformed and escaped expressions
		assertNotNull(BindUtil.validateMessageExpressions("{", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{|}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{ |}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{| }", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{|", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{ |", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{bean:}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{bean:|}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{bean: |}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{bean:| }", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{bean:|", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{bean: |", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("|}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("|", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{text\\}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("\\{text}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("\\{{text}", c, aapd));
		
		// Test EL operators
		assertNull(BindUtil.validateMessageExpressions("{el:bean.normalInteger + bean.normalInteger}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:bean.longInteger + bean.longInteger}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:bean.decimal2.bigDecimalValue() + bean.decimal2.bigDecimalValue()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:bean.decimal5.add(bean.decimal5)}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:bean.date == bean.date}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:empty bean.date}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:not empty bean.date and bean.booleanFlag}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:empty bean.date ? DATE : bean.date}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:bean.text.concat(bean.text)}", c, aapd));

		// Stash operation puts that expression evaluation into typeless mode
		assertNull(BindUtil.validateMessageExpressions("{el:stash['key'].concat(bean.text)}", c, aapd));
		// Stashy is still a problem though
		assertNotNull(BindUtil.validateMessageExpressions("{el:stashy['key'].concat(bean.text)}", c, aapd));
		// And the other expression for the concat parameter is still validated
		assertNotNull(BindUtil.validateMessageExpressions("{el:stash['key'].concat(bean.texty)}", c, aapd));
		// But concaty method name is not chacked coz we lost the type information retrieving from the stash
		assertNull(BindUtil.validateMessageExpressions("{el:stash['key'].concaty(bean.text)}", c, aapd));

		// Test type validation
		assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedAssociation}", Bean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:empty bean.aggregatedAssociation}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedCollection}", List.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.date.after(newDateOnly())}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.date.after(newDateOnly())}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.getDate().after(bean.getDate())}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:empty bean.date or bean.getDate().after(bean.getDate())}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.date}", DateOnly.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:empty bean.date or not empty bean.date}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.normalInteger + bean.normalInteger}", Number.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean}", AllAttributesPersistent.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{el:bean.aggregatedAssociation}", AllAttributesPersistent.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{aggregatedAssociation}", AllAttributesPersistent.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{aggregatedAssociation}", Bean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{bizId}", String.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{aggregatedAssociation.bizId}", String.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{aggregatedCollection}", List.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{date}", DateOnly.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{condition}", Boolean.class, c, m, aapd));
		assertNull(ExpressionEvaluator.validate("{aggregatedAssociation.condition}", Boolean.class, c, m, aapd));

		// Test formatter validation
		assertNotNull(BindUtil.validateMessageExpressions("{USER|Bogus}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{USER|DD_MMM_YYYY}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{TIMESTAMP|DD_MMM_YYYY}", c, aapd));
		assertNotNull(BindUtil.validateMessageExpressions("{TIMESTAMP|OneDecimalPlace}", c, aapd));
		// Any formatter should return a String
		assertNotNull(ExpressionEvaluator.validate("{date|MM_DD_YYYY}", DateOnly.class, c, m, aapd));
		assertNotNull(ExpressionEvaluator.validate("{el:bean.date|DD_MMM_YYYY}", String.class, c, m, aapd));
		
		// Test functions
		assertNull(BindUtil.validateMessageExpressions("{rtel:newDateOnly()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:newDateOnly()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:newTimeOnly()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:newTimeOnly()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:newDateTime()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:newDateTime()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{rtel:newTimestamp()}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:newTimestamp()}", c, aapd));
		
		assertNull(BindUtil.validateMessageExpressions("{el:Decimal2.ZERO}", c, aapd));
		assertNull(BindUtil.validateMessageExpressions("{el:newDecimal2(100)}", c, aapd));
	}
	
	@Test
	@SuppressWarnings({"static-method", "java:S5961"})
	void testExpressionPrefixing() {
		assertEquals("{USER}", BindUtil.prefixMessageExpressions("{USER}", "binding"));
		assertEquals("{USERID}", BindUtil.prefixMessageExpressions("{USERID}", "binding"));
		assertEquals("{USERNAME}", BindUtil.prefixMessageExpressions("{USERNAME}", "binding"));
		assertEquals("{DATAGROUPID}", BindUtil.prefixMessageExpressions("{DATAGROUPID}", "binding"));
		assertEquals("{CONTACTID}", BindUtil.prefixMessageExpressions("{CONTACTID}", "binding"));
		assertEquals("{CUSTOMER}", BindUtil.prefixMessageExpressions("{CUSTOMER}", "binding"));
		assertEquals("{DATE}", BindUtil.prefixMessageExpressions("{DATE}", "binding"));
		assertEquals("{TIME}", BindUtil.prefixMessageExpressions("{TIME}", "binding"));
		assertEquals("{DATETIME}", BindUtil.prefixMessageExpressions("{DATETIME}", "binding"));
		assertEquals("{TIMESTAMP}", BindUtil.prefixMessageExpressions("{TIMESTAMP}", "binding"));
		assertEquals("{URL}", BindUtil.prefixMessageExpressions("{URL}", "binding"));

		assertEquals("{binding.text}", BindUtil.prefixMessageExpressions("{text}", "binding"));
		assertEquals("{binding.text}", BindUtil.prefixMessageExpressions("{ text }", "binding"));
		assertEquals("Once {binding.text} {binding.text} Twice", BindUtil.prefixMessageExpressions("Once {text} {text} Twice", "binding"));
		assertEquals("{bean:binding.text}", BindUtil.prefixMessageExpressions("{bean:text}", "binding"));
		assertEquals("{bean:binding.text}", BindUtil.prefixMessageExpressions("{bean: text }", "binding"));
		assertEquals("Once {bean:binding.text} {bean:binding.text} Twice", BindUtil.prefixMessageExpressions("Once {bean:text} {bean:text} Twice", "binding"));
		assertEquals("Once {bean:binding.text} {binding.text} Twice", BindUtil.prefixMessageExpressions("Once {bean:text} {text} Twice", "binding"));
		assertEquals("{bean:binding.text}", BindUtil.prefixMessageExpressions("{bean : text }", "binding"));
		assertEquals("{disp:binding.text}", BindUtil.prefixMessageExpressions("{disp:text}", "binding"));
		assertEquals("{desc:binding.text}", BindUtil.prefixMessageExpressions("{desc:text}", "binding"));
		assertEquals("{rtel:bean.binding.text}", BindUtil.prefixMessageExpressions("{rtel:bean.text}", "binding"));
		assertEquals("{el:bean.binding.text}", BindUtil.prefixMessageExpressions("{el:bean.text}", "binding"));
		assertEquals("{rtel:stash['text']}", BindUtil.prefixMessageExpressions("{rtel:stash['text']}", "binding"));
		assertEquals("{el:stash['text']}", BindUtil.prefixMessageExpressions("{el:stash['text']}", "binding"));
		assertEquals("{rtel:user.attributes['text']}", BindUtil.prefixMessageExpressions("{rtel:user.attributes['text']}", "binding"));
		assertEquals("{el:user.attributes['text']}", BindUtil.prefixMessageExpressions("{el:user.attributes['text']}", "binding"));
		assertEquals("{rtel:stash['nothing']}", BindUtil.prefixMessageExpressions("{rtel:stash['nothing']}", "binding"));
		assertEquals("{el:stash['nothing']}", BindUtil.prefixMessageExpressions("{el:stash['nothing']}", "binding"));
		assertEquals("{rtel:user.attributes['nothing']}", BindUtil.prefixMessageExpressions("{rtel:user.attributes['nothing']}", "binding"));
		assertEquals("{el:user.attributes['nothing']}", BindUtil.prefixMessageExpressions("{el:user.attributes['nothing']}", "binding"));
		assertEquals("{i18n:some.non-existent.key}", BindUtil.prefixMessageExpressions("{i18n:some.non-existent.key}", "binding"));
		assertEquals("{role:admin.BasicUser}", BindUtil.prefixMessageExpressions("{role:admin.BasicUser}", "binding"));
		assertEquals("{stash:text}", BindUtil.prefixMessageExpressions("{stash:text}", "binding"));
		assertEquals("{stash:nothing}", BindUtil.prefixMessageExpressions("{stash:nothing}", "binding"));
		assertEquals("{user:text}", BindUtil.prefixMessageExpressions("{user:text}", "binding"));
		assertEquals("{user:nothing}", BindUtil.prefixMessageExpressions("{user:nothing}", "binding"));
		
		assertEquals("}", BindUtil.prefixMessageExpressions("}", "binding"));
		assertEquals("\\{text}", BindUtil.prefixMessageExpressions("\\{text}", "binding"));
		assertEquals("\\{{binding.text}", BindUtil.prefixMessageExpressions("\\{{text}", "binding"));

		assertEquals("{el:bean.binding.normalInteger + bean.binding.normalInteger}", BindUtil.prefixMessageExpressions("{el:bean.normalInteger + bean.normalInteger}", "binding"));
		assertEquals("{el:bean.binding.longInteger + bean.binding.longInteger}", BindUtil.prefixMessageExpressions("{el:bean.longInteger + bean.longInteger}", "binding"));
		assertEquals("{el:bean.binding.decimal2.bigDecimalValue() + bean.binding.decimal2.bigDecimalValue()}", BindUtil.prefixMessageExpressions("{el:bean.decimal2.bigDecimalValue() + bean.decimal2.bigDecimalValue()}", "binding"));
		assertEquals("{el:bean.binding.decimal5.add(bean.binding.decimal5)}", BindUtil.prefixMessageExpressions("{el:bean.decimal5.add(bean.decimal5)}", "binding"));
		assertEquals("{el:bean.binding.date == bean.binding.date}", BindUtil.prefixMessageExpressions("{el:bean.date == bean.date}", "binding"));
		assertEquals("{el:empty bean.binding.date}", BindUtil.prefixMessageExpressions("{el:empty bean.date}", "binding"));
		assertEquals("{el:not empty bean.binding.date and bean.binding.booleanFlag}", BindUtil.prefixMessageExpressions("{el:not empty bean.date and bean.booleanFlag}", "binding"));
		assertEquals("{el:empty bean.binding.date ? DATE : bean.binding.date}", BindUtil.prefixMessageExpressions("{el:empty bean.date ? DATE : bean.date}", "binding"));
		assertEquals("{el:bean.binding.text.concat(bean.binding.text)}", BindUtil.prefixMessageExpressions("{el:bean.text.concat(bean.text)}", "binding"));

		assertEquals("{el:stash['key'].concat(bean.binding.text)}", BindUtil.prefixMessageExpressions("{el:stash['key'].concat(bean.text)}", "binding"));

		assertEquals("{el:bean.binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedAssociation}", "binding"));
		assertEquals("{el:empty bean.binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{el:empty bean.aggregatedAssociation}", "binding"));
		assertEquals("{el:bean.binding.aggregatedCollection}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedCollection}", "binding"));
		assertEquals("{el:bean.binding.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", "binding"));
		assertEquals("{el:bean.binding.date.after(newDateOnly())}", ExpressionEvaluator.prefixBinding("{el:bean.date.after(newDateOnly())}", "binding"));
		assertEquals("{el:bean.binding.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedCollection.stream().anyMatch(c -> Boolean.FALSE.equals(c.booleanFlag))}", "binding"));
		assertEquals("{el:bean.binding.date.after(newDateOnly())}", ExpressionEvaluator.prefixBinding("{el:bean.date.after(newDateOnly())}", "binding"));
		assertEquals("{el:bean.binding.getDate().after(bean.binding.getDate())}", ExpressionEvaluator.prefixBinding("{el:bean.getDate().after(bean.getDate())}", "binding"));
		assertEquals("{el:empty bean.binding.date or bean.binding.getDate().after(bean.binding.getDate())}", ExpressionEvaluator.prefixBinding("{el:empty bean.date or bean.getDate().after(bean.getDate())}", "binding"));
		assertEquals("{el:bean.binding.date}", ExpressionEvaluator.prefixBinding("{el:bean.date}", "binding"));
		assertEquals("{el:empty bean.binding.date or not empty bean.binding.date}", ExpressionEvaluator.prefixBinding("{el:empty bean.date or not empty bean.date}", "binding"));
		assertEquals("{el:bean.binding.normalInteger + bean.binding.normalInteger}", ExpressionEvaluator.prefixBinding("{el:bean.normalInteger + bean.normalInteger}", "binding"));
		assertEquals("{el:bean.binding}", ExpressionEvaluator.prefixBinding("{el:bean}", "binding"));
		assertEquals("{el:bean.binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{el:bean.aggregatedAssociation}", "binding"));
		assertEquals("{binding.aggregatedAssociation}", ExpressionEvaluator.prefixBinding("{aggregatedAssociation}", "binding"));
		assertEquals("{binding.bizId}", ExpressionEvaluator.prefixBinding("{bizId}", "binding"));
		assertEquals("{binding.aggregatedAssociation.bizId}", ExpressionEvaluator.prefixBinding("{aggregatedAssociation.bizId}", "binding"));
		assertEquals("{binding.aggregatedCollection}", ExpressionEvaluator.prefixBinding("{aggregatedCollection}", "binding"));
		assertEquals("{binding.date}", ExpressionEvaluator.prefixBinding("{date}", "binding"));
		assertEquals("{binding.condition}", ExpressionEvaluator.prefixBinding("{condition}", "binding"));
		assertEquals("{binding.aggregatedAssociation.condition}", ExpressionEvaluator.prefixBinding("{aggregatedAssociation.condition}", "binding"));

		assertEquals("{rtel:newDateOnly()}", BindUtil.prefixMessageExpressions("{rtel:newDateOnly()}", "binding"));
		assertEquals("{el:newDateOnly()}", BindUtil.prefixMessageExpressions("{el:newDateOnly()}", "binding"));
		assertEquals("{rtel:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", BindUtil.prefixMessageExpressions("{rtel:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", "binding"));
		assertEquals("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", BindUtil.prefixMessageExpressions("{el:newDateOnlyFromLocalDate(newDateOnly().toLocalDate().plusDays(1))}", "binding"));
		assertEquals("{rtel:newTimeOnly()}", BindUtil.prefixMessageExpressions("{rtel:newTimeOnly()}", "binding"));
		assertEquals("{el:newTimeOnly()}", BindUtil.prefixMessageExpressions("{el:newTimeOnly()}", "binding"));
		assertEquals("{rtel:newDateTime()}", BindUtil.prefixMessageExpressions("{rtel:newDateTime()}", "binding"));
		assertEquals("{el:newDateTime()}", BindUtil.prefixMessageExpressions("{el:newDateTime()}", "binding"));
		assertEquals("{rtel:newTimestamp()}", BindUtil.prefixMessageExpressions("{rtel:newTimestamp()}", "binding"));
		assertEquals("{el:newTimestamp()}", BindUtil.prefixMessageExpressions("{el:newTimestamp()}", "binding"));
		
		assertEquals("{el:Decimal2.ZERO}", BindUtil.prefixMessageExpressions("{el:Decimal2.ZERO}", "binding"));
		assertEquals("{el:newDecimal2(100)}", BindUtil.prefixMessageExpressions("{el:newDecimal2(100)}", "binding"));
	}

	/**
	 * Test that a DynamicBean with a display binding with a dynamic domain defined by no THIS_ALIAS returns code.
	 */
	@Test
	void testDynamicBeanWithNoThisReturnsDynamicDomainCode() {
		Map<String, Object> properties = new TreeMap<>();
		properties.put(Snapshot.queryNamePropertyName, "dynamicDomainValue");
		DynamicBean bean = new DynamicBean(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME, properties);
		assertEquals("dynamicDomainValue",
									BindUtil.getDisplay(c, bean, Snapshot.queryNamePropertyName),
									"Dynamic domain code value with no THIS_ALIAS should return the code value for a DynamicBean");
	}
	
	@Test
	void testSanitiseAsFunction() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		aap.setText("Test<script>alert(1)</script>Me");

		assertEquals("<h1>TestMe</h1>",
									BindUtil.formatMessage("<h1>{text}</h1>", displayName -> OWASP.sanitise(Sanitisation.relaxed, displayName), aap),
									"Format Message with sanitise function should remove script tag");
	}

	@Test
	void testGetMetaDataForBindingThrowsOnParentBindingOfNonChildDocument() {
		MetaDataException mde = assertThrows(MetaDataException.class, () -> {
			BindUtil.getMetaDataForBinding(c, m, aapd, ChildBean.PARENT_NAME);
		});

		assertThat(mde.getMessage(), is(notNullValue()));
	}

	@Test
	void testGetMetaDataForBindingThrowsOnCompoundParentBindingOfNonChildDocument() {
		MetaDataException mde = assertThrows(MetaDataException.class, () -> {
			BindUtil.getMetaDataForBinding(c, m, aapd,
					AllAttributesPersistent.aggregatedAssociationPropertyName + ChildBean.CHILD_PARENT_NAME_SUFFIX);
		});

		assertThat(mde.getMessage(), is(notNullValue()));
	}
	
	@Test
	void testGetMetaDataForBindingThrowsOnCompoundBinding() {
		MetaDataException mde = assertThrows(MetaDataException.class, () -> {
			BindUtil.getMetaDataForBinding(c, m, aapd, "bogusPropertyName" + ChildBean.CHILD_PARENT_NAME_SUFFIX);
		});

		assertThat(mde.getMessage(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("java:S5961")
	void testGetMetaDataForBinding() {
		org.skyve.metadata.module.Module admin = c.getModule(Contact.MODULE_NAME);
		Document user = admin.getDocument(c, User.DOCUMENT_NAME);
		Document userRole = admin.getDocument(c, UserRole.DOCUMENT_NAME);
		
		assertNotNull(BindUtil.getMetaDataForBinding(c, admin, userRole, UserRole.roleNamePropertyName));
		BindUtil.getMetaDataForBinding(c, admin, userRole, ChildBean.PARENT_NAME);
		BindUtil.getMetaDataForBinding(c, admin, userRole, BindUtil.createCompoundBinding(ChildBean.PARENT_NAME, Bean.DOCUMENT_ID));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIndexedBinding(User.rolesPropertyName, 0), ChildBean.PARENT_NAME));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIdBinding(User.rolesPropertyName, "ID"), ChildBean.PARENT_NAME));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIndexedBinding(User.rolesPropertyName, 0), ChildBean.PARENT_NAME, Bean.DOCUMENT_ID));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIdBinding(User.rolesPropertyName, "ID"), ChildBean.PARENT_NAME, Bean.DOCUMENT_ID));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIndexedBinding(User.rolesPropertyName, 0), ChildBean.PARENT_NAME, UserProxy.userNamePropertyName));
		BindUtil.getMetaDataForBinding(c, admin, user, BindUtil.createCompoundBinding(BindUtil.createIdBinding(User.rolesPropertyName, "ID"), ChildBean.PARENT_NAME, UserProxy.userNamePropertyName));
	}
}
