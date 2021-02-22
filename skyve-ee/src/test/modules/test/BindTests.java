package modules.test;

import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.MapBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import modules.test.domain.AllAttributesPersistent;

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
	public void testSimpleMapBeanProperty() {
		Map<String, Object> map = new TreeMap<>();
		map.put(User.userNamePropertyName, "mike");
		MapBean bean = new MapBean(User.MODULE_NAME, User.DOCUMENT_NAME, map);
		Assert.assertEquals("mike", Binder.get(bean, User.userNamePropertyName)); 
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCompoundMapBeanProperty() {
		String binding = Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName);
		Map<String, Object> map = new TreeMap<>();
		map.put(binding, "mike");
		MapBean bean = new MapBean(User.MODULE_NAME, User.DOCUMENT_NAME, map);
		Assert.assertEquals("mike", Binder.get(bean, binding)); 
	}
	
	@Test
	public void testSimpleThisProperty() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DocumentQuery.THIS_ALIAS, aap);
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
	}
	
	@Test
	public void testCompoundThisProperty() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DocumentQuery.THIS_ALIAS, aap);
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	public void testSimpleMapPropertyOverThisProperty() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DocumentQuery.THIS_ALIAS, aap);
		map.put(AllAttributesPersistent.booleanFlagPropertyName, null);
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
		Assert.assertFalse(Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
	}

	@Test
	public void testCompoundMapPropertyOverThisProperty() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(DocumentQuery.THIS_ALIAS, aap);
		map.put(binding, null);
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
		Assert.assertFalse(Binder.get(bean, binding) instanceof Boolean);
	}
	
	@Test
	public void testCompoundPropertyWithoutMappedProperty2Deep() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(AllAttributesPersistent.aggregatedAssociationPropertyName, aap);
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
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
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
		Assert.assertTrue(Binder.get(bean, binding) instanceof Boolean);
	}

	@Test
	public void testCompoundPropertyWithoutMappedProperty4Deep() throws Exception {
		String binding = Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.aggregatedAssociationPropertyName,
														AllAttributesPersistent.booleanFlagPropertyName);
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		Map<String, Object> map = new TreeMap<>();
		map.put(Binder.createCompoundBinding(AllAttributesPersistent.aggregatedAssociationPropertyName,
												AllAttributesPersistent.aggregatedAssociationPropertyName),
					aap);
		MapBean bean = new MapBean(m.getName(), aapd.getName(), map);
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
		CORE.getStash().put("text", "Test");
		CORE.getUser().getAttributes().put("text", "Test");
		
		Assert.assertEquals("TestUser", Binder.formatMessage("{USER}", bean));
		Assert.assertEquals("TestUser", Binder.formatMessage("{USERID}", bean));
		Assert.assertEquals("", Binder.formatMessage("{USERNAME}", bean));
		Assert.assertEquals("", Binder.formatMessage("{DATAGROUPID}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{CONTACTID}", bean));
		Assert.assertEquals("bizhub", Binder.formatMessage("{CUSTOMER}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{DATE}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{TIME}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{DATETIME}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{TIMESTAMP}", bean));
		Assert.assertEquals(Util.getDocumentUrl(bean), Binder.formatMessage("{URL}", bean));
		
		Assert.assertEquals("Test", Binder.formatMessage("{text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{ text }", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean:text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean: text }", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{bean : text }", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{el:bean.text}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{el:stash['text']}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{el:user.attributes['text']}", bean));
		Assert.assertEquals("", Binder.formatMessage("{el:stash['nothing']}", bean));
		Assert.assertEquals("", Binder.formatMessage("{el:user.attributes['nothing']}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{el:DATE}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{el:DATE.set(DATE.toLocalDate().plusDays(1))}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{el:TIME}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{el:DATETIME}", bean));
		Assert.assertNotEquals("", Binder.formatMessage("{el:TIMESTAMP}", bean));
		Assert.assertEquals("some.non-existent.key", Binder.formatMessage("{i18n:some.non-existent.key}", bean));
		Assert.assertEquals("Yes", Binder.formatMessage("{role:admin.BasicUser}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{stash:text}", bean));
		Assert.assertEquals("", Binder.formatMessage("{stash:nothing}", bean));
		Assert.assertEquals("Test", Binder.formatMessage("{user:text}", bean));
		Assert.assertEquals("", Binder.formatMessage("{user:nothing}", bean));
	}
	
	@Test
	public void testExpressionValidation() throws Exception {
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{USER}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{USERID}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{USERNAME}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{DATAGROUPID}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{CONTACTID}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{CUSTOMER}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{DATE}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{TIME}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{DATETIME}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{TIMESTAMP}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{URL}"));
		
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{text}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{ text }"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{bean:text}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{bean: text }"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{bean : text }"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:bean.text}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:stash['text']}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:user.attributes['text']}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:stash['nothing']}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:user.attributes['nothing']}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:DATE}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:DATE.set(DATE.toLocalDate().plusDays(1))}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:TIME}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:DATETIME}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{el:TIMESTAMP}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{i18n:some.non-existent.key}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{role:admin.BasicUser}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{stash:text}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{stash:nothing}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{user:text}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "{user:nothing}"));
		
		Assert.assertFalse(BindUtil.messageExpressionsAreValid(c, m, aapd, "{"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "}"));
		Assert.assertFalse(BindUtil.messageExpressionsAreValid(c, m, aapd, "{}"));
		Assert.assertFalse(BindUtil.messageExpressionsAreValid(c, m, aapd, "{text\\}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "\\{text}"));
		Assert.assertTrue(BindUtil.messageExpressionsAreValid(c, m, aapd, "\\{{text}"));
	}
}
