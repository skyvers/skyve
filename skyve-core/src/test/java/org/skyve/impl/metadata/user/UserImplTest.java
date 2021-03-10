package org.skyve.impl.metadata.user;

import java.util.Locale;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

public class UserImplTest {
	@Test
	@SuppressWarnings("static-method")
	public void testSetLanguageTag() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		Assert.assertEquals(Locale.FRENCH, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testSetWebLocaleWithoutLanguageTag() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		Assert.assertNull(user.getLanguageTag());
		Assert.assertEquals(Locale.CHINESE, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testSetWebLocaleWithLanguageTag() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		Assert.assertEquals(Locale.FRENCH, user.getLocale());
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testLocaleSerialization() {
		UserImpl user = new UserImpl();
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user = UtilImpl.cloneBySerialization(user);
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setWebLocale(Locale.CHINESE);
		user = UtilImpl.cloneBySerialization(user);
		Assert.assertNull(user.getLocale());
		Assert.assertNull(user.getLanguageTag());
		user.setLanguageTag(Locale.FRENCH.getLanguage());
		user = UtilImpl.cloneBySerialization(user);
		Assert.assertEquals(Locale.FRENCH.getLanguage(), user.getLanguageTag());
		Assert.assertEquals(Locale.FRENCH, user.getLocale());
	}
}
