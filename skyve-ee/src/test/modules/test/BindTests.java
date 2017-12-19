package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.impl.bind.BindUtil;

public class BindTests {
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
}
