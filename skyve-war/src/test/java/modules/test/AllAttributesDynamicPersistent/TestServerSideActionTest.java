package modules.test.AllAttributesDynamicPersistent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

import java.util.HashMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class TestServerSideActionTest {

	@Test
	void executeReturnsBeanResult() throws Exception {
		TestServerSideAction action = new TestServerSideAction();
		DynamicPersistentBean bean = new DynamicPersistentBean("test", "AllAttributesDynamicPersistent", new HashMap<>());
		var result = action.execute(bean, null);
		assertNotNull(result);
		assertNotNull(result.getBean());
	}

	@Test
	void testDynamicImageGetImageReturnsBufferedImage() throws Exception {
		TestDynamicImage image = new TestDynamicImage();
		var bufferedImage = image.getImage(new DynamicPersistentBean("test", "AllAttributesDynamicPersistent", new HashMap<>()), 100, 100, mock(User.class));
		assertNotNull(bufferedImage);
	}

	@Test
	void testDynamicImageGetFormatReturnsNull() {
		TestDynamicImage image = new TestDynamicImage();
		assertNull(image.getFormat());
	}

	@Test
	void testDynamicImageGetCompressionQualityReturnsNull() {
		TestDynamicImage image = new TestDynamicImage();
		assertNull(image.getCompressionQuality());
	}
}
