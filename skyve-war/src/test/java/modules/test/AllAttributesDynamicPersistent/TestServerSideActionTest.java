package modules.test.AllAttributesDynamicPersistent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicPersistentBean;

@SuppressWarnings("static-method")
public class TestServerSideActionTest {

	@Test
	void executeReturnsBeanResult() throws Exception {
		TestServerSideAction action = new TestServerSideAction();
		DynamicPersistentBean bean = new DynamicPersistentBean("test", "AllAttributesDynamicPersistent", Map.of());
		var result = action.execute(bean, null);
		assertNotNull(result);
		assertNotNull(result.getBean());
	}

	@Test
	void testDynamicImageGetImageReturnsBufferedImage() throws Exception {
		TestDynamicImage image = new TestDynamicImage();
		var bufferedImage = image.getImage(new DynamicPersistentBean("test", "AllAttributesDynamicPersistent", Map.of()), 100, 100, null);
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
