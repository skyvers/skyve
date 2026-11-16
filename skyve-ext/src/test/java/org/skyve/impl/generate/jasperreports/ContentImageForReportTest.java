package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

/**
 * Tests the ContentImageForReport paths that do not require a real content manager or customer logo file.
 */
@SuppressWarnings("static-method")
class ContentImageForReportTest {
	private final Class<? extends AbstractContentManager> originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;

	@AfterEach
	void tearDown() {
		RecordingContentManager.attachments.clear();
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
	}

	@Test
	void imageWithBlankContentIdReturnsNonNullBufferedImage() throws Exception {
		// Blank contentId \u2192 smallBlankImage() path, no content manager used.
		BufferedImage result = ContentImageForReport.image("", 100, 100);
		assertThat(result, notNullValue());
	}

	@Test
	void imageWithNullContentIdReturnsNonNullBufferedImage() throws Exception {
		// null contentId \u2192 StringUtils.isBlank(null) is true \u2192 smallBlankImage() path.
		BufferedImage result = ContentImageForReport.image(null, 100, 100);
		assertThat(result, notNullValue());
	}

	@Test
	void imageWithWhitespaceContentIdReturnsNonNullBufferedImage() throws Exception {
		BufferedImage result = ContentImageForReport.image("   ", 50, 50);
		assertThat(result, notNullValue());
	}

	@Test
	void imageWithMissingContentReturnsBlankImage() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;

		BufferedImage result = ContentImageForReport.image("missing", 50, 50);

		assertThat(result, notNullValue());
		assertTrue(result.getWidth() > 0);
		assertTrue(result.getHeight() > 0);
	}

	@Test
	void imageWithAttachmentContentReturnsScaledImage() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		RecordingContentManager.attachments.put("content-1", pngAttachment());

		BufferedImage result = ContentImageForReport.image("content-1", 8, 8);

		assertThat(result, notNullValue());
		assertTrue(result.getWidth() > 0);
		assertTrue(result.getWidth() <= 8);
		assertTrue(result.getHeight() > 0);
		assertTrue(result.getHeight() <= 8);
	}

	@Test
	void customerLogoWithoutUiResourcesReturnsBlankImage() throws Exception {
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("demo");
		when(customer.getUiResources()).thenReturn(null);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);

		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		AbstractPersistence previous = threadLocal.get();
		try {
			threadLocal.set(persistence);
			BufferedImage result = ContentImageForReport.customerLogo(50, 50);
			assertThat(Integer.valueOf(result.getWidth()), is(Integer.valueOf(1)));
			assertThat(Integer.valueOf(result.getHeight()), is(Integer.valueOf(1)));
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(previous);
			}
		}
	}

	private static AttachmentContent pngAttachment() throws Exception {
		BufferedImage image = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB);
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		ImageIO.write(image, "png", out);
		AttachmentContent result = new AttachmentContent("demo", "admin", "User", null, "user", "bean", "image")
				.attachment("image.png", MimeType.png, out.toByteArray());
		result.setContentId("content-1");
		return result;
	}

	public static class RecordingContentManager extends NoOpContentManager {
		private static final Map<String, AttachmentContent> attachments = new HashMap<>();

		@Override
		public AttachmentContent getAttachment(String contentId) {
			return attachments.get(contentId);
		}
	}
}
