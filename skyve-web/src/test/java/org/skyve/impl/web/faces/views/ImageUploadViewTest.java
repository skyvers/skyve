package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class ImageUploadViewTest {
	@Test
	void constructorUsesImageUploadWhitelistAndLimits() {
		ImageUploadView view = new ImageUploadView();

		assertEquals(UtilImpl.UPLOADS_IMAGE_WHITELIST_REGEX, view.getWhitelistRegex());
		assertEquals(UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * AbstractUploadView.MB_IN_BYTES,
					 view.getMaximumSizeInBytes());
	}
}
