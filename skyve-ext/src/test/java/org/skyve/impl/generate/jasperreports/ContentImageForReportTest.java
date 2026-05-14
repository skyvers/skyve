package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.awt.image.BufferedImage;

import org.junit.jupiter.api.Test;

/**
 * Tests the ContentImageForReport paths that do not require a real content manager or customer logo file.
 */
@SuppressWarnings("static-method")
public class ContentImageForReportTest {

	@Test
	public void imageWithBlankContentIdReturnsNonNullBufferedImage() throws Exception {
		// Blank contentId \u2192 smallBlankImage() path, no content manager used.
		BufferedImage result = ContentImageForReport.image("", 100, 100);
		assertThat(result, notNullValue());
	}

	@Test
	public void imageWithNullContentIdReturnsNonNullBufferedImage() throws Exception {
		// null contentId \u2192 StringUtils.isBlank(null) is true \u2192 smallBlankImage() path.
		BufferedImage result = ContentImageForReport.image(null, 100, 100);
		assertThat(result, notNullValue());
	}

	@Test
	public void imageWithWhitespaceContentIdReturnsNonNullBufferedImage() throws Exception {
		BufferedImage result = ContentImageForReport.image("   ", 50, 50);
		assertThat(result, notNullValue());
	}
}
