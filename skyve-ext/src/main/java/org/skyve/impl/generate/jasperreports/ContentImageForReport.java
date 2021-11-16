package org.skyve.impl.generate.jasperreports;

import net.coobird.thumbnailator.Thumbnails;
import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.metadata.customer.Customer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.InputStream;

/**
 * A class used to generate a dynamic image of a content image.
 */
public class ContentImageForReport {
	private static final Logger logger = LoggerFactory.getLogger(ContentImageForReport.class);

	private static BufferedImage smallBlankImage() {
		return new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
	}

	public static BufferedImage image(String contentId, int imageWidth, int imageHeight) throws Exception {
		if (StringUtils.isBlank(contentId)) {
			logger.error("Attempted to generate image from null content id, returning blank image.");
			return smallBlankImage();
		}

		try (ContentManager cm = EXT.newContentManager()) {
			final AttachmentContent content = cm.getAttachment(contentId);

			if (content != null) {
				try (InputStream in = content.getContentStream()) {
					return Thumbnails.of(in)
							.width(imageWidth)
							.height(imageHeight)
							.keepAspectRatio(true)
							.asBufferedImage();
				}
			}
			
			logger.warn("Content with ID {} does not exist in the repository, returning blank image.", contentId);
			return smallBlankImage();
		}
	}

	public static BufferedImage customerLogo(int imageWidth, int imageHeight) throws Exception {
		final Customer customer = CORE.getCustomer();
		final File logo = CORE.getRepository().findResourceFile(customer.getUiResources().getLogoRelativeFileName(),
																	customer.getName(),
																	null);

		return Thumbnails.of(logo)
				.width(imageWidth)
				.height(imageHeight)
				.keepAspectRatio(true)
				.asBufferedImage();
	}
}
