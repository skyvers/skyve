package modules.test.AllAttributesDynamicPersistent;

import java.awt.image.BufferedImage;

import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Produces a deterministic placeholder image for dynamic-image integration tests.
 */
public class TestDynamicImage implements org.skyve.metadata.model.document.DynamicImage<DynamicPersistentBean> {
	/**
	 * Creates a generated image for the supplied bean.
	 *
	 * @param bean the current bean whose image is being requested
	 * @param width the requested width
	 * @param height the requested height
	 * @param user the active user requesting the image
	 * @return a simple in-memory ARGB image
	 * @throws Exception if generation fails
	 */
	@Override
	public @Nonnull BufferedImage getImage(@Nonnull DynamicPersistentBean bean, int width, int height, @Nonnull User user) throws Exception {
		return new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
	}

	/**
	 * Returns the default image format.
	 *
	 * @return {@code null} to use the framework default format
	 */
	@Override
	public @Nullable ImageFormat getFormat() {
		return null;
	}

	/**
	 * Returns the compression quality for generated images.
	 *
	 * @return {@code null} to use default compression behaviour
	 */
	@Override
	public @Nullable Float getCompressionQuality() {
		return null;
	}
}
