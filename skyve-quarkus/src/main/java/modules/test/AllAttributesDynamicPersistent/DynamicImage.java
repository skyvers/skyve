package modules.test.AllAttributesDynamicPersistent;

import java.awt.image.BufferedImage;

import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.user.User;

public class DynamicImage implements org.skyve.metadata.model.document.DynamicImage<DynamicPersistentBean> {
	@Override
	public BufferedImage getImage(DynamicPersistentBean bean, int width, int height, User user) throws Exception {
		return new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
	}

	@Override
	public ImageFormat getFormat() {
		return null;
	}

	@Override
	public Float getCompressionQuality() {
		return null;
	}

}
