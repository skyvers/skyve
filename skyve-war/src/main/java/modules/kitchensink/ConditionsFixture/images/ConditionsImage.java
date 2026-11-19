package modules.kitchensink.ConditionsFixture.images;

import java.awt.image.BufferedImage;

import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

import modules.kitchensink.domain.ConditionsFixture;

/**
 * Supplies a blank image, which is all the conditions fixture needs to put a condition on a dynamic image.
 */
public class ConditionsImage implements DynamicImage<ConditionsFixture> {
	@Override
	public BufferedImage getImage(ConditionsFixture bean, int width, int height, User user) throws Exception {
		return new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
	}
}
